unit u_InterfaceQueue;

interface

uses
  Windows,
  SysUtils,
  i_NotifierOperation,
  i_Listener,
  i_InterfaceQueue,
  u_BaseInterfacedObject;

type
  TInterfaceQueue = class(TBaseInterfacedObject, IInterfaceQueue)
  private
    FCapacity: Integer;
    FAppClosingNotifier: INotifierOneOperation;

    FAppClosingListener: IListener;

    FCapasitySemaphore: THandle;
    FReadyRequestSemaphore: THandle;
    FStopThreadEventHandle: THandle;

    FHeadCS: IReadWriteSync;
    FHeadIndex: Integer;

    FTailCS: IReadWriteSync;
    FTailIndex: Integer;

    FRequestArray: array of Pointer;

    procedure OnClosing;
  private
    { IInterfaceQueue }
    procedure Push(const AObj: IInterface);
    function Pull: IInterface;
    function GetIsEmpty: Boolean;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      ACapacity: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_ListenerByEvent;

{ TInterfaceQueue }

constructor TInterfaceQueue.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  ACapacity: Integer
);
begin
  Assert(ACapacity > 0);
  inherited Create;
  FAppClosingNotifier := AAppClosingNotifier;
  FCapacity := ACapacity;

  FHeadIndex := 0;
  FTailIndex := 0;

  FHeadCS := MakeSyncRW_Var(Self, False);
  FTailCS := MakeSyncRW_Var(Self, False);

  FCapasitySemaphore := CreateSemaphore(nil, ACapacity, ACapacity, nil);
  FReadyRequestSemaphore := CreateSemaphore(nil, 0, ACapacity, nil);
  FStopThreadEventHandle := CreateEvent(nil, TRUE, FALSE, nil);

  SetLength(FRequestArray, FCapacity);

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnClosing;
  end;
end;

destructor TInterfaceQueue.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FRequestArray) - 1 do begin
    if FRequestArray[i] <> nil then begin
      IInterface(FRequestArray[i])._Release;
      FRequestArray[i] := nil;
    end;
  end;
  FRequestArray := nil;

  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingListener := nil;
    FAppClosingNotifier := nil;
  end;

  CloseHandle(FStopThreadEventHandle);
  CloseHandle(FCapasitySemaphore);
  CloseHandle(FReadyRequestSemaphore);
  inherited;
end;

procedure TInterfaceQueue.OnClosing;
begin
  SetEvent(FStopThreadEventHandle);
end;

function TInterfaceQueue.Pull: IInterface;
var
  VIndex: Integer;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  Result := nil;
  VHandles[0] := FReadyRequestSemaphore;
  VHandles[1] := FStopThreadEventHandle;
  VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, 1000);
  case VWaitResult of
    WAIT_OBJECT_0: begin
      FHeadCS.BeginWrite;
      try
        VIndex := FHeadIndex;
        Inc(FHeadIndex);
        if FHeadIndex >= FCapacity then begin
          FHeadIndex := 0;
        end;
        Result := IInterface(FRequestArray[VIndex]);
        FRequestArray[VIndex] := nil;
      finally
        FHeadCS.EndWrite;
      end;
      Result._Release;
      ReleaseSemaphore(FCapasitySemaphore, 1, nil);
    end;
  end;
end;

procedure TInterfaceQueue.Push(const AObj: IInterface);
var
  VIndex: Integer;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  VHandles[0] := FCapasitySemaphore;
  VHandles[1] := FStopThreadEventHandle;
  VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
  case VWaitResult of
    WAIT_OBJECT_0: begin
      FTailCS.BeginWrite;
      try
        VIndex := FTailIndex;
        Inc(FTailIndex);
        if FTailIndex >= FCapacity then begin
          FTailIndex := 0;
        end;
        FRequestArray[VIndex] := Pointer(AObj);
      finally
        FTailCS.EndWrite;
      end;
      AObj._AddRef;
      ReleaseSemaphore(FReadyRequestSemaphore, 1, nil);
    end;
  end;
end;

function TInterfaceQueue.GetIsEmpty: Boolean;
begin
  FTailCS.BeginRead;
  try
    FHeadCS.BeginRead;
    try
      Result := FHeadIndex = FTailIndex;
    finally
      FHeadCS.EndRead;
    end;
  finally
    FTailCS.EndRead;
  end;
end;

end.
