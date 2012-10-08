unit u_InterfaceQueue;

interface

uses
  Windows,
  i_SimpleFlag,
  i_NotifierOperation,
  i_Listener,
  i_InterfaceQueue;

type
  TInterfaceQueue = class(TInterfacedObject, IInterfaceQueue)
  private
    FCapacity: Integer;
    FAppClosingNotifier: INotifierOneOperation;

    FAppClosingListener: IListener;

    FCapasitySemaphore: THandle;
    FReadyRequestSemaphore: THandle;
    FStopThreadEventHandle: THandle;

    FHeadIndex: ICounter;
    FTailIndex: ICounter;

    FRequestArray: array of Pointer;

    procedure OnClosing;
  private
    procedure Push(const AObj: IInterface);
    function Pull: IInterface;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      ACapacity: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_SimpleFlagWithInterlock,
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

  FHeadIndex := TCounterInterlock.Create;
  FTailIndex := TCounterInterlock.Create;

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
    FRequestArray[i] := nil;
  end;
  FRequestArray := nil;

  FAppClosingNotifier.Remove(FAppClosingListener);
  FAppClosingListener := nil;
  FAppClosingNotifier := nil;

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
      VIndex := FHeadIndex.Inc;
      VIndex := VIndex mod FCapacity;
      Result := IInterface(FRequestArray[VIndex]);
      FRequestArray[VIndex] := nil;
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
      VIndex := FTailIndex.Inc;
      VIndex := VIndex mod FCapacity;
      FRequestArray[VIndex] := Pointer(AObj);
      AObj._AddRef;
      ReleaseSemaphore(FReadyRequestSemaphore, 1, nil);
    end;
  end;
end;

end.
