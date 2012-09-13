unit u_TileRequestQueue;

interface

uses
  Windows,
  SysUtils,
  i_SimpleFlag,
  i_NotifierOperation,
  i_Listener,
  i_TileRequestTask,
  i_TileRequestQueue,
  i_ListenerTTLCheck,
  i_NotifierTTLCheck;

type
  TTileRequestQueue = class(TInterfacedObject, ITileRequestQueue)
  private
    type
    TArrayOfITileRequest = array of ITileRequestTask;
  private
    FCapacity: Integer;
    FGCList: INotifierTTLCheck;
    FAppClosingNotifier: INotifierOneOperation;

    FTTLListener: IListenerTTLCheck;
    FAppClosingListener: IListener;

    FCapasitySemaphore: THandle;
    FReadyRequestSemaphore: THandle;
    FStopThreadEventHandle: THandle;

    FSizeCounter: ICounter;
    FHeadIndex: ICounter;
    FTailIndex: ICounter;

    FRequestArray: TArrayOfITileRequest;
    FRequestArrayCS: IReadWriteSync;

    procedure OnTTLTrim(Sender: TObject);
    function GetOrInitArray: TArrayOfITileRequest;
    procedure OnClosing;
  private
    procedure Push(const ARequest: ITileRequestTask);
    function Pull: ITileRequestTask;
  public
    constructor Create(
      const AGCList: INotifierTTLCheck;
      const AAppClosingNotifier: INotifierOneOperation;
      ACapacity: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_ListenerTTLCheck;

{ TTileRequestQuery }

constructor TTileRequestQueue.Create(
  const AGCList: INotifierTTLCheck;
  const AAppClosingNotifier: INotifierOneOperation;
  ACapacity: Integer
);
begin
  inherited Create;
  FGCList := AGCList;
  FAppClosingNotifier := AAppClosingNotifier;
  FCapacity := ACapacity;

  FSizeCounter := TCounterInterlock.Create;
  FHeadIndex := TCounterInterlock.Create;
  FTailIndex := TCounterInterlock.Create;

  FCapasitySemaphore := CreateSemaphore(nil, ACapacity, ACapacity, nil);
  FReadyRequestSemaphore := CreateSemaphore(nil, 0, ACapacity, nil);
  FStopThreadEventHandle := CreateEvent(nil, TRUE, FALSE, nil);
  FRequestArrayCS := MakeSyncRW_Std(Self);

  FTTLListener := TListenerTTLCheck.Create(Self.OnTTLTrim, 100000, 1000);
  FGCList.Add(FTTLListener);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnClosing;
  end;
end;

destructor TTileRequestQueue.Destroy;
begin
  OnTTLTrim(nil);

  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;

  FAppClosingNotifier.Remove(FAppClosingListener);
  FAppClosingListener := nil;
  FAppClosingNotifier := nil;

  FRequestArrayCS := nil;

  CloseHandle(FStopThreadEventHandle);
  CloseHandle(FCapasitySemaphore);
  CloseHandle(FReadyRequestSemaphore);
  inherited;
end;

function TTileRequestQueue.GetOrInitArray: TArrayOfITileRequest;
begin
  FRequestArrayCS.BeginRead;
  try
    Result := FRequestArray;
  finally
    FRequestArrayCS.EndRead;
  end;

  if Result = nil then begin
    FRequestArrayCS.BeginWrite;
    try
      Result := FRequestArray;

      if Result = nil then begin
        SetLength(Result, FCapacity);
        FRequestArray := Result;
      end;
    finally
      FRequestArrayCS.EndWrite;
    end;
  end;
end;

procedure TTileRequestQueue.OnClosing;
begin
  SetEvent(FStopThreadEventHandle);
end;

procedure TTileRequestQueue.OnTTLTrim(Sender: TObject);
var
  VSize: Integer;
  i: Integer;
  VRequestArray: TArrayOfITileRequest;
begin
  VSize := FSizeCounter.GetValue;
  if VSize = 0 then begin
    FRequestArrayCS.BeginWrite;
    try
      VRequestArray := FRequestArray;
      FRequestArray := nil;
    finally
      FRequestArrayCS.EndWrite;
    end;

    for i := 0 to Length(VRequestArray) - 1 do begin
      VRequestArray[i] := nil;
    end;
    VRequestArray := nil;
  end;
end;

function TTileRequestQueue.Pull: ITileRequestTask;
var
  VIndex: Integer;
  VArray: TArrayOfITileRequest;
  VSize: Integer;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  Result := nil;
  VHandles[0] := FReadyRequestSemaphore;
  VHandles[1] := FStopThreadEventHandle;
  VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, 1000);
  case VWaitResult of
    WAIT_OBJECT_0: begin
      FTTLListener.UpdateUseTime;
      VSize := FSizeCounter.Dec;
      if VSize < 0 then begin
        FSizeCounter.Inc;
        raise Exception.Create('Почему-то пусто, а должно что-то быть');
      end;
      VIndex := FHeadIndex.Inc;
      VIndex := VIndex mod FCapacity;
      VArray := GetOrInitArray;
      Result := VArray[VIndex];
      VArray[VIndex] := nil;
      ReleaseSemaphore(FCapasitySemaphore, 1, nil);
    end;
  end;
end;

procedure TTileRequestQueue.Push(const ARequest: ITileRequestTask);
var
  VIndex: Integer;
  VArray: TArrayOfITileRequest;
  VSize: Integer;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  VHandles[0] := FCapasitySemaphore;
  VHandles[1] := FStopThreadEventHandle;
  VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
  case VWaitResult of
    WAIT_OBJECT_0: begin
      FTTLListener.UpdateUseTime;
      VSize := FSizeCounter.Inc;
      if VSize > FCapacity then begin
        FSizeCounter.Dec;
        raise Exception.Create('Полностью заполнено. Странно.');
      end;
      VIndex := FTailIndex.Inc;
      VIndex := VIndex mod FCapacity;
      VArray := GetOrInitArray;
      VArray[VIndex] := ARequest;
      ReleaseSemaphore(FReadyRequestSemaphore, 1, nil);
    end;
  end;
end;

end.
