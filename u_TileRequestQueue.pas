unit u_TileRequestQueue;

interface

uses
  Windows,
  SysUtils,
  i_Notify,
  i_TileRequest,
  i_TileRequestQueue,
  i_TTLCheckListener,
  i_TTLCheckNotifier;

type
  TTileRequestQueue = class(TInterfacedObject, ITileRequestQueue)
  private
    type
    TArrayOfITileRequest = array of ITileRequest;
  private
    FCapacity: Integer;
    FGCList: ITTLCheckNotifier;
    FAppClosingNotifier: INotifier;

    FTTLListener: ITTLCheckListener;
    FAppClosingListener: IListener;

    FCapasitySemaphore: THandle;
    FReadyRequestSemaphore: THandle;
    FStopThreadEventHandle: THandle;

    FSize: Integer;
    FHeadIndex: Integer;
    FTailIndex: Integer;

    FRequestArray: TArrayOfITileRequest;
    FRequestArrayCS: IReadWriteSync;

    procedure OnTTLTrim(Sender: TObject);
    function GetOrInitArray: TArrayOfITileRequest;
    procedure OnClosing;
  protected
    procedure Push(const ARequest: ITileRequest);
    function Pull: ITileRequest;
  public
    constructor Create(
      const AGCList: ITTLCheckNotifier;
      const AAppClosingNotifier: INotifier;
      ACapacity: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_NotifyEventListener,
  u_TTLCheckListener;

{ TTileRequestQuery }

constructor TTileRequestQueue.Create(
  const AGCList: ITTLCheckNotifier;
  const AAppClosingNotifier: INotifier;
  ACapacity: Integer
);
begin
  inherited Create;
  FGCList := AGCList;
  FAppClosingNotifier := AAppClosingNotifier;
  FCapacity := ACapacity;

  FSize := 0;
  FHeadIndex := 0;
  FTailIndex := 0;

  FCapasitySemaphore := CreateSemaphore(nil, ACapacity, ACapacity, nil);
  FReadyRequestSemaphore := CreateSemaphore(nil, 0, ACapacity, nil);
  FStopThreadEventHandle := CreateEvent(nil, TRUE, FALSE, nil);
  FRequestArrayCS := MakeSyncRW_Std(Self);

  FTTLListener := TTTLCheckListener.Create(Self.OnTTLTrim, 100000, 1000);
  FGCList.Add(FTTLListener);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
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
  VSize := InterlockedCompareExchange(FSize, 0, 0);
  if VSize > 0 then begin
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

function TTileRequestQueue.Pull: ITileRequest;
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
      VSize := InterlockedDecrement(FSize);
      if VSize < 0 then begin
        InterlockedIncrement(FSize);
        raise Exception.Create('Почему-то пусто, а должно что-то быть');
      end;
      VIndex := InterlockedIncrement(FHeadIndex);
      VIndex := VIndex mod FCapacity;
      VArray := GetOrInitArray;
      Result := VArray[VIndex];
      VArray[VIndex] := nil;
      ReleaseSemaphore(FCapasitySemaphore, 1, nil);
    end;
  end;
end;

procedure TTileRequestQueue.Push(const ARequest: ITileRequest);
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
      VSize := InterlockedIncrement(FSize);
      if VSize > FCapacity then begin
        InterlockedDecrement(FSize);
        raise Exception.Create('Полностью заполнено. Странно.');
      end;
      VIndex := InterlockedIncrement(FTailIndex);
      VIndex := VIndex mod FCapacity;
      VArray := GetOrInitArray;
      VArray[VIndex] := ARequest;
      ReleaseSemaphore(FReadyRequestSemaphore, 1, nil);
    end;
  end;
end;

end.
