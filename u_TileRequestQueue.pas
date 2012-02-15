unit u_TileRequestQueue;

interface

uses
  Windows,
  SyncObjs,
  i_JclNotify,
  i_TileRequest,
  i_TileRequestQueue,
  i_TTLCheckListener,
  i_TTLCheckNotifier;

type
  TTileRequestQueue = class(TInterfacedObject, ITileRequestQueue)
  private type
    TArrayOfITileRequest = array of ITileRequest;
  private
    FCapacity: Integer;
    FGCList: ITTLCheckNotifier;
    FAppClosingNotifier: IJclNotifier;

    FTTLListener: ITTLCheckListener;
    FAppClosingListener: IJclListener;
    FCS: TCriticalSection;
    FCapasitySemaphore: THandle;
    FReadyRequestSemaphore: THandle;
    FStopThreadEvent: TEvent;

    FSize: Integer;
    FHeadIndex: Integer;
    FTailIndex: Integer;

    FRequestArray: TArrayOfITileRequest;
    FRequestArrayCS: TCriticalSection;

    procedure OnTTLTrim(Sender: TObject);
    function GetOrInitArray: TArrayOfITileRequest;
    procedure OnClosing;
  protected
    procedure Push(ARequest: ITileRequest);
    function Pull: ITileRequest;
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      AAppClosingNotifier: IJclNotifier;
      ACapacity: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener,
  u_TTLCheckListener;

{ TTileRequestQuery }

constructor TTileRequestQueue.Create(
  AGCList: ITTLCheckNotifier;
  AAppClosingNotifier: IJclNotifier;
  ACapacity: Integer
);
begin
  FGCList := AGCList;
  FAppClosingNotifier := AAppClosingNotifier;
  FCapacity := ACapacity;
  FCS := TCriticalSection.Create;
  FSize := 0;
  FHeadIndex := 0;
  FTailIndex := 0;

  FCapasitySemaphore := CreateSemaphore(nil, ACapacity, ACapacity, nil);
  FReadyRequestSemaphore := CreateSemaphore(nil, 0, ACapacity, nil);
  FStopThreadEvent := TEvent.Create;
  FRequestArrayCS := TCriticalSection.Create;

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

  FreeAndNil(FRequestArrayCS);
  FreeAndNil(FCS);

  FreeAndNil(FStopThreadEvent);
  CloseHandle(FCapasitySemaphore);
  CloseHandle(FReadyRequestSemaphore);
  inherited;
end;

function TTileRequestQueue.GetOrInitArray: TArrayOfITileRequest;
begin
  FRequestArrayCS.Acquire;
  try
    Result := FRequestArray;
  finally
    FRequestArrayCS.Release;
  end;
  if Result = nil then begin
    FCS.Acquire;
    try
      FRequestArrayCS.Acquire;
      try
        Result := FRequestArray;
      finally
        FRequestArrayCS.Release;
      end;
      if Result = nil then begin
        SetLength(Result, FCapacity);
        FRequestArrayCS.Acquire;
        try
          FRequestArray := Result;
        finally
          FRequestArrayCS.Release;
        end;
      end;
    finally
      FCS.Release;
    end;
  end;
end;

procedure TTileRequestQueue.OnClosing;
begin
  FStopThreadEvent.SetEvent;
end;

procedure TTileRequestQueue.OnTTLTrim(Sender: TObject);
var
  VSize: Integer;
  i: Integer;
  VRequestArray: TArrayOfITileRequest;
begin
  VSize := InterlockedCompareExchange(FSize, 0, 0);
  if VSize > 0 then begin
    FCS.Acquire;
    try
      FRequestArrayCS.Acquire;
      try
        VRequestArray := FRequestArray;
        FRequestArray := nil;
      finally
        FRequestArrayCS.Release;
      end;
    finally
      FCS.Release;
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
  VHandles[1] := FStopThreadEvent.Handle;
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

procedure TTileRequestQueue.Push(ARequest: ITileRequest);
var
  VIndex: Integer;
  VArray: TArrayOfITileRequest;
  VSize: Integer;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  VHandles[0] := FCapasitySemaphore;
  VHandles[1] := FStopThreadEvent.Handle;
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
