unit u_TileDownloaderEventProcessor;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  i_TileDownlodSession,
  u_MapLayerShowError,
  u_MapType;

type
  TEventRec = record
    TileMap    : TMapType;
    TileXY     : TPoint;
    TileZoom   : Byte;
    TileSize   : Int64;
    DownResult : TDownloadTileResult;
  end;

  TTileDownloaderEventProcessor = class (TTHread)
  private
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorShowLayer: TTileErrorInfoLayer;
    FEventBuffer: array of TEventRec;
    FEventCount: Integer;
    FThreadSafe: TCriticalSection;
    procedure Lock;
    procedure Unlock;
    procedure ProcessEventBuffer;
    procedure ProcessSingleEvent(EventId: Integer);
    function  GetErrStr(Aerr: TDownloadTileResult): string;
  protected
    procedure Execute; override;
  public
    constructor Create(AMapTileUpdateEvent: TMapTileUpdateEvent; AErrorShowLayer: TTileErrorInfoLayer);
    destructor Destroy; override;
    procedure Process(AMapType: TMapType; ATile: TPoint; AZoom: Byte; ATileSize: Int64; AResult: TDownloadTileResult);
  end;

implementation

uses
  u_GlobalState,
  u_ResStrings;

const
  EventBufferIncStep = 32;

{ TTileDownloaderEventProcessor }

constructor TTileDownloaderEventProcessor.Create(AMapTileUpdateEvent: TMapTileUpdateEvent; AErrorShowLayer: TTileErrorInfoLayer);
begin
  inherited Create(True);
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorShowLayer := AErrorShowLayer;
  FEventCount := 0;
  FThreadSafe := TCriticalSection.Create;
  SetLength(FEventBuffer, EventBufferIncStep);
  Self.FreeOnTerminate := True;
  Self.Resume;
end;

destructor TTileDownloaderEventProcessor.Destroy;
begin
  FThreadSafe.Free;
  inherited Destroy;
end;

procedure TTileDownloaderEventProcessor.Lock;
begin
  FThreadSafe.Acquire;
end;

procedure TTileDownloaderEventProcessor.Unlock;
begin
  FThreadSafe.Release;
end;

procedure TTileDownloaderEventProcessor.Process(AMapType: TMapType; ATile: TPoint; AZoom: Byte; ATileSize: Int64; AResult: TDownloadTileResult);
begin
    Lock;
  try
    if Length(FEventBuffer) <= FEventCount then
      SetLength(FEventBuffer, Length(FEventBuffer) + EventBufferIncStep);

    with FEventBuffer[FEventCount] do
    begin
      TileMap    := AMapType;
      TileXY     := ATile;
      TileZoom   := AZoom;
      TileSize   := ATileSize;
      DownResult := AResult;
    end;

    Inc(FEventCount);

  finally
    Unlock;
  end;
end;

procedure TTileDownloaderEventProcessor.ProcessSingleEvent(EventId: Integer);
var
  VErrorString: string;
begin
  VErrorString := GetErrStr(FEventBuffer[EventId].DownResult);
  if (FEventBuffer[EventId].DownResult = dtrOK) or (FEventBuffer[EventId].DownResult = dtrSameTileSize) then begin
    GState.DownloadInfo.Add(1, FEventBuffer[EventId].TileSize);
  end;
  if VErrorString <> '' then begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.ShowError(FEventBuffer[EventId].TileXY, FEventBuffer[EventId].TileZoom, FEventBuffer[EventId].TileMap, VErrorString);
    end;
  end else begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.Visible := False;
    end;
    if Addr(FMapTileUpdateEvent) <> nil then begin
      FMapTileUpdateEvent(FEventBuffer[EventId].TileMap, FEventBuffer[EventId].TileZoom, FEventBuffer[EventId].TileXY);
    end;
  end;
end;

procedure TTileDownloaderEventProcessor.ProcessEventBuffer;
var
  i: Integer;
begin
  try
    for i := 0 to FEventCount - 1 do
      ProcessSingleEvent(i);
  finally
    FEventCount := 0;
  end;
end;

procedure TTileDownloaderEventProcessor.Execute;
begin
  repeat
    if Terminated then
      Break;

      Lock;
    try
      if FEventCount > 0 then
        Synchronize(ProcessEventBuffer);
    finally
      Unlock;
    end;

     Sleep(60);

  until False;
end;

function TTileDownloaderEventProcessor.GetErrStr(Aerr: TDownloadTileResult): string;
begin
  case Aerr of
    dtrProxyAuthError:
    begin
      result := SAS_ERR_Authorization;
    end;
    dtrBanError:
    begin
      result := SAS_ERR_Ban;
    end;
    dtrTileNotExists:
    begin
      result := SAS_ERR_TileNotExists;
    end;
    dtrDownloadError,
    dtrErrorInternetOpen,
    dtrErrorInternetOpenURL:
    begin
      result := SAS_ERR_Noconnectionstointernet;
    end;
    dtrErrorMIMEType:
    begin
      result := SAS_ERR_TileDownloadContentTypeUnexpcted;
    end;
    dtrUnknownError:
    begin
      Result := SAS_ERR_TileDownloadUnexpectedError;
    end else begin
    result := '';
  end;
  end;
end;

end.
