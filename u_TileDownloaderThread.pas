unit u_TileDownloaderThread;

interface

uses
  Windows,
  Classes,
  i_TileError,
  i_TileDownloader,
  u_MapType,
  u_TileDownloaderEventElement;

type
  TTileDownloaderThread = class (TThread)
  protected
    FMapType: TMapType;
    FErrorLogger: ITileErrorLogger;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FMaxRequestCount: Integer;
    FSemaphore: THandle;
    function  GetNewEventElement(ATile: TPoint; AZoom: Byte; ACheckExistsTileSize: Boolean; AExistsTileSize: Cardinal): ITileDownloaderEvent;
    procedure Download(ATile: TPoint; AZoom: Byte; ACheckExistsTileSize: Boolean = False; AExistsTileSize: Cardinal = 0);
  public
    constructor Create(
      ACreateSuspended: Boolean;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger;
      AMaxRequestCount: Integer
    );
    destructor Destroy; override;
    procedure OnTileDownload(AEvent: ITileDownloaderEvent); virtual;
  end;

implementation

{ TTileDownloaderThread }

constructor TTileDownloaderThread.Create(
  ACreateSuspended: Boolean;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger;
  AMaxRequestCount: Integer);
begin
  inherited Create(ACreateSuspended);
  FMapType := nil;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FMaxRequestCount := AMaxRequestCount;
  FSemaphore := CreateSemaphore(nil, FMaxRequestCount, FMaxRequestCount, nil);
end;

destructor TTileDownloaderThread.Destroy;
begin
  CloseHandle(FSemaphore);
  inherited Destroy;
end;

procedure TTileDownloaderThread.OnTileDownload(AEvent: ITileDownloaderEvent);
begin
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

function TTileDownloaderThread.GetNewEventElement(ATile: TPoint; AZoom: Byte;
  ACheckExistsTileSize: Boolean; AExistsTileSize: Cardinal): ITileDownloaderEvent;
begin
  Result := TTileDownloaderEventElement.Create(FMapTileUpdateEvent, FErrorLogger, FMapType);
  Result.AddToCallBackList(Self.OnTileDownload);
  Result.TileXY := ATile;
  Result.TileZoom := AZoom;
  Result.CheckTileSize := ACheckExistsTileSize;
  Result.OldTileSize := AExistsTileSize;
end;

procedure TTileDownloaderThread.Download(ATile: TPoint; AZoom: Byte;
  ACheckExistsTileSize: Boolean = False; AExistsTileSize: Cardinal = 0);
begin
  repeat
    if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0  then
      Break
    else
      if Terminated then
        Break;
  until False;
  if not Terminated then
    FMapType.DownloadTile( GetNewEventElement(ATile, AZoom, ACheckExistsTileSize, AExistsTileSize) );
end;

end.
