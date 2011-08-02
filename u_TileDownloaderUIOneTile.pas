unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  Classes,
  Types,
  i_TileError,
  i_DownloadInfoSimple,
  u_TileDownloaderThread,
  u_MapType;

type
  TTileDownloaderUIOneTile = class(TTileDownloaderThread)
  private
    //FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorLogger: ITileErrorLogger;
    FDownloadInfo: IDownloadInfoSimple;
    FLoadXY: TPoint;
    FZoom: Byte;
    //procedure AfterWriteToFile;
  protected
    procedure Execute; override;
  public
    constructor Create(
      AXY: TPoint;
      AZoom: byte;
      AMapType: TMapType;
      ADownloadInfo: IDownloadInfoSimple;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger
    ); overload;
  end;

implementation

uses
  SysUtils,
  i_DownloadResult,
  u_TileErrorInfo,
  u_ResStrings;

constructor TTileDownloaderUIOneTile.Create(
  AXY: TPoint;
  AZoom: byte;
  AMapType: TMapType;
  ADownloadInfo: IDownloadInfoSimple;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger
);
begin
  inherited Create(False, AMapTileUpdateEvent, AErrorLogger, 1);
  //FMapTileUpdateEvent := AMapTileUpdateEvent;
  FDownloadInfo := ADownloadInfo;
  FErrorLogger := AErrorLogger;
  FLoadXY := AXY;
  FZoom := AZoom;
  FMapType := AMapType;

  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

//procedure TTileDownloaderUIOneTile.AfterWriteToFile;
//begin
//  if Addr(FMapTileUpdateEvent) <> nil then begin
//    FMapTileUpdateEvent(FMapType, FZoom, FLoadXY);
//  end;
//end;

procedure TTileDownloaderUIOneTile.Execute;
begin
  if FMapType.UseDwn then
  try
    Download(FLoadXY, FZoom, OnTileDownload, False, FCancelNotifier);
  except
    on E: Exception do
      FErrorLogger.LogError( TTileErrorInfo.Create(FMapType, FZoom, FLoadXY, E.Message) );
  end;
end;

end.
