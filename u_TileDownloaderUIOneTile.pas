unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  Classes,
  i_TileError,
  u_MapType,
  u_TileDownloaderThread;

type
  TTileDownloaderUIOneTile = class(TTileDownloaderThread)
  private
    FLoadXY: TPoint;
    FZoom: Byte;
  protected
    procedure Execute; override;
  public
    constructor Create(
      AXY: TPoint;
      AZoom: byte;
      AMapType: TMapType;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger
    );
  end;

implementation

uses
  SysUtils,
  u_TileErrorInfo;

constructor TTileDownloaderUIOneTile.Create(
  AXY: TPoint;
  AZoom: byte;
  AMapType: TMapType;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger
);
begin
  inherited Create(False, AMapTileUpdateEvent, AErrorLogger, 1);
  FMapType := AMapType;
  FLoadXY := AXY;
  FZoom := AZoom;
  Priority := tpLower;
  FreeOnTerminate := True;
  randomize;
end;


procedure TTileDownloaderUIOneTile.Execute;
begin
  if FMapType.UseDwn then
  try
    Download(FLoadXY, FZoom, OnTileDownload);
  except
    on E: Exception do
      FErrorLogger.LogError( TTileErrorInfo.Create(FMapType, FZoom, FLoadXY, E.Message) );
  end;
end;

end.
