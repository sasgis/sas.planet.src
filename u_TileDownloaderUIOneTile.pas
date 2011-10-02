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
    FLoadXY: TPoint;
    FZoom: Byte;
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
  inherited Create(False, ADownloadInfo, AMapTileUpdateEvent, AErrorLogger, 1);
  FLoadXY := AXY;
  FZoom := AZoom;
  FMapType := AMapType;
  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  VOperatonID: Integer;
begin
  if FMapType.Abilities.UseDownload  then
  try
    VOperatonID := FCancelNotifier.CurrentOperation;  //TODO: Заюзать VOperatonID
    Download(FLoadXY, FZoom, OnTileDownload, False, FCancelNotifier);
  except
    on E: Exception do begin
      FErrorLogger.LogError(
        TTileErrorInfo.Create(
          FMapType,
          FZoom,
          FLoadXY,
          E.Message
        )
      );
    end;
  end;
end;

end.
