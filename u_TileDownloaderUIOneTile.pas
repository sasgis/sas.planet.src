unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  Classes,
  Types,
  i_TileError,
  u_TileDownloaderThreadBase,
  u_MapType;

type
  TTileDownloaderUIOneTile = class(TTileDownloaderThreadBase)
  private
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorLogger: ITileErrorLogger;

    procedure AfterWriteToFile;
  protected
    procedure Execute; override;
  public
    constructor Create(
      AXY: TPoint;
      AZoom: byte;
      AMapType: TMapType;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger
    ); overload;
  end;

implementation

uses
  SysUtils,
  u_GlobalState,
  i_DownloadResult,
  u_TileErrorInfo,
  u_ResStrings;

constructor TTileDownloaderUIOneTile.Create(
  AXY: TPoint;
  AZoom: byte;
  AMapType: TMapType;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger
);
begin
  inherited Create(False);
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FLoadXY := AXY;
  FZoom := AZoom;
  FMapType := AMapType;

  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

procedure TTileDownloaderUIOneTile.AfterWriteToFile;
begin
  if Addr(FMapTileUpdateEvent) <> nil then begin
    FMapTileUpdateEvent(FMapType, FZoom, FLoadXY);
  end;
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  VResult: IDownloadResult;
  VErrorString: string;
  VResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
begin
  if FMapType.UseDwn then begin
      try
        VResult := FMapType.DownloadTile(FCancelNotifier, FLoadXY, FZoom, false);
        if not Terminated then begin
          VErrorString := '';
          if Supports(VResult, IDownloadResultOk, VResultOk) then begin
            GState.DownloadInfo.Add(1, VResultOk.Size);
          end else if Supports(VResult, IDownloadResultError, VResultDownloadError) then begin
            VErrorString := VResultDownloadError.ErrorText;
          end;
        end;
      except
        on E: Exception do begin
          VErrorString := E.Message;
        end;
      end;
  end else begin
    VErrorString := SAS_ERR_NotLoads;
  end;
  if not Terminated then begin
    if VErrorString = '' then begin
      Synchronize(AfterWriteToFile);
    end else begin
      FErrorLogger.LogError(
        TTileErrorInfo.Create(
          FMapType,
          FZoom,
          FLoadXY,
          VErrorString
        )
      );
    end;
  end;
end;

end.
