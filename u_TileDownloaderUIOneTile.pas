unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  Classes,
  Types,
  t_LoadEvent,
  u_TileDownloaderBase,
  UMapType;

type
  TTileDownloaderUIOneTile = class(TThread)
  private
    lastLoad:TlastLoad;
    typemap:TMapType;
    LoadXY: TPoint;
    Zoom:byte;
    ErrorString:string;
    url_ifban: string;
    FDownloader: TTileDownloaderBase;
    procedure dwnOne;
    function DownloadTile(AXY: TPoint; AZoom: byte;MT:TMapType; AOldTileSize: Integer; out ty: string; fileBuf:TMemoryStream): TDownloadTileResult;
    procedure ban;
    function GetErrStr(Aerr: TDownloadTileResult): string;
    procedure AfterWriteToFile;
  protected
    procedure Execute; override;
  public
    constructor Create(AXY: TPoint; AZoom: byte; MT:TMapType);overload;
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  u_GlobalState,
  UResStrings,
  Unit1;

function TTileDownloaderUIOneTile.GetErrStr(Aerr: TDownloadTileResult): string;
begin
 case Aerr of
  dtrProxyAuthError: result:=SAS_ERR_Authorization;
  dtrBanError: result:=SAS_ERR_Ban;
  dtrTileNotExists: result:=SAS_ERR_TileNotExists;
  dtrDownloadError,
  dtrErrorInternetOpen,
  dtrErrorInternetOpenURL: result:=SAS_ERR_Noconnectionstointernet;
  dtrErrorMIMEType: result := 'Ошибочный тип данных'; //TODO: Заменить на ресурсную строку
  dtrUnknownError: Result := 'Неизвестная ошибка при скачивании'
  else result:='';
 end;
end;

procedure TTileDownloaderUIOneTile.dwnOne;
var
  ty: string;
  fileBuf:TMemoryStream;
  res: TDownloadTileResult;
begin
  lastload.X:=LoadXY.X-(abs(LoadXY.X) mod 256);
  lastload.Y:=LoadXY.Y-(abs(LoadXY.Y) mod 256);
  lastload.z:=zoom;
  lastLoad.mt:=typemap;
  lastLoad.use:=true;
  if typemap.UseDwn then begin
    FileBuf:=TMemoryStream.Create;
    try
      res :=DownloadTile(LoadXY, Zoom, typemap, 0, ty, fileBuf);
      ErrorString:=GetErrStr(res);
      if (res = dtrOK) or (res = dtrSameTileSize) then begin
        GState.IncrementDownloaded(fileBuf.Size/1024, 1);
      end;
      if (res = dtrTileNotExists) and (GState.SaveTileNotExists) then begin
        typemap.SaveTileNotExists(LoadXY.X, LoadXY.Y, Zoom);
      end;
      if res = dtrOK then begin
        typemap.SaveTileDownload(LoadXY.X, LoadXY.Y, Zoom, fileBuf, ty);
      end;
    finally
      FileBuf.Free;
    end;
  end else begin
    ErrorString:=SAS_ERR_NotLoads;
  end;
  Synchronize(AfterWriteToFile);
end;

function TTileDownloaderUIOneTile.DownloadTile(AXY: TPoint; AZoom: byte;
  MT: TMapType; AOldTileSize: Integer; out ty: string; fileBuf: TMemoryStream): TDownloadTileResult;
var
  StatusCode: Cardinal;
begin
  Result := dtrUnknownError;
  if terminated then exit;
  url_ifban := MT.GetLink(AXY.X, AXY.Y, AZoom);
  FDownloader.ExpectedMIMETypes := MT.CONTENT_TYPE;
  FDownloader.SleepOnResetConnection := MT.Sleep;
  Result := FDownloader.DownloadTile(url_ifban, false, 0, fileBuf, StatusCode, ty);
  if (ty <> MT.Content_type)
    and(fileBuf.Size <> 0)
    and(MT.BanIfLen <> 0)
    and(fileBuf.Size < (MT.BanIfLen + 50))
    and(fileBuf.Size >(MT.BanIfLen-50)) then
  begin
    result := dtrBanError;
  end;

  if Result = dtrBanError  then begin
    Synchronize(Ban);
  end;
end;

procedure TTileDownloaderUIOneTile.AfterWriteToFile;
begin
 if (Fmain.Enabled)and(not(Fmain.MapMoving))and(not(FMain.MapZoomAnimtion=1)) then
  begin
   Fmain.generate_im(lastload,ErrorString);
  end
 else Fmain.toSh;
end;

procedure TTileDownloaderUIOneTile.ban;
begin
 if typemap.ban_pg_ld then
  begin
   Fmain.ShowCaptcha(url_ifban);
   typemap.ban_pg_ld:=false;
  end;
end;

constructor TTileDownloaderUIOneTile.Create(AXY: TPoint; AZoom: byte; MT:TMapType);
var
  VDownloadTryCount: Integer;
begin
  inherited Create(False);
  LoadXY := AXY;
  Zoom := AZoom;
  typemap := MT;

  Priority := tpLower;
  FreeOnTerminate := true;
  if GState.TwoDownloadAttempt then begin
    VDownloadTryCount := 2;
  end else begin
    VDownloadTryCount := 1;
  end;
  FDownloader := TTileDownloaderBase.Create(MT.CONTENT_TYPE, VDownloadTryCount, GState.InetConnect);
  randomize;
end;

destructor TTileDownloaderUIOneTile.Destroy;
begin
  FreeAndNil(FDownloader);
  inherited;
end;

procedure TTileDownloaderUIOneTile.Execute;
begin
  inherited;
  dwnOne;
end;

end.
