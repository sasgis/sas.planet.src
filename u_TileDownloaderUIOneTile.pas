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
    FLastLoad: TlastLoad;
    FTypeMap: TMapType;
    FLoadXY: TPoint;
    FZoom: byte;
    FErrorString: string;
    FLoadUrl: string;
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

constructor TTileDownloaderUIOneTile.Create(AXY: TPoint; AZoom: byte; MT:TMapType);
begin
  inherited Create(False);
  FLoadXY := AXY;
  FZoom := AZoom;
  FTypeMap := MT;

  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

destructor TTileDownloaderUIOneTile.Destroy;
begin
  inherited;
end;


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

procedure TTileDownloaderUIOneTile.AfterWriteToFile;
begin
 if (Fmain.Enabled)and(not(Fmain.MapMoving))and(not(FMain.MapZoomAnimtion=1)) then
  begin
   Fmain.generate_im(FLastLoad, FErrorString);
  end
 else Fmain.toSh;
end;

procedure TTileDownloaderUIOneTile.ban;
begin
  FTypeMap.ExecOnBan(FLoadUrl);
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  ty: string;
  fileBuf:TMemoryStream;
  res: TDownloadTileResult;
begin
  Flastload.X := FLoadXY.X-(abs(FLoadXY.X) mod 256);
  Flastload.Y := FLoadXY.Y-(abs(FLoadXY.Y) mod 256);
  Flastload.z := Fzoom;
  FlastLoad.mt := Ftypemap;
  FlastLoad.use :=true;
  if FTypeMap.UseDwn then begin
    FileBuf:=TMemoryStream.Create;
    try
      res :=FTypeMap.DownloadTile(FLoadXY, FZoom, false, 0, FLoadUrl, ty, fileBuf);
      if res = dtrBanError  then begin
        Synchronize(Ban);
      end;
      FErrorString:=GetErrStr(res);
      if (res = dtrOK) or (res = dtrSameTileSize) then begin
        GState.IncrementDownloaded(fileBuf.Size/1024, 1);
      end;
      case res of
        dtrOK,
        dtrSameTileSize,
        dtrErrorMIMEType,
        dtrTileNotExists,
        dtrBanError: begin
          if FTypeMap.IncDownloadedAndCheckAntiBan then begin
            Synchronize(FTypeMap.addDwnforban);
          end;
        end;
      end;
      if (res = dtrTileNotExists) and (GState.SaveTileNotExists) then begin
        FTypeMap.SaveTileNotExists(FLoadXY.X, FLoadXY.Y, FZoom);
      end;
      if res = dtrOK then begin
        FTypeMap.SaveTileDownload(FLoadXY.X, FLoadXY.Y, FZoom, fileBuf, ty);
      end;
    finally
      FileBuf.Free;
    end;
  end else begin
    FErrorString:=SAS_ERR_NotLoads;
  end;
  Synchronize(AfterWriteToFile);
end;

end.
