unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  t_LoadEvent,
  u_TileDownloaderBase,
  UMapType;

type
  TTileDownloaderUI = class(TThread)
  private
    FZoom: byte;
    FTypeMap: TMapType;
    UPos: TPoint;
    FLoadXY: TPoint;
    FDownloader: TTileDownloaderBase;
    FLastLoad: TlastLoad;
    Fmapsload: boolean;
    FErrorString: string;
    FLoadUrl: string;
    procedure GetCurrentMapAndPos;
    procedure addDwnforban;
    procedure AfterWriteToFile;
    procedure ban;
    function GetErrStr(Aerr: TDownloadTileResult): string;
    function DownloadTile(AXY: TPoint; AZoom: byte;MT:TMapType; AOldTileSize: Integer; out ty: string; fileBuf:TMemoryStream): TDownloadTileResult;
  protected
    procedure Execute; override;
  public
    constructor Create();overload;
    destructor Destroy; override;
  end;
implementation

uses
  SysUtils,
  u_GlobalState,
  UResStrings,
  Unit1;

constructor TTileDownloaderUI.Create;
var
  VDownloadTryCount: Integer;
begin
  inherited Create(False);
  Priority := tpLower;
  Fmapsload := false;
  if GState.TwoDownloadAttempt then begin
    VDownloadTryCount := 2;
  end else begin
    VDownloadTryCount := 1;
  end;
  FDownloader := TTileDownloaderBase.Create('', VDownloadTryCount, GState.InetConnect);
  randomize;
end;

destructor TTileDownloaderUI.Destroy;
begin
  FreeAndNil(FDownloader);
  inherited;
end;


procedure TTileDownloaderUI.GetCurrentMapAndPos;
begin
 FTypeMap:=Sat_map_Both;
 Upos:= FMain.pos;
 FZoom:= GState.zoom_size;
end;

procedure TTileDownloaderUI.addDwnforban;
begin
  if (Fmapsload=false)and(FTypeMap.UseAntiBan>0) then begin
    Fmain.WebBrowser1.Navigate('http://maps.google.com/?ie=UTF8&ll='+inttostr(random(100)-50)+','+inttostr(random(300)-150)+'&spn=1,1&t=k&z=8');
    Fmapsload:=true;
  end;
end;

function TTileDownloaderUI.DownloadTile(AXY: TPoint; AZoom: byte;
  MT: TMapType; AOldTileSize: Integer; out ty: string; fileBuf: TMemoryStream): TDownloadTileResult;
var
  StatusCode: Cardinal;
begin
  Result := dtrUnknownError;
  if terminated then exit;
  FLoadUrl := MT.GetLink(AXY.X, AXY.Y, AZoom);
  FDownloader.ExpectedMIMETypes := MT.CONTENT_TYPE;
  FDownloader.SleepOnResetConnection := MT.Sleep;
  Result := FDownloader.DownloadTile(FLoadUrl, false, 0, fileBuf, StatusCode, ty);
  if (ty <> MT.Content_type)
    and(fileBuf.Size <> 0)
    and(MT.BanIfLen <> 0)
    and(fileBuf.Size < (MT.BanIfLen + 50))
    and(fileBuf.Size >(MT.BanIfLen-50)) then
  begin
    result := dtrBanError;
  end;
end;

function TTileDownloaderUI.GetErrStr(Aerr: TDownloadTileResult): string;
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

procedure TTileDownloaderUI.AfterWriteToFile;
begin
 if (Fmain.Enabled)and(not(Fmain.MapMoving))and(not(FMain.MapZoomAnimtion=1)) then begin
   Fmain.generate_im(FLastLoad, FErrorString);
 end else begin
  Fmain.toSh;
 end;
end;

procedure TTileDownloaderUI.ban;
begin
 if FTypeMap.ban_pg_ld then
  begin
   Fmain.ShowCaptcha(FLoadUrl);
   FTypeMap.ban_pg_ld:=false;
  end;
end;

procedure TTileDownloaderUI.Execute;
var i,j,ii,k,r,XX,YY,g,x,y,m1,num_dwn:integer;
    Bpos:TPoint;
    ty: string;
    fileBuf:TMemoryStream;
    VMap: TMapType;
    res: TDownloadTileResult;
begin
  num_dwn:=0;
  repeat
    if Fmain.TileSource = tsCache then begin
      Sleep(1000);
    end else begin
      if(not FMain.change_scene)then begin
        sleep(100);
      end else begin
        FMain.change_scene:=false;
        Synchronize(GetCurrentMapAndPos);
        if FTypeMap = nil then begin
          Sleep(1000);
        end else begin
          Synchronize(addDwnforban);
          j:=0;
          i:=-1;
          for r:=1 to (hg_x div 2)+2 do begin
            if Terminated then break;
            if FMain.change_scene then Break;
            g:=(r*2-2);
            if r=1 then m1:=0 else m1:=1;
            for k:=0 to g*4-m1 do begin
              if Terminated then break;
              if FMain.change_scene then Break;
              if (k=0) then inc(i);
              if (k>0)and(k<g) then inc(j);
              if (k>=g)and(k<g*2) then dec(i);
              if (k>=g*2)and(k<g*3) then dec(j);
              if (k>=g*3) then inc(i);
              if g=0 then i:=0;
              x:=(hg_x div 2)+i;
              y:=(hg_y div 2)+j;
              Synchronize(GetCurrentMapAndPos);
              for ii:=0 to length(MapType)-1 do begin
                if Terminated then break;
                if FMain.change_scene then Break;
                VMap := MapType[ii];
                if VMap.active then begin
                  BPos:=UPos;
                  BPos := FTypeMap.GeoConvert.Pos2OtherMap(Upos, (Fzoom - 1) + 8, VMap.GeoConvert);
                  xx:=Fmain.X2AbsX(BPos.x-pr_x+(x shl 8),Fzoom);
                  yy:=Fmain.X2AbsX(BPos.y-pr_y+(y shl 8),Fzoom);
                  FLoadXY.X := xx;
                  FLoadXY.Y := yy;

                  Flastload.X:=XX-(abs(XX) mod 256);
                  Flastload.Y:=YY-(abs(YY) mod 256);
                  Flastload.z:=Fzoom;
                  FlastLoad.mt:=VMap;
                  FlastLoad.use:=true;
                  if (FMain.TileSource=tsInternet)or((FMain.TileSource=tsCacheInternet)and(not(VMap.TileExists(xx,yy,Fzoom)))) then begin
                    If (VMap.UseAntiBan>1) then begin
                      inc(num_dwn);
                      If ((num_dwn>0)and((num_dwn mod VMap.UseAntiBan)=0)) then begin
                        Fmapsload:=false;
                      end;
                    end;
                    if VMap.UseDwn then begin
                      FileBuf:=TMemoryStream.Create;
                      try
                        res:=DownloadTile(FLoadXY, FZoom, VMap, 0, ty, fileBuf);
                        if Res = dtrBanError  then begin
                          Synchronize(Ban);
                        end;
                        FErrorString:=GetErrStr(res);
                        if (res = dtrOK) or (res = dtrSameTileSize) then begin
                          GState.IncrementDownloaded(fileBuf.Size/1024, 1);
                        end;
                        if (res = dtrTileNotExists)and(GState.SaveTileNotExists) then begin
                          VMap.SaveTileNotExists(FLoadXY.X, FLoadXY.Y, FZoom);
                        end;
                        if res = dtrOK then begin
                          VMap.SaveTileDownload(FLoadXY.x, FLoadXY.y, Fzoom, fileBuf, ty);
                        end;
                        Synchronize(AfterWriteToFile);
                      finally
                        FileBuf.Free;
                      end;
                    end else begin
                      FErrorString:=SAS_ERR_NotLoads;
                    end;
                    sleep(FTypeMap.Sleep);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  until Terminated;
end;

end.
