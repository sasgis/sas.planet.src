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
    FLastLoad: TlastLoad;
    FErrorString: string;
    FLoadUrl: string;
    procedure GetCurrentMapAndPos;
    procedure AfterWriteToFile;
    procedure ban;
    function GetErrStr(Aerr: TDownloadTileResult): string;
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
begin
  inherited Create(False);
  Priority := tpLower;
  randomize;
end;

destructor TTileDownloaderUI.Destroy;
begin
  inherited;
end;


procedure TTileDownloaderUI.GetCurrentMapAndPos;
begin
 FTypeMap:=Sat_map_Both;
 Upos:= FMain.pos;
 FZoom:= GState.zoom_size;
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
  FTypeMap.ExecOnBan(FLoadUrl);
end;

procedure TTileDownloaderUI.Execute;
var i,j,ii,k,r,XX,YY,g,x,y,m1:integer;
    Bpos:TPoint;
    ty: string;
    fileBuf:TMemoryStream;
    VMap: TMapType;
    VMainMap: TMapType;
    res: TDownloadTileResult;
begin
  repeat
    if Fmain.TileSource = tsCache then begin
      Sleep(1000);
    end else begin
      if(not FMain.change_scene)then begin
        sleep(100);
      end else begin
        FMain.change_scene:=false;
        Synchronize(GetCurrentMapAndPos);
        VMainMap := FTypeMap;
        if VMainMap = nil then begin
          Sleep(1000);
        end else begin
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
              for ii:=0 to length(MapType)-1 do begin
                if Terminated then break;
                if FMain.change_scene then Break;
                VMap := MapType[ii];
                if VMap.active then begin
                  BPos:=UPos;
                  BPos := VMainMap.GeoConvert.Pos2OtherMap(Upos, (Fzoom - 1) + 8, VMap.GeoConvert);
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
                    if VMap.UseDwn then begin
                      FileBuf:=TMemoryStream.Create;
                      try
                        sleep(VMap.Sleep);
                        res :=VMap.DownloadTile(FLoadXY, FZoom, false, 0, FLoadUrl, ty, fileBuf);
                        if Res = dtrBanError  then begin
                          FTypeMap := VMap;
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
                            if VMap.IncDownloadedAndCheckAntiBan then begin
                              Synchronize(VMap.addDwnforban);
                            end;
                          end;
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
