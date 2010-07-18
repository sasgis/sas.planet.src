unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  t_LoadEvent,
  u_TileDownloaderThreadBase,
  UMapType;

type
  TTileDownloaderUI = class(TTileDownloaderThreadBase)
  private
    UPos: TPoint;
    FSizeInTile: TPoint;
    FSizeInPixels: TPoint;
    FLastLoad: TlastLoad;
    FErrorString: string;
    procedure GetCurrentMapAndPos;
    procedure AfterWriteToFile;
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
  i_ITileDownlodSession,
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
  GState.ViewState.LockRead;
  try
     FTypeMap:=GState.ViewState.GetCurrentMap;
     Upos:= GState.ViewState.GetCenterMapPixel;
     FZoom:= GState.ViewState.GetCurrentZoom;
  finally
    GState.ViewState.UnLockRead;
  end;
 //TODO: Переписать нормально с учетом настроек.
 FSizeInPixels.X := ((GState.ScreenSize.X + 255) div 256) * 256;
 FSizeInPixels.Y := ((GState.ScreenSize.Y + 255) div 256) * 256;

 FSizeInTile.X := FSizeInPixels.X div 256;
 FSizeInTile.Y := FSizeInPixels.Y div 256;
end;

procedure TTileDownloaderUI.AfterWriteToFile;
begin
 if (Fmain.Enabled)and(not(Fmain.MapMoving))and(not(FMain.MapZoomAnimtion=1)) then begin
   Fmain.generate_im(FLastLoad, FErrorString);
 end;
end;

procedure TTileDownloaderUI.Execute;
var i,j,ii,k,r,g,x,y,m1:integer;
    Bpos:TPoint;
    ty: string;
    fileBuf:TMemoryStream;
    VMap: TMapType;
    VMainMap: TMapType;
    res: TDownloadTileResult;
    VZoom: Byte;
begin
  repeat
    if Fmain.TileSource = tsCache then begin
      if Terminated then break;
      Sleep(1000);
      if Terminated then break;
    end else begin
      if(not FMain.change_scene)then begin
        if Terminated then break;
        sleep(100);
        if Terminated then break;
      end else begin
        if Terminated then break;
        FMain.change_scene:=false;
        Synchronize(GetCurrentMapAndPos);
        if Terminated then break;
        VMainMap := FTypeMap;
        if VMainMap = nil then begin
          if Terminated then break;
          Sleep(1000);
        end else begin
          j:=0;
          i:=-1;
          for r:=1 to (FSizeInTile.x div 2)+2 do begin
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
              x:=(FSizeInTile.x div 2)+i;
              y:=(FSizeInTile.y div 2)+j;
              for ii:=0 to length(GState.MapType)-1 do begin
                if Terminated then break;
                if FMain.change_scene then Break;
                VMap := GState.MapType[ii];
                if (VMap = VMainMap) or (VMap.asLayer and GState.ViewState.IsHybrGUIDSelected(VMap.GUID)) then begin
                  BPos:=UPos;
                  VZoom := FZoom;
                  BPos := VMainMap.GeoConvert.Pos2OtherMap(Upos, (Fzoom) + 8, VMap.GeoConvert);
                  FLoadXY.X := BPos.x-(FSizeInPixels.X div 2)+(x shl 8);
                  FLoadXY.Y := BPos.y-(FSizeInPixels.Y div 2)+(y shl 8);
                  VMap.GeoConvert.CheckPixelPosStrict(FLoadXY, VZoom, True);

                  Flastload.TilePos.X:=FLoadXY.X shr 8;
                  Flastload.TilePos.Y:=FLoadXY.Y shr 8;
                  Flastload.Zoom:=Fzoom;
                  FlastLoad.mt:=VMap;
                  FlastLoad.use:=true;
                  if (FMain.TileSource=tsInternet)or((FMain.TileSource=tsCacheInternet)and(not(VMap.TileExists(FLoadXY.x,FLoadXY.y, Fzoom + 1)))) then begin
                    if VMap.UseDwn then begin
                      if GState.IgnoreTileNotExists or not VMap.TileNotExistsOnServer(FLoadXY.x,FLoadXY.y,Fzoom + 1) then begin
                        FileBuf:=TMemoryStream.Create;
                        try
                          if VMap.IncDownloadedAndCheckAntiBan and not Terminated then begin
                            Synchronize(VMap.addDwnforban);
                          end;
                          res :=VMap.DownloadTile(FLoadXY.X, FLoadXY.Y, FZoom + 1, false, 0, FLoadUrl, ty, fileBuf);
                          if Res = dtrBanError  then begin
                            FTypeMap := VMap;
                            Synchronize(Ban);
                          end;
                          FErrorString:=GetErrStr(res);
                          if (res = dtrOK) or (res = dtrSameTileSize) then begin
                            GState.IncrementDownloaded(fileBuf.Size/1024, 1);
                          end;
                          if (res = dtrTileNotExists)and(GState.SaveTileNotExists) then begin
                            VMap.SaveTileNotExists(FLoadXY.X, FLoadXY.Y, FZoom + 1);
                          end;
                          if res = dtrOK then begin
                            try
                              VMap.SaveTileDownload(FLoadXY.x, FLoadXY.y, Fzoom + 1, fileBuf, ty);
                            except
                              on E: Exception do begin
                                FErrorString := E.Message;
                              end;
                            end;
                          end;
                          if Terminated then break;
                          Synchronize(AfterWriteToFile);
                        finally
                          FileBuf.Free;
                        end;
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
