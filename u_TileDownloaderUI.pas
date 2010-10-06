unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  t_LoadEvent,
  t_CommonTypes,
  u_TileDownloaderThreadBase,
  UMapType;

type
  TTileDownloaderUI = class(TTileDownloaderThreadBase)
  private
    FUPos: TPoint;
    FSizeInTile: TPoint;
    FSizeInPixels: TPoint;
    FLastLoad: TlastLoad;
    FErrorString: string;
    procedure GetCurrentMapAndPos;
    procedure AfterWriteToFile;
  protected
    procedure Execute; override;
  public
    change_scene: boolean;
    UseDownload: TTileSource;
    constructor Create(); overload;
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
  inherited Create(True);
  Priority := tpLower;
  UseDownload := tsCache;
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
    FMapType := GState.ViewState.GetCurrentMap;
    FUPos := GState.ViewState.GetCenterMapPixel;
    FZoom := GState.ViewState.GetCurrentZoom;
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
  Fmain.generate_im(FLastLoad, FErrorString);
end;

procedure TTileDownloaderUI.Execute;
var
  i, j, ii, k, r, g, x, y, m1: integer;
  Bpos: TPoint;
  ty: string;
  fileBuf: TMemoryStream;
  VMap: TMapType;
  VMainMap: TMapType;
  res: TDownloadTileResult;
  VZoom: Byte;
begin
  repeat
    if UseDownload = tsCache then begin
      if Terminated then begin
        break;
      end;
      Sleep(1000);
      if Terminated then begin
        break;
      end;
    end else begin
      if (not change_scene) then begin
        if Terminated then begin
          break;
        end;
        sleep(100);
        if Terminated then begin
          break;
        end;
      end else begin
        if Terminated then begin
          break;
        end;
        change_scene := false;
        Synchronize(GetCurrentMapAndPos);
        if Terminated then begin
          break;
        end;
        VMainMap := FMapType;
        if VMainMap = nil then begin
          if Terminated then begin
            break;
          end;
          Sleep(1000);
        end else begin
          j := 0;
          i := -1;
          for r := 1 to (FSizeInTile.x div 2) + 2 do begin
            if Terminated then begin
              break;
            end;
            if change_scene then begin
              Break;
            end;
            g := (r * 2 - 2);
            if r = 1 then begin
              m1 := 0;
            end else begin
              m1 := 1;
            end;
            for k := 0 to g * 4 - m1 do begin
              if Terminated then begin
                break;
              end;
              if change_scene then begin
                Break;
              end;
              if (k = 0) then begin
                inc(i);
              end;
              if (k > 0) and (k < g) then begin
                inc(j);
              end;
              if (k >= g) and (k < g * 2) then begin
                dec(i);
              end;
              if (k >= g * 2) and (k < g * 3) then begin
                dec(j);
              end;
              if (k >= g * 3) then begin
                inc(i);
              end;
              if g = 0 then begin
                i := 0;
              end;
              x := (FSizeInTile.x div 2) + i;
              y := (FSizeInTile.y div 2) + j;
              for ii := 0 to length(GState.MapType) - 1 do begin
                if Terminated then begin
                  break;
                end;
                if change_scene then begin
                  Break;
                end;
                VMap := GState.MapType[ii];
                if (VMap = VMainMap) or (VMap.asLayer and GState.ViewState.IsHybrGUIDSelected(VMap.GUID)) then begin
                  BPos := FUPos;
                  VZoom := FZoom;
                  BPos := VMainMap.GeoConvert.PixelPos2OtherMap(FUPos, Fzoom, VMap.GeoConvert);
                  FLoadXY.X := BPos.x - (FSizeInPixels.X div 2) + (x shl 8);
                  FLoadXY.Y := BPos.y - (FSizeInPixels.Y div 2) + (y shl 8);
                  FLoadXY.X := FLoadXY.X shr 8;
                  FLoadXY.Y := FLoadXY.Y shr 8;
                  VMap.GeoConvert.CheckTilePosStrict(FLoadXY, VZoom, True);

                  Flastload.TilePos.X := FLoadXY.X;
                  Flastload.TilePos.Y := FLoadXY.Y;
                  Flastload.Zoom := Fzoom;
                  FlastLoad.mt := VMap;
                  FlastLoad.use := true;
                  if VMap.UseDwn then begin
                    if (UseDownload = tsInternet) or ((UseDownload = tsCacheInternet) and (not (VMap.TileExists(FLoadXY, Fzoom)))) then begin
                        if GState.IgnoreTileNotExists or not VMap.TileNotExistsOnServer(FLoadXY, Fzoom) then begin
                        FileBuf := TMemoryStream.Create;
                        try
                          try
                            res := VMap.DownloadTile(Self, FLoadXY, FZoom, false, 0, FLoadUrl, ty, fileBuf);
                            FErrorString := GetErrStr(res);
                            if (res = dtrOK) or (res = dtrSameTileSize) then begin
                              GState.IncrementDownloaded(fileBuf.Size / 1024, 1);
                            end;
                          except
                            on E: Exception do begin
                              FErrorString := E.Message;
                            end;
                          end;
                          if Terminated then begin
                            break;
                          end;
                          Synchronize(AfterWriteToFile);
                        finally
                          FileBuf.Free;
                        end;
                      end;
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
