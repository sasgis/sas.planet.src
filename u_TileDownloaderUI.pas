unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  i_JclNotify,
  t_LoadEvent,
  t_CommonTypes,
  u_TileDownloaderThreadBase,
  u_MapViewPortState,
  u_MapLayerShowError,
  u_MapLayerBasic,
  UMapType;

type
  TTileDownloaderUI = class(TTileDownloaderThreadBase)
  private
    change_scene: boolean;
    FUPos: TPoint;
    FSizeInTile: TPoint;
    FSizeInPixels: TPoint;
    FLastLoad: TlastLoad;
    FErrorString: string;
    FTileMaxAgeInInternet: TDateTime;
    FMainLayer: TMapLayerBasic;
    FKmlLayer: TMapLayerBasic;
    FErrorShowLayer: TTileErrorInfoLayer;
    FViewPortState: TMapViewPortState;
    FUseDownloadChangeNotifier: IJclNotifier;
    FUseDownload: TTileSource;
    FChangePosListener: IJclListener;
    procedure GetCurrentMapAndPos;
    procedure AfterWriteToFile;
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(const Value: TTileSource);
    procedure ChangePos(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AViewPortState: TMapViewPortState); overload;
    destructor Destroy; override;
    property TileMaxAgeInInternet: TDateTime read FTileMaxAgeInInternet;
    property MainLayer: TMapLayerBasic read FMainLayer write FMainLayer;
    property KmlLayer: TMapLayerBasic read FKmlLayer write FKmlLayer;
    property ErrorShowLayer: TTileErrorInfoLayer read FErrorShowLayer write FErrorShowLayer;
    property UseDownload: TTileSource read GetUseDownload write SetUseDownload;
    property UseDownloadChangeNotifier: IJclNotifier read FUseDownloadChangeNotifier;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_GlobalState,
  i_ITileDownlodSession,
  u_NotifyEventListener,
  Unit1;

constructor TTileDownloaderUI.Create(AViewPortState: TMapViewPortState);
begin
  inherited Create(True);
  FViewPortState := AViewPortState;
  Priority := tpLower;
  FUseDownload := tsCache;
  randomize;
  FTileMaxAgeInInternet :=  1/24/60;
  FUseDownloadChangeNotifier := TJclBaseNotifier.Create;
  FChangePosListener := TNotifyEventListener.Create(ChangePos);
  FViewPortState.PosChangeNotifier.Add(FChangePosListener);
  FViewPortState.MapChangeNotifier.Add(FChangePosListener);
  FViewPortState.HybrChangeNotifier.Add(FChangePosListener);
end;

destructor TTileDownloaderUI.Destroy;
begin
  FViewPortState.PosChangeNotifier.Remove(FChangePosListener);
  FViewPortState.MapChangeNotifier.Remove(FChangePosListener);
  FViewPortState.HybrChangeNotifier.Remove(FChangePosListener);
  FChangePosListener := nil;
  FUseDownloadChangeNotifier := nil;
  inherited;
end;


procedure TTileDownloaderUI.ChangePos(Sender: TObject);
begin
  change_scene := True;
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

function TTileDownloaderUI.GetUseDownload: TTileSource;
begin
  Result := FUseDownload;
end;

procedure TTileDownloaderUI.SetUseDownload(const Value: TTileSource);
begin
  if FUseDownload <> Value then begin
    FUseDownload := Value;
    change_scene := True;
    FUseDownloadChangeNotifier.Notify(nil);
  end;
end;

procedure TTileDownloaderUI.AfterWriteToFile;
begin
  if FErrorString <> '' then begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.ShowError(FLastLoad.TilePos, FLastLoad.Zoom, FLastLoad.mt, FErrorString);
    end;
  end else begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.Visible := False;
    end;
    if FLastLoad.mt.IsBitmapTiles then begin
      if FMainLayer <> nil then begin
        FMainLayer.Redraw;
      end;
    end else if FLastLoad.mt.IsKmlTiles then begin
      if FKmlLayer <> nil then begin
        FKmlLayer.Redraw;
      end;
    end;
  end;
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
  VNeedDownload: Boolean;
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
                  if VMap.UseDwn then begin
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
                    VNeedDownload := False;
                    if VMap.TileExists(FLoadXY, Fzoom) then begin
                      if UseDownload = tsInternet then begin
                        if Now - VMap.TileLoadDate(FLoadXY, FZoom) > FTileMaxAgeInInternet then begin
                          VNeedDownload := True;
                        end;
                      end;
                    end else begin
                      if (UseDownload = tsInternet) or (UseDownload = tsCacheInternet) then begin
                        if not(VMap.TileNotExistsOnServer(FLoadXY, Fzoom)) then begin
                          VNeedDownload := True;
                        end;
                      end;
                    end;
                    if VNeedDownload then begin
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
  until Terminated;
end;

end.
