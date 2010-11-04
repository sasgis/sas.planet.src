unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  i_JclNotify,
  t_CommonTypes,
  i_ICoordConverter,
  i_ITileDownlodSession,
  u_MapViewPortState,
  u_MapLayerShowError,
  u_MapLayerBasic,
  UMapType;

type
  TTileDownloaderUI = class(TThread)
  private
    FCoordConverter: ICoordConverter;
    FZoom: byte;
    FPixelRect: TRect;
    FMainMap: TMapType;
    FMapType: TMapType;

    change_scene: boolean;

    FErrorString: string;
    FTileMaxAgeInInternet: TDateTime;
    FMainLayer: TMapLayerBasic;
    FKmlLayer: TMapLayerBasic;
    FErrorShowLayer: TTileErrorInfoLayer;
    FViewPortState: TMapViewPortState;
    FUseDownloadChangeNotifier: IJclNotifier;
    FUseDownload: TTileSource;
    FChangePosListener: IJclListener;

    FLoadXY: TPoint;
    FLoadUrl: string;

    class function GetErrStr(Aerr: TDownloadTileResult): string; virtual;
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
  u_NotifyEventListener,
  u_TileIteratorAbstract,
  u_TileIteratorSpiralByRect,
  UResStrings;

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
    FCoordConverter := GState.ViewState.GetCurrentCoordConverter;
    FMainMap := GState.ViewState.GetCurrentMap;
    FZoom := GState.ViewState.GetCurrentZoom;
    FPixelRect := GState.ViewState.GetViewRectInMapPixel;
  finally
    GState.ViewState.UnLockRead;
  end;
end;

class function TTileDownloaderUI.GetErrStr(Aerr: TDownloadTileResult): string;
begin
  case Aerr of
    dtrProxyAuthError:
    begin
      result := SAS_ERR_Authorization;
    end;
    dtrBanError:
    begin
      result := SAS_ERR_Ban;
    end;
    dtrTileNotExists:
    begin
      result := SAS_ERR_TileNotExists;
    end;
    dtrDownloadError,
    dtrErrorInternetOpen,
    dtrErrorInternetOpenURL:
    begin
      result := SAS_ERR_Noconnectionstointernet;
    end;
    dtrErrorMIMEType:
    begin
      result := 'Ошибочный тип данных';
    end; //TODO: Заменить на ресурсную строку
    dtrUnknownError:
    begin
      Result := 'Неизвестная ошибка при скачивании';
    end else begin
    result := '';
  end;
  end;
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
      FErrorShowLayer.ShowError(FLoadXY, FZoom, FMapType, FErrorString);
    end;
  end else begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.Visible := False;
    end;
    if FMapType.IsBitmapTiles then begin
      if FMainLayer <> nil then begin
        FMainLayer.Redraw;
      end;
    end else if FMapType.IsKmlTiles then begin
      if FKmlLayer <> nil then begin
        FKmlLayer.Redraw;
      end;
    end;
  end;
end;

procedure TTileDownloaderUI.Execute;
var
  ii: integer;
  VPixelInTargetMap: TPoint;
  ty: string;
  fileBuf: TMemoryStream;
  res: TDownloadTileResult;
  VNeedDownload: Boolean;
  VIterator: TTileIteratorAbstract;
  VTile: TPoint;
begin
  repeat
    if FUseDownload = tsCache then begin
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
        if FCoordConverter = nil then begin
          if Terminated then begin
            break;
          end;
          Sleep(1000);
        end else begin
          VIterator := TTileIteratorSpiralByRect.Create(FCoordConverter.PixelRect2TileRect(FPixelRect, FZoom));
          try
            while VIterator.Next(VTile) do begin
              if Terminated then begin
                break;
              end;
              if change_scene then begin
                Break;
              end;
              for ii := 0 to length(GState.MapType) - 1 do begin
                if Terminated then begin
                  break;
                end;
                if change_scene then begin
                  Break;
                end;
                FMapType := GState.MapType[ii];
                if (FMapType = FMainMap) or (FMapType.asLayer and GState.ViewState.IsHybrGUIDSelected(FMapType.GUID)) then begin
                  if FMapType.UseDwn then begin
                    VPixelInTargetMap := FCoordConverter.PixelPos2OtherMap(
                      FCoordConverter.TilePos2PixelPos(VTile, FZoom),
                      Fzoom,
                      FMapType.GeoConvert
                    );
                    FLoadXY := FMapType.GeoConvert.PixelPos2TilePos(VPixelInTargetMap, FZoom);
                    VNeedDownload := False;
                    if FMapType.TileExists(FLoadXY, Fzoom) then begin
                      if FUseDownload = tsInternet then begin
                        if Now - FMapType.TileLoadDate(FLoadXY, FZoom) > FTileMaxAgeInInternet then begin
                          VNeedDownload := True;
                        end;
                      end;
                    end else begin
                      if (FUseDownload = tsInternet) or (FUseDownload = tsCacheInternet) then begin
                        if not(FMapType.TileNotExistsOnServer(FLoadXY, Fzoom)) then begin
                          VNeedDownload := True;
                        end;
                      end;
                    end;
                    if VNeedDownload then begin
                      FileBuf := TMemoryStream.Create;
                      try
                        try
                          res := FMapType.DownloadTile(Self, FLoadXY, FZoom, false, 0, FLoadUrl, ty, fileBuf);
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
          finally
            VIterator.Free;
          end;
        end;
      end;
    end;
  until Terminated;
end;

end.
