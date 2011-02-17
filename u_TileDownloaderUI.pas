unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  i_JclNotify,
  i_IJclListenerNotifierLinksList,
  t_CommonTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  i_IActiveMapsConfig,
  i_IViewPortState,
  i_MapTypes,
  i_ITileDownlodSession,
  i_IDownloadUIConfig,
  u_MapLayerShowError,
  UMapType;

type
  TTileDownloaderUI = class(TThread)
  private
    FConfig: IDownloadUIConfig;
    FMapsSet: IActiveMapsSet;
    FViewPortState: IViewPortState;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorShowLayer: TTileErrorInfoLayer;

    FTileMaxAgeInInternet: TDateTime;
    FUseDownload: TTileSource;
    FLinksList: IJclListenerNotifierLinksList;

    FVisualCoordConverter: ILocalCoordConverter;
    FActiveMapsList: IMapTypeList;

    change_scene: boolean;

    FMapType: TMapType;
    FLoadXY: TPoint;
    FErrorString: string;

    class function GetErrStr(Aerr: TDownloadTileResult): string; virtual;
    procedure GetCurrentMapAndPos;
    procedure AfterWriteToFile;
    procedure OnPosChange(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(
      AConfig: IDownloadUIConfig;
      AViewPortState: IViewPortState;
      AMapsSet: IActiveMapsSet;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorShowLayer: TTileErrorInfoLayer
    ); overload;
    destructor Destroy; override;
    procedure StartThreads;
    procedure SendTerminateToThreads;
  end;

implementation

uses
  SysUtils,
  ActiveX,
  u_JclNotify,
  t_GeoTypes,
  u_GlobalState,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  i_ITileIterator,
  u_TileIteratorSpiralByRect,
  UResStrings;

constructor TTileDownloaderUI.Create(
  AConfig: IDownloadUIConfig;
  AViewPortState: IViewPortState;
  AMapsSet: IActiveMapsSet;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorShowLayer: TTileErrorInfoLayer
);
var
  VChangePosListener: IJclListener;
begin
  inherited Create(True);
  FConfig := AConfig;
  FViewPortState := AViewPortState;
  FMapsSet := AMapsSet;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorShowLayer := AErrorShowLayer;
  FViewPortState := AViewPortState;
  FLinksList := TJclListenerNotifierLinksList.Create;

  Priority := tpLower;
  FUseDownload := tsCache;
  randomize;
  FTileMaxAgeInInternet :=  1/24/60;

  VChangePosListener := TNotifyEventListener.Create(Self.OnPosChange);
  FLinksList.Add(
    VChangePosListener,
    FViewPortState.GetChangeNotifier
  );
  FLinksList.Add(
    VChangePosListener,
    FMapsSet.GetChangeNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

destructor TTileDownloaderUI.Destroy;
var
  VWaitResult: DWORD;
begin
  FLinksList := nil;

  VWaitResult := WaitForSingleObject(Handle, 10000);
  if VWaitResult = WAIT_TIMEOUT then begin
    TerminateThread(Handle, 0);
  end;
  FMapsSet := nil;
  inherited;
end;


procedure TTileDownloaderUI.OnPosChange(Sender: TObject);
begin
  change_scene := True;
end;

procedure TTileDownloaderUI.GetCurrentMapAndPos;
begin
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter;
  FActiveMapsList := FMapsSet.GetSelectedMapsList;
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
      result := SAS_ERR_TileDownloadContentTypeUnexpcted;
    end;
    dtrUnknownError:
    begin
      Result := SAS_ERR_TileDownloadUnexpectedError;
    end else begin
    result := '';
  end;
  end;
end;

procedure TTileDownloaderUI.OnConfigChange(Sender: TObject);
begin
  FConfig.LockRead;
  try
    FUseDownload := FConfig.UseDownload;
    FTileMaxAgeInInternet := FConfig.TileMaxAgeInInternet;
    change_scene := True;
  finally
    FConfig.UnlockRead;
  end;
end;

procedure TTileDownloaderUI.SendTerminateToThreads;
begin
  inherited;
  FLinksList.DeactivateLinks;
  Terminate;
end;

procedure TTileDownloaderUI.StartThreads;
begin
  inherited;
  FLinksList.ActivateLinks;
  Resume;
end;

procedure TTileDownloaderUI.AfterWriteToFile;
begin
  if FErrorString <> '' then begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.ShowError(FLoadXY, FVisualCoordConverter.GetZoom, FMapType, FErrorString);
    end;
  end else begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.Visible := False;
    end;
    if Addr(FMapTileUpdateEvent) <> nil then begin
      FMapTileUpdateEvent(FMapType, FVisualCoordConverter.GetZoom, FLoadXY);
    end;
  end;
end;

procedure TTileDownloaderUI.Execute;
var
  VPixelInTargetMap: TPoint;
  ty: string;
  fileBuf: TMemoryStream;
  res: TDownloadTileResult;
  VNeedDownload: Boolean;
  VIterator: ITileIterator;
  VTile: TPoint;
  VLocalConverter: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VZoom: Byte;
  VActiveMapsList: IMapTypeList;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMap: IMapType;
  VLoadUrl: string;
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
        VLocalConverter := FVisualCoordConverter;
        if VLocalConverter = nil then begin
          if Terminated then begin
            break;
          end;
          Sleep(1000);
        end else begin
          VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
          VZoom := VLocalConverter.GetZoom;
          VActiveMapsList := FActiveMapsList;
          VGeoConverter := VLocalConverter.GetGeoConverter;
          VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
          VIterator := TTileIteratorSpiralByRect.Create(VGeoConverter.PixelRectFloat2TileRect(VMapPixelRect, VZoom));
          try
            while VIterator.Next(VTile) do begin
              if Terminated then begin
                break;
              end;
              if change_scene then begin
                Break;
              end;
              VEnum := VActiveMapsList.GetIterator;
              while VEnum.Next(1, VGUID, i) = S_OK do begin
                VMap := VActiveMapsList.GetMapTypeByGUID(VGUID);
                if Terminated then begin
                  break;
                end;
                if change_scene then begin
                  Break;
                end;
                if VMap <> nil then begin
                  FMapType := VMap.MapType;
                  if FMapType.UseDwn then begin
                    VPixelInTargetMap := VGeoConverter.PixelPos2OtherMap(
                      VGeoConverter.TilePos2PixelPos(VTile, VZoom),
                      VZoom,
                      FMapType.GeoConvert
                    );
                    FLoadXY := FMapType.GeoConvert.PixelPos2TilePos(VPixelInTargetMap, VZoom);
                    VNeedDownload := False;
                    if FMapType.TileExists(FLoadXY, VZoom) then begin
                      if FUseDownload = tsInternet then begin
                        if Now - FMapType.TileLoadDate(FLoadXY, VZoom) > FTileMaxAgeInInternet then begin
                          VNeedDownload := True;
                        end;
                      end;
                    end else begin
                      if (FUseDownload = tsInternet) or (FUseDownload = tsCacheInternet) then begin
                        if not(FMapType.TileNotExistsOnServer(FLoadXY, VZoom)) then begin
                          VNeedDownload := True;
                        end;
                      end;
                    end;
                    if VNeedDownload then begin
                      FileBuf := TMemoryStream.Create;
                      try
                        try
                          res := FMapType.DownloadTile(Self, FLoadXY, VZoom, false, 0, VLoadUrl, ty, fileBuf);
                          FErrorString := GetErrStr(res);
                          if (res = dtrOK) or (res = dtrSameTileSize) then begin
                            GState.DownloadInfo.Add(1, fileBuf.Size);
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
            VIterator := nil;
          end;
        end;
      end;
    end;
  until Terminated;
end;

end.
