unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  t_CommonTypes,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_ActiveMapsConfig,
  i_ViewPortState,
  i_MapTypes,
  i_TileDownlodSession,
  i_DownloadUIConfig,
  u_MapLayerShowError,
  u_MapType;

type
  TTileDownloaderUI = class(TThread)
  private
    FConfig: IDownloadUIConfig;
    FMapsSet: IActiveMapsSet;
    FViewPortState: IViewPortState;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorShowLayer: TTileErrorInfoLayer;

    FTileMaxAgeInInternet: TDateTime;
    FTilesOut: Integer;
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
    procedure OnTileDownloadEvent(AMapType: TMapType; ATile: TPoint; AZoom: Byte; ATileSize: Int64; AResult: TDownloadTileResult);
  end;

implementation

uses
  SysUtils,
  ActiveX,
  t_GeoTypes,
  u_GlobalState,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  i_TileIterator,
  u_TileIteratorSpiralByRect,
  u_ResStrings;

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
    FTilesOut := FConfig.TilesOut;
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
  OnConfigChange(nil);
  Resume;
end;

procedure TTileDownloaderUI.OnTileDownloadEvent(AMapType: TMapType; ATile: TPoint; AZoom: Byte; ATileSize: Int64; AResult: TDownloadTileResult);
//var
//  VErrorString: string;
begin
  // TODO: отправить данные в поток TTileDownloaderEventProcessor

  {
  VErrorString := GetErrStr(AResult);
  if (AResult = dtrOK) or (AResult = dtrSameTileSize) then begin
    GState.DownloadInfo.Add(1, ATileSize);
  end;
  if VErrorString <> '' then begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.ShowError(ATile, AZoom, AMapType, VErrorString);
    end;
  end else begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.Visible := False;
    end;
    if Addr(FMapTileUpdateEvent) <> nil then begin
      FMapTileUpdateEvent(AMapType, AZoom, ATile);
    end;
  end;
   }
end;

procedure TTileDownloaderUI.Execute;
var
  ty: string;
  fileBuf: TMemoryStream;
  res: TDownloadTileResult;
  VNeedDownload: Boolean;
  VIterator: ITileIterator;
  VTile: TPoint;
  VLocalConverter: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VMapGeoConverter: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VLonLatRectInMap: TDoubleRect;
  VMapTileRect: TRect;
  VZoom: Byte;
  VActiveMapsList: IMapTypeList;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMap: IMapType;
  VLoadUrl: string;
  VIteratorsList: IInterfaceList;
  VMapsList: IInterfaceList;
  VAllIteratorsFinished: Boolean;
  VOnTileDownloadEvent: TParentThreadEvent;
begin
  VIteratorsList := TInterfaceList.Create;
  VMapsList := TInterfaceList.Create;
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
          VActiveMapsList := FActiveMapsList;
          VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
          VZoom := VLocalConverter.GetZoom;
          VGeoConverter := VLocalConverter.GetGeoConverter;
          VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
          VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
          VIteratorsList.Clear;
          VMapsList.Clear;
          VEnum := VActiveMapsList.GetIterator;
          while VEnum.Next(1, VGUID, i) = S_OK do begin
            VMap := VActiveMapsList.GetMapTypeByGUID(VGUID);
            if VMap <> nil then begin
              FMapType := VMap.MapType;
              if FMapType.UseDwn then begin
                VMapGeoConverter := FMapType.GeoConvert;
                VLonLatRectInMap := VLonLatRect;
                VMapGeoConverter.CheckLonLatRect(VLonLatRectInMap);

                VMapTileRect := VMapGeoConverter.LonLatRect2TileRect(VLonLatRectInMap, VZoom);
                Dec(VMapTileRect.Left, FTilesOut);
                Dec(VMapTileRect.Top, FTilesOut);
                Inc(VMapTileRect.Right, FTilesOut);
                Inc(VMapTileRect.Bottom, FTilesOut);
                VMapGeoConverter.CheckTileRect(VMapTileRect, VZoom);
                VIterator := TTileIteratorSpiralByRect.Create(VMapTileRect);
                VIteratorsList.Add(VIterator);
                VMapsList.Add(VMap);
              end;
            end;
          end;
          VAllIteratorsFinished := not(VIteratorsList.Count > 0);
          while not VAllIteratorsFinished do begin
            VAllIteratorsFinished := True;
            for i := 0 to VIteratorsList.Count - 1 do begin
              if Terminated then begin
                break;
              end;
              if change_scene then begin
                break;
              end;
              VIterator := ITileIterator(VIteratorsList.Items[i]);
              if VIterator.Next(VTile) then begin
                VAllIteratorsFinished := False;
                VMap := IMapType(VMapsList.Items[i]);
                FMapType := VMap.MapType;
                FLoadXY := VTile;
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
                if VNeedDownload then
                try
                  VOnTileDownloadEvent := OnTileDownloadEvent;
                  FMapType.DownloadTile(@VOnTileDownloadEvent, FLoadXY, VZoom, false, 0);
                except

                end;
                if Terminated then
                  Break;
              end;
            end;
            if Terminated then begin
              break;
            end;
            if change_scene then begin
              break;
            end;
          end;
        end;
      end;
    end;
  until Terminated;
end;

end.
