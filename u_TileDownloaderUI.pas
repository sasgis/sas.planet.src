unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  t_CommonTypes,
  i_OperationNotifier,
  u_OperationNotifier,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_TileError,
  i_ActiveMapsConfig,
  i_ViewPortState,
  i_DownloadInfoSimple,
  i_MapTypes,
  i_DownloadUIConfig,
  u_MapType,
  u_TileDownloaderThreadBase;

type
  TTileDownloaderUI = class(TTileDownloaderThreadBase)
  private
    FConfig: IDownloadUIConfig;
    FMapsSet: IActiveMapsSet;
    FDownloadInfo: IDownloadInfoSimple;
    FViewPortState: IViewPortState;
    FErrorLogger: ITileErrorLogger;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FCancelNotifierInternal: IOperationNotifierInternal;
    FCancelNotifier: IOperationNotifier;


    FTileMaxAgeInInternet: TDateTime;
    FTilesOut: Integer;
    FUseDownload: TTileSource;
    FLinksList: IJclListenerNotifierLinksList;

    FVisualCoordConverter: ILocalCoordConverter;
    FActiveMapsSet: IMapTypeSet;

    change_scene: boolean;

    FLoadXY: TPoint;

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
      ADownloadInfo: IDownloadInfoSimple;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger
    ); overload;
    destructor Destroy; override;
    procedure StartThreads;
    procedure SendTerminateToThreads;
  end;

implementation

uses
  SysUtils,
  ActiveX,
  t_GeoTypes,
  i_DownloadResult,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  i_TileIterator,
  u_TileIteratorSpiralByRect,
  u_TileErrorInfo,
  u_ResStrings;

constructor TTileDownloaderUI.Create(
  AConfig: IDownloadUIConfig;
  AViewPortState: IViewPortState;
  AMapsSet: IActiveMapsSet;
  ADownloadInfo: IDownloadInfoSimple;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger
);
var
  VChangePosListener: IJclListener;
  VOperationNotifier: TOperationNotifier;
begin
  inherited Create(True);
  FConfig := AConfig;

  VOperationNotifier := TOperationNotifier.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;

  FViewPortState := AViewPortState;
  FMapsSet := AMapsSet;
  FDownloadInfo := ADownloadInfo;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FViewPortState := AViewPortState;
  FLinksList := TJclListenerNotifierLinksList.Create;

  FMapType := nil;
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
begin
  FLinksList := nil;
  FCancelNotifier := nil;
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
  FActiveMapsSet := FMapsSet.GetSelectedMapsSet;
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
  FCancelNotifierInternal.NextOperation;
  Terminate;
end;

procedure TTileDownloaderUI.StartThreads;
begin
  inherited;
  FLinksList.ActivateLinks;
  OnConfigChange(nil);
  Resume;
end;

procedure TTileDownloaderUI.AfterWriteToFile;
begin
  if Addr(FMapTileUpdateEvent) <> nil then begin
    FMapTileUpdateEvent(FMapType, FVisualCoordConverter.GetZoom, FLoadXY);
  end;
end;

procedure TTileDownloaderUI.Execute;
var
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
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
  VActiveMapsSet: IMapTypeSet;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMap: IMapType;
  VIteratorsList: IInterfaceList;
  VMapsList: IInterfaceList;
  VAllIteratorsFinished: Boolean;
  VErrorString: string;
  VOperatonID: Integer;
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
        GetCurrentMapAndPos;
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
          VActiveMapsSet := FActiveMapsSet;
          VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
          VZoom := VLocalConverter.GetZoom;
          VGeoConverter := VLocalConverter.GetGeoConverter;
          VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
          VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
          VIteratorsList.Clear;
          VMapsList.Clear;
          VEnum := VActiveMapsSet.GetIterator;
          while VEnum.Next(1, VGUID, i) = S_OK do begin
            VMap := VActiveMapsSet.GetMapTypeByGUID(VGUID);
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
                VOperatonID := FCancelNotifier.CurrentOperation;
                if VNeedDownload then begin
                    VErrorString := '';
                    try
                      VResult := FMapType.DownloadTile(VOperatonID, FCancelNotifier, FLoadXY, VZoom, false);
                      if Terminated then begin
                        break;
                      end;
                      if Supports(VResult, IDownloadResultOk, VResultOk) then begin
                        FDownloadInfo.Add(1, VResultOk.Size);
                        Synchronize(AfterWriteToFile);
                      end else if Supports(VResult, IDownloadResultError, VResultDownloadError) then begin
                        VErrorString := VResultDownloadError.ErrorText;
                      end;
                    except
                      on E: Exception do begin
                        VErrorString := E.Message;
                      end;
                    else
                      VErrorString := SAS_ERR_TileDownloadUnexpectedError;
                    end;
                    if Terminated then begin
                      break;
                    end;

                    if VErrorString <> '' then begin
                      FErrorLogger.LogError(
                        TTileErrorInfo.Create(
                          FMapType,
                          VZoom,
                          FLoadXY,
                          VErrorString
                        )
                      );
                    end;
                end;
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
