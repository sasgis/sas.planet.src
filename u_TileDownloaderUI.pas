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
  i_DownloadUIConfig,
  i_TileDownloaderEvent,
  u_TileDownloaderEventElement,
  u_MapLayerShowError,
  u_MapType,
  u_TileDownloaderThreadBase;

type
  TTileDownloaderUI = class(TTileDownloaderThreadBase)
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
    
    FMaxRequestCount: Integer;
    FSemaphore: THandle;

    function  GetNewEventElement: ITileDownloaderEvent;
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
    procedure OnTileDownload(AEvent: ITileDownloaderEvent);
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
  u_TileIteratorSpiralByRect;

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
  FViewPortState := AViewPortState;
  FLinksList := TJclListenerNotifierLinksList.Create;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorShowLayer := AErrorShowLayer;
  FMaxRequestCount := 4;
  FSemaphore := CreateSemaphore(nil, FMaxRequestCount, FMaxRequestCount, nil);

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
  CloseHandle(FSemaphore);
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

function TTileDownloaderUI.GetNewEventElement: ITileDownloaderEvent;
begin
  Result := TTileDownloaderEventElement.Create(FMapTileUpdateEvent, FErrorShowLayer, FMapType);
  Result.CheckTileSize := False;
  Result.AddToCallBackList(Self.OnTileDownload);
end;

procedure TTileDownloaderUI.OnTileDownload (AEvent: ITileDownloaderEvent);
begin
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

procedure TTileDownloaderUI.Execute;
var
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
  VIteratorsList: IInterfaceList;
  VMapsList: IInterfaceList;
  VAllIteratorsFinished: Boolean;
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
                  repeat
                    if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0  then
                      Break
                    else if Terminated then
                         Break;
                  until False;
                  if not Terminated then
                    FMapType.DownloadTile(GetNewEventElement);
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
