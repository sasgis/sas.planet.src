unit u_UiTileDownload;

interface

uses
  Windows,
  SyncObjs,
  i_JclNotify,
  t_CommonTypes,
  i_OperationNotifier,
  i_TTLCheckNotifier,
  i_TTLCheckListener,
  i_DownloadUIConfig,
  i_DownloadInfoSimple,
  i_BackgroundTask,
  i_TileError,
  i_LocalCoordConverterFactorySimpe,
  i_TileDownloaderState,
  i_ViewPortState,
  i_JclListenerNotifierLinksList,
  i_ActiveMapsConfig,
  i_LocalCoordConverter;

type
  TUiTileDownload = class(TInterfacedObject)
  private
    FConfig: IDownloadUIConfig;
    FGCList: ITTLCheckNotifier;
    FAppClosingNotifier: IJclNotifier;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FViewPortState: IViewPortState;
    FMapTypeActive: IActiveMapSingle;
    FDownloadInfo: IDownloadInfoSimple;
    FErrorLogger: ITileErrorLogger;

    FCS: TCriticalSection;
    FLinksList: IJclListenerNotifierLinksList;
    FDownloadTask: IBackgroundTask;
    FTTLListener: ITTLCheckListener;
    FTileDownloadFinishListener: IJclListenerDisconnectable;
    FDownloadState: ITileDownloaderStateChangeble;

    FUseDownload: TTileSource;
    FTileMaxAgeInInternet: TDateTime;
    FTilesOut: Integer;
    FRequestCount: Integer;

    FSemaphore: THandle;
    FCancelEvent: TEvent;
    FCancelListener: IJclListener;
    FMapActive: Boolean;
    FVisualCoordConverter: ILocalCoordConverter;
    FVisualCoordConverterCS: TCriticalSection;

    procedure OnTTLTrim(Sender: TObject);
    procedure OnPosChange;
    procedure OnMapTypeActiveChange;
    procedure OnConfigChange;
    procedure OnCancel;
    procedure DoProcessDownloadRequests(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    );
    procedure RetartDownloadIfNeed;
    procedure OnTileDownloadFinish(AMsg: IInterface);
    procedure OnAppClosing;
  public
    constructor Create(
      AConfig: IDownloadUIConfig;
      AGCList: ITTLCheckNotifier;
      AAppClosingNotifier: IJclNotifier;
      ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      AViewPortState: IViewPortState;
      AMapTypeActive: IActiveMapSingle;
      ADownloadInfo: IDownloadInfoSimple;
      AErrorLogger: ITileErrorLogger
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  Classes,
  SysUtils,
  t_GeoTypes,
  i_TileIterator,
  i_TileRequest,
  i_TileRequestResult,
  i_DownloadResult,
  i_CoordConverter,
  u_GlobalInternetState,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  u_TTLCheckListener,
  u_TileIteratorSpiralByRect,
  u_BackgroundTaskLayerDrawBase,
  u_MapType,
  u_TileErrorInfo;

const
  CMaximumRequestCount = 32;
{ TUiTileDownload }

constructor TUiTileDownload.Create(
  AConfig: IDownloadUIConfig;
  AGCList: ITTLCheckNotifier;
  AAppClosingNotifier: IJclNotifier;
  ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  AViewPortState: IViewPortState;
  AMapTypeActive: IActiveMapSingle;
  ADownloadInfo: IDownloadInfoSimple;
  AErrorLogger: ITileErrorLogger
);
begin
  FConfig := AConfig;
  FGCList :=  AGCList;
  FAppClosingNotifier := AAppClosingNotifier;
  FConverterFactory := ACoordConverterFactory;
  FViewPortState := AViewPortState;
  FMapTypeActive := AMapTypeActive;
  FDownloadInfo := ADownloadInfo;
  FErrorLogger := AErrorLogger;

  FVisualCoordConverterCS := TCriticalSection.Create;

  FDownloadState := FMapTypeActive.GetMapType.MapType.TileDownloadSubsystem.State;

  FRequestCount := 4;

  FSemaphore := CreateSemaphore(nil, FRequestCount, CMaximumRequestCount, nil);
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancel);

  FCS := TCriticalSection.Create;
  FTileDownloadFinishListener := TNotifyEventListener.Create(Self.OnTileDownloadFinish);

  FLinksList := TJclListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMapTypeActiveChange),
    FMapTypeActive.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMapTypeActiveChange),
    FDownloadState.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FViewPortState.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnAppClosing),
    FAppClosingNotifier
  );

  FLinksList.ActivateLinks;
  OnConfigChange;
  OnMapTypeActiveChange;
  OnPosChange;

  FTTLListener := TTTLCheckListener.Create(Self.OnTTLTrim, 30000, 1000);
  FGCList.Add(FTTLListener);
end;

destructor TUiTileDownload.Destroy;
begin
  FTileDownloadFinishListener.Disconnect;

  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;

  FLinksList.DeactivateLinks;
  FCS.Acquire;
  try
    if FDownloadTask <> nil then begin
      FDownloadTask.Terminate;
      FDownloadTask := nil;
    end;
  finally
    FCS.Release;
  end;
  CloseHandle(FSemaphore);
  FreeAndNil(FCancelEvent);
  FreeAndNil(FCS);
  FreeAndNil(FVisualCoordConverterCS);
  inherited;
end;

procedure TUiTileDownload.OnAppClosing;
begin
  OnTTLTrim(nil);
end;

procedure TUiTileDownload.OnCancel;
begin
  FCancelEvent.SetEvent;
end;

procedure TUiTileDownload.OnConfigChange;
begin
  FConfig.LockRead;
  try
    FUseDownload := FConfig.UseDownload;
    FTileMaxAgeInInternet := FConfig.TileMaxAgeInInternet;
    FTilesOut := FConfig.TilesOut;
  finally
    FConfig.UnlockRead;
  end;
  RetartDownloadIfNeed;
end;

procedure TUiTileDownload.OnMapTypeActiveChange;
begin
  if FDownloadState.GetStatic.Enabled then begin
    FMapActive := FMapTypeActive.GetIsActive;
  end else begin
    FMapActive := False;
  end;
  RetartDownloadIfNeed;
end;

procedure TUiTileDownload.OnPosChange;
var
  VConverter: ILocalCoordConverter;
  VNeedRestart: Boolean;
begin
  VConverter :=
    FConverterFactory.CreateBySourceWithStableTileRectAndOtherGeo(
      FViewPortState.GetVisualCoordConverter,
      FMapTypeActive.GetMapType.MapType.GeoConvert
    );
  VNeedRestart := False;
  FVisualCoordConverterCS.Acquire;
  try
    if (FVisualCoordConverter = nil) or not VConverter.GetIsSameConverter(FVisualCoordConverter) then begin
      FVisualCoordConverter := VConverter;
      VNeedRestart := True;
    end;
  finally
    FVisualCoordConverterCS.Release
  end;
  if VNeedRestart then begin
    RetartDownloadIfNeed;
  end;
end;

procedure TUiTileDownload.DoProcessDownloadRequests(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier
);
var
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
  VMapType: TMapType;
  VNeedDownload: Boolean;
  VRequest: ITileRequest;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  FTTLListener.UpdateUseTime;
  VMapType := FMapTypeActive.GetMapType.MapType;
  FVisualCoordConverterCS.Acquire;
  try
    VLocalConverter := FVisualCoordConverter;
  finally
    FVisualCoordConverterCS.Release;
  end;
  if VLocalConverter <> nil then begin
    FCancelEvent.ResetEvent;
    ACancelNotifier.AddListener(FCancelListener);
    try
      VHandles[0] := FCancelEvent.Handle;
      VHandles[1] := FSemaphore;
      VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
      VZoom := VLocalConverter.GetZoom;
      VGeoConverter := VLocalConverter.GetGeoConverter;
      VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
      VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

      VMapGeoConverter := VMapType.GeoConvert;
      VLonLatRectInMap := VLonLatRect;
      VMapGeoConverter.CheckLonLatRect(VLonLatRectInMap);

      VMapTileRect := VMapGeoConverter.LonLatRect2TileRect(VLonLatRectInMap, VZoom);
      Dec(VMapTileRect.Left, FTilesOut);
      Dec(VMapTileRect.Top, FTilesOut);
      Inc(VMapTileRect.Right, FTilesOut);
      Inc(VMapTileRect.Bottom, FTilesOut);
      VMapGeoConverter.CheckTileRect(VMapTileRect, VZoom);
      VIterator := TTileIteratorSpiralByRect.Create(VMapTileRect);

      while VIterator.Next(VTile) do begin
        VNeedDownload := False;
        if VMapType.TileExists(VTile, VZoom) then begin
          if FUseDownload = tsInternet then begin
            if Now - VMapType.TileLoadDate(VTile, VZoom) > FTileMaxAgeInInternet then begin
              VNeedDownload := True;
            end;
          end;
        end else begin
          if (FUseDownload = tsInternet) or (FUseDownload = tsCacheInternet) then begin
            if not(VMapType.TileNotExistsOnServer(VTile, VZoom)) then begin
              VNeedDownload := True;
            end;
          end;
        end;
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
        if VNeedDownload then begin
          VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
          case VWaitResult of
            WAIT_OBJECT_0 + 1: begin
              if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                Break;
              end;
              VRequest := VMapType.TileDownloadSubsystem.GetRequest(ACancelNotifier, AOperationID, VTile, VZoom, False);
              VRequest.FinishNotifier.Add(FTileDownloadFinishListener);
              GInternetState.IncTaskCount;
              VMapType.TileDownloadSubsystem.Download(VRequest);
            end;
          end;
        end;
      end;
    finally
      ACancelNotifier.RemoveListener(FCancelListener);
    end;
  end;
end;

procedure TUiTileDownload.OnTileDownloadFinish(AMsg: IInterface);
var
  VResult: ITileRequestResult;
  VResultWithDownload: ITileRequestResultWithDownloadResult;
  VDownloadResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
  VResultNotNecessary: IDownloadResultNotNecessary;
  VResultDataNotExists: IDownloadResultDataNotExists;
  VErrorString: string;
begin
  GInternetState.DecTaskCount;

  VResult := AMsg as ITileRequestResult;

  ReleaseSemaphore(FSemaphore, 1, nil);
  VErrorString := '';
  if Supports(VResult, ITileRequestResultWithDownloadResult, VResultWithDownload) then begin
    if Supports(VResultWithDownload.DownloadResult, IDownloadResultOk, VDownloadResultOk) then begin
      if FDownloadInfo <> nil then begin
        FDownloadInfo.Add(1, VDownloadResultOk.Size);
      end;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultDataNotExists, VResultDataNotExists) then begin
      VErrorString := VResultDataNotExists.ReasonText;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultError, VResultDownloadError) then begin
      VErrorString := VResultDownloadError.ErrorText;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultNotNecessary, VResultNotNecessary) then begin
      VErrorString := VResultNotNecessary.ReasonText;
    end else begin
      VErrorString := 'Unexpected error';
    end;
  end;

  if VErrorString <> '' then begin
    if FErrorLogger <> nil then begin
      VErrorString := 'Error: ' + VErrorString;
      FErrorLogger.LogError(
        TTileErrorInfo.Create(
          FMapTypeActive.GetMapType.MapType,
          VResult.Request.Zoom,
          VResult.Request.Tile,
          VErrorString
        )
      );
    end;
  end;
end;

procedure TUiTileDownload.OnTTLTrim(Sender: TObject);
var
  VDownloadTask: IBackgroundTask;
begin
  FCS.Acquire;
  try
    VDownloadTask := FDownloadTask;
    if VDownloadTask <> nil then begin
      FDownloadTask := nil;
      VDownloadTask.StopExecute;
      VDownloadTask.Terminate;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TUiTileDownload.RetartDownloadIfNeed;
var
  VDownloadTask: IBackgroundTask;
  VVisualCoordConverter: ILocalCoordConverter;
begin
  FCS.Acquire;
  try
    VDownloadTask := FDownloadTask;
    if VDownloadTask <> nil then begin
      VDownloadTask.StopExecute;
    end;
    FVisualCoordConverterCS.Acquire;
    try
      VVisualCoordConverter := FVisualCoordConverter;
    finally
      FVisualCoordConverterCS.Release;
    end;
    if (FUseDownload in [tsInternet, tsCacheInternet]) and FMapActive and (VVisualCoordConverter <> nil) then begin
      if VDownloadTask = nil then begin
        VDownloadTask := TBackgroundTaskLayerDrawBase.Create(FAppClosingNotifier, DoProcessDownloadRequests, tpLowest);
        VDownloadTask.Start;
        FDownloadTask := VDownloadTask;
      end;
      VDownloadTask.StartExecute;
    end;
  finally
    FCS.Release;
  end;
end;

end.
