unit u_UiTileDownload;

interface

uses
  Windows,
  SysUtils,
  i_Notifier,
  i_Listener,
  t_CommonTypes,
  i_NotifierOperation,
  i_NotifierTime,
  i_ListenerTime,
  i_DownloadUIConfig,
  i_DownloadInfoSimple,
  i_BackgroundTask,
  i_TileError,
  i_TileRequestTask,
  i_TileRequestResult,
  i_LocalCoordConverterFactorySimpe,
  i_TileDownloaderState,
  i_GlobalInternetState,
  i_LocalCoordConverterChangeable,
  i_ListenerNotifierLinksList,
  i_ActiveMapsConfig,
  i_LocalCoordConverter,
  u_BaseInterfacedObject;

type
  TUiTileDownload = class(TBaseInterfacedObject)
  private
    FConfig: IDownloadUIConfig;
    FGCNotifier: INotifierTime;
    FAppClosingNotifier: INotifierOneOperation;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FViewPortState: ILocalCoordConverterChangeable;
    FMapTypeActive: IActiveMapSingle;
    FDownloadInfo: IDownloadInfoSimple;
    FGlobalInternetState: IGlobalInternetState;
    FErrorLogger: ITileErrorLogger;

    FCS: IReadWriteSync;
    FLinksList: IListenerNotifierLinksList;
    FDownloadTask: IBackgroundTask;
    FTTLListener: IListenerTimeWithUsedFlag;
    FMemCacheTTLListener: IListenerTime;
    FDownloadState: ITileDownloaderStateChangeble;

    FUseDownload: TTileSource;
    FTileMaxAgeInInternet: TDateTime;
    FTilesOut: Integer;
    FRequestCount: Integer;

    FSemaphore: THandle;
    FCancelEventHandle: THandle;
    FCancelListener: IListener;
    FMapActive: Boolean;
    FVisualCoordConverter: ILocalCoordConverter;
    FVisualCoordConverterCS: IReadWriteSync;
    FTaskFinishNotifier: ITileRequestTaskFinishNotifier;
    FSoftCancelNotifier: INotifierOneOperation;
    FHardCancelNotifierInternal: INotifierOperationInternal;

    procedure OnTTLTrim;
    procedure OnMemCacheTTLTrim;
    procedure OnPosChange;
    procedure OnMapTypeActiveChange;
    procedure OnConfigChange;
    procedure OnCancel;
    procedure DoProcessDownloadRequests(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    procedure RestartDownloadIfNeed;
    procedure OnTileDownloadFinish(
      const ATask: ITileRequestTask;
      const AResult: ITileRequestResult
    );
    procedure OnAppClosing;
  public
    constructor Create(
      const AConfig: IDownloadUIConfig;
      const AGCNotifier: INotifierTime;
      const AAppClosingNotifier: INotifierOneOperation;
      const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AMapTypeActive: IActiveMapSingle;
      const ADownloadInfo: IDownloadInfoSimple;
      const AGlobalInternetState: IGlobalInternetState;
      const AErrorLogger: ITileErrorLogger
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  u_Synchronizer,
  t_GeoTypes,
  i_TileIterator,
  i_DownloadResult,
  i_CoordConverter,
  i_TileInfoBasic,
  i_TileStorage,
  u_Notifier,
  u_NotifierOperation,
  u_ListenerNotifierLinksList,
  u_ListenerByEvent,
  u_ListenerTime,
  u_TileRequestTask,
  u_TileIteratorSpiralByRect,
  u_BackgroundTask,
  u_MapType,
  u_GeoFunc,
  u_TileErrorInfo;

{ TUiTileDownload }

constructor TUiTileDownload.Create(
  const AConfig: IDownloadUIConfig;
  const AGCNotifier: INotifierTime;
  const AAppClosingNotifier: INotifierOneOperation;
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AMapTypeActive: IActiveMapSingle;
  const ADownloadInfo: IDownloadInfoSimple;
  const AGlobalInternetState: IGlobalInternetState;
  const AErrorLogger: ITileErrorLogger
);
begin
  inherited Create;
  FConfig := AConfig;
  FGCNotifier := AGCNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FConverterFactory := ACoordConverterFactory;
  FViewPortState := AViewPortState;
  FMapTypeActive := AMapTypeActive;
  FDownloadInfo := ADownloadInfo;
  FGlobalInternetState := AGlobalInternetState;
  FErrorLogger := AErrorLogger;

  FVisualCoordConverterCS := MakeSyncRW_Var(Self);

  FDownloadState := FMapTypeActive.GetMapType.MapType.TileDownloadSubsystem.State;

  FRequestCount := FConfig.MapUiRequestCount;

  FSemaphore := CreateSemaphore(nil, FRequestCount, FRequestCount, nil);
  FCancelEventHandle := CreateEvent(nil, TRUE, FALSE, nil);
  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancel);

  FSoftCancelNotifier := nil;
  FHardCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);

  FCS := MakeSyncRW_Std(Self, False);
  FTaskFinishNotifier := TTileRequestTaskFinishNotifier.Create(Self.OnTileDownloadFinish);

  FLinksList := TListenerNotifierLinksList.Create;

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

  FTTLListener := TListenerTTLCheck.Create(Self.OnTTLTrim, 300000);
  FGCNotifier.Add(FTTLListener);

  if
    FMapTypeActive.GetMapType.MapType.StorageConfig.UseMemCache and
    FMapTypeActive.GetMapType.MapType.Zmp.TileDownloaderConfig.RestartDownloadOnMemCacheTTL
  then begin
    FMemCacheTTLListener := TListenerTimeCheck.Create(
      Self.OnMemCacheTTLTrim,
      FMapTypeActive.GetMapType.MapType.StorageConfig.MemCacheTTL
    );
    FGCNotifier.Add(FMemCacheTTLListener);
  end else begin
    FMemCacheTTLListener := nil;
  end;

  FLinksList.ActivateLinks;
  OnConfigChange;
  OnMapTypeActiveChange;
  OnPosChange;
end;

destructor TUiTileDownload.Destroy;
begin
  if Assigned(FTaskFinishNotifier) then begin
    FTaskFinishNotifier.Enabled := False;
    FTaskFinishNotifier := nil;
  end;

  if Assigned(FGCNotifier) then begin
    if Assigned(FMemCacheTTLListener) then begin
      FGCNotifier.Remove(FMemCacheTTLListener);
      FMemCacheTTLListener := nil;
    end;
    if Assigned(FTTLListener) then begin
      FGCNotifier.Remove(FTTLListener);
      FTTLListener := nil;
    end;
    FGCNotifier := nil;
  end;
  if Assigned(FLinksList) then begin
    FLinksList.DeactivateLinks;
  end;

  if Assigned(FCS) then begin
    FCS.BeginWrite;
    try
      if Assigned(FDownloadTask) then begin
        FDownloadTask.StopExecute;
        FDownloadTask.Terminate;
        FDownloadTask := nil;
      end;
    finally
      FCS.EndWrite;
    end;
    CloseHandle(FSemaphore);
    CloseHandle(FCancelEventHandle);
  end;

  FCS := nil;
  FVisualCoordConverterCS := nil;

  inherited;
end;

procedure TUiTileDownload.OnAppClosing;
begin
  if Assigned(FHardCancelNotifierInternal) then begin
    FHardCancelNotifierInternal.NextOperation;
  end;
  if Assigned(FTaskFinishNotifier) then begin
    FTaskFinishNotifier.Enabled := False;
  end;
  OnTTLTrim;
end;

procedure TUiTileDownload.OnCancel;
begin
  SetEvent(FCancelEventHandle);
end;

procedure TUiTileDownload.OnMemCacheTTLTrim;
begin
  RestartDownloadIfNeed;
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
  if not (FUseDownload in [tsInternet, tsCacheInternet]) then begin
    if Assigned(FHardCancelNotifierInternal) then begin
      FHardCancelNotifierInternal.NextOperation;
    end;
  end;
  RestartDownloadIfNeed;
end;

procedure TUiTileDownload.OnMapTypeActiveChange;
begin
  if FDownloadState.GetStatic.Enabled then begin
    FMapActive := FMapTypeActive.GetIsActive;
  end else begin
    FMapActive := False;
  end;
  if not FMapActive then begin
    if Assigned(FHardCancelNotifierInternal) then begin
      FHardCancelNotifierInternal.NextOperation;
    end;
  end;
  RestartDownloadIfNeed;
end;

procedure TUiTileDownload.OnPosChange;
var
  VConverter: ILocalCoordConverter;
  VNeedRestart: Boolean;
begin
  VConverter :=
    FConverterFactory.CreateBySourceWithTileRectAndOtherGeo(
      FViewPortState.GetStatic,
      FMapTypeActive.GetMapType.MapType.GeoConvert
    );
  VNeedRestart := False;

  FVisualCoordConverterCS.BeginWrite;
  try
    if (FVisualCoordConverter = nil) or not VConverter.GetIsSameConverter(FVisualCoordConverter) then begin
      FVisualCoordConverter := VConverter;
      VNeedRestart := True;
    end;
  finally
    FVisualCoordConverterCS.EndWrite;
  end;

  if VNeedRestart then begin
    RestartDownloadIfNeed;
  end;
end;

procedure TUiTileDownload.DoProcessDownloadRequests(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
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
  VTask: ITileRequestTask;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
  VTileInfo: ITileInfoBasic;
  VCurrentOperation: Integer;
begin
  FTTLListener.UpdateUseTime;
  VMapType := FMapTypeActive.GetMapType.MapType;

  FVisualCoordConverterCS.BeginRead;
  try
    VLocalConverter := FVisualCoordConverter;
  finally
    FVisualCoordConverterCS.EndRead;
  end;

  if VLocalConverter <> nil then begin
    ResetEvent(FCancelEventHandle);
    ACancelNotifier.AddListener(FCancelListener);
    FSoftCancelNotifier := TNotifierOneOperationByNotifier.Create(ACancelNotifier, AOperationID);
    VCurrentOperation := FHardCancelNotifierInternal.CurrentOperation;
    try
      VHandles[0] := FCancelEventHandle;
      VHandles[1] := FSemaphore;
      VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
      VZoom := VLocalConverter.GetZoom;
      VGeoConverter := VLocalConverter.GetGeoConverter;
      VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
      VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

      VMapGeoConverter := VMapType.GeoConvert;
      VLonLatRectInMap := VLonLatRect;
      VMapGeoConverter.CheckLonLatRect(VLonLatRectInMap);

      VMapTileRect :=
        RectFromDoubleRect(
          VMapGeoConverter.LonLatRect2TileRectFloat(VLonLatRectInMap, VZoom),
          rrOutside
        );
      Dec(VMapTileRect.Left, FTilesOut);
      Dec(VMapTileRect.Top, FTilesOut);
      Inc(VMapTileRect.Right, FTilesOut);
      Inc(VMapTileRect.Bottom, FTilesOut);
      VMapGeoConverter.CheckTileRect(VMapTileRect, VZoom);
      VIterator := TTileIteratorSpiralByRect.Create(VMapTileRect);

      while VIterator.Next(VTile) do begin
        VNeedDownload := False;
        VTileInfo := VMapType.TileStorage.GetTileInfo(VTile, VZoom, VMapType.VersionConfig.Version, gtimAsIs);
        if VTileInfo.IsExists then begin
          if FUseDownload = tsInternet then begin
            if Now - VTileInfo.LoadDate > FTileMaxAgeInInternet then begin
              VNeedDownload := True;
            end;
          end;
        end else begin
          if (FUseDownload = tsInternet) or (FUseDownload = tsCacheInternet) then begin
            if not VTileInfo.IsExistsTNE then begin
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
                ReleaseSemaphore(FSemaphore, 1, nil);
                Break;
              end;

              VTask :=
                VMapType.TileDownloadSubsystem.GetRequestTask(
                  FSoftCancelNotifier,
                  FHardCancelNotifierInternal as INotifierOperation,
                  VCurrentOperation,
                  FTaskFinishNotifier,
                  VTile,
                  VZoom,
                  VMapType.VersionConfig.Version,
                  False
                );

              if VTask <> nil then begin
                FGlobalInternetState.IncQueueCount;
                VMapType.TileDownloadSubsystem.Download(VTask);
              end else begin
                ReleaseSemaphore(FSemaphore, 1, nil);
              end;
            end;
          end;
        end;
      end;
    finally
      ACancelNotifier.RemoveListener(FCancelListener);
    end;
  end;
end;

procedure TUiTileDownload.OnTileDownloadFinish(
  const ATask: ITileRequestTask;
  const AResult: ITileRequestResult
);
var
  VResultWithDownload: ITileRequestResultWithDownloadResult;
  VDownloadResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
  VResultNotNecessary: IDownloadResultNotNecessary;
  VResultDataNotExists: IDownloadResultDataNotExists;
  VRequestError: ITileRequestResultError;
  VError: ITileErrorInfo;
begin
  Assert(ATask <> nil);

  FTTLListener.UpdateUseTime;

  FGlobalInternetState.DecQueueCount;
  ReleaseSemaphore(FSemaphore, 1, nil);

  VError := nil;
  if Supports(AResult, ITileRequestResultError, VRequestError) then begin
    VError :=
      TTileErrorInfoByTileRequestResult.Create(
        FMapTypeActive.GetMapType.GUID,
        VRequestError
      );
  end else if Supports(AResult, ITileRequestResultWithDownloadResult, VResultWithDownload) then begin
    if Supports(VResultWithDownload.DownloadResult, IDownloadResultOk, VDownloadResultOk) then begin
      if FDownloadInfo <> nil then begin
        FDownloadInfo.Add(1, VDownloadResultOk.Data.Size);
      end;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultDataNotExists, VResultDataNotExists) then begin
      VError :=
        TTileErrorInfoByDataNotExists.Create(
          FMapTypeActive.GetMapType.GUID,
          ATask.TileRequest.Zoom,
          ATask.TileRequest.Tile,
          VResultDataNotExists
        );
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultError, VResultDownloadError) then begin
      VError :=
        TTileErrorInfoByDownloadResultError.Create(
          FMapTypeActive.GetMapType.GUID,
          ATask.TileRequest.Zoom,
          ATask.TileRequest.Tile,
          VResultDownloadError
        );
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultNotNecessary, VResultNotNecessary) then begin
      VError :=
        TTileErrorInfoByNotNecessary.Create(
          FMapTypeActive.GetMapType.GUID,
          ATask.TileRequest.Zoom,
          ATask.TileRequest.Tile,
          VResultNotNecessary
        );
    end else begin
      VError :=
        TTileErrorInfo.Create(
          FMapTypeActive.GetMapType.GUID,
          ATask.TileRequest.Zoom,
          ATask.TileRequest.Tile,
          'Unexpected error'
        );
    end;
  end;
  if VError <> nil then begin
    if FErrorLogger <> nil then begin
      FErrorLogger.LogError(VError);
    end;
  end;
end;

procedure TUiTileDownload.OnTTLTrim;
var
  VDownloadTask: IBackgroundTask;
begin
  FCS.BeginWrite;
  try
    VDownloadTask := FDownloadTask;
    if VDownloadTask <> nil then begin
      FDownloadTask := nil;
      VDownloadTask.StopExecute;
      VDownloadTask.Terminate;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TUiTileDownload.RestartDownloadIfNeed;
var
  VDownloadTask: IBackgroundTask;
  VVisualCoordConverter: ILocalCoordConverter;
begin
  FCS.BeginWrite;
  try
    VDownloadTask := FDownloadTask;
    if VDownloadTask <> nil then begin
      VDownloadTask.StopExecute;
    end;

    if (FUseDownload in [tsInternet, tsCacheInternet]) and FMapActive then begin
      // allow download
      FVisualCoordConverterCS.BeginRead;
      try
        VVisualCoordConverter := FVisualCoordConverter;
      finally
        FVisualCoordConverterCS.EndRead;
      end;

      if (VVisualCoordConverter <> nil) then begin
        if VDownloadTask = nil then begin
          VDownloadTask := TBackgroundTask.Create(
            FAppClosingNotifier,
            Self.DoProcessDownloadRequests,
            FConfig.ThreadConfig,
            Self.ClassName
          );
          VDownloadTask.Start;
          FDownloadTask := VDownloadTask;
        end;
        VDownloadTask.StartExecute;
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

end.
