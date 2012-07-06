unit u_UiTileDownload;

interface

uses
  Windows,
  SysUtils,
  i_Notifier,
  i_Listener,
  t_CommonTypes,
  i_NotifierOperation,
  i_NotifierTTLCheck,
  i_ListenerTTLCheck,
  i_DownloadUIConfig,
  i_DownloadInfoSimple,
  i_BackgroundTask,
  i_TileError,
  i_LocalCoordConverterFactorySimpe,
  i_TileDownloaderState,
  i_GlobalInternetState,
  i_LocalCoordConverterChangeable,
  i_ListenerNotifierLinksList,
  i_ActiveMapsConfig,
  i_LocalCoordConverter;

type
  TUiTileDownload = class(TInterfacedObject)
  private
    FConfig: IDownloadUIConfig;
    FGCList: INotifierTTLCheck;
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
    FTTLListener: IListenerTTLCheck;
    FTileDownloadFinishListener: IListenerDisconnectable;
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

    procedure OnTTLTrim(Sender: TObject);
    procedure OnPosChange;
    procedure OnMapTypeActiveChange;
    procedure OnConfigChange;
    procedure OnCancel;
    procedure DoProcessDownloadRequests(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    procedure RetartDownloadIfNeed;
    procedure OnTileDownloadFinish(const AMsg: IInterface);
    procedure OnAppClosing;
  public
    constructor Create(
      const AConfig: IDownloadUIConfig;
      const AGCList: INotifierTTLCheck;
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
  i_TileRequest,
  i_TileRequestResult,
  i_DownloadResult,
  i_CoordConverter,
  u_ListenerNotifierLinksList,
  u_ListenerByEvent,
  u_ListenerTTLCheck,
  u_TileIteratorSpiralByRect,
  u_BackgroundTask,
  u_MapType,
  u_TileErrorInfo;

{ TUiTileDownload }

constructor TUiTileDownload.Create(
  const AConfig: IDownloadUIConfig;
  const AGCList: INotifierTTLCheck;
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
  FGCList := AGCList;
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

  FCS := MakeSyncRW_Std(Self, False);
  FTileDownloadFinishListener := TNotifyEventListener.Create(Self.OnTileDownloadFinish);

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

  FLinksList.ActivateLinks;
  OnConfigChange;
  OnMapTypeActiveChange;
  OnPosChange;

  FTTLListener := TListenerTTLCheck.Create(Self.OnTTLTrim, 30000, 1000);
  FGCList.Add(FTTLListener);
end;

destructor TUiTileDownload.Destroy;
begin
  FTileDownloadFinishListener.Disconnect;

  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;

  FLinksList.DeactivateLinks;

  FCS.BeginWrite;
  try
    if FDownloadTask <> nil then begin
      FDownloadTask.StopExecute;
      FDownloadTask.Terminate;
      FDownloadTask := nil;
    end;
  finally
    FCS.EndWrite;
  end;

  CloseHandle(FSemaphore);
  CloseHandle(FCancelEventHandle);

  FCS := nil;
  FVisualCoordConverterCS := nil;

  inherited;
end;

procedure TUiTileDownload.OnAppClosing;
begin
  OnTTLTrim(nil);
end;

procedure TUiTileDownload.OnCancel;
begin
  SetEvent(FCancelEventHandle);
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
    RetartDownloadIfNeed;
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
  VRequest: ITileRequest;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
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
            if not (VMapType.TileNotExistsOnServer(VTile, VZoom)) then begin
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
              FGlobalInternetState.IncQueueCount;
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

procedure TUiTileDownload.OnTileDownloadFinish(const AMsg: IInterface);
var
  VResult: ITileRequestResult;
  VResultWithDownload: ITileRequestResultWithDownloadResult;
  VDownloadResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
  VResultNotNecessary: IDownloadResultNotNecessary;
  VResultDataNotExists: IDownloadResultDataNotExists;
  VRequestError: ITileRequestResultError;
  VErrorString: string;
  VError: ITileErrorInfo;
begin
  FGlobalInternetState.DecQueueCount;

  VResult := AMsg as ITileRequestResult;

  ReleaseSemaphore(FSemaphore, 1, nil);
  VErrorString := '';
  if Supports(VResult, ITileRequestResultWithDownloadResult, VResultWithDownload) then begin
    if Supports(VResultWithDownload.DownloadResult, IDownloadResultOk, VDownloadResultOk) then begin
      if FDownloadInfo <> nil then begin
        FDownloadInfo.Add(1, VDownloadResultOk.Data.Size);
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
  end else begin
    if Supports(VResult, ITileRequestResultError, VRequestError) then begin
      VErrorString := VRequestError.ErrorText;
    end;
  end;

  if VErrorString <> '' then begin
    if FErrorLogger <> nil then begin
      VErrorString := 'Error: ' + VErrorString;
      VError :=
        TTileErrorInfo.Create(
          FMapTypeActive.GetMapType.MapType,
          VResult.Request.Zoom,
          VResult.Request.Tile,
          VErrorString
        );
      FErrorLogger.LogError(VError);
    end;
  end;
end;

procedure TUiTileDownload.OnTTLTrim(Sender: TObject);
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

procedure TUiTileDownload.RetartDownloadIfNeed;
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
          VDownloadTask := TBackgroundTask.Create(FAppClosingNotifier, DoProcessDownloadRequests, FConfig.ThreadConfig);
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
