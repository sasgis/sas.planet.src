{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_UiTileDownload;

interface

uses
  SysUtils,
  i_Notifier,
  t_CommonTypes,
  i_NotifierOperation,
  i_NotifierTime,
  i_ListenerTime,
  i_DownloadUIConfig,
  i_DownloadInfoSimple,
  i_BackgroundTask,
  i_MapType,
  i_MapTypeSetChangeable,
  i_TileError,
  i_TileRequestTask,
  i_TileRequestResult,
  i_LocalCoordConverterFactorySimpe,
  i_TileDownloaderState,
  i_GlobalInternetState,
  i_LocalCoordConverterChangeable,
  i_ListenerNotifierLinksList,
  i_LocalCoordConverter,
  u_UiTileRequestManager,
  u_BaseInterfacedObject;

type
  TUiTileDownload = class(TBaseInterfacedObject)
  private
    FConfig: IDownloadUIConfig;
    FGCNotifier: INotifierTime;
    FAppClosingNotifier: INotifierOneOperation;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FViewPortState: ILocalCoordConverterChangeable;
    FMapType: IMapType;
    FActiveMaps: IMapTypeSetChangeable;
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

    FRequestManager: TUiTileRequestManager;

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
    procedure OnVersionConfigChange;
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
      const AMapType: IMapType;
      const AActiveMaps: IMapTypeSetChangeable;
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
  i_DownloadResult,
  i_CoordConverter,
  i_TileInfoBasic,
  i_TileStorage,
  i_TileRequest,
  i_MapVersionRequest,
  u_Notifier,
  u_NotifierOperation,
  u_ListenerNotifierLinksList,
  u_ListenerByEvent,
  u_ListenerTime,
  u_TileRequestTask,
  u_BackgroundTask,
  u_GeoFunc,
  u_TileErrorInfo;

{ TUiTileDownload }

constructor TUiTileDownload.Create(
  const AConfig: IDownloadUIConfig;
  const AGCNotifier: INotifierTime;
  const AAppClosingNotifier: INotifierOneOperation;
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AMapType: IMapType;
  const AActiveMaps: IMapTypeSetChangeable;
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
  FMapType := AMapType;
  FActiveMaps := AActiveMaps;
  FDownloadInfo := ADownloadInfo;
  FGlobalInternetState := AGlobalInternetState;
  FErrorLogger := AErrorLogger;

  FVisualCoordConverterCS := GSync.SyncVariable.Make(Self.ClassName);

  FDownloadState := FMapType.TileDownloadSubsystem.State;

  FRequestManager := TUiTileRequestManager.Create(FConfig.MapUiRequestCount);

  FSoftCancelNotifier := nil;
  FHardCancelNotifierInternal :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );

  FCS := GSync.SyncStd.Make(Self.ClassName);
  FTaskFinishNotifier := TTileRequestTaskFinishNotifier.Create(Self.OnTileDownloadFinish);

  FLinksList := TListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMapTypeActiveChange),
    FActiveMaps.ChangeNotifier
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
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnVersionConfigChange),
    FMapType.VersionRequestConfig.ChangeNotifier
  );

  FTTLListener := TListenerTTLCheck.Create(Self.OnTTLTrim, 300000);
  FGCNotifier.Add(FTTLListener);

  if
    FMapType.StorageConfig.UseMemCache and
    FMapType.Zmp.TileDownloaderConfig.RestartDownloadOnMemCacheTTL
  then begin
    FMemCacheTTLListener := TListenerTimeCheck.Create(
      Self.OnMemCacheTTLTrim,
      FMapType.StorageConfig.MemCacheTTL
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
  end;

  FreeAndNil(FRequestManager);

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

procedure TUiTileDownload.OnMemCacheTTLTrim;
begin
  RestartDownloadIfNeed;
end;

procedure TUiTileDownload.OnVersionConfigChange;
begin
  if Assigned(FHardCancelNotifierInternal) then begin
    FHardCancelNotifierInternal.NextOperation;
  end;
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
    FMapActive := FActiveMaps.GetStatic.IsExists(FMapType.GUID);
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
      FMapType.GeoConvert
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
  VTile: TPoint;
  VLocalConverter: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VMapGeoConverter: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VLonLatRectInMap: TDoubleRect;
  VMapTileRect: TRect;
  VZoom: Byte;
  VNeedDownload: Boolean;
  VTask: ITileRequestTask;
  VVersionInfo: IMapVersionRequest;
  VTileInfo: ITileInfoBasic;
  VCurrentOperation: Integer;
  VStorage: ITileStorage;
begin
  FTTLListener.UpdateUseTime;

  VStorage := FMapType.TileStorage;
  VVersionInfo := FMapType.VersionRequestConfig.GetStatic;

  FVisualCoordConverterCS.BeginRead;
  try
    VLocalConverter := FVisualCoordConverter;
  finally
    FVisualCoordConverterCS.EndRead;
  end;

  if VLocalConverter <> nil then begin
    ACancelNotifier.AddListener(FRequestManager.SessionCancelListener);
    FSoftCancelNotifier := TNotifierOneOperationByNotifier.Create(ACancelNotifier, AOperationID);
    VCurrentOperation := FHardCancelNotifierInternal.CurrentOperation;
    try
      VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
      VZoom := VLocalConverter.GetZoom;
      VGeoConverter := VLocalConverter.GetGeoConverter;
      VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
      VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

      VMapGeoConverter := FMapType.GeoConvert;
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

      FRequestManager.InitSession(VZoom, VMapTileRect, VVersionInfo.BaseVersion);

      while FRequestManager.Acquire(VTile) do begin
        VNeedDownload := False;
        VTileInfo := VStorage.GetTileInfoEx(VTile, VZoom, VVersionInfo, gtimWithoutData);

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
          FRequestManager.Release(VTile, VZoom, VVersionInfo.BaseVersion, True);
          Break;
        end;

        VTask := nil;
        if VNeedDownload then begin
          VTask :=
            FMapType.TileDownloadSubsystem.GetRequestTask(
              FSoftCancelNotifier,
              FHardCancelNotifierInternal as INotifierOperation,
              VCurrentOperation,
              FTaskFinishNotifier,
              VTile,
              VZoom,
              VVersionInfo.BaseVersion,
              False
            );
        end;

        if VTask <> nil then begin
          FGlobalInternetState.IncQueueCount;
          FMapType.TileDownloadSubsystem.Download(VTask);
        end else begin
          FRequestManager.Release(VTile, VZoom, VVersionInfo.BaseVersion, VNeedDownload);
        end;
      end;
    finally
      ACancelNotifier.RemoveListener(FRequestManager.SessionCancelListener);
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
  VTileRequest: ITileRequest;
begin
  Assert(ATask <> nil);

  FTTLListener.UpdateUseTime;

  FGlobalInternetState.DecQueueCount;

  Assert(Assigned(FRequestManager));

  VTileRequest := ATask.TileRequest;
  FRequestManager.Release(
    VTileRequest.Tile,
    VTileRequest.Zoom,
    VTileRequest.VersionInfo,
    Supports(AResult, ITileRequestResultCanceled)
  );

  VError := nil;
  if Supports(AResult, ITileRequestResultError, VRequestError) then begin
    VError :=
      TTileErrorInfoByTileRequestResult.Create(
        FMapType.GUID,
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
          FMapType.GUID,
          ATask.TileRequest.Zoom,
          ATask.TileRequest.Tile,
          VResultDataNotExists
        );
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultError, VResultDownloadError) then begin
      VError :=
        TTileErrorInfoByDownloadResultError.Create(
          FMapType.GUID,
          ATask.TileRequest.Zoom,
          ATask.TileRequest.Tile,
          VResultDownloadError
        );
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultNotNecessary, VResultNotNecessary) then begin
      VError :=
        TTileErrorInfoByNotNecessary.Create(
          FMapType.GUID,
          ATask.TileRequest.Zoom,
          ATask.TileRequest.Tile,
          VResultNotNecessary
        );
    end else begin
      VError :=
        TTileErrorInfo.Create(
          FMapType.GUID,
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
