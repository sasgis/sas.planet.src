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

unit u_TileDownloadSubsystem;

interface

uses
  Types,
  SysUtils,
  i_Listener,
  i_NotifierOperation,
  i_BinaryDataListStatic,
  i_CoordConverterFactory,
  i_CoordConverter,
  i_ThreadConfig,
  i_NotifierTime,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_ContentTypeSubst,
  i_TilePostDownloadCropConfig,
  i_DownloadResultFactory,
  i_LanguageManager,
  i_GlobalDownloadConfig,
  i_ContentTypeInfo,
  i_TileRequest,
  i_TileRequestTask,
  i_TileDownloaderState,
  i_TileDownloaderConfig,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory,
  i_TileDownloader,
  i_TileDownloadResultSaver,
  i_MapAbilitiesConfig,
  i_ImageResamplerFactoryChangeable,
  i_MapVersionInfo,
  i_Bitmap32StaticFactory,
  i_InvisibleBrowser,
  i_ProjConverter,
  i_TileStorage,
  i_TileDownloadSubsystem,
  u_BaseInterfacedObject;

type
  TTileDownloadSubsystem = class(TBaseInterfacedObject, ITileDownloadSubsystem)
  private
    FTileDownloaderConfig: ITileDownloaderConfig;
    FTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    FCoordConverter: ICoordConverter;
    FAppClosingNotifier: INotifierOneOperation;

    FDestroyNotifierInternal: INotifierOperationInternal;
    FDestroyNotifier: INotifierOperation;
    FDestroyOperationID: Integer;
    FAppClosingListener: IListener;
    FTileRequestTaskSync: IReadWriteSync;

    FZmpDownloadEnabled: Boolean;
    FState: ITileDownloaderStateChangeble;
    FDownloadResultSaver: ITileDownloadResultSaver;
    FTileDownloader: ITileDownloaderAsync;
    FTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
    FTileDownloadRequestBuilderFactory: ITileDownloadRequestBuilderFactory;
    function GetScriptText(const AConfig: IConfigDataProvider): AnsiString;
    procedure OnAppClosing;
  private
    { ITileDownloadSubsystem }
    function GetRequestTask(
      const ASoftCancelNotifier: INotifierOneOperation;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const AFinishNotifier: ITileRequestTaskFinishNotifier;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionInfo;
      const ACheckTileSize: Boolean
    ): ITileRequestTask;
    function GetLink(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionInfo
    ): string;
    procedure Download(
      const ATileRequestTask: ITileRequestTask
    );
    function GetState: ITileDownloaderStateChangeble;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AAppClosingNotifier: INotifierOneOperation;
      const ACoordConverter: ICoordConverter;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALanguageManager: ILanguageManager;
      const AGlobalDownloadConfig: IGlobalDownloadConfig;
      const AInvisibleBrowser: IInvisibleBrowser;
      const ADownloadResultFactory: IDownloadResultFactory;
      const AZmpTileDownloaderConfig: ITileDownloaderConfigStatic;
      const AImageResampler: IImageResamplerFactoryChangeable;
      const ABitmapFactory: IBitmap32BufferFactory;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const AThreadConfig: IThreadConfig;
      const ATileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
      const AContentTypeManager: IContentTypeManager;
      const AContentTypeSubst: IContentTypeSubst;
      const ASaveContentType: IContentTypeInfoBasic;
      const ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
      const AEmptyTileSamples: IBinaryDataListStatic;
      const ABanTileSamples: IBinaryDataListStatic;
      const AMapAbilitiesConfig: IMapAbilitiesConfig;
      const AZmpData: IConfigDataProvider;
      const AProjFactory: IProjConverterFactory;
      const AStorage: ITileStorage
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_TileDownloadRequest,
  i_TileDownloaderList,
  i_PredicateByBinaryData,
  i_DownloadChecker,
  u_Notifier,
  u_NotifierOperation,
  u_ListenerByEvent,
  u_TileRequest,
  u_TileRequestTask,
  u_TileDownloaderList,
  u_AntiBanStuped,
  u_DownloaderFaked,
  u_DownloadCheckerStuped,
  u_PredicateByStaticSampleList,
  u_TileDownloadRequestBuilderLazy,
  u_TileDownloadSubsystemState,
  u_TileDownloadResultSaverStuped,
  u_TileDownloaderWithQueue,
  u_Synchronizer,
  u_TileDownloadRequestBuilderFactoryPascalScript;

const
  PascalScriptFileName = 'GetUrlScript.txt';

{ TTileDownloadSubsystem }

constructor TTileDownloadSubsystem.Create(
  const AGCNotifier: INotifierTime;
  const AAppClosingNotifier: INotifierOneOperation;
  const ACoordConverter: ICoordConverter;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALanguageManager: ILanguageManager;
  const AGlobalDownloadConfig: IGlobalDownloadConfig;
  const AInvisibleBrowser: IInvisibleBrowser;
  const ADownloadResultFactory: IDownloadResultFactory;
  const AZmpTileDownloaderConfig: ITileDownloaderConfigStatic;
  const AImageResampler: IImageResamplerFactoryChangeable;
  const ABitmapFactory: IBitmap32BufferFactory;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const AThreadConfig: IThreadConfig;
  const ATileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
  const AContentTypeManager: IContentTypeManager;
  const AContentTypeSubst: IContentTypeSubst;
  const ASaveContentType: IContentTypeInfoBasic;
  const ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
  const AEmptyTileSamples: IBinaryDataListStatic;
  const ABanTileSamples: IBinaryDataListStatic;
  const AMapAbilitiesConfig: IMapAbilitiesConfig;
  const AZmpData: IConfigDataProvider;
  const AProjFactory: IProjConverterFactory;
  const AStorage: ITileStorage
);
var
  VDownloaderList: ITileDownloaderList;
  VDownloadChecker: IDownloadChecker;
  VOperationNotifier: TNotifierOperation;
  VEmptyPredicate: IPredicateByBinaryData;
  VBanPredicate: IPredicateByBinaryData;
begin
  inherited Create;
  FCoordConverter := ACoordConverter;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FTileDownloadRequestBuilderConfig := ATileDownloadRequestBuilderConfig;
  FAppClosingNotifier := AAppClosingNotifier;

  VOperationNotifier := TNotifierOperation.Create(TNotifierBase.Create);
  FDestroyNotifierInternal := VOperationNotifier;
  FDestroyNotifier := VOperationNotifier;
  FDestroyOperationID := FDestroyNotifier.CurrentOperation;

  FZmpDownloadEnabled := AZmpTileDownloaderConfig.Enabled;

  if FZmpDownloadEnabled then begin
    if ABanTileSamples <> nil then begin
      VBanPredicate := TPredicateByStaticSampleList.Create(ABanTileSamples);
    end;

    if AEmptyTileSamples <> nil then begin
      VEmptyPredicate := TPredicateByStaticSampleList.Create(AEmptyTileSamples);
    end;

    VDownloadChecker := TDownloadCheckerStuped.Create(
      TAntiBanStuped.Create(AInvisibleBrowser, AZmpData),
      VBanPredicate,
      VEmptyPredicate,
      FTileDownloaderConfig,
      AStorage
    );
    FTileDownloadRequestBuilderFactory :=
      TTileDownloadRequestBuilderFactoryPascalScript.Create(
        GetScriptText(AZmpData),
        FTileDownloadRequestBuilderConfig,
        FTileDownloaderConfig,
        FCoordConverter,
        VDownloadChecker,
        AProjFactory,
        ALanguageManager
      );

    FTileDownloadRequestBuilder :=
      TTileDownloadRequestBuilderLazy.Create(
        TDownloaderFaked.Create(ADownloadResultFactory),
        FTileDownloadRequestBuilderFactory
      );
    FDownloadResultSaver :=
      TTileDownloadResultSaverStuped.Create(
        AGlobalDownloadConfig,
        AImageResampler,
        ABitmapFactory,
        AContentTypeManager,
        AContentTypeSubst,
        ASaveContentType,
        ATilePostDownloadCropConfig,
        AStorage
      );

    FState :=
      TTileDownloadSubsystemState.Create(
        FZmpDownloadEnabled,
        FTileDownloadRequestBuilderFactory.State,
        FDownloadResultSaver.State,
        AMapAbilitiesConfig
      );

    VDownloaderList :=
      TTileDownloaderList.Create(
        AGCNotifier,
        AAppClosingNotifier,
        ADownloadResultFactory,
        FState,
        FTileDownloaderConfig,
        FDownloadResultSaver,
        FTileDownloadRequestBuilderFactory
      );
    FTileDownloader :=
      TTileDownloaderWithQueue.Create(
        VDownloaderList,
        AGCNotifier,
        AThreadConfig,
        AAppClosingNotifier,
        256
      );
    FTileRequestTaskSync := MakeSyncRW_Var(Self, False);
  end else begin
    FState :=
      TTileDownloadSubsystemState.Create(
        FZmpDownloadEnabled,
        nil,
        nil,
        nil
      );

  end;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TTileDownloadSubsystem.Destroy;
begin
  if Assigned(FDestroyNotifierInternal) then begin
    FDestroyNotifierInternal.NextOperation;
  end;
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingListener := nil;
    FAppClosingNotifier := nil;
  end;
  inherited;
end;

procedure TTileDownloadSubsystem.Download(
  const ATileRequestTask: ITileRequestTask
);
var
  VTaskInternal: ITileRequestTaskInternal;
begin
  if Supports(ATileRequestTask, ITileRequestTaskInternal, VTaskInternal) then begin
    if FZmpDownloadEnabled then begin
      if FState.GetStatic.Enabled then begin
        FTileDownloader.Download(ATileRequestTask);
      end else begin
        VTaskInternal.SetFinished(nil);
      end;
    end else begin
      VTaskInternal.SetFinished(nil);
    end;
  end;
end;

function TTileDownloadSubsystem.GetLink(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersion: IMapVersionInfo
): string;
var
  VRequest: ITileRequest;
  VDownloadRequest: ITileDownloadRequest;
begin
  Result := '';
  if FZmpDownloadEnabled then begin
    if FTileDownloadRequestBuilderFactory.State.GetStatic.Enabled then begin
      VRequest :=
        TTileRequest.Create(
          AXY,
          AZoom,
          AVersion
        );
      VDownloadRequest := nil;
      if VRequest <> nil then begin
        VDownloadRequest := FTileDownloadRequestBuilder.BuildRequest(VRequest, nil, FDestroyNotifier, FDestroyOperationID);
      end;
      if VDownloadRequest <> nil then begin
        Result := VDownloadRequest.Url;
      end;
    end;
  end;
end;

function TTileDownloadSubsystem.GetRequestTask(
  const ASoftCancelNotifier: INotifierOneOperation;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const AFinishNotifier: ITileRequestTaskFinishNotifier;
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersion: IMapVersionInfo;
  const ACheckTileSize: Boolean
): ITileRequestTask;
var
  VRequest: ITileRequest;
  VZoom: Byte;
  VTile: TPoint;
begin
  Result := nil;
  if FZmpDownloadEnabled then begin
    if FState.GetStatic.Enabled then begin
      VZoom := AZoom;
      VTile := AXY;
      if FCoordConverter.CheckTilePosStrict(VTile, VZoom, False) then begin
        if ACheckTileSize then begin
          VRequest :=
            TTileRequestWithSizeCheck.Create(
              VTile,
              VZoom,
              AVersion
            );
        end else begin
          VRequest :=
            TTileRequest.Create(
              VTile,
              VZoom,
              AVersion
            );
        end;
        Result :=
          TTileRequestTask.Create(
            VRequest,
            ASoftCancelNotifier,
            ACancelNotifier,
            AOperationID,
            AFinishNotifier
          );
      end;
    end;
  end;
end;

function TTileDownloadSubsystem.GetScriptText(
  const AConfig: IConfigDataProvider
): AnsiString;
begin
  Result := AConfig.ReadAnsiString(PascalScriptFileName, '');
end;

function TTileDownloadSubsystem.GetState: ITileDownloaderStateChangeble;
begin
  Result := FState;
end;

procedure TTileDownloadSubsystem.OnAppClosing;
begin
  FDestroyNotifierInternal.NextOperation;
end;

end.
