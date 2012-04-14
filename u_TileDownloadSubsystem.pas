unit u_TileDownloadSubsystem;

interface

uses
  Types,
  i_JclNotify,
  i_OperationNotifier,
  i_CoordConverterFactory,
  i_CoordConverter,
  i_ThreadConfig,
  i_TTLCheckNotifier,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_ContentTypeSubst,
  i_TilePostDownloadCropConfig,
  i_SimpleTileStorageConfig,
  i_DownloadResultFactory,
  i_LanguageManager,
  i_GlobalDownloadConfig,
  i_TileRequest,
  i_TileDownloaderState,
  i_TileDownloaderConfig,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory,
  i_TileDownloader,
  i_TileDownloadResultSaver,
  i_MapAbilitiesConfig,
  i_ImageResamplerConfig,
  i_MapVersionConfig,
  i_InvisibleBrowser,
  i_TileDownloadSubsystem,
  u_OperationNotifier,
  u_TileStorageAbstract;

type
  TTileDownloadSubsystem = class(TInterfacedObject, ITileDownloadSubsystem)
  private
    FTileDownloaderConfig: ITileDownloaderConfig;
    FTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    FCoordConverter : ICoordConverter;
    FVersionConfig: IMapVersionConfig;
    FAppClosingNotifier: IJclNotifier;

    FDestroyNotifierInternal: IOperationNotifierInternal;
    FDestroyNotifier: IOperationNotifier;
    FDestroyOperationID: Integer;
    FAppClosingListener: IJclListener;

    FZmpDownloadEnabled: Boolean;
    FState: ITileDownloaderStateChangeble;
    FDownloadResultSaver: ITileDownloadResultSaver;
    FTileDownloader: ITileDownloader;
    FTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
    FTileDownloadRequestBuilderFactory: ITileDownloadRequestBuilderFactory;
    function GetScriptText(const AConfig: IConfigDataProvider): string;
    procedure OnAppClosing;
  protected
    function GetRequest(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const AXY: TPoint;
      Azoom: byte;
      ACheckTileSize: Boolean
    ): ITileRequest;
    function GetLink(const AXY: TPoint; Azoom: byte): string;
    procedure Download(
      const ATileRequest: ITileRequest
    );

    function GetState: ITileDownloaderStateChangeble;
  public
    constructor Create(
      const AGCList: ITTLCheckNotifier;
      const AAppClosingNotifier: IJclNotifier;
      const ACoordConverter : ICoordConverter;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALanguageManager: ILanguageManager;
      const AGlobalDownloadConfig: IGlobalDownloadConfig;
      const AInvisibleBrowser: IInvisibleBrowser;
      const ADownloadResultFactory: IDownloadResultFactory;
      const AZmpTileDownloaderConfig: ITileDownloaderConfigStatic;
      const AImageResamplerConfig: IImageResamplerConfig;
      const AVersionConfig: IMapVersionConfig;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const AThreadConfig: IThreadConfig;
      const ATileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
      const AContentTypeManager: IContentTypeManager;
      const AContentTypeSubst: IContentTypeSubst;
      const ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
      const AMapAbilitiesConfig: IMapAbilitiesConfig;
      const AZmpData: IConfigDataProvider;
      const AStorageConfig: ISimpleTileStorageConfig;
      AStorage: TTileStorageAbstract
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_TileDownloadRequest,
  i_TileDownloaderList,
  i_DownloadChecker,
  u_NotifyEventListener,
  u_TileRequest,
  u_TileDownloaderList,
  u_AntiBanStuped,
  u_DownloaderFaked,
  u_DownloadCheckerStuped,
  u_TileDownloadRequestBuilderLazy,
  u_TileDownloadSubsystemState,
  u_TileDownloadResultSaverStuped,
  u_TileDownloaderWithQueue,
  u_TileDownloadRequestBuilderFactoryPascalScript;

const
  PascalScriptFileName = 'GetUrlScript.txt';

{ TTileDownloadSubsystem }

constructor TTileDownloadSubsystem.Create(
  const AGCList: ITTLCheckNotifier;
  const AAppClosingNotifier: IJclNotifier;
  const ACoordConverter : ICoordConverter;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALanguageManager: ILanguageManager;
  const AGlobalDownloadConfig: IGlobalDownloadConfig;
  const AInvisibleBrowser: IInvisibleBrowser;
  const ADownloadResultFactory: IDownloadResultFactory;
  const AZmpTileDownloaderConfig: ITileDownloaderConfigStatic;
  const AImageResamplerConfig: IImageResamplerConfig;
  const AVersionConfig: IMapVersionConfig;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const AThreadConfig: IThreadConfig;
  const ATileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
  const AContentTypeManager: IContentTypeManager;
  const AContentTypeSubst: IContentTypeSubst;
  const ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
  const AMapAbilitiesConfig: IMapAbilitiesConfig;
  const AZmpData: IConfigDataProvider;
  const AStorageConfig: ISimpleTileStorageConfig;
  AStorage: TTileStorageAbstract
);
var
  VDownloaderList: ITileDownloaderList;
  VDownloadChecker: IDownloadChecker;
  VOperationNotifier: TOperationNotifier;
begin
  FCoordConverter := ACoordConverter;
  FVersionConfig := AVersionConfig;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FTileDownloadRequestBuilderConfig := ATileDownloadRequestBuilderConfig;
  FAppClosingNotifier := AAppClosingNotifier;

  VOperationNotifier := TOperationNotifier.Create;
  FDestroyNotifierInternal := VOperationNotifier;
  FDestroyNotifier := VOperationNotifier;
  FDestroyOperationID := FDestroyNotifier.CurrentOperation;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);

  FZmpDownloadEnabled := AZmpTileDownloaderConfig.Enabled;

  if FZmpDownloadEnabled then begin
    VDownloadChecker := TDownloadCheckerStuped.Create(
      TAntiBanStuped.Create(AInvisibleBrowser, AZmpData),
      FTileDownloaderConfig,
      AStorage
    );
    FTileDownloadRequestBuilderFactory :=
      TTileDownloadRequestBuilderFactoryPascalScript.Create(
        GetScriptText(AZmpData),
        FTileDownloadRequestBuilderConfig,
        FTileDownloaderConfig,
        VDownloadChecker,
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
        AImageResamplerConfig,
        AContentTypeManager,
        AContentTypeSubst,
        ATilePostDownloadCropConfig,
        AStorageConfig,
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
        AGCList,
        AAppClosingNotifier,
        ADownloadResultFactory,
        FState,
        FTileDownloaderConfig,
        FDownloadResultSaver,
        FTileDownloadRequestBuilderFactory
      );
    FTileDownloader := TTileDownloaderWithQueue.Create(
      VDownloaderList,
      AGCList,
      AThreadConfig,
      AAppClosingNotifier,
      256
    );
  end else begin
    FState :=
      TTileDownloadSubsystemState.Create(
        FZmpDownloadEnabled,
        nil,
        nil,
        nil
      );

  end;

end;

destructor TTileDownloadSubsystem.Destroy;
begin
  FDestroyNotifierInternal.NextOperation;
  FAppClosingNotifier.Remove(FAppClosingListener);
  FAppClosingListener := nil;
  FAppClosingNotifier := nil;

  inherited;
end;

procedure TTileDownloadSubsystem.Download(const ATileRequest: ITileRequest);
begin
  if FZmpDownloadEnabled then begin
    if FState.GetStatic.Enabled then begin
      FTileDownloader.Download(ATileRequest);
    end;
  end;
end;

function TTileDownloadSubsystem.GetLink(const AXY: TPoint; Azoom: byte): string;
var
  VRequest: ITileRequest;
  VDownloadRequest: ITileDownloadRequest;
begin
  Result := '';
  if FZmpDownloadEnabled then begin
    if FTileDownloadRequestBuilderFactory.State.GetStatic.Enabled then begin
      VRequest :=
        GetRequest(
          nil,
          0,
          AXY,
          Azoom,
          False
        );
      VDownloadRequest:= nil;
      if VRequest <> nil then begin
        VDownloadRequest := FTileDownloadRequestBuilder.BuildRequest(VRequest, nil, FDestroyNotifier, FDestroyOperationID);
      end;
      if VDownloadRequest <> nil then begin
        Result := VDownloadRequest.Url;
      end;
    end;
  end;
end;

function TTileDownloadSubsystem.GetRequest(
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  const AXY: TPoint;
  Azoom: byte;
  ACheckTileSize: Boolean
): ITileRequest;
var
  VZoom: Byte;
  VTile: TPoint;
begin
  Result := nil;
  if FZmpDownloadEnabled then begin
    VZoom := VZoom;
    VTile := AXY;
    if FCoordConverter.CheckTilePosStrict(VTile, VZoom, False) then begin
      if ACheckTileSize then begin
        Result :=
          TTileRequestWithSizeCheck.Create(
            VTile,
            VZoom,
            FVersionConfig.Version,
            ACancelNotifier,
            AOperationID
          );
      end else begin
        Result :=
          TTileRequest.Create(
            VTile,
            VZoom,
            FVersionConfig.Version,
            ACancelNotifier,
            AOperationID
          );
      end;
    end;
  end;
end;

function TTileDownloadSubsystem.GetScriptText(
  const AConfig: IConfigDataProvider
): string;
begin
  Result := AConfig.ReadString(PascalScriptFileName, '');
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
