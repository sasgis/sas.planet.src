unit u_MapType;

interface

uses
  Windows,
  sysutils,
  Classes,
  Dialogs,
  GR32,
  i_JclNotify,
  t_GeoTypes,
  t_CommonTypes,
  i_ContentTypeInfo,
  i_ConfigDataProvider,
  i_OperationCancelNotifier,
  i_TileObjCache,
  i_DownloadResult,
  i_TileDownloaderConfig,
  i_LanguageManager,
  i_CoordConverter,
  i_DownloadChecker,
  i_TileDownlodSession,
  i_LastResponseInfo,
  i_MapVersionConfig,
  i_TileRequestBuilder,
  i_TileRequestBuilderConfig,
  i_IPoolOfObjectsSimple,
  i_BitmapTypeExtManager,
  i_BitmapTileSaveLoad,
  i_KmlInfoSimpleLoader,
  i_TileDownloadResultFactoryProvider,
  i_AntiBan,
  i_ZmpInfo,
  i_VectorDataItemSimple,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract,
  u_ResStrings;

type
  EBadGUID = class(Exception);

 TMapType = class
   private
    FZmp: IZmpInfo;
    FName: string;
    FasLayer: boolean;
    FTileRect: TRect;
    FUseDwn: boolean;
    FIsCanShowOnSmMap: Boolean;
    FUseStick: boolean;
    FUseGenPrevious: boolean;
    FAntiBan: IAntiBan;
    FCache: ITileObjCache;
    FStorage: TTileStorageAbstract;
    FTileRequestBuilder: ITileRequestBuilder;
    FBitmapLoaderFromStorage: IBitmapTileLoader;
    FBitmapSaverToStorage: IBitmapTileSaver;
    FKmlLoaderFromStorage: IKmlInfoSimpleLoader;
    FCoordConverter : ICoordConverter;
    FViewCoordConverter : ICoordConverter;
    FPoolOfDownloaders: IPoolOfObjectsSimple;
    FLoadPrevMaxZoomDelta: Integer;
    FContentType: IContentTypeInfoBasic;
    FLanguageManager: ILanguageManager;
    FLastResponseInfo: ILastResponseInfo;
    FVersionConfig: IMapVersionConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FTileRequestBuilderConfig: ITileRequestBuilderConfig;
    FTileDownloadResultFactoryProvider: ITileDownloadResultFactoryProvider;

    function GetUseDwn: Boolean;
    function GetIsCanShowOnSmMap: boolean;
    function GetUseStick: boolean;
    function GetIsCropOnDownload: Boolean;
    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
    function GetIsHybridLayer: Boolean;
    procedure LoadUrlScript(AConfig : IConfigDataProvider);
    procedure LoadDownloader(AConfig : IConfigDataProvider);
    procedure LoadStorageParams(AConfig : IConfigDataProvider);
    procedure LoadWebSourceParams(AConfig : IConfigDataProvider);
    procedure LoadUIParams(AConfig : IConfigDataProvider);
    procedure SaveTileDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileNotExists(AXY: TPoint; Azoom: byte);
    procedure CropOnDownload(ABtm: TCustomBitmap32; ATileSize: TPoint);
    procedure SaveBitmapTileToStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
    function LoadBitmapTileFromStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32): Boolean;
    function LoadKmlTileFromStorage(AXY: TPoint; Azoom: byte; var AKml: IVectorDataItemList): boolean;
    procedure LoadMapType(AConfig : IConfigDataProvider);

    procedure SaveTileKmlDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileBitmapDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; AMimeType: string);
    function GetUseGenPrevious: boolean;
    function LoadTileFromPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte; caching: boolean; IgnoreError: Boolean): boolean;
   public
    FSortIndex: integer;
    HotKey: TShortCut;
    separator: boolean;
    ParentSubMenu: string;
    Enabled: boolean;

    function GetLink(AXY: TPoint; Azoom: byte): string;
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
    function GetTileShowName(AXY: TPoint; Azoom: byte): string;
    function TileExists(AXY: TPoint; Azoom: byte): Boolean;
    function TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;
    function LoadTile(
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      caching: boolean;
      IgnoreError: Boolean
    ): boolean; overload;
    function LoadTile(
      var AKml: IVectorDataItemList;
      AXY: TPoint;
      Azoom: byte;
      caching: boolean;
      IgnoreError: Boolean
    ): boolean; overload;
    function LoadTileOrPreZ(
      spr: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      caching: boolean;
      IgnoreError: Boolean;
      AUsePre: Boolean
    ): boolean;
    function LoadTileUni(
      spr: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      caching: boolean;
      ACoordConverterTarget: ICoordConverter;
      AUsePre, AAllowPartial, IgnoreError: Boolean
    ): boolean;
    function LoadBtimap(
      spr: TCustomBitmap32;
      APixelRectTarget: TRect;
      Azoom: byte;
      caching: boolean;
      AUsePre, AAllowPartial, IgnoreError: Boolean
    ): boolean;
    function LoadBtimapUni(
      spr: TCustomBitmap32;
      APixelRectTarget: TRect;
      Azoom: byte;
      caching: boolean;
      ACoordConverterTarget: ICoordConverter;
      AUsePre, AAllowPartial, IgnoreError: Boolean
    ): boolean;
    function DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
    procedure SaveTileSimple(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
    function TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
    function TileSize(AXY: TPoint; Azoom: byte): integer;
    function TileExportToFile(AXY: TPoint; Azoom: byte; AFileName: string; OverWrite: boolean): boolean;
    // Строит карту заполнения дл тайла на уровне AZoom тайлами уровня ASourceZoom
    // Должна регулярно проверять по указателю IsStop не нужно ли прерваться
    function LoadFillingMap(
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      AIsStop: TIsCancelChecker;
      ANoTileColor: TColor32;
      AShowTNE: Boolean;
      ATNEColor: TColor32
    ): boolean;
    function GetShortFolderName: string;
    function DownloadTile(
      ACancelNotifier: IOperationCancelNotifier;
      ATile: TPoint;
      AZoom: byte;
      ACheckTileSize: Boolean
    ): IDownloadResult;
    property Zmp: IZmpInfo read FZmp;
    property GeoConvert: ICoordConverter read FCoordConverter;
    property ViewGeoConvert: ICoordConverter read FViewCoordConverter;

    property asLayer: boolean read FasLayer;
    property IsBitmapTiles: Boolean read GetIsBitmapTiles;
    property IsKmlTiles: Boolean read GetIsKmlTiles;
    property IsHybridLayer: Boolean read GetIsHybridLayer;
    property UseDwn: Boolean read GetUseDwn;
    property UseGenPrevious: boolean read GetUseGenPrevious;
    property IsCanShowOnSmMap: boolean read GetIsCanShowOnSmMap;
    property UseStick: boolean read GetUseStick;
    property IsCropOnDownload: Boolean read GetIsCropOnDownload;

    property TileStorage: TTileStorageAbstract read FStorage;
    property Name: string read FName;
    property TileDownloaderConfig: ITileDownloaderConfig read FTileDownloaderConfig;
    property TileRequestBuilderConfig: ITileRequestBuilderConfig read FTileRequestBuilderConfig;
    property Cache: ITileObjCache read FCache;

    constructor Create(
      ALanguageManager: ILanguageManager;
      AZmp: IZmpInfo;
      AConfig: IConfigDataProvider
    );
    destructor Destroy; override;
 end;

type
  TMapUpdateEvent = procedure(AMapType: TMapType) of object;
  TMapTileUpdateEvent = procedure(AMapType: TMapType; AZoom: Byte;
    ATile: TPoint) of object;

implementation

uses
  Types,
  GR32_Resamplers,
  KAZip,
  u_GlobalState,
  i_ObjectWithTTL,
  i_DownloadResultFactory,
  i_PoolElement,
  i_TileInfoBasic,
  u_PoolOfObjectsSimple,
  u_TileDownloaderConfig,
  u_TileRequestBuilderConfig,
  u_TileRequestBuilderPascalScript,
  u_TileDownloaderBaseFactory,
  u_TileDownloadResultFactoryProvider,
  u_AntiBanStuped,
  u_TileCacheSimpleGlobal,
  u_LastResponseInfo,
  u_MapVersionConfig,
  u_DownloadCheckerStuped,
  u_TileStorageGE,
  u_TileStorageFileSystem;

procedure TMapType.LoadUrlScript(AConfig: IConfigDataProvider);
begin
  FTileRequestBuilder := nil;
  if FUseDwn then begin
    try
      FTileRequestBuilder := TTileRequestBuilderPascalScript.Create(FTileRequestBuilderConfig, Zmp.DataProvider);
    except
      on E: Exception do begin
        ShowMessageFmt(SAS_ERR_UrlScriptError, [FZmp.Name, E.Message, FZmp.FileName]);
        FTileRequestBuilder := nil;
      end;
    else
      ShowMessageFmt(SAS_ERR_UrlScriptUnexpectedError, [FZmp.Name, FZmp.FileName]);
      FTileRequestBuilder := nil;
    end;
  end;
  if FTileRequestBuilder = nil then begin
    FUseDwn := False;
  end;
end;

procedure TMapType.LoadStorageParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VContentTypeKml: IContentTypeInfoKml;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  if VParams.ReadInteger('CacheType', 0) = 5  then begin
    FStorage := TTileStorageGE.Create(AConfig);
  end else begin
    FStorage := TTileStorageFileSystem.Create(AConfig);
  end;
  FContentType := FStorage.GetMainContentType;
  if Supports(FContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
    FBitmapLoaderFromStorage := VContentTypeBitmap.GetLoader;
    if FStorage.GetUseSave then begin
      FBitmapSaverToStorage := VContentTypeBitmap.GetSaver;
    end;
  end else if Supports(FContentType, IContentTypeInfoKml, VContentTypeKml) then begin
    FKmlLoaderFromStorage := VContentTypeKml.GetLoader;
  end;
  FCache := TTileCacheSimpleGlobal.Create(Self, GState.MainMemCache);
end;

procedure TMapType.LoadWebSourceParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FTileRect.Left:=VParams.ReadInteger('TileRLeft',0);
  FTileRect.Top:=VParams.ReadInteger('TileRTop',0);
  FTileRect.Right:=VParams.ReadInteger('TileRRight',0);
  FTileRect.Bottom:=VParams.ReadInteger('TileRBottom',0);

  FUseDwn:=VParams.ReadBool('UseDwn',true);
end;

procedure TMapType.LoadUIParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FName := Zmp.Name;
  FIsCanShowOnSmMap := VParams.ReadBool('CanShowOnSmMap', true);
  HotKey:=VParams.ReadInteger('HotKey',Zmp.HotKey);
  ParentSubMenu:=VParams.ReadString('LOCAL:ParentSubMenu', Zmp.ParentSubMenu);
  separator:=VParams.ReadBool('separator', Zmp.Separator);
  Enabled:=VParams.ReadBool('Enabled', Zmp.Enabled);
  FSortIndex:=VParams.ReadInteger('pnum', Zmp.SortIndex);
end;

procedure TMapType.LoadDownloader(AConfig: IConfigDataProvider);
var
  VDownloader: TTileDownloaderFactory;
begin
  if FUseDwn then begin
    try
      VDownloader := TTileDownloaderFactory.Create(FTileDownloaderConfig);
      FPoolOfDownloaders :=
        TPoolOfObjectsSimple.Create(
          FTileDownloaderConfig.MaxConnectToServerCount,
          VDownloader,
          60000,
          60000
        );
      GState.GCThread.List.AddObject(FPoolOfDownloaders as IObjectWithTTL);
      FAntiBan := TAntiBanStuped.Create(AConfig);
    except
      if ExceptObject <> nil then begin
        ShowMessageFmt(SAS_ERR_MapDownloadByError,[ZMP.FileName, (ExceptObject as Exception).Message]);
      end;
      FUseDwn := false;
    end;
  end;
end;

procedure TMapType.LoadMapType(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FasLayer:= VParams.ReadBool('asLayer', false);
  LoadUIParams(AConfig);
  if FSortIndex < 0 then begin
    FSortIndex := 1000;
  end;
  FVersionConfig.ReadConfig(VParams);
  FTileDownloaderConfig.ReadConfig(VParams);
  LoadStorageParams(AConfig);
  FCoordConverter := FStorage.GetCoordConverter;
  FViewCoordConverter := Zmp.ViewGeoConvert;
  LoadWebSourceParams(AConfig);
  FUsestick:=VParams.ReadBool('Usestick',true);
  FUseGenPrevious:=VParams.ReadBool('UseGenPrevious',true);
  FTileRequestBuilderConfig.ReadConfig(VParams);
  LoadUrlScript(AConfig);
  LoadDownloader(AConfig);
end;

function TMapType.GetLink(AXY: TPoint; Azoom: byte): string;
begin
  if FUseDwn then begin
    FCoordConverter.CheckTilePosStrict(AXY, Azoom, True);
    Result := FTileRequestBuilder.BuildRequestUrl(AXY, AZoom, FVersionConfig.GetStatic);
  end;
end;

function TMapType.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Result := FStorage.GetTileFileName(AXY, Azoom, FVersionConfig.GetStatic);
end;

function TMapType.TileExists(AXY: TPoint; Azoom: byte): Boolean;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.GetStatic);
  Result := VTileInfo.GetIsExists;
end;

function TMapType.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := FStorage.DeleteTile(AXY, Azoom, FVersionConfig.GetStatic);
end;

function TMapType.TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.GetStatic);
  Result := VTileInfo.GetIsExistsTNE;
end;

procedure TMapType.SaveBitmapTileToStorage(AXY: TPoint; Azoom: byte;
  btm: TCustomBitmap32);
var
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    FBitmapSaverToStorage.SaveToStream(btm, VMemStream);
    FStorage.SaveTile(AXY, Azoom, FVersionConfig.GetStatic, VMemStream);
  finally
    VMemStream.Free;
  end;
end;

function TMapType.LoadBitmapTileFromStorage(AXY: TPoint; Azoom: byte;
  btm: TCustomBitmap32): Boolean;
var
  VTileInfo: ITileInfoBasic;
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    Result := FStorage.LoadTile(AXY, Azoom, FVersionConfig.GetStatic, VMemStream, VTileInfo);
    if Result then begin
      FBitmapLoaderFromStorage.LoadFromStream(VMemStream, btm);
    end;
  finally
    VMemStream.Free;
  end;
end;

function TMapType.LoadKmlTileFromStorage(AXY: TPoint; Azoom: byte;
  var AKml: IVectorDataItemList): boolean;
var
  VTileInfo: ITileInfoBasic;
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    Result := FStorage.LoadTile(AXY, Azoom, FVersionConfig.GetStatic, VMemStream, VTileInfo);
    if Result then  begin
      FKmlLoaderFromStorage.LoadFromStream(VMemStream, AKml);
    end;
  finally
    VMemStream.Free;
  end;
end;

procedure TMapType.SaveTileBitmapDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; AMimeType: string);
var
  btmSrc:TCustomBitmap32;
  VManager: IBitmapTypeExtManager;
begin
  VManager := GState.BitmapTypeManager;
  if VManager.GetIsBitmapType(AMimeType) then begin
    if not IsCropOnDownload and SameText(FStorage.TileFileExt, VManager.GetExtForType(AMimeType)) then begin
      FStorage.SaveTile(AXY, Azoom, FVersionConfig.GetStatic, ATileStream);
    end else begin
      btmsrc := TCustomBitmap32.Create;
      try
        ATileStream.Position := 0;
        VManager.GetBitmapLoaderForType(AMimeType).LoadFromStream(ATileStream, btmSrc);

        if IsCropOnDownload then begin
          CropOnDownload(btmSrc, FCoordConverter.GetTileSize(AXY, Azoom));
        end;
        SaveBitmapTileToStorage(AXY, Azoom, btmSrc);
      finally
        FreeAndNil(btmSrc);
      end;
    end;
    FCache.DeleteTileFromCache(AXY, Azoom);
  end else begin
    raise Exception.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AMimeType]);
  end;
end;

procedure TMapType.SaveTileKmlDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
var
  UnZip:TKAZip;
  VMemStream: TMemoryStream;
begin
  if (ty='application/vnd.google-earth.kmz') then begin
    try
      UnZip:=TKAZip.Create(nil);
      try
        UnZip.Open(ATileStream);
        VMemStream := TMemoryStream.Create;
        try
          UnZip.Entries.Items[0].ExtractToStream(VMemStream);
          FStorage.SaveTile(AXY, Azoom, FVersionConfig.GetStatic, VMemStream);
        finally
          VMemStream.Free;
        end;
      finally
        UnZip.Free;
      end;
    except
      try
        FStorage.SaveTile(AXY, Azoom, FVersionConfig.GetStatic, ATileStream);
      except
      end;
    end;
  end else if (copy(ty,1,8)='text/xml')or(ty='application/vnd.google-earth.kml+xml') then begin
    FStorage.SaveTile(AXY, Azoom, FVersionConfig.GetStatic, ATileStream);
  end;
end;

procedure TMapType.SaveTileDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
begin
  if FStorage.GetUseSave then begin
    if GetIsKmlTiles then begin
      SaveTileKmlDownload(AXY, Azoom, ATileStream, ty);
    end else if GetIsBitmapTiles then begin
      SaveTileBitmapDownload(AXY, Azoom, ATileStream, ty);
    end else begin
      raise Exception.Create('В этой карте неизвестный тип тайлов.');
    end;
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

function TMapType.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.GetStatic);
  Result := VTileInfo.GetLoadDate;
end;

function TMapType.TileSize(AXY: TPoint; Azoom: byte): integer;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.GetStatic);
  Result := VTileInfo.GetSize;
end;

procedure TMapType.SaveTileNotExists(AXY: TPoint; Azoom: byte);
begin
  FStorage.SaveTNE(AXY, Azoom, FVersionConfig.GetStatic);
end;

procedure TMapType.SaveTileSimple(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
begin
  SaveBitmapTileToStorage(AXY, Azoom, btm);
end;

function TMapType.TileExportToFile(AXY: TPoint; Azoom: byte;
  AFileName: string; OverWrite: boolean): boolean;
var
  VFileStream: TFileStream;
  VFileExists: Boolean;
  VExportPath: string;
  VTileInfo: ITileInfoBasic;
begin
  VFileExists := FileExists(AFileName);
  if VFileExists and not OverWrite then begin
    Result := False;
  end else begin
    if VFileExists then begin
      DeleteFile(AFileName);
    end else begin
      VExportPath := ExtractFilePath(AFileName);
      ForceDirectories(VExportPath);
    end;
    VFileStream := TFileStream.Create(AFileName, fmCreate);
    try
      Result := FStorage.LoadTile(AXY, Azoom, FVersionConfig.GetStatic, VFileStream, VTileInfo);
      if Result then begin
        FileSetDate(AFileName, DateTimeToFileDate(VTileInfo.GetLoadDate));
      end;
    finally
      VFileStream.Free;
    end;
  end;
end;

function TMapType.LoadFillingMap(
  btm: TCustomBitmap32;
  AXY: TPoint;
  Azoom, ASourceZoom: byte;
  AIsStop: TIsCancelChecker;
  ANoTileColor: TColor32;
  AShowTNE: Boolean;
  ATNEColor: TColor32
): boolean;
begin
  Result := FStorage.LoadFillingMap(btm, AXY, Azoom, ASourceZoom, FVersionConfig.GetStatic, AIsStop, ANoTileColor, AShowTNE, ATNEColor);
end;

function TMapType.GetShortFolderName: string;
begin
  Result := ExtractFileName(ExtractFileDir(IncludeTrailingPathDelimiter(FStorage.CacheConfig.NameInCache)));
end;

constructor TMapType.Create(
  ALanguageManager: ILanguageManager;
  AZmp: IZmpInfo;
  AConfig: IConfigDataProvider
);
begin
  FZmp := AZmp;
  FLanguageManager := ALanguageManager;
  FTileDownloaderConfig := TTileDownloaderConfig.Create(GState.InetConfig, Zmp.TileDownloaderConfig);
  FTileRequestBuilderConfig := TTileRequestBuilderConfig.Create(Zmp.TileRequestBuilderConfig);
  FLastResponseInfo := TLastResponseInfo.Create;
  FVersionConfig := TMapVersionConfig.Create(Zmp.VersionConfig);
  LoadMapType(AConfig);
  if FasLayer then begin
    FLoadPrevMaxZoomDelta := 4;
  end else begin
    FLoadPrevMaxZoomDelta := 6;
  end;
  FTileDownloadResultFactoryProvider := TTileDownloadResultFactoryProvider.Create(Self, GState.DownloadResultTextProvider);
end;

destructor TMapType.Destroy;
begin
  FCoordConverter := nil;
  FPoolOfDownloaders := nil;
  FCache := nil;
  FreeAndNil(FStorage);
  inherited;
end;

function TMapType.DownloadTile(
  ACancelNotifier: IOperationCancelNotifier;
  ATile: TPoint;
  AZoom: byte;
  ACheckTileSize: Boolean
): IDownloadResult;
var
  VPoolElement: IPoolElement;
  VDownloader: ITileDownlodSession;
  VRequestHead: string;
  VDownloadChecker: IDownloadChecker;
  VConfig: ITileDownloaderConfigStatic;
  VResultFactory: IDownloadResultFactory;
  VResultOk: IDownloadResultOk;
  VUrl: string;
  VOldTileSize: Integer;
  VResultStream: TMemoryStream;
  VContentType: string;
begin
  if FUseDwn then begin
    VRequestHead := '';
    FCoordConverter.CheckTilePosStrict(ATile, AZoom, True);
    FTileRequestBuilder.BuildRequest(ATile, AZoom, FVersionConfig.GetStatic, FLastResponseInfo, VUrl, VRequestHead);
    VResultFactory := FTileDownloadResultFactoryProvider.BuildFactory(AZoom, ATile, VUrl, VRequestHead);
    if VUrl = '' then begin
      Result := VResultFactory.BuildCanceled;
    end else begin
      VPoolElement := FPoolOfDownloaders.TryGetPoolElement(ACancelNotifier);
      if (ACancelNotifier <> nil) and ACancelNotifier.Canceled then begin
        Result := VResultFactory.BuildCanceled;
      end else begin
        VDownloader := VPoolElement.GetObject as ITileDownlodSession;
        if FAntiBan <> nil then begin
          FAntiBan.PreDownload(VDownloader, ATile, AZoom, VUrl);
        end;
        if (ACancelNotifier <> nil) and ACancelNotifier.Canceled then begin
          Result := VResultFactory.BuildCanceled;
        end else begin
          VConfig := FTileDownloaderConfig.GetStatic;
          VOldTileSize := FStorage.GetTileInfo(ATile, AZoom, FVersionConfig.GetStatic).GetSize;
          VDownloadChecker := TDownloadCheckerStuped.Create(
            VResultFactory,
            VConfig.IgnoreMIMEType,
            VConfig.ExpectedMIMETypes,
            VConfig.DefaultMIMEType,
            ACheckTileSize,
            VOldTileSize
          );
          Result := VDownloader.DownloadTile(ACancelNotifier, VResultFactory, VUrl, VRequestHead, VDownloadChecker);
          if FAntiBan <> nil then begin
            Result :=
              FAntiBan.PostCheckDownload(
                VResultFactory,
                VDownloader,
                Result
              );
          end;
        end;
      end;
    end;
    if Supports(Result, IDownloadResultOk, VResultOk) then begin
      FLastResponseInfo.ResponseHead := VResultOk.RawResponseHeader;
      VResultStream := TMemoryStream.Create;
      try
        VResultStream.WriteBuffer(VResultOk.Buffer^, VResultOk.Size);
        VContentType := VResultOk.ContentType;
        VContentType := Zmp.ContentTypeSubst.GetContentType(VContentType);
        SaveTileDownload(ATile, AZoom, VResultStream, VContentType);
      finally
        VResultStream.Free;
      end;
    end else if Supports(Result, IDownloadResultDataNotExists) then begin
      if GState.SaveTileNotExists then begin
        SaveTileNotExists(ATile, AZoom);
      end;
    end;
  end else begin
    raise Exception.Create('Для этой карты загрузка запрещена.');
  end;
end;

function TMapType.GetTileShowName(AXY: TPoint; Azoom: byte): string;
begin
  if FStorage.GetIsStoreFileCache then begin
    Result := FStorage.CacheConfig.GetTileFileName(AXY, Azoom)
  end else begin
    Result := 'z' + IntToStr(Azoom + 1) + 'x' + IntToStr(AXY.X) + 'y' + IntToStr(AXY.Y);
  end;
end;

function TMapType.GetUseDwn: Boolean;
begin
  if FStorage.GetUseSave then begin
    Result := FUseDwn;
  end else begin
    Result := false;
  end;
end;

function TMapType.GetUseGenPrevious: boolean;
begin
  Result := False;
  if FStorage.GetUseSave then begin
    if GetIsBitmapTiles then begin
      Result := FUseGenPrevious;
    end;
  end;
end;

function TMapType.GetUseStick: boolean;
begin
  if GetIsBitmapTiles then begin
    Result := FUseStick;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetIsCanShowOnSmMap: boolean;
begin
  if GetIsBitmapTiles then begin
    Result := FIsCanShowOnSmMap;
  end else begin
    Result := False;
  end;
end;

procedure TMapType.CropOnDownload(ABtm: TCustomBitmap32; ATileSize: TPoint);
var
  VBtmSrc: TCustomBitmap32;
  VBtmDest: TCustomBitmap32;
begin
  VBtmSrc := TCustomBitmap32.Create;
  try
    VBtmSrc.Assign(ABtm);
    VBtmSrc.Resampler := TLinearResampler.Create;
    VBtmDest := TCustomBitmap32.Create;
    try
      VBtmDest.SetSize(ATileSize.X, ATileSize.Y);
      VBtmDest.Draw(Bounds(0, 0, ATileSize.X, ATileSize.Y), FTileRect, VBtmSrc);
      ABtm.Assign(VBtmDest);
    finally
      VBtmDest.Free;
    end;
  finally
    VBtmSrc.Free;
  end;
end;

function TMapType.GetIsCropOnDownload: Boolean;
begin
  if (FTileRect.Left<>0)
    or (FTileRect.Top<>0)
    or (FTileRect.Right<>0)
    or (FTileRect.Bottom<>0)
  then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetIsBitmapTiles: Boolean;
begin
  Result := FBitmapLoaderFromStorage <> nil;
end;

function TMapType.GetIsKmlTiles: Boolean;
begin
  Result := FKmlLoaderFromStorage <> nil;
end;

function TMapType.GetIsHybridLayer: Boolean;
begin
  Result := IsBitmapTiles and asLayer;
end;

function TMapType.LoadTile(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean; IgnoreError: Boolean): boolean;
begin
  try
    if (not caching)or(not FCache.TryLoadTileFromCache(btm, AXY, Azoom)) then begin
      result:=LoadBitmapTileFromStorage(AXY, Azoom, btm);
      if ((result)and(caching)) then FCache.AddTileToCache(btm, AXY, Azoom);
    end else begin
      result:=true;
    end;
  except
    if not IgnoreError then begin
      raise;
    end else begin
      Result := False;
    end;
  end;
end;

function TMapType.LoadTile(var AKml: IVectorDataItemList; AXY: TPoint; Azoom: byte;
  caching: boolean; IgnoreError: Boolean): boolean;
begin
  try
    if (not caching)or(not FCache.TryLoadTileFromCache(AKml, AXY, Azoom)) then begin
      result:=LoadKmlTileFromStorage(AXY, Azoom, AKml);
      if ((result)and(caching)) then FCache.AddTileToCache(AKml, AXY, Azoom);
    end else begin
      result:=true;
    end;
  except
    if not IgnoreError then begin
      raise;
    end else begin
      Result := False;
    end;
  end;
end;

function TMapType.LoadTileFromPreZ(spr: TCustomBitmap32; AXY: TPoint;
  Azoom: byte; caching: boolean; IgnoreError: Boolean): boolean;
var
  i: integer;
  VBmp: TCustomBitmap32;
  VTileTargetBounds:TRect;
  VTileSourceBounds:TRect;
  VTileParent: TPoint;
  VTargetTilePixelRect: TRect;
  VSourceTilePixelRect: TRect;
  VRelative: TDoublePoint;
  VRelativeRect: TDoubleRect;
  VParentZoom: Byte;
  VMinZoom: Integer;
begin
  result:=false;
  VRelative := FCoordConverter.TilePos2Relative(AXY, Azoom);
  VMinZoom :=  Azoom - FLoadPrevMaxZoomDelta;
  if VMinZoom < 0 then begin
    VMinZoom := 0;
  end;
  if Azoom - 1 > VMinZoom then begin
    VBmp:=TCustomBitmap32.Create;
    try
      for i := Azoom - 1 downto VMinZoom do begin
        VParentZoom := i;
        VTileParent := FCoordConverter.Relative2Tile(VRelative, i);
        if LoadTile(VBmp, VTileParent, VParentZoom, caching, IgnoreError)then begin
          VTargetTilePixelRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
          VRelativeRect := FCoordConverter.PixelRect2RelativeRect(VTargetTilePixelRect, Azoom);
          VTileTargetBounds.Left := 0;
          VTileTargetBounds.Top := 0;
          VTileTargetBounds.Right := VTargetTilePixelRect.Right - VTargetTilePixelRect.Left;
          VTileTargetBounds.Bottom := VTargetTilePixelRect.Bottom - VTargetTilePixelRect.Top;

          VBmp.Resampler := GState.ImageResamplerConfig.GetActiveFactory.CreateResampler;

          VSourceTilePixelRect := FCoordConverter.TilePos2PixelRect(VTileParent, VParentZoom);
          VTargetTilePixelRect := FCoordConverter.RelativeRect2PixelRect(VRelativeRect, VParentZoom);
          VTileSourceBounds.Left := VTargetTilePixelRect.Left - VSourceTilePixelRect.Left;
          VTileSourceBounds.Top := VTargetTilePixelRect.Top - VSourceTilePixelRect.Top;
          VTileSourceBounds.Right := VTargetTilePixelRect.Right - VSourceTilePixelRect.Left;
          VTileSourceBounds.Bottom := VTargetTilePixelRect.Bottom - VSourceTilePixelRect.Top;
          try
            VBmp.DrawMode := dmOpaque;
            spr.SetSize(VTileTargetBounds.Right, VTileTargetBounds.Bottom);
            spr.Draw(VTileTargetBounds, VTileSourceBounds, VBmp);
            Result := true;
            if caching then begin
              FCache.AddTileToCache(spr, AXY, Azoom);
            end;
            Break;
          except
            if not IgnoreError then begin
              raise
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(VBmp);
    end;
  end;
end;

function TMapType.LoadTileOrPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean; IgnoreError: Boolean; AUsePre: Boolean): boolean;
var
  VRect: TRect;
  VSize: TPoint;
  bSpr:TCustomBitmap32;
begin
  VRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
  VSize := Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
  Result := LoadTile(spr, AXY, Azoom, caching, IgnoreError);
  if Result then begin
    if (spr.Width < VSize.X) or
      (spr.Height < VSize.Y) then begin
      bSpr:=TCustomBitmap32.Create;
      try
        bSpr.Assign(spr);
        spr.SetSize(VSize.X, VSize.Y);
        spr.Clear(0);
        spr.Draw(0,0,bSpr);
      finally
        bSpr.Free;
      end;
    end;
  end;
  if not Result then begin
    if AUsePre then begin
      Result := LoadTileFromPreZ(spr, AXY, Azoom, caching, IgnoreError);
    end;
  end;
end;

function TMapType.LoadBtimap(spr: TCustomBitmap32; APixelRectTarget: TRect;
  Azoom: byte; caching, AUsePre, AAllowPartial,
  IgnoreError: Boolean): boolean;
var
  VPixelRectTarget: TRect;
  VTileRect: TRect;
  VTargetImageSize: TPoint;
  VPixelRectCurrTile: TRect;
  i, j: Integer;
  VTile: TPoint;
  VSpr:TCustomBitmap32;
  VLoadResult: Boolean;
  VSourceBounds: TRect;
  VTargetBounds: TRect;
begin
  Result := False;

  VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
  VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

  VPixelRectTarget := APixelRectTarget;
  FCoordConverter.CheckPixelRect(VPixelRectTarget, Azoom);
  VTileRect := FCoordConverter.PixelRect2TileRect(VPixelRectTarget, Azoom);
  if (VTileRect.Left = VTileRect.Right - 1) and
    (VTileRect.Top = VTileRect.Bottom - 1)
  then begin
    VPixelRectCurrTile := FCoordConverter.TilePos2PixelRect(VTileRect.TopLeft, Azoom);
    if
      (VPixelRectCurrTile.Left = APixelRectTarget.Left) and
      (VPixelRectCurrTile.Top = APixelRectTarget.Top) and
      (VPixelRectCurrTile.Right = APixelRectTarget.Right) and
      (VPixelRectCurrTile.Bottom = APixelRectTarget.Bottom)
    then begin
      Result := LoadTileOrPreZ(spr, VTileRect.TopLeft, Azoom, caching, IgnoreError, AUsePre);
      exit;
    end;
  end;

  spr.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
  spr.Clear(0);

  VSpr := TCustomBitmap32.Create;
  try
    for i := VTileRect.Top to VTileRect.Bottom - 1 do begin
      VTile.Y := i;
      for j := VTileRect.Left to VTileRect.Right - 1 do begin
        VTile.X := j;
        VLoadResult := LoadTileOrPreZ(VSpr, VTile, Azoom, caching, IgnoreError, AUsePre);
        if VLoadResult then begin
          VPixelRectCurrTile := FCoordConverter.TilePos2PixelRect(VTile, Azoom);

          if VPixelRectCurrTile.Top < APixelRectTarget.Top then begin
            VSourceBounds.Top := APixelRectTarget.Top - VPixelRectCurrTile.Top;
          end else begin
            VSourceBounds.Top := 0;
          end;

          if VPixelRectCurrTile.Left < APixelRectTarget.Left then begin
            VSourceBounds.Left := APixelRectTarget.Left - VPixelRectCurrTile.Left;
          end else begin
            VSourceBounds.Left := 0;
          end;

          if VPixelRectCurrTile.Bottom < APixelRectTarget.Bottom then begin
            VSourceBounds.Bottom := VPixelRectCurrTile.Bottom - VPixelRectCurrTile.Top;
          end else begin
            VSourceBounds.Bottom := APixelRectTarget.Bottom - VPixelRectCurrTile.Top;
          end;

          if VPixelRectCurrTile.Right < APixelRectTarget.Right then begin
            VSourceBounds.Right := VPixelRectCurrTile.Right - VPixelRectCurrTile.Left;
          end else begin
            VSourceBounds.Right := APixelRectTarget.Right - VPixelRectCurrTile.Left;
          end;

          if VPixelRectCurrTile.Top < APixelRectTarget.Top then begin
            VTargetBounds.Top := 0;
          end else begin
            VTargetBounds.Top := VPixelRectCurrTile.Top - APixelRectTarget.Top;
          end;

          if VPixelRectCurrTile.Left < APixelRectTarget.Left then begin
            VTargetBounds.Left := 0;
          end else begin
            VTargetBounds.Left := VPixelRectCurrTile.Left - APixelRectTarget.Left;
          end;

          if VPixelRectCurrTile.Bottom < APixelRectTarget.Bottom then begin
            VTargetBounds.Bottom := VPixelRectCurrTile.Bottom - APixelRectTarget.Top;
          end else begin
            VTargetBounds.Bottom := APixelRectTarget.Bottom - APixelRectTarget.Top;
          end;

          if VPixelRectCurrTile.Right < APixelRectTarget.Right then begin
            VTargetBounds.Right := VPixelRectCurrTile.Right - APixelRectTarget.Left;
          end else begin
            VTargetBounds.Right := APixelRectTarget.Right - APixelRectTarget.Left;
          end;

          spr.Draw(VTargetBounds, VSourceBounds, VSpr);
        end else begin
          if not AAllowPartial then begin
            Exit;
          end;
        end;
      end;
    end;
    Result := True;
  finally
    VSpr.Free;
  end;
end;

function TMapType.LoadBtimapUni(spr: TCustomBitmap32; APixelRectTarget: TRect;
  Azoom: byte; caching: boolean; ACoordConverterTarget: ICoordConverter;
  AUsePre, AAllowPartial, IgnoreError: Boolean): boolean;
var
  VPixelRectTarget: TRect;
  VLonLatRectTarget: TDoubleRect;
  VTileRectInSource: TRect;
  VPixelRectOfTargetPixelRectInSource: TRect;
  i, j: Integer;
  VTile: TPoint;
  VSpr:TCustomBitmap32;
  VLoadResult: Boolean;
  VPixelRectCurTileInSource:  TRect;
  VLonLatRectCurTile:  TDoubleRect;
  VPixelRectCurTileInTarget:  TRect;
  VSourceBounds: TRect;
  VTargetBounds: TRect;
  VTargetImageSize: TPoint;
begin
  Result := False;

  if FCoordConverter.IsSameConverter(ACoordConverterTarget) then begin
    Result := LoadBtimap(spr, APixelRectTarget, Azoom, caching, AUsePre, AAllowPartial, IgnoreError);
  end else begin
    VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
    VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

    spr.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
    spr.Clear(0);
    VPixelRectTarget := APixelRectTarget;
    ACoordConverterTarget.CheckPixelRect(VPixelRectTarget, Azoom);
    VLonLatRectTarget := ACoordConverterTarget.PixelRect2LonLatRect(VPixelRectTarget, Azoom);
    FCoordConverter.CheckLonLatRect(VLonLatRectTarget);
    VPixelRectOfTargetPixelRectInSource := FCoordConverter.LonLatRect2PixelRect(VLonLatRectTarget, Azoom);
    VTileRectInSource := FCoordConverter.PixelRect2TileRect(VPixelRectOfTargetPixelRectInSource, Azoom);

    VSpr := TCustomBitmap32.Create;
    try
      for i := VTileRectInSource.Top to VTileRectInSource.Bottom - 1 do begin
        VTile.Y := i;
        for j := VTileRectInSource.Left to VTileRectInSource.Right - 1 do begin
          VTile.X := j;
          VLoadResult := LoadTileOrPreZ(VSpr, VTile, Azoom, caching, IgnoreError, AUsePre);
          if VLoadResult then begin
            VPixelRectCurTileInSource := FCoordConverter.TilePos2PixelRect(VTile, Azoom);
            VLonLatRectCurTile := FCoordConverter.PixelRect2LonLatRect(VPixelRectCurTileInSource, Azoom);
            ACoordConverterTarget.CheckLonLatRect(VLonLatRectCurTile);
            VPixelRectCurTileInTarget := ACoordConverterTarget.LonLatRect2PixelRect(VLonLatRectCurTile, Azoom);

            if VPixelRectCurTileInSource.Top < VPixelRectOfTargetPixelRectInSource.Top then begin
              VSourceBounds.Top := VPixelRectOfTargetPixelRectInSource.Top - VPixelRectCurTileInSource.Top;
            end else begin
              VSourceBounds.Top := 0;
            end;

            if VPixelRectCurTileInSource.Left < VPixelRectOfTargetPixelRectInSource.Left then begin
              VSourceBounds.Left := VPixelRectOfTargetPixelRectInSource.Left - VPixelRectCurTileInSource.Left;
            end else begin
              VSourceBounds.Left := 0;
            end;

            if VPixelRectCurTileInSource.Bottom < VPixelRectOfTargetPixelRectInSource.Bottom then begin
              VSourceBounds.Bottom := VPixelRectCurTileInSource.Bottom - VPixelRectCurTileInSource.Top;
            end else begin
              VSourceBounds.Bottom := VPixelRectOfTargetPixelRectInSource.Bottom - VPixelRectCurTileInSource.Top;
            end;

            if VPixelRectCurTileInSource.Right < VPixelRectOfTargetPixelRectInSource.Right then begin
              VSourceBounds.Right := VPixelRectCurTileInSource.Right - VPixelRectCurTileInSource.Left;
            end else begin
              VSourceBounds.Right := VPixelRectOfTargetPixelRectInSource.Right - VPixelRectCurTileInSource.Left;
            end;

            if VPixelRectCurTileInTarget.Top < APixelRectTarget.Top then begin
              VTargetBounds.Top := 0;
            end else begin
              VTargetBounds.Top := VPixelRectCurTileInTarget.Top - APixelRectTarget.Top;
            end;

            if VPixelRectCurTileInTarget.Left < APixelRectTarget.Left then begin
              VTargetBounds.Left := 0;
            end else begin
              VTargetBounds.Left := VPixelRectCurTileInTarget.Left - APixelRectTarget.Left;
            end;

            if VPixelRectCurTileInTarget.Bottom < APixelRectTarget.Bottom then begin
              VTargetBounds.Bottom := VPixelRectCurTileInTarget.Bottom - APixelRectTarget.Top;
            end else begin
              VTargetBounds.Bottom := APixelRectTarget.Bottom - APixelRectTarget.Top;
            end;

            if VPixelRectCurTileInTarget.Right < APixelRectTarget.Right then begin
              VTargetBounds.Right := VPixelRectCurTileInTarget.Right - APixelRectTarget.Left;
            end else begin
              VTargetBounds.Right := APixelRectTarget.Right - APixelRectTarget.Left;
            end;

            spr.Draw(VTargetBounds, VSourceBounds, VSpr);
          end else begin
            if not AAllowPartial then begin
              Exit;
            end;
          end;
        end;
      end;
      Result := True;
    finally
      VSpr.Free;
    end;
  end;
end;

function TMapType.LoadTileUni(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean; ACoordConverterTarget: ICoordConverter; AUsePre, AAllowPartial,
  IgnoreError: Boolean): boolean;
var
  VPixelRect: TRect;
begin
  VPixelRect := ACoordConverterTarget.TilePos2PixelRect(AXY, Azoom);
  Result := LoadBtimapUni(spr, VPixelRect, Azoom, caching, ACoordConverterTarget, AUsePre, AAllowPartial, IgnoreError);
end;

end.


