{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapType;

interface

uses
  Windows,
  sysutils,
  Classes,
  GR32,
  t_GeoTypes,
  i_Bitmap32Static,
  i_FillingMapColorer,
  i_ThreadConfig,
  i_ContentTypeInfo,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LayerDrawConfig,
  i_TileObjCache,
  i_TileDownloaderConfig,
  i_LanguageManager,
  i_CoordConverter,
  i_MapVersionConfig,
  i_TileDownloadRequestBuilderConfig,
  i_BitmapTileSaveLoad,
  i_VectorDataLoader,
  i_NotifierTTLCheck,
  i_DownloadResultFactory,
  i_InetConfig,
  i_DownloadResultTextProvider,
  i_ImageResamplerConfig,
  i_ContentTypeManager,
  i_GlobalDownloadConfig,
  i_MapAbilitiesConfig,
  
  
  i_MapAttachmentsFactory,
  i_MapVersionInfo,
  i_SimpleTileStorageConfig,
  i_ZmpInfo,
  i_InvisibleBrowser,
  i_MapTypeGUIConfig,
  i_CoordConverterFactory,
  i_MainMemCacheConfig,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_NotifierTileRectUpdate,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  i_ProjConverter,
  i_TileDownloadSubsystem,
  i_InternalPerformanceCounter,
  u_GlobalCahceConfig,
  
  u_TileStorageAbstract;

type
  TMapType = class
  private
    FZmp: IZmpInfo;

    FCacheBitmap: ITileObjCacheBitmap;
    FCacheVector: ITileObjCacheVector;
    FStorage: TTileStorageAbstract;
    FBitmapLoaderFromStorage: IBitmapTileLoader;
    FBitmapSaverToStorage: IBitmapTileSaver;
    FKmlLoaderFromStorage: IVectorDataLoader;
    FVectorDataFactory: IVectorDataFactory;
    FCoordConverter: ICoordConverter;
    FViewCoordConverter: ICoordConverter;
    FLoadPrevMaxZoomDelta: Integer;
    FContentType: IContentTypeInfoBasic;
    FLanguageManager: ILanguageManager;
    FVersionConfig: IMapVersionConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    FDownloadResultFactory: IDownloadResultFactory;
    FImageResamplerConfig: IImageResamplerConfig;
    FContentTypeManager: IContentTypeManager;
    FGlobalDownloadConfig: IGlobalDownloadConfig;
    FGUIConfig: IMapTypeGUIConfig;
    FLayerDrawConfig: ILayerDrawConfig;
    FAbilitiesConfig: IMapAbilitiesConfig;
    FMapAttachmentsFactory: IMapAttachmentsFactory;
    FStorageConfig: ISimpleTileStorageConfig;
    FTileDownloadSubsystem: ITileDownloadSubsystem;
    FPerfCounterList: IInternalPerformanceCounterList;

    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
    function GetIsHybridLayer: Boolean;
    procedure SaveBitmapTileToStorage(
      const AXY: TPoint;
      const Azoom: byte;
      const ABitmap: IBitmap32Static
    );
    function LoadBitmapTileFromStorage(
      const AXY: TPoint;
      const Azoom: Byte;
      var AVersionInfo: IMapVersionInfo
    ): IBitmap32Static;
    function LoadKmlTileFromStorage(
      const AXY: TPoint;
      const Azoom: byte;
      var AVersionInfo: IMapVersionInfo
    ): IVectorDataItemList;
    function LoadTileFromPreZ(
      const AXY: TPoint;
      const Azoom: byte;
      IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadTileOrPreZ(
      const AXY: TPoint;
      const Azoom: byte;
      IgnoreError: Boolean;
      AUsePre: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function GetNotifierByZoom(AZoom: Byte): INotifierTileRectUpdate;
  public
    function AllowListOfTileVersions: Boolean;
    procedure SaveConfig(const ALocalConfig: IConfigDataWriteProvider);
    function GetTileFileName(
      const AXY: TPoint;
      Azoom: byte
    ): string;
    function GetTileShowName(
      const AXY: TPoint;
      Azoom: byte
    ): string;
    function TileExists(
      const AXY: TPoint;
      Azoom: byte
    ): Boolean;
    function TileNotExistsOnServer(
      const AXY: TPoint;
      Azoom: byte
    ): Boolean;
    function LoadTile(
      const AXY: TPoint;
      const Azoom: byte;
      IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadTileVector(
      const AXY: TPoint;
      const Azoom: byte;
      IgnoreError: Boolean;
      const ACache: ITileObjCacheVector = nil
    ): IVectorDataItemList;
    function LoadTileUni(
      const AXY: TPoint;
      const Azoom: byte;
      const ACoordConverterTarget: ICoordConverter;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadBtimap(
      const APixelRectTarget: TRect;
      const Azoom: byte;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadBtimapUni(
      const APixelRectTarget: TRect;
      const Azoom: byte;
      const ACoordConverterTarget: ICoordConverter;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function DeleteTile(
      const AXY: TPoint;
      Azoom: byte
    ): Boolean;
    procedure SaveTileSimple(
      const AXY: TPoint;
      Azoom: byte;
      const ABitmap: IBitmap32Static
    );
    function TileLoadDate(
      const AXY: TPoint;
      Azoom: byte
    ): TDateTime;
    function TileSize(
      const AXY: TPoint;
      Azoom: byte
    ): integer;
    function TileExportToFile(
      const AXY: TPoint;
      Azoom: byte;
      const AFileName: string;
      OverWrite: boolean
    ): boolean;

    function GetFillingMapBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter;
      ASourceZoom: byte;
      const AColorer: IFillingMapColorer
    ): IBitmap32Static;

    function LoadFillingMap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      btm: TCustomBitmap32;
      const AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      const AColorer: IFillingMapColorer
    ): boolean;
    function GetShortFolderName: string;

    function GetLanguageManager: ILanguageManager;
    // parse vector object description

    property Zmp: IZmpInfo read FZmp;
    property GeoConvert: ICoordConverter read FCoordConverter;
    property ViewGeoConvert: ICoordConverter read FViewCoordConverter;
    property VersionConfig: IMapVersionConfig read FVersionConfig;

    property Abilities: IMapAbilitiesConfig read FAbilitiesConfig;
    property StorageConfig: ISimpleTileStorageConfig read FStorageConfig;
    property IsBitmapTiles: Boolean read GetIsBitmapTiles;
    property IsKmlTiles: Boolean read GetIsKmlTiles;
    property IsHybridLayer: Boolean read GetIsHybridLayer;

    property TileDownloadSubsystem: ITileDownloadSubsystem read FTileDownloadSubsystem;
    property TileStorage: TTileStorageAbstract read FStorage;
    property GUIConfig: IMapTypeGUIConfig read FGUIConfig;
    property LayerDrawConfig: ILayerDrawConfig read FLayerDrawConfig;
    property TileDownloaderConfig: ITileDownloaderConfig read FTileDownloaderConfig;
    property TileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig read FTileDownloadRequestBuilderConfig;
    property CacheBitmap: ITileObjCacheBitmap read FCacheBitmap;
    property CacheVector: ITileObjCacheVector read FCacheVector;
    property NotifierByZoom[AZoom: Byte]: INotifierTileRectUpdate read GetNotifierByZoom;

    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AZmp: IZmpInfo;
      const AMainMemCacheConfig: IMainMemCacheConfig;
      const AGlobalCacheConfig: TGlobalCahceConfig;
      const ATileNameGeneratorList: ITileFileNameGeneratorsList;
      const ATileNameParserList: ITileFileNameParsersList;
      const AGCList: INotifierTTLCheck;
      const AAppClosingNotifier: INotifierOneOperation;
      const AInetConfig: IInetConfig;
      const AImageResamplerConfig: IImageResamplerConfig;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloaderThreadConfig: IThreadConfig;
      const AContentTypeManager: IContentTypeManager;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ADownloadResultTextProvider: IDownloadResultTextProvider;
      const AInvisibleBrowser: IInvisibleBrowser;
      const AProjFactory: IProjConverterFactory;
      const AConfig: IConfigDataProvider;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  GR32_Resamplers,
  i_BinaryData,
  i_TileInfoBasic,
  i_TileIterator,
  u_Bitmap32Static,
  u_LayerDrawConfig,
  u_TileDownloaderConfig,
  u_TileDownloadRequestBuilderConfig,
  u_DownloadResultFactory,
  
  u_MemTileCache,
  u_SimpleTileStorageConfig,
  u_MapAbilitiesConfig,
  
  u_TileIteratorByRect,
  u_VectorDataFactorySimple,
  u_HtmlToHintTextConverterStuped,
  u_MapTypeGUIConfig,
  u_MapVersionConfig,
  u_TileDownloadSubsystem,
  u_GeoFun,
  u_TileStorageGE,
  u_TileStorageBerkeleyDB,
  u_TileStorageFileSystem;

constructor TMapType.Create(
  const ALanguageManager: ILanguageManager;
  const AZmp: IZmpInfo;
  const AMainMemCacheConfig: IMainMemCacheConfig;
  const AGlobalCacheConfig: TGlobalCahceConfig;
  const ATileNameGeneratorList: ITileFileNameGeneratorsList;
  const ATileNameParserList: ITileFileNameParsersList;
  const AGCList: INotifierTTLCheck;
  const AAppClosingNotifier: INotifierOneOperation;
  const AInetConfig: IInetConfig;
  const AImageResamplerConfig: IImageResamplerConfig;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloaderThreadConfig: IThreadConfig;
  const AContentTypeManager: IContentTypeManager;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ADownloadResultTextProvider: IDownloadResultTextProvider;
  const AInvisibleBrowser: IInvisibleBrowser;
  const AProjFactory: IProjConverterFactory;
  const AConfig: IConfigDataProvider;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VContentTypeKml: IContentTypeInfoVectorData;
begin
  inherited Create;
  FZmp := AZmp;
  FPerfCounterList := APerfCounterList.CreateAndAddNewSubList(FZmp.GUI.Name.GetDefault);
  FGUIConfig :=
    TMapTypeGUIConfig.Create(
      ALanguageManager,
      FZmp.GUI
    );
  FLayerDrawConfig := TLayerDrawConfig.Create(FZmp);
  FMapAttachmentsFactory := nil;
  FLanguageManager := ALanguageManager;
  FImageResamplerConfig := AImageResamplerConfig;
  FGlobalDownloadConfig := ADownloadConfig;
  FContentTypeManager := AContentTypeManager;
  FTileDownloaderConfig := TTileDownloaderConfig.Create(AInetConfig, FZmp.TileDownloaderConfig);
  FTileDownloadRequestBuilderConfig := TTileDownloadRequestBuilderConfig.Create(FZmp.TileDownloadRequestBuilderConfig);
  FVersionConfig := TMapVersionConfig.Create(FZmp.VersionConfig);
  FStorageConfig := TSimpleTileStorageConfig.Create(FZmp.StorageConfig);
  FAbilitiesConfig :=
    TMapAbilitiesConfig.Create(
      FZmp.Abilities,
      FStorageConfig
    );
  FDownloadResultFactory :=
    TDownloadResultFactory.Create(
      ADownloadResultTextProvider
    );

  FGUIConfig.ReadConfig(AConfig);
  FLayerDrawConfig.ReadConfig(AConfig);
  FStorageConfig.ReadConfig(AConfig);
  FAbilitiesConfig.ReadConfig(AConfig);
  FVersionConfig.ReadConfig(AConfig);
  FTileDownloaderConfig.ReadConfig(AConfig);
  FTileDownloadRequestBuilderConfig.ReadConfig(AConfig);

  if FStorageConfig.CacheTypeCode = c_File_Cache_Id_DEFAULT then begin
    FStorageConfig.CacheTypeCode := AGlobalCacheConfig.DefCache;
  end;

  if FStorageConfig.CacheTypeCode = c_File_Cache_Id_BDB then begin
    FStorage := TTileStorageBerkeleyDB.Create(AGCList, FStorageConfig, AGlobalCacheConfig, FContentTypeManager, FPerfCounterList);
  end else if FStorageConfig.CacheTypeCode = c_File_Cache_Id_GE then begin
    FStorage := TTileStorageGE.Create(FStorageConfig, AGlobalCacheConfig, FContentTypeManager);
  end else if FStorageConfig.CacheTypeCode = c_File_Cache_Id_GC then begin
    FStorage := TTileStorageGC.Create(FStorageConfig, AGlobalCacheConfig, FContentTypeManager);
  end else begin
    FStorage :=
      TTileStorageFileSystem.Create(
        FStorageConfig,
        AGlobalCacheConfig,
        ATileNameGeneratorList,
        ATileNameParserList,
        FContentTypeManager,
        FPerfCounterList
      );
  end;
  FContentType := FStorage.GetMainContentType;
  if Supports(FContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
    FBitmapLoaderFromStorage := VContentTypeBitmap.GetLoader;
    FBitmapSaverToStorage := VContentTypeBitmap.GetSaver;
    FCacheBitmap := TMemTileCacheBitmap.Create(AGCList, FStorage, FStorageConfig.CoordConverter, AMainMemCacheConfig);
  end else if Supports(FContentType, IContentTypeInfoVectorData, VContentTypeKml) then begin
    FKmlLoaderFromStorage := VContentTypeKml.GetLoader;
    FCacheVector := TMemTileCacheVector.Create(AGCList, FStorage, FStorageConfig.CoordConverter, AMainMemCacheConfig);
    FVectorDataFactory := TVectorDataFactorySimple.Create(THtmlToHintTextConverterStuped.Create);
  end;
  FVersionConfig.VersionFactory := FStorage.MapVersionFactory;

  FCoordConverter := FStorageConfig.CoordConverter;
  FViewCoordConverter := FZmp.ViewGeoConvert;

  FTileDownloadSubsystem :=
    TTileDownloadSubsystem.Create(
      AGCList,
      AAppClosingNotifier,
      FCoordConverter,
      ACoordConverterFactory,
      FLanguageManager,
      FGlobalDownloadConfig,
      AInvisibleBrowser,
      FDownloadResultFactory,
      FZmp.TileDownloaderConfig,
      AImageResamplerConfig,
      FVersionConfig,
      FTileDownloaderConfig,
      ADownloaderThreadConfig,
      FTileDownloadRequestBuilderConfig,
      FContentTypeManager,
      FZmp.ContentTypeSubst,
      FZmp.TilePostDownloadCropConfig,
      FAbilitiesConfig,
      FZmp.DataProvider,
      AProjFactory,
      FStorageConfig,
      FStorage
    );

  if FAbilitiesConfig.IsLayer then begin
    FLoadPrevMaxZoomDelta := 4;
  end else begin
    FLoadPrevMaxZoomDelta := 6;
  end;
end;

destructor TMapType.Destroy;
begin
  FCoordConverter := nil;
  FCacheBitmap := nil;
  FCacheVector := nil;

  FTileDownloadSubsystem := nil;
  FBitmapLoaderFromStorage := nil;
  FBitmapSaverToStorage := nil;
  FKmlLoaderFromStorage := nil;
  FViewCoordConverter := nil;
  FContentType := nil;
  FLanguageManager := nil;
  FVersionConfig := nil;
  FTileDownloaderConfig := nil;
  FTileDownloadRequestBuilderConfig := nil;
  FDownloadResultFactory := nil;
  FImageResamplerConfig := nil;
  FContentTypeManager := nil;
  FGlobalDownloadConfig := nil;
  FGUIConfig := nil;
  FMapAttachmentsFactory := nil;
  FAbilitiesConfig := nil;
  FStorageConfig := nil;

  FreeAndNil(FStorage);
  inherited;
end;

function TMapType.GetNotifierByZoom(AZoom: Byte): INotifierTileRectUpdate;
begin
  Result := FStorage.NotifierByZoom[AZoom];
end;

function TMapType.GetTileFileName(
  const AXY: TPoint;
  Azoom: byte
): string;
begin
  Result := FStorage.GetTileFileName(AXY, Azoom, FVersionConfig.Version);
end;

function TMapType.AllowListOfTileVersions: Boolean;
begin
  // only for GE and GC
  Result := (FStorageConfig.CacheTypeCode in [c_File_Cache_Id_GE, c_File_Cache_Id_GC]);
end;

function TMapType.TileExists(
  const AXY: TPoint;
  Azoom: byte
): Boolean;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.Version);
  Result := VTileInfo.GetIsExists;
end;

function TMapType.DeleteTile(
  const AXY: TPoint;
  Azoom: byte
): Boolean;
begin
  Result := FStorage.DeleteTile(AXY, Azoom, FVersionConfig.Version);
end;

function TMapType.TileNotExistsOnServer(
  const AXY: TPoint;
  Azoom: byte
): Boolean;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.Version);
  Result := VTileInfo.GetIsExistsTNE;
end;

procedure TMapType.SaveBitmapTileToStorage(
  const AXY: TPoint;
  const Azoom: byte;
  const ABitmap: IBitmap32Static
);
var
  VData: IBinaryData;
begin
  VData := FBitmapSaverToStorage.Save(ABitmap);
  FStorage.SaveTile(AXY, Azoom, FVersionConfig.Version, VData);
end;

procedure TMapType.SaveConfig(const ALocalConfig: IConfigDataWriteProvider);
begin
  FGUIConfig.WriteConfig(ALocalConfig);
  FLayerDrawConfig.WriteConfig(ALocalConfig);
  FTileDownloadRequestBuilderConfig.WriteConfig(ALocalConfig);
  FTileDownloaderConfig.WriteConfig(ALocalConfig);
  FVersionConfig.WriteConfig(ALocalConfig);
  FStorageConfig.WriteConfig(ALocalConfig);
  FAbilitiesConfig.WriteConfig(ALocalConfig);
end;

function TMapType.LoadBitmapTileFromStorage(
  const AXY: TPoint;
  const Azoom: Byte;
  var AVersionInfo: IMapVersionInfo
): IBitmap32Static;
var
  VTileInfo: ITileInfoBasic;
  VData: IBinaryData;
begin
  Result := nil;
  VData := FStorage.LoadTile(AXY, Azoom, AVersionInfo, VTileInfo);
  if VData <> nil then begin
    Result := FBitmapLoaderFromStorage.Load(VData);
    if Assigned(VTileInfo) then begin
      AVersionInfo := VTileInfo.VersionInfo;
    end;
  end;
end;

function TMapType.LoadKmlTileFromStorage(
  const AXY: TPoint;
  const Azoom: byte;
  var AVersionInfo: IMapVersionInfo
): IVectorDataItemList;
var
  VTileInfo: ITileInfoBasic;
  VData: IBinaryData;
begin
  Result := nil;
  VData := FStorage.LoadTile(AXY, Azoom, AVersionInfo, VTileInfo);
  if VData <> nil then begin
    Result := FKmlLoaderFromStorage.Load(VData, FVectorDataFactory);
    if Assigned(VTileInfo) then begin
      AVersionInfo := VTileInfo.VersionInfo;
    end;
  end;
end;

function TMapType.TileLoadDate(
  const AXY: TPoint;
  Azoom: byte
): TDateTime;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.Version);
  Result := VTileInfo.GetLoadDate;
end;

function TMapType.TileSize(
  const AXY: TPoint;
  Azoom: byte
): integer;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.Version);
  Result := VTileInfo.GetSize;
end;

procedure TMapType.SaveTileSimple(
  const AXY: TPoint;
  Azoom: byte;
  const ABitmap: IBitmap32Static
);
begin
  SaveBitmapTileToStorage(AXY, Azoom, ABitmap);
end;

function TMapType.TileExportToFile(
  const AXY: TPoint;
  Azoom: byte;
  const AFileName: string;
  OverWrite: boolean
): boolean;
var
  VFileStream: TFileStream;
  VFileExists: Boolean;
  VExportPath: string;
  VTileInfo: ITileInfoBasic;
  VData: IBinaryData;
begin
  Result := False;
  VFileExists := FileExists(AFileName);
  if not VFileExists or OverWrite then begin
    VData := FStorage.LoadTile(AXY, Azoom, FVersionConfig.Version, VTileInfo);
    if VData <> nil then begin
      if VFileExists then begin
        DeleteFile(AFileName);
      end else begin
        VExportPath := ExtractFilePath(AFileName);
        ForceDirectories(VExportPath);
      end;

      VFileStream := TFileStream.Create(AFileName, fmCreate);
      try
        VFileStream.WriteBuffer(VData.Buffer^, VData.Size);
        FileSetDate(AFileName, DateTimeToFileDate(VTileInfo.GetLoadDate));
      finally
        VFileStream.Free;
      end;
      Result := True;
    end;
  end;
end;

function TMapType.LoadFillingMap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  btm: TCustomBitmap32;
  const AXY: TPoint;
  Azoom, ASourceZoom: byte;
  const AColorer: IFillingMapColorer
): boolean;
begin
  Result :=
    FStorage.LoadFillingMap(
      AOperationID,
      ACancelNotifier,
      btm,
      AXY,
      Azoom,
      ASourceZoom,
      FVersionConfig.Version,
      AColorer
    );
end;

function TMapType.GetShortFolderName: string;
begin
  Result := ExtractFileName(ExtractFileDir(IncludeTrailingPathDelimiter(FStorageConfig.NameInCache)));
end;

function TMapType.GetTileShowName(
  const AXY: TPoint;
  Azoom: byte
): string;
begin
  if FStorageConfig.IsStoreFileCache then begin
    Result := FStorage.GetTileFileName(AXY, Azoom, FVersionConfig.Version);
  end else begin
    Result := 'z' + IntToStr(Azoom + 1) + 'x' + IntToStr(AXY.X) + 'y' + IntToStr(AXY.Y);
  end;
end;

function TMapType.GetFillingMapBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter;
  ASourceZoom: byte;
  const AColorer: IFillingMapColorer
): IBitmap32Static;
var
  VBitmap: TCustomBitmap32;
  VSize: TPoint;
  VTargetMapPixelRect: TDoubleRect;
  VSourceTileRect: TRect;
  VSourceRelativeRect: TDoubleRect;
  VSourceConverter: ICoordConverter;
  VTargetConverter: ICoordConverter;
  VSameSourceAndTarget: Boolean;
  VTargetZoom: Byte;
  VLonLatRect: TDoubleRect;
  VIterator: ITileIterator;
  VRelativeRectOfTile: TDoubleRect;
  VLonLatRectOfTile: TDoubleRect;
  VSolidDrow: Boolean;
  VTileRectInfo: ITileRectInfo;
  VEnumTileInfo: IEnumTileInfo;
  VTileInfo: TTileInfo;
  VMapPixelRectOfTile: TDoubleRect;
  VLocalPixelRectOfTile: TRect;
  VTileColor: TColor32;
begin
  VBitmap := TCustomBitmap32.Create;
  try
    VSize := ALocalConverter.GetLocalRectSize;
    VBitmap.SetSize(VSize.X, VSize.Y);
    VBitmap.Clear(0);

    VSourceConverter := FCoordConverter;
    VTargetConverter := ALocalConverter.GeoConverter;
    VTargetZoom := ALocalConverter.Zoom;

    VTargetMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
    VTargetConverter.CheckPixelRectFloat(VTargetMapPixelRect, VTargetZoom);

    VSameSourceAndTarget := VSourceConverter.IsSameConverter(VTargetConverter);
    if VSameSourceAndTarget then begin
      VSourceRelativeRect := VSourceConverter.PixelRectFloat2RelativeRect(VTargetMapPixelRect, VTargetZoom);
    end else begin
      VLonLatRect := VTargetConverter.PixelRectFloat2LonLatRect(VTargetMapPixelRect, VTargetZoom);
      VSourceConverter.CheckLonLatRect(VLonLatRect);
      VSourceRelativeRect := VSourceConverter.LonLatRect2RelativeRect(VLonLatRect);
    end;
    VSourceTileRect := VSourceConverter.RelativeRect2TileRect(VSourceRelativeRect, ASourceZoom);
    VSolidDrow :=
      (VSize.X <= (VSourceTileRect.Right - VSourceTileRect.Left) * 2) or
      (VSize.Y <= (VSourceTileRect.Bottom - VSourceTileRect.Top) * 2);
    VTileRectInfo := FStorage.GetTileRectInfo(VSourceTileRect, ASourceZoom, FVersionConfig.Version);
    if VTileRectInfo <> nil then begin
      VIterator := TTileIteratorByRect.Create(VSourceTileRect);
      VEnumTileInfo := VTileRectInfo.GetEnum(VIterator);
      while VEnumTileInfo.Next(VTileInfo) do begin
        VTileColor := AColorer.GetColor(VTileInfo);
        if VTileColor <> 0 then begin
          if VSameSourceAndTarget then begin
            VRelativeRectOfTile := VSourceConverter.TilePos2RelativeRect(VTileInfo.FTile, ASourceZoom);
          end else begin
            VLonLatRectOfTile := VSourceConverter.TilePos2LonLatRect(VTileInfo.FTile, ASourceZoom);
            VTargetConverter.CheckLonLatRect(VLonLatRectOfTile);
            VRelativeRectOfTile := VTargetConverter.LonLatRect2RelativeRect(VLonLatRectOfTile);
          end;
          VMapPixelRectOfTile := VTargetConverter.RelativeRect2PixelRectFloat(VRelativeRectOfTile, VTargetZoom);
          VLocalPixelRectOfTile := RectFromDoubleRect(ALocalConverter.MapRectFloat2LocalRectFloat(VMapPixelRectOfTile), rrToTopLeft);
          if not VSolidDrow then begin
            Dec(VLocalPixelRectOfTile.Right);
            Dec(VLocalPixelRectOfTile.Bottom);
          end;
          VBitmap.FillRectS(VLocalPixelRectOfTile, VTileColor);
        end;
      end;
    end;
    Result := TBitmap32Static.CreateWithOwn(VBitmap);
    VBitmap := nil;
  finally
    VBitmap.Free;
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

function TMapType.GetLanguageManager: ILanguageManager;
begin
  Result := FLanguageManager;
end;

function TMapType.GetIsHybridLayer: Boolean;
begin
  Result := IsBitmapTiles and FAbilitiesConfig.IsLayer;
end;

function TMapType.LoadTile(
  const AXY: TPoint;
  const Azoom: byte;
  IgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VVersionInfo: IMapVersionInfo;
  VRect: TRect;
  VSize: TPoint;
  VBitmap: TCustomBitmap32;
  VResampler: TCustomResampler;
begin
  try
    Result := nil;
    VVersionInfo := FVersionConfig.Version;
    if ACache = nil then begin
      Result := LoadBitmapTileFromStorage(AXY, Azoom, VVersionInfo);
    end else begin
      Result := ACache.TryLoadTileFromCache(AXY, Azoom, VVersionInfo);
      if Result = nil then begin
        Result := LoadBitmapTileFromStorage(AXY, Azoom, VVersionInfo);
        if Result <> nil then begin
          ACache.AddTileToCache(Result, AXY, Azoom, VVersionInfo);
        end;
      end;
    end;
    if Result <> nil then begin
      VRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
      VSize := Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
      if (Result.Bitmap.Width <> VSize.X) or
        (Result.Bitmap.Height <> VSize.Y) then begin
        VResampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;
        try
          VBitmap := TCustomBitmap32.Create;
          try
            VBitmap.SetSize(VSize.X, VSize.Y);
            StretchTransfer(
              VBitmap,
              VBitmap.BoundsRect,
              VBitmap.ClipRect,
              Result.Bitmap,
              Result.Bitmap.BoundsRect,
              VResampler,
              dmOpaque
            );
            Result := TBitmap32Static.CreateWithOwn(VBitmap);
            VBitmap := nil;
          finally
            VBitmap.Free;
          end;
        finally
          VResampler.Free;
        end;
      end;
    end;
  except
    if not IgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
end;

function TMapType.LoadTileVector(
  const AXY: TPoint;
  const Azoom: byte;
  IgnoreError: Boolean;
  const ACache: ITileObjCacheVector
): IVectorDataItemList;
var
  VVersionInfo: IMapVersionInfo;
begin
  Result := nil;
  try
    VVersionInfo := FVersionConfig.Version;
    if ACache = nil then begin
      Result := LoadKmlTileFromStorage(AXY, Azoom, VVersionInfo);
    end else begin
      Result := ACache.TryLoadTileFromCache(AXY, Azoom, VVersionInfo);
      if Result = nil then begin
        Result := LoadKmlTileFromStorage(AXY, Azoom, VVersionInfo);
        if Result <> nil then begin
          ACache.AddTileToCache(Result, AXY, Azoom, VVersionInfo);
        end;
      end;
    end;
  except
    if not IgnoreError then begin
      raise;
    end;
  end;
end;

function TMapType.LoadTileFromPreZ(
  const AXY: TPoint;
  const Azoom: byte;
  IgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  i: integer;
  VBmp: IBitmap32Static;
  VTileTargetBounds: TRect;
  VTileSourceBounds: TRect;
  VTileParent: TPoint;
  VTargetTilePixelRect: TRect;
  VSourceTilePixelRect: TRect;
  VRelative: TDoublePoint;
  VRelativeRect: TDoubleRect;
  VParentZoom: Byte;
  VMinZoom: Integer;
  VBitmap: TCustomBitmap32;
  VResampler: TCustomResampler;
begin
  Result := nil;
  VRelative := FCoordConverter.TilePos2Relative(AXY, Azoom);
  VMinZoom := Azoom - FLoadPrevMaxZoomDelta;
  if VMinZoom < 0 then begin
    VMinZoom := 0;
  end;
  if Azoom - 1 > VMinZoom then begin
    for i := Azoom - 1 downto VMinZoom do begin
      VParentZoom := i;
      VTileParent := PointFromDoublePoint(FCoordConverter.Relative2TilePosFloat(VRelative, i), prToTopLeft);
      VBmp := LoadTile(VTileParent, VParentZoom, IgnoreError, ACache);
      if VBmp <> nil then begin
        VTargetTilePixelRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
        VRelativeRect := FCoordConverter.PixelRect2RelativeRect(VTargetTilePixelRect, Azoom);
        VTileTargetBounds.Left := 0;
        VTileTargetBounds.Top := 0;
        VTileTargetBounds.Right := VTargetTilePixelRect.Right - VTargetTilePixelRect.Left;
        VTileTargetBounds.Bottom := VTargetTilePixelRect.Bottom - VTargetTilePixelRect.Top;

        VSourceTilePixelRect := FCoordConverter.TilePos2PixelRect(VTileParent, VParentZoom);
        VTargetTilePixelRect :=
          RectFromDoubleRect(
            FCoordConverter.RelativeRect2PixelRectFloat(VRelativeRect, VParentZoom),
            rrToTopLeft
          );
        VTileSourceBounds.Left := VTargetTilePixelRect.Left - VSourceTilePixelRect.Left;
        VTileSourceBounds.Top := VTargetTilePixelRect.Top - VSourceTilePixelRect.Top;
        VTileSourceBounds.Right := VTargetTilePixelRect.Right - VSourceTilePixelRect.Left;
        VTileSourceBounds.Bottom := VTargetTilePixelRect.Bottom - VSourceTilePixelRect.Top;
        VResampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;
        try
          try
            VBitmap := TCustomBitmap32.Create;
            try
              VBitmap.SetSize(VTileTargetBounds.Right, VTileTargetBounds.Bottom);
              StretchTransfer(
                VBitmap,
                VTileTargetBounds,
                VBitmap.ClipRect,
                VBmp.Bitmap,
                VTileSourceBounds,
                VResampler,
                dmOpaque
              );
              Result := TBitmap32Static.CreateWithOwn(VBitmap);
              VBitmap := nil;
            finally
              VBitmap.Free;
            end;
            Break;
          except
            if not IgnoreError then begin
              raise;
            end;
          end;
        finally
          VResampler.Free;
        end;
      end;
    end;
  end;
end;

function TMapType.LoadTileOrPreZ(
  const AXY: TPoint;
  const Azoom: byte;
  IgnoreError: Boolean;
  AUsePre: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
begin
  Result := LoadTile(AXY, Azoom, IgnoreError, ACache);
  if Result = nil then begin
    if AUsePre then begin
      Result := LoadTileFromPreZ(AXY, Azoom, IgnoreError, ACache);
    end;
  end;
end;

function TMapType.LoadBtimap(
  const APixelRectTarget: TRect;
  const AZoom: byte;
  AUsePre, AAllowPartial, IgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VZoom: Byte;
  VPixelRectTarget: TRect;
  VTileRect: TRect;
  VTargetImageSize: TPoint;
  VPixelRectCurrTile: TRect;
  i, j: Integer;
  VTile: TPoint;
  VSpr: IBitmap32Static;
  VSourceBounds: TRect;
  VTargetBounds: TRect;
  VBitmap: TCustomBitmap32;
begin
  Result := nil;

  VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
  VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

  VPixelRectTarget := APixelRectTarget;
  VZoom := Azoom;
  FCoordConverter.CheckPixelRect(VPixelRectTarget, VZoom);
  VTileRect := FCoordConverter.PixelRect2TileRect(VPixelRectTarget, VZoom);
  if (VTileRect.Left = VTileRect.Right - 1) and
    (VTileRect.Top = VTileRect.Bottom - 1) then begin
    VPixelRectCurrTile := FCoordConverter.TilePos2PixelRect(VTileRect.TopLeft, VZoom);
    if (VPixelRectCurrTile.Left = APixelRectTarget.Left) and
      (VPixelRectCurrTile.Top = APixelRectTarget.Top) and
      (VPixelRectCurrTile.Right = APixelRectTarget.Right) and
      (VPixelRectCurrTile.Bottom = APixelRectTarget.Bottom) then begin
      Result := LoadTileOrPreZ(VTileRect.TopLeft, VZoom, IgnoreError, AUsePre, ACache);
      exit;
    end;
  end;
  VBitmap := TCustomBitmap32.Create;
  try
    VBitmap.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
    VBitmap.Clear(0);

    for i := VTileRect.Top to VTileRect.Bottom - 1 do begin
      VTile.Y := i;
      for j := VTileRect.Left to VTileRect.Right - 1 do begin
        VTile.X := j;
        VSpr := LoadTileOrPreZ(VTile, VZoom, IgnoreError, AUsePre, ACache);
        if VSpr <> nil then begin
          VPixelRectCurrTile := FCoordConverter.TilePos2PixelRect(VTile, VZoom);

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

          BlockTransfer(
            VBitmap,
            VTargetBounds.Left,
            VTargetBounds.Top,
            VBitmap.ClipRect,
            VSpr.Bitmap,
            VSourceBounds,
            dmOpaque
          );
        end else begin
          if not AAllowPartial then begin
            Exit;
          end;
        end;
      end;
    end;
    Result := TBitmap32Static.CreateWithOwn(VBitmap);
    VBitmap := nil;
  finally
    VBitmap.Free;
  end;
end;

function TMapType.LoadBtimapUni(
  const APixelRectTarget: TRect;
  const AZoom: byte;
  const ACoordConverterTarget: ICoordConverter;
  AUsePre, AAllowPartial, IgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VPixelRectTarget: TRect;
  VZoom: Byte;
  VLonLatRectTarget: TDoubleRect;
  VTileRectInSource: TRect;
  VPixelRectOfTargetPixelRectInSource: TRect;
  VSpr: IBitmap32Static;
  VTargetImageSize: TPoint;
  VResampler: TCustomResampler;
  VBitmap: TCustomBitmap32;
begin
  Result := nil;

  if FCoordConverter.IsSameConverter(ACoordConverterTarget) then begin
    Result := LoadBtimap(APixelRectTarget, Azoom, AUsePre, AAllowPartial, IgnoreError, ACache);
  end else begin
    VZoom := Azoom;
    VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
    VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

    VPixelRectTarget := APixelRectTarget;
    ACoordConverterTarget.CheckPixelRect(VPixelRectTarget, VZoom);
    VLonLatRectTarget := ACoordConverterTarget.PixelRect2LonLatRect(VPixelRectTarget, VZoom);
    FCoordConverter.CheckLonLatRect(VLonLatRectTarget);
    VPixelRectOfTargetPixelRectInSource := FCoordConverter.LonLatRect2PixelRect(VLonLatRectTarget, VZoom);
    VTileRectInSource := FCoordConverter.PixelRect2TileRect(VPixelRectOfTargetPixelRectInSource, VZoom);
    VSpr := LoadBtimap(VPixelRectOfTargetPixelRectInSource, VZoom, AUsePre, AAllowPartial, IgnoreError, ACache);
    if VSpr <> nil then begin
      VResampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;
      try
        VBitmap := TCustomBitmap32.Create;
        try
          VBitmap.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
          VBitmap.Clear(0);
          StretchTransfer(
            VBitmap,
            VBitmap.BoundsRect,
            VBitmap.ClipRect,
            VSpr.Bitmap,
            VSpr.Bitmap.BoundsRect,
            VResampler,
            dmOpaque
          );
          Result := TBitmap32Static.CreateWithOwn(VBitmap);
          VBitmap := nil;
        finally
          VBitmap.Free;
        end;
      finally
        VResampler.Free;
      end;
    end;
  end;
end;

function TMapType.LoadTileUni(
  const AXY: TPoint;
  const Azoom: byte;
  const ACoordConverterTarget: ICoordConverter;
  AUsePre, AAllowPartial, IgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VPixelRect: TRect;
begin
  VPixelRect := ACoordConverterTarget.TilePos2PixelRect(AXY, Azoom);
  Result := LoadBtimapUni(VPixelRect, Azoom, ACoordConverterTarget, AUsePre, AAllowPartial, IgnoreError, ACache);
end;

end.
