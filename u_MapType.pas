{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  Dialogs,
  GR32,
  t_GeoTypes,
  i_FillingMapColorer,
  i_ContentTypeInfo,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_OperationNotifier,
  i_TileObjCache,
  i_DownloadResult,
  i_TileDownloaderConfig,
  i_LanguageManager,
  i_CoordConverter,
  i_DownloadChecker,
  i_TileDownloader,
  i_TileDownloaderAsync,
  i_LastResponseInfo,
  i_MapVersionConfig,
  i_TileRequest,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderConfig,
  i_BitmapTileSaveLoad,
  i_VectorDataLoader,
  i_TTLCheckNotifier,
  i_DownloadResultFactory,
  i_InetConfig,
  i_DownloadResultTextProvider,
  i_ImageResamplerConfig,
  i_ContentTypeManager,
  i_GlobalDownloadConfig,
  i_MapAbilitiesConfig,
  i_SimpleTileStorageConfig,
  i_ZmpInfo,
  i_InvisibleBrowser,
  i_MapTypeGUIConfig,
  i_ProxySettings,
  i_TileDownloadChecker,
  i_CoordConverterFactory,
  i_MainMemCacheConfig,
  i_TileFileNameGeneratorsList,
  i_TileRectUpdateNotifier,
  i_TileDownloadRequest,
  i_VectorDataItemSimple,
  u_GlobalCahceConfig,
  u_TileStorageAbstract,
  u_TileDownloaderFrontEnd,
  u_ResStrings;

type
 TMapType = class
   private
    FZmp: IZmpInfo;

    FCacheBitmap: ITileObjCacheBitmap;
    FCacheVector: ITileObjCacheVector;
    FStorage: TTileStorageAbstract;
    FTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
    FBitmapLoaderFromStorage: IBitmapTileLoader;
    FBitmapSaverToStorage: IBitmapTileSaver;
    FKmlLoaderFromStorage: IVectorDataLoader;
    FCoordConverter : ICoordConverter;
    FViewCoordConverter : ICoordConverter;
    FLoadPrevMaxZoomDelta: Integer;
    FContentType: IContentTypeInfoBasic;
    FLanguageManager: ILanguageManager;
    FVersionConfig: IMapVersionConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    FDownloadResultFactory: IDownloadResultFactory;
    FImageResamplerConfig: IImageResamplerConfig;
    FContentTypeManager: IContentTypeManager;
    FDownloadConfig: IGlobalDownloadConfig;
    FGUIConfig: IMapTypeGUIConfig;
    FAbilitiesConfig: IMapAbilitiesConfig;
    FStorageConfig: ISimpleTileStorageConfig;
    FTileDownloader: ITileDownloader;
    FTileDownloaderFrontEnd: TTileDownloaderFrontEnd;
    FDownloadChecker: ITileDownloadChecker;

    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
    function GetIsHybridLayer: Boolean;
    procedure LoadUrlScript;
    procedure LoadDownloader(
      AGCList: ITTLCheckNotifier;
      ACoordConverterFactory: ICoordConverterFactory;
      AInvisibleBrowser: IInvisibleBrowser
    );
    procedure LoadStorageParams(
      AMainMemCacheConfig: IMainMemCacheConfig;
      AGCList: ITTLCheckNotifier;
      AGlobalCacheConfig: TGlobalCahceConfig;
      ATileNameGeneratorList: ITileFileNameGeneratorsList;
      ACoordConverterFactory: ICoordConverterFactory
    );
    procedure SaveBitmapTileToStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
    function LoadBitmapTileFromStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32): Boolean;
    function LoadKmlTileFromStorage(AXY: TPoint; Azoom: byte; var AKml: IVectorDataItemList): boolean;
    procedure LoadMapType(
      AMainMemCacheConfig: IMainMemCacheConfig;
      AGCList: ITTLCheckNotifier;
      AProxyConfig: IProxyConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      ATileNameGeneratorList: ITileFileNameGeneratorsList;
      ACoordConverterFactory: ICoordConverterFactory;
      AInvisibleBrowser: IInvisibleBrowser;
      AConfig : IConfigDataProvider
    );

    function LoadTileFromPreZ(
      spr: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      IgnoreError: Boolean;
      ACache: ITileObjCacheBitmap = nil
    ): boolean;
    function LoadTileOrPreZ(
      spr: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      IgnoreError: Boolean;
      AUsePre: Boolean;
      ACache: ITileObjCacheBitmap = nil
    ): boolean;
    function GetAbilitiesConfigStatic: IMapAbilitiesConfigStatic;
    function GetNotifierByZoom(AZoom: Byte): ITileRectUpdateNotifier;
   public
    procedure SaveConfig(ALocalConfig: IConfigDataWriteProvider);
    function GetRequest(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AXY: TPoint;
      Azoom: byte;
      ACheckTileSize: Boolean
    ): ITileRequest;
    function GetLink(AXY: TPoint; Azoom: byte): string;
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
    function GetTileShowName(AXY: TPoint; Azoom: byte): string;
    function TileExists(AXY: TPoint; Azoom: byte): Boolean;
    function TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;
    function LoadTile(
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      IgnoreError: Boolean;
      ACache: ITileObjCacheBitmap = nil
    ): boolean; overload;
    function LoadTile(
      var AKml: IVectorDataItemList;
      AXY: TPoint;
      Azoom: byte;
      IgnoreError: Boolean;
      ACache: ITileObjCacheVector = nil
    ): boolean; overload;
    function LoadTileUni(
      spr: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ACoordConverterTarget: ICoordConverter;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      ACache: ITileObjCacheBitmap = nil
    ): boolean;
    function LoadBtimap(
      spr: TCustomBitmap32;
      APixelRectTarget: TRect;
      Azoom: byte;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      ACache: ITileObjCacheBitmap = nil
    ): boolean;
    function LoadBtimapUni(
      spr: TCustomBitmap32;
      APixelRectTarget: TRect;
      Azoom: byte;
      ACoordConverterTarget: ICoordConverter;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      ACache: ITileObjCacheBitmap = nil
    ): boolean;
    function DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
    procedure SaveTileSimple(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
    function TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
    function TileSize(AXY: TPoint; Azoom: byte): integer;
    function TileExportToFile(AXY: TPoint; Azoom: byte; AFileName: string; OverWrite: boolean): boolean;

    function LoadFillingMap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      AColorer: IFillingMapColorer
    ): boolean;
    function GetShortFolderName: string;
    procedure DownloadTile(AEvent: ITileDownloaderEvent);

    property Zmp: IZmpInfo read FZmp;
    property GeoConvert: ICoordConverter read FCoordConverter;
    property ViewGeoConvert: ICoordConverter read FViewCoordConverter;
    property VersionConfig: IMapVersionConfig read FVersionConfig;

    property Abilities: IMapAbilitiesConfigStatic read GetAbilitiesConfigStatic;
    property StorageConfig: ISimpleTileStorageConfig read FStorageConfig;
    property IsBitmapTiles: Boolean read GetIsBitmapTiles;
    property IsKmlTiles: Boolean read GetIsKmlTiles;
    property IsHybridLayer: Boolean read GetIsHybridLayer;

    property TileDownloader: ITileDownloader read FTileDownloader;
    property TileStorage: TTileStorageAbstract read FStorage;
    property GUIConfig: IMapTypeGUIConfig read FGUIConfig;
    property TileDownloaderConfig: ITileDownloaderConfig read FTileDownloaderConfig;
    property TileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig read FTileDownloadRequestBuilderConfig;
    property CacheBitmap: ITileObjCacheBitmap read FCacheBitmap;
    property CacheVector: ITileObjCacheVector read FCacheVector;
    property NotifierByZoom[AZoom: Byte]: ITileRectUpdateNotifier read GetNotifierByZoom;

    constructor Create(
      ALanguageManager: ILanguageManager;
      AZmp: IZmpInfo;
      AMainMemCacheConfig: IMainMemCacheConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      ATileNameGeneratorList: ITileFileNameGeneratorsList;
      AGCList: ITTLCheckNotifier;
      AInetConfig: IInetConfig;
      AImageResamplerConfig: IImageResamplerConfig;
      ADownloadConfig: IGlobalDownloadConfig;
      AContentTypeManager: IContentTypeManager;
      ACoordConverterFactory: ICoordConverterFactory;
      ADownloadResultTextProvider: IDownloadResultTextProvider;
      AInvisibleBrowser: IInvisibleBrowser;
      AConfig: IConfigDataProvider
    );
    destructor Destroy; override;
 end;

implementation

uses
  Types,
  GR32_Resamplers,
  i_TileInfoBasic,
  i_ContentConverter,
  i_TileDownloaderList,
  u_TileDownloaderList,
  u_TileDownloaderWithQueue,
  u_AntiBanStuped,
  u_TileDownloaderConfig,
  u_TileDownloadRequestBuilderConfig,
  u_TileDownloadRequestBuilderPascalScript,
  u_DownloadResultFactory,
  u_MemTileCache,
  u_TileRequest,
  u_SimpleTileStorageConfig,
  u_MapAbilitiesConfig,
  u_MapTypeGUIConfig,
  u_LastResponseInfo,
  u_MapVersionConfig,
  u_DownloadCheckerStuped,
  u_TileStorageGE,
  u_TileStorageFileSystem;

procedure TMapType.LoadUrlScript;
begin
  FTileDownloadRequestBuilder := nil;
  FAbilitiesConfig.LockWrite;
  try
    if FAbilitiesConfig.UseDownload then begin
      try
        FTileDownloadRequestBuilder :=
          TTileDownloadRequestBuilderPascalScript.Create(
            FZmp,
            FTileDownloadRequestBuilderConfig,
            FTileDownloaderConfig,
            FLanguageManager
          );
      except
        on E: Exception do begin
          ShowMessageFmt(SAS_ERR_UrlScriptError, [FZmp.GUI.Name.GetDefault, E.Message, FZmp.FileName]);
          FTileDownloadRequestBuilder := nil;
        end;
      else
        ShowMessageFmt(SAS_ERR_UrlScriptUnexpectedError, [FZmp.GUI.Name.GetDefault, FZmp.FileName]);
        FTileDownloadRequestBuilder := nil;
      end;
    end;
    if FTileDownloadRequestBuilder = nil then begin
      FAbilitiesConfig.UseDownload := False;
    end;
  finally
    FAbilitiesConfig.UnlockWrite;
  end;
end;

procedure TMapType.LoadStorageParams(
  AMainMemCacheConfig: IMainMemCacheConfig;
  AGCList: ITTLCheckNotifier;
  AGlobalCacheConfig: TGlobalCahceConfig;
  ATileNameGeneratorList: ITileFileNameGeneratorsList;
  ACoordConverterFactory: ICoordConverterFactory
);
var
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VContentTypeKml: IContentTypeInfoVectorData;
begin
  if FStorageConfig.CacheTypeCode = 5  then begin
    FStorage := TTileStorageGE.Create(FStorageConfig, AGlobalCacheConfig, FContentTypeManager);
  end else begin
    FStorage := TTileStorageFileSystem.Create(FStorageConfig, AGlobalCacheConfig, ATileNameGeneratorList, FContentTypeManager);
  end;
  FContentType := FStorage.GetMainContentType;
  if Supports(FContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
    FBitmapLoaderFromStorage := VContentTypeBitmap.GetLoader;
    if FStorageConfig.AllowAdd then begin
      FBitmapSaverToStorage := VContentTypeBitmap.GetSaver;
    end;
    FCacheBitmap := TMemTileCacheBitmap.Create(AGCList, FStorage, FStorageConfig.CoordConverter, AMainMemCacheConfig);
  end else if Supports(FContentType, IContentTypeInfoVectorData, VContentTypeKml) then begin
    FKmlLoaderFromStorage := VContentTypeKml.GetLoader;
    FCacheVector := TMemTileCacheVector.Create(AGCList, FStorage, FStorageConfig.CoordConverter, AMainMemCacheConfig);
  end;
end;

procedure TMapType.LoadDownloader(
  AGCList: ITTLCheckNotifier;
  ACoordConverterFactory: ICoordConverterFactory;
  AInvisibleBrowser: IInvisibleBrowser
);
var
  VDownloaderList: ITileDownloaderList;
begin
  FAbilitiesConfig.LockWrite;
  try
    if FAbilitiesConfig.UseDownload then begin
      try
        FDownloadChecker := TDownloadCheckerStuped.Create(
          TAntiBanStuped.Create(AInvisibleBrowser, FZmp.DataProvider),
          FTileDownloaderConfig,
          FDownloadConfig,
          FContentTypeManager,
          FZmp.ContentTypeSubst,
          FZmp.TilePostDownloadCropConfig,
          FStorageConfig,
          FStorage
        );
        VDownloaderList :=
          TTileDownloaderList.Create(
            AGCList,
            FDownloadResultFactory,
            FTileDownloaderConfig,
            FTileDownloadRequestBuilderConfig,
            FZmp,
            FLanguageManager
          );
          FTileDownloader := TTileDownloaderWithQueue.Create(
            AGCList,
            256
          );
        FTileDownloaderFrontEnd := TTileDownloaderFrontEnd.Create(
          FTileDownloaderConfig,
          FTileDownloadRequestBuilderConfig,
          FZmp,
          FLanguageManager,
          AInvisibleBrowser
        );
        FAbilitiesConfig.UseDownload := FTileDownloaderFrontEnd.Enabled;
      except
        if ExceptObject <> nil then begin
          ShowMessageFmt(SAS_ERR_MapDownloadByError,[ZMP.FileName, (ExceptObject as Exception).Message]);
        end;
        FAbilitiesConfig.UseDownload := False;
      end;
    end;
  finally
    FAbilitiesConfig.UnlockWrite;
  end;
end;

procedure TMapType.LoadMapType(
  AMainMemCacheConfig: IMainMemCacheConfig;
  AGCList: ITTLCheckNotifier;
  AProxyConfig: IProxyConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  ATileNameGeneratorList: ITileFileNameGeneratorsList;
  ACoordConverterFactory: ICoordConverterFactory;
  AInvisibleBrowser: IInvisibleBrowser;
  AConfig: IConfigDataProvider
);
begin
  FGUIConfig.ReadConfig(AConfig);
  FStorageConfig.ReadConfig(AConfig);
  FAbilitiesConfig.ReadConfig(AConfig);
  FVersionConfig.ReadConfig(AConfig);
  FTileDownloaderConfig.ReadConfig(AConfig);
  LoadStorageParams(
    AMainMemCacheConfig,
    AGCList,
    AGlobalCacheConfig,
    ATileNameGeneratorList,
    ACoordConverterFactory
  );
  FCoordConverter := FStorageConfig.CoordConverter;
  FViewCoordConverter := Zmp.ViewGeoConvert;
  FTileDownloadRequestBuilderConfig.ReadConfig(AConfig);
  LoadUrlScript;
  LoadDownloader(
    AGCList,
    ACoordConverterFactory,
    AInvisibleBrowser
  );
end;

function TMapType.GetLink(AXY: TPoint; Azoom: byte): string;
var
  VRequest: ITileRequest;
  VDownloadRequest: ITileDownloadRequest;
begin
  Result := '';
  if FAbilitiesConfig.UseDownload then begin
    VRequest := GetRequest(
      nil,
      0,
      AXY,
      Azoom,
      False
    );
    VDownloadRequest:= nil;
    if VRequest <> nil then begin
      VDownloadRequest := FTileDownloadRequestBuilder.BuildRequest(VRequest, nil);
    end;
    if VDownloadRequest <> nil then begin
      Result := VDownloadRequest.Url;
    end;
  end;
end;

function TMapType.GetNotifierByZoom(AZoom: Byte): ITileRectUpdateNotifier;
begin
  Result := FStorage.NotifierByZoom[AZoom];
end;

function TMapType.GetRequest(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  AXY: TPoint;
  Azoom: byte;
  ACheckTileSize: Boolean
): ITileRequest;
begin
  Result := nil;
  if FCoordConverter.CheckTilePosStrict(AXY, Azoom, False) then begin
    if ACheckTileSize then begin
      Result :=
        TTileRequestWithSizeCheck.Create(
          FZmp,
          AXY,
          Azoom,
          FVersionConfig.GetStatic,
          FDownloadChecker,
          ACancelNotifier,
          AOperationID
        );
    end else begin
      Result :=
        TTileRequest.Create(
          FZmp,
          AXY,
          Azoom,
          FVersionConfig.GetStatic,
          FDownloadChecker,
          ACancelNotifier,
          AOperationID
        );
    end;
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

procedure TMapType.SaveConfig(ALocalConfig: IConfigDataWriteProvider);
begin
  FGUIConfig.WriteConfig(ALocalConfig);
  FTileDownloadRequestBuilderConfig.WriteConfig(ALocalConfig);
  FTileDownloaderConfig.WriteConfig(ALocalConfig);
  FVersionConfig.WriteConfig(ALocalConfig);
  FStorageConfig.WriteConfig(ALocalConfig);
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
      Result := AKml <> nil;
    end;
  finally
    VMemStream.Free;
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
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  btm: TCustomBitmap32;
  AXY: TPoint;
  Azoom, ASourceZoom: byte;
  AColorer: IFillingMapColorer
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
      FVersionConfig.GetStatic,
      AColorer
    );
end;

function TMapType.GetShortFolderName: string;
begin
  Result := ExtractFileName(ExtractFileDir(IncludeTrailingPathDelimiter(FStorageConfig.NameInCache)));
end;

constructor TMapType.Create(
  ALanguageManager: ILanguageManager;
  AZmp: IZmpInfo;
  AMainMemCacheConfig: IMainMemCacheConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  ATileNameGeneratorList: ITileFileNameGeneratorsList;
  AGCList: ITTLCheckNotifier;
  AInetConfig: IInetConfig;
  AImageResamplerConfig: IImageResamplerConfig;
  ADownloadConfig: IGlobalDownloadConfig;
  AContentTypeManager: IContentTypeManager;
  ACoordConverterFactory: ICoordConverterFactory;
  ADownloadResultTextProvider: IDownloadResultTextProvider;
  AInvisibleBrowser: IInvisibleBrowser;
  AConfig: IConfigDataProvider
);
begin
  FZmp := AZmp;
  FGUIConfig :=
    TMapTypeGUIConfig.Create(
      ALanguageManager,
      FZmp.GUI
    );
  FLanguageManager := ALanguageManager;
  FImageResamplerConfig := AImageResamplerConfig;
  FDownloadConfig := ADownloadConfig;
  FContentTypeManager := AContentTypeManager;
  FTileDownloaderConfig := TTileDownloaderConfig.Create(AInetConfig, Zmp.TileDownloaderConfig);
  FTileDownloadRequestBuilderConfig := TTileDownloadRequestBuilderConfig.Create(Zmp.TileDownloadRequestBuilderConfig);
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
  LoadMapType(
    AMainMemCacheConfig,
    AGCList,
    AInetConfig.ProxyConfig,
    AGlobalCacheConfig,
    ATileNameGeneratorList,
    ACoordConverterFactory,
    AInvisibleBrowser,
    AConfig
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
  FreeAndNil(FStorage);
  FreeAndNil(FTileDownloaderFrontEnd);
  inherited;
end;

procedure TMapType.DownloadTile(AEvent: ITileDownloaderEvent);
begin
  Assert(AEvent <> nil);
  if Assigned(AEvent) then begin
    if FAbilitiesConfig.UseDownload then begin
      FTileDownloaderFrontEnd.Download(AEvent);
    end else begin
      raise Exception.Create('Для этой карты загрузка запрещена.');
    end;
  end;
end;

function TMapType.GetTileShowName(AXY: TPoint; Azoom: byte): string;
begin
  if FStorageConfig.IsStoreFileCache then begin
    Result := FStorage.GetTileFileName(AXY, Azoom, FVersionConfig.GetStatic)
  end else begin
    Result := 'z' + IntToStr(Azoom + 1) + 'x' + IntToStr(AXY.X) + 'y' + IntToStr(AXY.Y);
  end;
end;

function TMapType.GetAbilitiesConfigStatic: IMapAbilitiesConfigStatic;
begin
  Result := FAbilitiesConfig.GetStatic;
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
  Result := IsBitmapTiles and FAbilitiesConfig.IsLayer;
end;

function TMapType.LoadTile(
  btm: TCustomBitmap32;
  AXY: TPoint;
  Azoom: byte;
  IgnoreError: Boolean;
  ACache: ITileObjCacheBitmap
): boolean;
begin
  try
    if (ACache = nil) or (not ACache.TryLoadTileFromCache(btm, AXY, Azoom)) then begin
      result:=LoadBitmapTileFromStorage(AXY, Azoom, btm);
      if ((result)and(ACache <> nil)) then ACache.AddTileToCache(btm, AXY, Azoom);
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

function TMapType.LoadTile(
  var AKml: IVectorDataItemList;
  AXY: TPoint;
  Azoom: byte;
  IgnoreError: Boolean;
  ACache: ITileObjCacheVector
): boolean;
begin
  try
    if (ACache = nil) or (not ACache.TryLoadTileFromCache(AKml, AXY, Azoom)) then begin
      result:=LoadKmlTileFromStorage(AXY, Azoom, AKml);
      if ((result)and(ACache <> nil)) then ACache.AddTileToCache(AKml, AXY, Azoom);
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

function TMapType.LoadTileFromPreZ(
  spr: TCustomBitmap32;
  AXY: TPoint;
  Azoom: byte;
  IgnoreError: Boolean;
  ACache: ITileObjCacheBitmap
): boolean;
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
          if LoadTile(VBmp, VTileParent, VParentZoom, IgnoreError, ACache)then begin
            VTargetTilePixelRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
            VRelativeRect := FCoordConverter.PixelRect2RelativeRect(VTargetTilePixelRect, Azoom);
            VTileTargetBounds.Left := 0;
            VTileTargetBounds.Top := 0;
            VTileTargetBounds.Right := VTargetTilePixelRect.Right - VTargetTilePixelRect.Left;
            VTileTargetBounds.Bottom := VTargetTilePixelRect.Bottom - VTargetTilePixelRect.Top;

            VBmp.Resampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;

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

function TMapType.LoadTileOrPreZ(
  spr: TCustomBitmap32;
  AXY: TPoint;
  Azoom: byte;
  IgnoreError: Boolean;
  AUsePre: Boolean;
  ACache: ITileObjCacheBitmap
): boolean;
var
  VRect: TRect;
  VSize: TPoint;
  bSpr:TCustomBitmap32;
begin
  VRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
  VSize := Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
  Result := LoadTile(spr, AXY, Azoom, IgnoreError, ACache);
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
      Result := LoadTileFromPreZ(spr, AXY, Azoom, IgnoreError, ACache);
    end;
  end;
end;

function TMapType.LoadBtimap(
  spr: TCustomBitmap32;
  APixelRectTarget: TRect;
  Azoom: byte;
  AUsePre, AAllowPartial, IgnoreError: Boolean;
  ACache: ITileObjCacheBitmap
): boolean;
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
      Result := LoadTileOrPreZ(spr, VTileRect.TopLeft, Azoom, IgnoreError, AUsePre, ACache);
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
        VLoadResult := LoadTileOrPreZ(VSpr, VTile, Azoom, IgnoreError, AUsePre, ACache);
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

function TMapType.LoadBtimapUni(
  spr: TCustomBitmap32;
  APixelRectTarget: TRect;
  Azoom: byte;
  ACoordConverterTarget: ICoordConverter;
  AUsePre, AAllowPartial, IgnoreError: Boolean;
  ACache: ITileObjCacheBitmap
): boolean;
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
    Result := LoadBtimap(spr, APixelRectTarget, Azoom, AUsePre, AAllowPartial, IgnoreError, ACache);
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
          VLoadResult := LoadTileOrPreZ(VSpr, VTile, Azoom, IgnoreError, AUsePre, ACache);
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

function TMapType.LoadTileUni(
  spr: TCustomBitmap32;
  AXY: TPoint;
  Azoom: byte;
  ACoordConverterTarget: ICoordConverter;
  AUsePre, AAllowPartial, IgnoreError: Boolean;
  ACache: ITileObjCacheBitmap
): boolean;
var
  VPixelRect: TRect;
begin
  VPixelRect := ACoordConverterTarget.TilePos2PixelRect(AXY, Azoom);
  Result := LoadBtimapUni(spr, VPixelRect, Azoom, ACoordConverterTarget, AUsePre, AAllowPartial, IgnoreError, ACache);
end;

end.


