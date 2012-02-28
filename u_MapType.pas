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
  i_JclNotify,
  i_FillingMapColorer,
  i_ContentTypeInfo,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_OperationNotifier,
  i_TileObjCache,
  i_TileDownloaderConfig,
  i_LanguageManager,
  i_CoordConverter,
  i_MapVersionConfig,
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
  t_MapAttachments,
  i_MapAttachmentsInfo,
  i_MapAttachmentsFactory,
  i_SimpleTileStorageConfig,
  i_ZmpInfo,
  i_InvisibleBrowser,
  i_MapTypeGUIConfig,
  i_CoordConverterFactory,
  i_MainMemCacheConfig,
  i_TileFileNameGeneratorsList,
  i_TileRectUpdateNotifier,
  i_VectorDataItemSimple,
  i_TileDownloadSubsystem,
  u_GlobalCahceConfig,
  u_BaseTileDownloaderThread,
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
    FGlobalDownloadConfig: IGlobalDownloadConfig;
    FGUIConfig: IMapTypeGUIConfig;
    FAbilitiesConfig: IMapAbilitiesConfig;
    FMapAttachmentsFactory: IMapAttachmentsFactory;
    FStorageConfig: ISimpleTileStorageConfig;
    FTileDownloadSubsystem: ITileDownloadSubsystem;

    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
    function GetIsHybridLayer: Boolean;
    procedure SaveBitmapTileToStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
    function LoadBitmapTileFromStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32): Boolean;
    function LoadKmlTileFromStorage(AXY: TPoint; Azoom: byte; var AKml: IVectorDataItemList): boolean;
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
    function GetNotifierByZoom(AZoom: Byte): ITileRectUpdateNotifier;
   public
    function AllowListOfTileVersions: Boolean;
    procedure SaveConfig(ALocalConfig: IConfigDataWriteProvider);
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
    function DeleteAttachments(const AXY: TPoint;
                               const Azoom: byte;
                               const ADelBytes: Boolean;
                               const ADelBytesNum: Integer): Integer;
    function DownloadAttachments(const AXY: TPoint;
                                 const Azoom: byte;
                                 const ACountersPtr: PMapAttachmentsCounters;
                                 const AThread: TBaseTileDownloaderThread): Boolean;
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

    function GetLanguageManager: ILanguageManager;
    // parse vector object description
    procedure MapAttachmentsInfoParser(Sender: TObject; var ADescr: String; var AOnlyCheckAllowRunImmediately: Boolean);

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
      AAppClosingNotifier: IJclNotifier;
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
  i_Bitmap32Static,
  i_TileInfoBasic,
  u_Bitmap32Static,
  u_TileDownloaderConfig,
  u_TileDownloadRequestBuilderConfig,
  u_DownloadResultFactory,
  u_TileRequestBuilderHelpers,
  u_MemTileCache,
  u_SimpleTileStorageConfig,
  u_MapAbilitiesConfig,
  u_MapAttachmentsPascalScript,
  u_MapTypeGUIConfig,
  u_MapVersionConfig,
  u_TileDownloadSubsystem,
  u_TileStorageGE,
  u_TileStorageBerkeleyDB,
  u_TileStorageFileSystem;

constructor TMapType.Create(
  ALanguageManager: ILanguageManager;
  AZmp: IZmpInfo;
  AMainMemCacheConfig: IMainMemCacheConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  ATileNameGeneratorList: ITileFileNameGeneratorsList;
  AGCList: ITTLCheckNotifier;
  AAppClosingNotifier: IJclNotifier;
  AInetConfig: IInetConfig;
  AImageResamplerConfig: IImageResamplerConfig;
  ADownloadConfig: IGlobalDownloadConfig;
  AContentTypeManager: IContentTypeManager;
  ACoordConverterFactory: ICoordConverterFactory;
  ADownloadResultTextProvider: IDownloadResultTextProvider;
  AInvisibleBrowser: IInvisibleBrowser;
  AConfig: IConfigDataProvider
);
var
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VContentTypeKml: IContentTypeInfoVectorData;
begin
  FZmp := AZmp;
  FGUIConfig :=
    TMapTypeGUIConfig.Create(
      ALanguageManager,
      FZmp.GUI
    );
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
  FStorageConfig.ReadConfig(AConfig);
  FAbilitiesConfig.ReadConfig(AConfig);
  FVersionConfig.ReadConfig(AConfig);
  FTileDownloaderConfig.ReadConfig(AConfig);
  FTileDownloadRequestBuilderConfig.ReadConfig(AConfig);

  if FStorageConfig.CacheTypeCode = 0 then begin
    FStorageConfig.CacheTypeCode := AGlobalCacheConfig.DefCache;
  end;

  if FStorageConfig.CacheTypeCode = 6 then begin
    FStorage := TTileStorageBerkeleyDB.Create(AGCList, FStorageConfig, AGlobalCacheConfig, FContentTypeManager);
  end else if FStorageConfig.CacheTypeCode = 5  then begin
    FStorage := TTileStorageGE.Create(FStorageConfig, AGlobalCacheConfig, FContentTypeManager);
  end else begin
    FStorage := TTileStorageFileSystem.Create(FStorageConfig, AGlobalCacheConfig, ATileNameGeneratorList, FContentTypeManager);
  end;
  FContentType := FStorage.GetMainContentType;
  if Supports(FContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
    FBitmapLoaderFromStorage := VContentTypeBitmap.GetLoader;
    FBitmapSaverToStorage := VContentTypeBitmap.GetSaver;
    FCacheBitmap := TMemTileCacheBitmap.Create(AGCList, FStorage, FStorageConfig.CoordConverter, AMainMemCacheConfig);
  end else if Supports(FContentType, IContentTypeInfoVectorData, VContentTypeKml) then begin
    FKmlLoaderFromStorage := VContentTypeKml.GetLoader;
    FCacheVector := TMemTileCacheVector.Create(AGCList, FStorage, FStorageConfig.CoordConverter, AMainMemCacheConfig);
  end;

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
      FVersionConfig,
      FTileDownloaderConfig,
      FTileDownloadRequestBuilderConfig,
      FContentTypeManager,
      FZmp.ContentTypeSubst,
      FZmp.TilePostDownloadCropConfig,
      FAbilitiesConfig,
      FZmp.DataProvider,
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

function TMapType.DownloadAttachments(const AXY: TPoint;
                                      const Azoom: byte;
                                      const ACountersPtr: PMapAttachmentsCounters;
                                      const AThread: TBaseTileDownloaderThread): Boolean;
var
  VKml: IVectorDataItemList;
  VSimpleAttach: IVectorDataItemSimple;
  VMapAttachmentsInfo: IMapAttachmentsInfo;
  VDesc,VNumber,VSubCache,VFullPath,VFullUrl: String;
  i,j,VSize,VMaxSubIndex: Integer;

  function _Thread_Terminated: Boolean;
  begin
    Result := FALSE;
    if (AThread<>nil) then
    if AThread.Terminated or AThread.PausedByUser then begin
      Result := TRUE;
      // calc cancelled objects
      if (nil<>ACountersPtr) then
        ACountersPtr^.ac_Cancelled := (VKml.Count - i);
    end;
  end;
begin
  // init
  Result := FALSE;
  if (nil<>ACountersPtr) then
    FillChar(ACountersPtr^, sizeof(ACountersPtr^), 0);

  // attachments
  VMapAttachmentsInfo := Self.Zmp.MapAttachmentsInfo;
  if (not Assigned(VMapAttachmentsInfo)) then
    Exit;
  VMaxSubIndex := VMapAttachmentsInfo.MaxSubIndex;

  // foreach tile
  if Self.LoadKmlTileFromStorage(AXY, Azoom, VKml) then
  if Assigned(VKml) then
  if (0<VKml.Count) then
  for i := 0 to VKml.Count-1 do begin
    // check terminated
    if _Thread_Terminated then
      Exit;

    // get item description (full text)
    VSimpleAttach:=VKml.GetItem(i);
    if Assigned(VSimpleAttach) then begin
      VDesc:=VSimpleAttach.Desc;

      if (Length(VDesc)>0) then begin
        // parse description
        VNumber:=GetNumberAfter(VMapAttachmentsInfo.GetParseNumberAfter, VDesc);
        if (Length(VNumber)>0) then begin
          VSubCache := GetDiv3Path(VNumber);

          // 0 just for default values - starts from 1
          for j := 1 to VMaxSubIndex do
          if VMapAttachmentsInfo.GetEnabled(j) then begin
            // check terminated or paused
            if _Thread_Terminated then
              Exit;
            
            // foreach url and cache item for single attachment
            VFullPath := VMapAttachmentsInfo.GetNameInCache(j)+VSubCache+VNumber+VMapAttachmentsInfo.GetExt(j);
            
            // if attachment exists - skip downloading
            if (FileExists(VFullPath)) then begin
              // skip existing attachments
              if (nil<>ACountersPtr) then
                Inc(ACountersPtr^.ac_Skipped);
            end else begin
              // make full source url
              VFullUrl := VMapAttachmentsInfo.GetDefURLBase(j)+VNumber+VMapAttachmentsInfo.GetExt(j);
              VSize := DownloadFileToLocal(VFullUrl, VFullPath, VMapAttachmentsInfo.GetContentType(j));
              if (VSize>0) then begin
                // downloaded ok
                if (nil<>ACountersPtr) then
                  Inc(ACountersPtr^.ac_Downloaded);
                if (nil<>ACountersPtr) then
                  Inc(ACountersPtr^.ac_Size,VSize);
                Result:=TRUE;
              end else begin
                // error
                if (nil<>ACountersPtr) then
                  Inc(ACountersPtr^.ac_Failed);
              end;
            end;
          end;
        end;
     end;
    end;
  end;
end;

function TMapType.GetNotifierByZoom(AZoom: Byte): ITileRectUpdateNotifier;
begin
  Result := FStorage.NotifierByZoom[AZoom];
end;

function TMapType.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Result := FStorage.GetTileFileName(AXY, Azoom, FVersionConfig.GetStatic);
end;

function TMapType.AllowListOfTileVersions: Boolean;
begin
  // only for GE
  Result := (FStorageConfig.CacheTypeCode = 5);
end;

function TMapType.TileExists(AXY: TPoint; Azoom: byte): Boolean;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersionConfig.GetStatic);
  Result := VTileInfo.GetIsExists;
end;

function TMapType.DeleteAttachments(const AXY: TPoint;
                                    const Azoom: byte;
                                    const ADelBytes: Boolean;
                                    const ADelBytesNum: Integer): Integer;
var
  VKml: IVectorDataItemList;
  VSimpleAttach: IVectorDataItemSimple;
  VMapAttachmentsInfo: IMapAttachmentsInfo;
  VDesc,VNumber,VSubCache,VFullPath: String;
  i,j: Integer;
begin
  Result:=0;
  // get file from storage
  // FStorage.GetTileFileName(AXY, Azoom, FVersionConfig.GetStatic);
  if Self.LoadKmlTileFromStorage(AXY, Azoom, VKml) then
  if Assigned(VKml) then
  if (VKml.Count>0) then
  for i := 0 to VKml.Count-1 do begin
    // get item description (full text)
    VSimpleAttach:=VKml.GetItem(i);
    if Assigned(VSimpleAttach) then begin
      VMapAttachmentsInfo := Self.Zmp.MapAttachmentsInfo;
      VDesc:=VSimpleAttach.Desc;
      if (Length(VDesc)>0) and Assigned(VMapAttachmentsInfo) then begin
        // parse description
        VNumber:=GetNumberAfter(VMapAttachmentsInfo.GetParseNumberAfter, VDesc);
        if (Length(VNumber)>0) then begin
          VSubCache := GetDiv3Path(VNumber);
          // 0 just for default values - starts from 1
          for j := 1 to VMapAttachmentsInfo.MaxSubIndex do
          if VMapAttachmentsInfo.GetEnabled(j) then begin
            // foreach cache item for single attachment
            VFullPath := VMapAttachmentsInfo.GetNameInCache(j)+VSubCache+VNumber+VMapAttachmentsInfo.GetExt(j);
            // delete attached file (what about ADelBytesNum?)
            if FileExists(VFullPath) then
              if DeleteFile(VFullPath) then
                Inc(Result);
          end;
        end;
     end;
    end;
  end;
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

function TMapType.GetTileShowName(AXY: TPoint; Azoom: byte): string;
begin
  if FStorageConfig.IsStoreFileCache then begin
    Result := FStorage.GetTileFileName(AXY, Azoom, FVersionConfig.GetStatic)
  end else begin
    Result := 'z' + IntToStr(Azoom + 1) + 'x' + IntToStr(AXY.X) + 'y' + IntToStr(AXY.Y);
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
  btm: TCustomBitmap32;
  AXY: TPoint;
  Azoom: byte;
  IgnoreError: Boolean;
  ACache: ITileObjCacheBitmap
): boolean;
var
  VBitmap: IBitmap32Static;
begin
  try
    if ACache = nil then begin
      Result := LoadBitmapTileFromStorage(AXY, Azoom, btm);
    end else begin
      VBitmap := ACache.TryLoadTileFromCache(AXY, Azoom);
      if VBitmap <> nil then begin
        btm.Assign(VBitmap.Bitmap);
        Result := True;
      end else begin
        Result := LoadBitmapTileFromStorage(AXY, Azoom, btm);
        if Result then begin
          VBitmap := TBitmap32Static.CreateWithCopy(btm);
          ACache.AddTileToCache(VBitmap, AXY, Azoom);
        end;
      end;
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
    if ACache = nil then begin
      Result := LoadKmlTileFromStorage(AXY, Azoom, AKml);
    end else begin
      AKml := ACache.TryLoadTileFromCache(AXY, Azoom);
      if AKml <> nil then begin
        Result := True;
      end else begin
        Result := LoadKmlTileFromStorage(AXY, Azoom, AKml);
        if Result then begin
          ACache.AddTileToCache(AKml, AXY, Azoom);
        end;
      end;
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
          VTileParent := FCoordConverter.Relative2TilePos(VRelative, i);
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
  VSpr:TCustomBitmap32;
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
      if LoadBtimap(VSpr, VPixelRectOfTargetPixelRectInSource, Azoom, AUsePre, AAllowPartial, IgnoreError, ACache) then begin
        VSpr.DrawTo(
          spr,
          spr.ClipRect,
          VSpr.ClipRect
        );
        Result := True;
      end;
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

procedure TMapType.MapAttachmentsInfoParser(Sender: TObject;
                                            var ADescr: String;
                                            var AOnlyCheckAllowRunImmediately: Boolean);
var
  VMapAttachmentsInfo: IMapAttachmentsInfo;
  VParseAttachmentScript: String;

  function _SenderTerminated: Boolean;
  begin
    Result := (Sender<>nil) and (Sender is TBaseTileDownloaderThread) and TBaseTileDownloaderThread(Sender).Terminated;
  end;

  function _CheckForeachAttachment(const ADownload: Boolean): Boolean;
  var
    j: Integer;
    VNumber, VSubCache, VFullPath, VFullUrl: String;
  begin
    Result := TRUE;
    VNumber := GetNumberAfter(VMapAttachmentsInfo.GetParseNumberAfter, ADescr);
    VSubCache := GetDiv3Path(VNumber);
    for j := 1 to VMapAttachmentsInfo.MaxSubIndex do
    if VMapAttachmentsInfo.GetEnabled(j) then begin
      // check terminated
      if _SenderTerminated then begin
        Result := FALSE;
        Exit;
      end;

      // full local path
      VFullPath := VMapAttachmentsInfo.GetNameInCache(j)+VSubCache+VNumber+VMapAttachmentsInfo.GetExt(j);
      // if file exists - nothing to do
      if (not FileExists(VFullPath)) then begin
        if ADownload then begin
          // download now
          VFullUrl := VMapAttachmentsInfo.GetDefURLBase(j)+VNumber+VMapAttachmentsInfo.GetExt(j);
          DownloadFileToLocal(VFullUrl, VFullPath, VMapAttachmentsInfo.GetContentType(j));
        end else begin
          // cannot show immediately
          Result := FALSE;
          Exit;
        end;
      end;
    end;
  end;

begin
  VMapAttachmentsInfo := Self.Zmp.MapAttachmentsInfo;
  if not Assigned(VMapAttachmentsInfo) then
    Exit;

  // real kml layer with attachments
  if AOnlyCheckAllowRunImmediately then begin
    // check attachments (for all enabled items)
    AOnlyCheckAllowRunImmediately := _CheckForeachAttachment(FALSE);
    if (not AOnlyCheckAllowRunImmediately) then
      Exit;
  end
  else begin
    // we can donload all attachments
    AOnlyCheckAllowRunImmediately := _CheckForeachAttachment(TRUE);
    if _SenderTerminated then
      Exit;
  end;

  // full description parser
  VParseAttachmentScript := Self.Zmp.DataProvider.ReadString('ParseAttachmentScript.txt', '');
  if (0<Length(VParseAttachmentScript)) then begin
    RunParseAttachmentScript(FMapAttachmentsFactory, VMapAttachmentsInfo, VParseAttachmentScript, ADescr);
  end;
end;

end.


