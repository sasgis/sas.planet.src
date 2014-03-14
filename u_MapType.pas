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

unit u_MapType;

interface

uses
  SysUtils,
  GR32,
  t_GeoTypes,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_ThreadConfig,
  i_ContentTypeInfo,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_NotifierOperation,
  i_LayerDrawConfig,
  i_TileObjCache,
  i_TileDownloaderConfig,
  i_LanguageManager,
  i_CoordConverter,
  i_MapVersionRequest,
  i_MapVersionRequestConfig,
  i_MapVersionFactoryList,
  i_TileDownloadRequestBuilderConfig,
  i_HashFunction,
  i_BitmapTileSaveLoad,
  i_VectorDataLoader,
  i_NotifierTime,
  i_InetConfig,
  i_ImageResamplerConfig,
  i_ContentTypeManager,
  i_GlobalDownloadConfig,
  i_MapAbilitiesConfig,
  i_Listener,
  i_MapVersionInfo,
  i_SimpleTileStorageConfig,
  i_ZmpInfo,
  i_InvisibleBrowser,
  i_MapTypeGUIConfig,
  i_CoordConverterFactory,
  i_MainMemCacheConfig,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_ProjConverter,
  i_TileDownloadSubsystem,
  i_InternalPerformanceCounter,
  i_TileStorageTypeList,
  i_TileInfoBasicMemCache,
  i_GlobalCacheConfig,
  i_TileStorage,
  i_MapTypes,
  u_BaseInterfacedObject;

type
  TMapType = class(TBaseInterfacedObject, IMapType)
  private
    FZmp: IZmpInfo;
    FMapDataUrlPrefix: string;
    FCacheTileInfo: ITileInfoBasicMemCache;
    FCacheBitmap: ITileObjCacheBitmap;
    FCacheVector: ITileObjCacheVector;
    FStorage: ITileStorage;
    FBitmapLoaderFromStorage: IBitmapTileLoader;
    FBitmapSaverToStorage: IBitmapTileSaver;
    FKmlLoaderFromStorage: IVectorDataLoader;
    FVectorDataFactory: IVectorDataItemMainInfoFactory;
    FCoordConverter: ICoordConverter;
    FViewCoordConverter: ICoordConverter;
    FLoadPrevMaxZoomDelta: Integer;
    FContentType: IContentTypeInfoBasic;
    FVersionRequestConfig: IMapVersionRequestConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    FResamplerConfigLoad: IImageResamplerConfig;
    FResamplerConfigGetPrev: IImageResamplerConfig;
    FResamplerConfigChangeProjection: IImageResamplerConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FGUIConfig: IMapTypeGUIConfig;
    FLayerDrawConfig: ILayerDrawConfig;
    FAbilitiesConfig: IMapAbilitiesConfig;
    FStorageConfig: ISimpleTileStorageConfig;
    FTileDownloadSubsystem: ITileDownloadSubsystem;

    FVersionChangeListener: IListener;
    procedure OnVersionChange;

    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
    function LoadBitmapTileFromStorage(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionRequest
    ): IBitmap32Static;
    function LoadKmlTileFromStorage(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest
    ): IVectorItemSubset;
    function LoadTileFromPreZ(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadTileOrPreZ(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      IgnoreError: Boolean;
      AUsePre: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    procedure SaveConfig(const ALocalConfig: IConfigDataWriteProvider);
    procedure ClearMemCache;
    function GetTileShowName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): string;
    function LoadTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadTileVector(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      IgnoreError: Boolean;
      const ACache: ITileObjCacheVector = nil
    ): IVectorItemSubset;
    function LoadTileUni(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      const ACoordConverterTarget: ICoordConverter;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadBitmap(
      const APixelRectTarget: TRect;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadBitmapUni(
      const APixelRectTarget: TRect;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      const ACoordConverterTarget: ICoordConverter;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;

    function GetShortFolderName: string;

    function GetGUID: TGUID;

    function GetZmp: IZmpInfo;
    function GetGeoConvert: ICoordConverter;
    function GetViewGeoConvert: ICoordConverter;
    function GetVersionRequestConfig: IMapVersionRequestConfig;
    function GetContentType: IContentTypeInfoBasic;

    function GetAbilities: IMapAbilitiesConfig;
    function GetStorageConfig: ISimpleTileStorageConfig;

    function GetTileDownloadSubsystem: ITileDownloadSubsystem;
    function GetTileStorage: ITileStorage;
    function GetGUIConfig: IMapTypeGUIConfig;
    function GetLayerDrawConfig: ILayerDrawConfig;
    function GetTileDownloaderConfig: ITileDownloaderConfig;
    function GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    function GetCacheBitmap: ITileObjCacheBitmap;
    function GetCacheVector: ITileObjCacheVector;

  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AZmp: IZmpInfo;
      const AMapVersionFactoryList: IMapVersionFactoryList;
      const AMainMemCacheConfig: IMainMemCacheConfig;
      const AGlobalCacheConfig: IGlobalCacheConfig;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const AGCNotifier: INotifierTime;
      const AAppClosingNotifier: INotifierOneOperation;
      const AInetConfig: IInetConfig;
      const AResamplerConfigLoad: IImageResamplerConfig;
      const AResamplerConfigGetPrev: IImageResamplerConfig;
      const AResamplerConfigChangeProjection: IImageResamplerConfig;
      const AResamplerConfigDownload: IImageResamplerConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AHashFunction: IHashFunction;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloaderThreadConfig: IThreadConfig;
      const AContentTypeManager: IContentTypeManager;
      const ACoordConverterFactory: ICoordConverterFactory;
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
  c_InternalBrowser,
  i_BasicMemCache,
  i_TileInfoBasic,
  i_MapVersionFactory,
  i_DownloadResultFactory,
  u_StringProviderForMapTileItem,
  u_LayerDrawConfig,
  u_TileDownloaderConfig,
  u_TileDownloadRequestBuilderConfig,
  u_DownloadResultFactory,
  u_MemTileCache,
  u_SimpleTileStorageConfig,
  u_MapAbilitiesConfig,
  u_VectorDataFactoryForMap,
  u_HtmlToHintTextConverterStuped,
  u_MapTypeGUIConfig,
  u_MapVersionFactoryChangeable,
  u_MapVersionRequestConfig,
  u_TileDownloadSubsystem,
  u_Bitmap32ByStaticBitmap,
  u_GeoFunc,
  u_BitmapFunc,
  u_TileStorageOfMapType,
  u_TileInfoBasicMemCache,
  u_ListenerByEvent;

procedure TMapType.ClearMemCache;
var
  VBasicMemCache: IBasicMemCache;
begin
  if FCacheTileInfo <> nil then begin
    FCacheTileInfo.Clear;
  end;
  if FCacheBitmap <> nil then begin
    FCacheBitmap.Clear;
  end;
  if FCacheVector <> nil then begin
    FCacheVector.Clear;
  end;
  // clear internal MemCache
  if Assigned(FStorage) then
  if Supports(FStorage, IBasicMemCache, VBasicMemCache) then begin
    VBasicMemCache.Clear;
  end;
end;

constructor TMapType.Create(
  const ALanguageManager: ILanguageManager;
  const AZmp: IZmpInfo;
  const AMapVersionFactoryList: IMapVersionFactoryList;
  const AMainMemCacheConfig: IMainMemCacheConfig;
  const AGlobalCacheConfig: IGlobalCacheConfig;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AGCNotifier: INotifierTime;
  const AAppClosingNotifier: INotifierOneOperation;
  const AInetConfig: IInetConfig;
  const AResamplerConfigLoad: IImageResamplerConfig;
  const AResamplerConfigGetPrev: IImageResamplerConfig;
  const AResamplerConfigChangeProjection: IImageResamplerConfig;
  const AResamplerConfigDownload: IImageResamplerConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AHashFunction: IHashFunction;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloaderThreadConfig: IThreadConfig;
  const AContentTypeManager: IContentTypeManager;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AInvisibleBrowser: IInvisibleBrowser;
  const AProjFactory: IProjConverterFactory;
  const AConfig: IConfigDataProvider;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VContentTypeKml: IContentTypeInfoVectorData;
  VVersionFactory: IMapVersionFactoryChangeableInternal;
  VPerfCounterList: IInternalPerformanceCounterList;
  VDownloadResultFactory: IDownloadResultFactory;
begin
  inherited Create;
  FZmp := AZmp;
  VPerfCounterList := APerfCounterList.CreateAndAddNewSubList(FZmp.GUI.Name.GetDefault);
  FGUIConfig :=
    TMapTypeGUIConfig.Create(
      ALanguageManager,
      FZmp.GUI
    );
  FLayerDrawConfig := TLayerDrawConfig.Create(FZmp);
  FResamplerConfigLoad := AResamplerConfigLoad;
  FResamplerConfigGetPrev := AResamplerConfigGetPrev;
  FResamplerConfigChangeProjection := AResamplerConfigChangeProjection;
  FBitmapFactory := ABitmapFactory;
  FTileDownloaderConfig := TTileDownloaderConfig.Create(AInetConfig, FZmp.TileDownloaderConfig);
  FTileDownloadRequestBuilderConfig := TTileDownloadRequestBuilderConfig.Create(FZmp.TileDownloadRequestBuilderConfig);

  VVersionFactory :=
    TMapVersionFactoryChangeable.Create(
      AMapVersionFactoryList.GetSimpleVersionFactory
    );
  FVersionRequestConfig :=
    TMapVersionRequestConfig.Create(
      FZmp.VersionConfig,
      VVersionFactory
    );
  FVersionChangeListener := TNotifyNoMmgEventListener.Create(Self.OnVersionChange);
  FVersionRequestConfig.ChangeNotifier.Add(FVersionChangeListener);

  FStorageConfig := TSimpleTileStorageConfig.Create(FZmp.StorageConfig);
  FAbilitiesConfig :=
    TMapAbilitiesConfig.Create(
      FZmp.Abilities,
      FStorageConfig
    );

  FGUIConfig.ReadConfig(AConfig);
  FLayerDrawConfig.ReadConfig(AConfig);
  FStorageConfig.ReadConfig(AConfig);
  FAbilitiesConfig.ReadConfig(AConfig);
  FVersionRequestConfig.ReadConfig(AConfig);
  FTileDownloaderConfig.ReadConfig(AConfig);
  FTileDownloadRequestBuilderConfig.ReadConfig(AConfig);
  FContentType := AContentTypeManager.GetInfoByExt(FStorageConfig.TileFileExt);
  FCoordConverter := FZmp.GeoConvert;
  FViewCoordConverter := FZmp.ViewGeoConvert;

  if FStorageConfig.UseMemCache then begin
    FCacheTileInfo :=
      TTileInfoBasicMemCache.Create(
        FStorageConfig.MemCacheCapacity,
        FStorageConfig.MemCacheTTL,
        FStorageConfig.MemCacheClearStrategy,
        AGCNotifier,
        VPerfCounterList.CreateAndAddNewSubList('TileInfoInMem')
      );
  end else begin
    FCacheTileInfo := nil;
  end;

  FStorage :=
    TTileStorageOfMapType.Create(
      AGlobalCacheConfig,
      FCoordConverter,
      ATileStorageTypeList,
      FStorageConfig,
      FCacheTileInfo,
      AContentTypeManager,
      VPerfCounterList
    );
  if Supports(FContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
    FBitmapLoaderFromStorage := VContentTypeBitmap.GetLoader;
    FBitmapSaverToStorage := VContentTypeBitmap.GetSaver;
    FCacheBitmap :=
      TMemTileCacheBitmap.Create(
        AGCNotifier,
        FStorage,
        FCoordConverter,
        AMainMemCacheConfig,
        VPerfCounterList.CreateAndAddNewSubList('BmpInMem')
      );
  end else if Supports(FContentType, IContentTypeInfoVectorData, VContentTypeKml) then begin
    FKmlLoaderFromStorage := VContentTypeKml.GetLoader;
    FCacheVector :=
      TMemTileCacheVector.Create(
        AGCNotifier,
        FStorage,
        FCoordConverter,
        AMainMemCacheConfig,
        VPerfCounterList.CreateAndAddNewSubList('VectorInMem')
      );
    FMapDataUrlPrefix := CMapDataInternalURL + GUIDToString(FZmp.GUID) + '/';
    FVectorDataFactory :=
      TVectorDataItemMainInfoFactoryForMap.Create(
        AHashFunction,
        THtmlToHintTextConverterStuped.Create
      );
  end;

  VDownloadResultFactory := TDownloadResultFactory.Create;
  FTileDownloadSubsystem :=
    TTileDownloadSubsystem.Create(
      AGCNotifier,
      AAppClosingNotifier,
      FCoordConverter,
      ACoordConverterFactory,
      ALanguageManager,
      ADownloadConfig,
      AInvisibleBrowser,
      VDownloadResultFactory,
      FZmp.TileDownloaderConfig,
      AResamplerConfigDownload,
      ABitmapFactory,
      FTileDownloaderConfig,
      ADownloaderThreadConfig,
      FTileDownloadRequestBuilderConfig,
      AContentTypeManager,
      FZmp.ContentTypeSubst,
      FContentType,
      FZmp.TilePostDownloadCropConfig,
      FZmp.EmptyTileSamples,
      FZmp.BanTileSamples,
      FAbilitiesConfig,
      FZmp.DataProvider,
      AProjFactory,
      FStorage
    );

  if FZmp.IsLayer then begin
    FLoadPrevMaxZoomDelta := 4;
  end else begin
    FLoadPrevMaxZoomDelta := 6;
  end;
end;

destructor TMapType.Destroy;
begin
  if Assigned(FVersionRequestConfig) and Assigned(FVersionChangeListener) then begin
    FVersionRequestConfig.ChangeNotifier.Remove(FVersionChangeListener);
    FVersionRequestConfig := nil;
    FVersionChangeListener := nil;
  end;
  inherited;
end;

procedure TMapType.SaveConfig(const ALocalConfig: IConfigDataWriteProvider);
begin
  FGUIConfig.WriteConfig(ALocalConfig);
  FLayerDrawConfig.WriteConfig(ALocalConfig);
  FTileDownloadRequestBuilderConfig.WriteConfig(ALocalConfig);
  FTileDownloaderConfig.WriteConfig(ALocalConfig);
  FVersionRequestConfig.WriteConfig(ALocalConfig);
  FStorageConfig.WriteConfig(ALocalConfig);
  FAbilitiesConfig.WriteConfig(ALocalConfig);
end;

function TMapType.LoadBitmapTileFromStorage(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersion: IMapVersionRequest
): IBitmap32Static;
var
  VTileInfoWithData: ITileInfoWithData;
  VContentType: IContentTypeInfoBitmap;
begin
  Result := nil;
  if Supports(FStorage.GetTileInfoEx(AXY, AZoom, AVersion, gtimWithData), ITileInfoWithData, VTileInfoWithData) then begin
    if Supports(VTileInfoWithData.ContentType, IContentTypeInfoBitmap, VContentType) then begin
      Result := VContentType.GetLoader.Load(VTileInfoWithData.TileData);
    end;
  end;
end;

function TMapType.LoadKmlTileFromStorage(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionRequest
): IVectorItemSubset;
var
  VTileInfoWithData: ITileInfoWithData;
  VIdData: TIdData;
  VContentType: IContentTypeInfoVectorData;
begin
  Result := nil;
  if Supports(FStorage.GetTileInfoEx(AXY, AZoom, AVersion, gtimWithData), ITileInfoWithData, VTileInfoWithData) then begin
    if Supports(VTileInfoWithData.ContentType, IContentTypeInfoVectorData, VContentType) then begin
      VIdData.UrlPrefix := TStringProviderForMapTileItem.Create(FMapDataUrlPrefix, AXY, AZoom);
      try
        VIdData.NextIndex := 0;
        Result := VContentType.GetLoader.Load(VTileInfoWithData.TileData, @VIdData, FVectorDataFactory);
      finally
        VIdData.UrlPrefix := nil;
      end;
    end;
  end;
end;

function TMapType.GetShortFolderName: string;
begin
  Result := ExtractFileName(ExtractFileDir(IncludeTrailingPathDelimiter(FStorageConfig.NameInCache)));
end;

function TMapType.GetStorageConfig: ISimpleTileStorageConfig;
begin
  Result := FStorageConfig;
end;

function TMapType.GetTileDownloaderConfig: ITileDownloaderConfig;
begin
  Result := FTileDownloaderConfig;
end;

function TMapType.GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
begin
  Result := FTileDownloadRequestBuilderConfig;
end;

function TMapType.GetTileDownloadSubsystem: ITileDownloadSubsystem;
begin
  Result := FTileDownloadSubsystem;
end;

function TMapType.GetTileShowName(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo
): string;
begin
  Result := FStorage.GetTileFileName(AXY, AZoom, AVersion);
  if not FStorage.StorageTypeAbilities.IsFileCache then begin
    Result :=
      IncludeTrailingPathDelimiter(Result) +
      'z' + IntToStr(AZoom + 1) + PathDelim +
      'x' + IntToStr(AXY.X) + PathDelim +
      'y' + IntToStr(AXY.Y) + FContentType.GetDefaultExt;
  end;
  if FStorage.StorageTypeAbilities.IsVersioned and (AVersion.StoreString <> '') then begin
    Result := Result + PathDelim + 'v' + AVersion.StoreString;
  end;
end;

function TMapType.GetAbilities: IMapAbilitiesConfig;
begin
  Result := FAbilitiesConfig;
end;

function TMapType.GetCacheBitmap: ITileObjCacheBitmap;
begin
  Result := FCacheBitmap;
end;

function TMapType.GetCacheVector: ITileObjCacheVector;
begin
  Result := FCacheVector;
end;

function TMapType.GetContentType: IContentTypeInfoBasic;
begin
  Result := FContentType;
end;

function TMapType.GetGeoConvert: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TMapType.GetGUIConfig: IMapTypeGUIConfig;
begin
  Result := FGUIConfig;
end;

function TMapType.GetGUID: TGUID;
begin
  Result := FZmp.GUID;
end;

function TMapType.GetTileStorage: ITileStorage;
begin
  Result := FStorage;
end;

function TMapType.GetVersionRequestConfig: IMapVersionRequestConfig;
begin
  Result := FVersionRequestConfig;
end;

function TMapType.GetViewGeoConvert: ICoordConverter;
begin
  Result := FViewCoordConverter;
end;

function TMapType.GetZmp: IZmpInfo;
begin
  Result := FZmp;
end;

function TMapType.GetIsBitmapTiles: Boolean;
begin
  Result := FBitmapLoaderFromStorage <> nil;
end;

function TMapType.GetIsKmlTiles: Boolean;
begin
  Result := FKmlLoaderFromStorage <> nil;
end;

function TMapType.GetLayerDrawConfig: ILayerDrawConfig;
begin
  Result := FLayerDrawConfig;
end;

function TMapType.LoadTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionRequest;
  IgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VRect: TRect;
  VSize: TPoint;
  VBitmap: TBitmap32ByStaticBitmap;
  VResampler: TCustomResampler;
begin
  try
    Result := nil;
    if ACache = nil then begin
      Result := LoadBitmapTileFromStorage(AXY, AZoom, AVersion);
    end else begin
      Result := ACache.TryLoadTileFromCache(AXY, AZoom);
      if Result = nil then begin
        Result := LoadBitmapTileFromStorage(AXY, AZoom, AVersion);
        if Result <> nil then begin
          ACache.AddTileToCache(Result, AXY, AZoom);
        end;
      end;
    end;
    if Result <> nil then begin
      VRect := FCoordConverter.TilePos2PixelRect(AXY, AZoom);
      VSize := Types.Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
      if (Result.Size.X <> VSize.X) or
        (Result.Size.Y <> VSize.Y) then begin
        VResampler := FResamplerConfigLoad.GetActiveFactory.CreateResampler;
        try
          VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
          try
            VBitmap.SetSize(VSize.X, VSize.Y);
            StretchTransferFull(
              VBitmap,
              VBitmap.BoundsRect,
              Result,
              VResampler,
              dmOpaque
            );
            Result := VBitmap.MakeAndClear;
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
  const AZoom: byte;
  const AVersion: IMapVersionRequest;
  IgnoreError: Boolean;
  const ACache: ITileObjCacheVector
): IVectorItemSubset;
begin
  Result := nil;
  try
    if ACache = nil then begin
      Result := LoadKmlTileFromStorage(AXY, AZoom, AVersion);
    end else begin
      Result := ACache.TryLoadTileFromCache(AXY, AZoom);
      if Result = nil then begin
        Result := LoadKmlTileFromStorage(AXY, AZoom, AVersion);
        if Result <> nil then begin
          ACache.AddTileToCache(Result, AXY, AZoom);
        end;
      end;
    end;
  except
    if not IgnoreError then begin
      raise;
    end;
  end;
end;

procedure TMapType.OnVersionChange;
begin
  ClearMemCache;
end;

function TMapType.LoadTileFromPreZ(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionRequest;
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
  VBitmap: TBitmap32ByStaticBitmap;
  VResampler: TCustomResampler;
begin
  Result := nil;
  VRelative := FCoordConverter.TilePos2Relative(AXY, AZoom);
  VMinZoom := AZoom - FLoadPrevMaxZoomDelta;
  if VMinZoom < 0 then begin
    VMinZoom := 0;
  end;
  if AZoom - 1 > VMinZoom then begin
    for i := AZoom - 1 downto VMinZoom do begin
      VParentZoom := i;
      VTileParent := PointFromDoublePoint(FCoordConverter.Relative2TilePosFloat(VRelative, i), prToTopLeft);
      VBmp := LoadTile(VTileParent, VParentZoom, AVersion, IgnoreError, ACache);
      if VBmp <> nil then begin
        VTargetTilePixelRect := FCoordConverter.TilePos2PixelRect(AXY, AZoom);
        VRelativeRect := FCoordConverter.PixelRect2RelativeRect(VTargetTilePixelRect, AZoom);
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
        VResampler := FResamplerConfigGetPrev.GetActiveFactory.CreateResampler;
        try
          try
            VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
            try
              VBitmap.SetSize(VTileTargetBounds.Right, VTileTargetBounds.Bottom);
              StretchTransfer(
                VBitmap,
                VTileTargetBounds,
                VBmp,
                VTileSourceBounds,
                VResampler,
                dmOpaque
              );
              Result := VBitmap.MakeAndClear;
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
  const AZoom: byte;
  const AVersion: IMapVersionRequest;
  IgnoreError: Boolean;
  AUsePre: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
begin
  Result := LoadTile(AXY, AZoom, AVersion, IgnoreError, ACache);
  if Result = nil then begin
    if AUsePre then begin
      Result := LoadTileFromPreZ(AXY, AZoom, AVersion, IgnoreError, ACache);
    end;
  end;
end;

function TMapType.LoadBitmap(
  const APixelRectTarget: TRect;
  const AZoom: byte;
  const AVersion: IMapVersionRequest;
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
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := nil;

  VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
  VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

  VPixelRectTarget := APixelRectTarget;
  VZoom := AZoom;
  FCoordConverter.CheckPixelRect(VPixelRectTarget, VZoom);
  VTileRect := FCoordConverter.PixelRect2TileRect(VPixelRectTarget, VZoom);
  if (VTileRect.Left = VTileRect.Right - 1) and
    (VTileRect.Top = VTileRect.Bottom - 1) then begin
    VPixelRectCurrTile := FCoordConverter.TilePos2PixelRect(VTileRect.TopLeft, VZoom);
    if Types.EqualRect(VPixelRectCurrTile, APixelRectTarget) then begin
      Result := LoadTileOrPreZ(VTileRect.TopLeft, VZoom, AVersion, IgnoreError, AUsePre, ACache);
      Exit;
    end;
  end;
  VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
  try
    VBitmap.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
    VBitmap.Clear(0);

    for i := VTileRect.Top to VTileRect.Bottom - 1 do begin
      VTile.Y := i;
      for j := VTileRect.Left to VTileRect.Right - 1 do begin
        VTile.X := j;
        VSpr := LoadTileOrPreZ(VTile, VZoom, AVersion, IgnoreError, AUsePre, ACache);
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
            VSpr,
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
    Result := VBitmap.MakeAndClear;
  finally
    VBitmap.Free;
  end;
end;

function TMapType.LoadBitmapUni(
  const APixelRectTarget: TRect;
  const AZoom: byte;
  const AVersion: IMapVersionRequest;
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
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := nil;

  if FCoordConverter.IsSameConverter(ACoordConverterTarget) then begin
    Result := LoadBitmap(APixelRectTarget, AZoom, AVersion, AUsePre, AAllowPartial, IgnoreError, ACache);
  end else begin
    VZoom := AZoom;
    VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
    VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

    VPixelRectTarget := APixelRectTarget;
    ACoordConverterTarget.CheckPixelRect(VPixelRectTarget, VZoom);
    VLonLatRectTarget := ACoordConverterTarget.PixelRect2LonLatRect(VPixelRectTarget, VZoom);
    FCoordConverter.CheckLonLatRect(VLonLatRectTarget);
    VPixelRectOfTargetPixelRectInSource :=
      RectFromDoubleRect(
        FCoordConverter.LonLatRect2PixelRectFloat(VLonLatRectTarget, VZoom),
        rrToTopLeft
      );
    VTileRectInSource := FCoordConverter.PixelRect2TileRect(VPixelRectOfTargetPixelRectInSource, VZoom);
    VSpr := LoadBitmap(VPixelRectOfTargetPixelRectInSource, VZoom, AVersion, AUsePre, AAllowPartial, IgnoreError, ACache);
    if VSpr <> nil then begin
      VResampler := FResamplerConfigChangeProjection.GetActiveFactory.CreateResampler;
      try
        VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
        try
          VBitmap.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
          VBitmap.Clear(0);
          StretchTransferFull(
            VBitmap,
            VBitmap.BoundsRect,
            VSpr,
            VResampler,
            dmOpaque
          );
          Result := VBitmap.MakeAndClear;
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
  const AZoom: byte;
  const AVersion: IMapVersionRequest;
  const ACoordConverterTarget: ICoordConverter;
  AUsePre, AAllowPartial, IgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VPixelRect: TRect;
begin
  VPixelRect := ACoordConverterTarget.TilePos2PixelRect(AXY, AZoom);
  Result := LoadBitmapUni(VPixelRect, AZoom, AVersion, ACoordConverterTarget, AUsePre, AAllowPartial, IgnoreError, ACache);
end;

end.
