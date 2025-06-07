{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MapTypeProxy;

interface

uses
  SysUtils,
  Types,
  t_GeoTypes,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ThreadConfig,
  i_ContentTypeInfo,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_NotifierOperation,
  i_LayerDrawConfig,
  i_TileObjCache,
  i_TileDownloaderConfig,
  i_LanguageManager,
  i_Projection,
  i_ProjectionSet,
  i_LocalCoordConverter,
  i_MapVersionFactory,
  i_MapVersionRequest,
  i_MapVersionRequestConfig,
  i_MapVersionRequestChangeable,
  i_MapVersionFactoryList,
  i_TileDownloadRequestBuilderConfig,
  i_HashFunction,
  i_BitmapTileSaveLoad,
  i_VectorDataLoader,
  i_NotifierTime,
  i_InetConfig,
  i_ImageResamplerFactoryChangeable,
  i_ContentTypeManager,
  i_GlobalDownloadConfig,
  i_DownloaderFactory,
  i_MapAbilitiesConfig,
  i_Listener,
  i_MapVersionInfo,
  i_SimpleTileStorageConfig,
  i_ZmpInfo,
  i_InvisibleBrowser,
  i_MapTypeGUIConfig,
  i_ProjectionSetFactory,
  i_MainMemCacheConfig,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  i_ProjConverter,
  i_TileDownloadSubsystem,
  i_InternalPerformanceCounter,
  i_TileStorageTypeList,
  i_TileInfoBasicMemCache,
  i_GlobalCacheConfig,
  i_TileStorage,
  i_MapType,
  u_BaseInterfacedObject;

type
  TMapTypeProxy = class(TBaseInterfacedObject, IMapType, IMapTypeProxy)
  protected
    class var FLock: IReadWriteSync;
  private
    FLanguageManager: ILanguageManager;
    FMapVersionFactoryList: IMapVersionFactoryList;
    FMainMemCacheConfig: IMainMemCacheConfig;
    FGlobalCacheConfig: IGlobalCacheConfig;
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FGCNotifier: INotifierTime;
    FAppClosingNotifier: INotifierOneOperation;
    FInetConfig: IInetConfig;
    FResamplerLoad: IImageResamplerFactoryChangeable;
    FResamplerGetPrev: IImageResamplerFactoryChangeable;
    FResamplerChangeProjection: IImageResamplerFactoryChangeable;
    FResamplerDownload: IImageResamplerFactoryChangeable;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FHashFunction: IHashFunction;
    FDownloadConfig: IGlobalDownloadConfig;
    FDownloaderThreadConfig: IThreadConfig;
    FDownloaderFactory: IDownloaderFactory;
    FContentTypeManager: IContentTypeManager;
    FProjectionSetFactory: IProjectionSetFactory;
    FInvisibleBrowser: IInvisibleBrowser;
    FProjFactory: IProjConverterFactory;
    FLocalMapConfig: IConfigDataProvider;
    FPerfCounterList: IInternalPerformanceCounterList;

    FZmpProxy: IZmpInfoProxy;
    FGuiConfigProxy: IMapTypeGUIConfigProxy;
    FAbilitiesConfig: IMapAbilitiesConfig;

    FMapType: IMapType;
    function GetMapType: IMapType;
    procedure SetMapType(const AValue: IMapType);
  private
    { IMapType }
    procedure SaveConfig(const ALocalConfig: IConfigDataWriteProvider);

    function GetGUID: TGUID;

    procedure ClearMemCache;

    function GetTileShowName(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionInfo
    ): string;

    function LoadTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionRequest;
      const AIgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;

    function LoadTileVector(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionRequest;
      const AUsePre, AIgnoreError: Boolean;
      const ACache: ITileObjCacheVector = nil
    ): IVectorItemSubset;

    function LoadTileUni(
      const AXY: TPoint;
      const AProjection: IProjection;
      const AVersion: IMapVersionRequest;
      const AUsePre, AAllowPartial, AIgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;

    function LoadBitmap(
      const APixelRectTarget: TRect;
      const AZoom: Byte;
      const AVersion: IMapVersionRequest;
      const AUsePre, AAllowPartial, AIgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;

    function LoadBitmapUni(
      const APixelRectTarget: TRect;
      const AProjection: IProjection;
      const AVersion: IMapVersionRequest;
      const AUsePre, AAllowPartial, AIgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;

    function GetShortFolderName: string;
    procedure NextVersion(const AView: ILocalCoordConverter; AStep: Integer);

    function GetZmp: IZmpInfo;
    function GetProjectionSet: IProjectionSet;
    function GetViewProjectionSet: IProjectionSet;
    function GetVersionFactory: IMapVersionFactoryChangeable;
    function GetVersionRequestConfig: IMapVersionRequestConfig;
    function GetVersionRequest: IMapVersionRequestChangeable;
    function GetContentType: IContentTypeInfoBasic;
    function GetAbilities: IMapAbilitiesConfig;
    function GetStorageConfig: ISimpleTileStorageConfig;
    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
    function GetTileDownloadSubsystem: ITileDownloadSubsystem;
    function GetTileStorage: ITileStorage;
    function GetGUIConfig: IMapTypeGUIConfig;
    function GetLayerDrawConfig: ILayerDrawConfig;
    function GetTileDownloaderConfig: ITileDownloaderConfig;
    function GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    function GetCacheBitmap: ITileObjCacheBitmap;
    function GetCacheVector: ITileObjCacheVector;
    function GetCacheTileInfo: ITileInfoBasicMemCache;
  private
    { IMapTypeProxy }
    procedure Initialize(const AZmpMapConfig: IConfigDataProvider);
    procedure Reset;

    function GetIsInitialized: Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AZmp: IZmpInfoProxy;
      const AMapVersionFactoryList: IMapVersionFactoryList;
      const AMainMemCacheConfig: IMainMemCacheConfig;
      const AGlobalCacheConfig: IGlobalCacheConfig;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const AGCNotifier: INotifierTime;
      const AAppClosingNotifier: INotifierOneOperation;
      const AInetConfig: IInetConfig;
      const AResamplerLoad: IImageResamplerFactoryChangeable;
      const AResamplerGetPrev: IImageResamplerFactoryChangeable;
      const AResamplerChangeProjection: IImageResamplerFactoryChangeable;
      const AResamplerDownload: IImageResamplerFactoryChangeable;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AHashFunction: IHashFunction;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloaderThreadConfig: IThreadConfig;
      const ADownloaderFactory: IDownloaderFactory;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AInvisibleBrowser: IInvisibleBrowser;
      const AProjFactory: IProjConverterFactory;
      const AConfig: IConfigDataProvider;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  Math,
  GR32,
  c_InternalBrowser,
  i_TileInfoBasic,
  i_TileStorageAbilities,
  i_MapVersionListStatic,
  i_ImportConfig,
  i_DownloadResultFactory,
  u_StringProviderForMapTileItem,
  u_LayerDrawConfig,
  u_TileIteratorByRect,
  u_TileDownloaderConfig,
  u_TileDownloadRequestBuilderConfig,
  u_DownloadResultFactory,
  u_MemTileCache,
  u_SimpleTileStorageConfig,
  u_MapAbilitiesConfig,
  u_VectorDataFactoryForMap,
  u_VectorItemSubsetBuilder,
  u_HtmlToHintTextConverterStuped,
  u_MapTypeGuiConfigProxy,
  u_MapVersionFactoryChangeable,
  u_MapVersionRequestConfig,
  u_MapVersionRequestChangeable,
  u_TileDownloadSubsystem,
  u_Bitmap32ByStaticBitmap,
  u_GeoFunc,
  u_BitmapFunc,
  u_TileStorageOfMapType,
  u_TileInfoBasicMemCache,
  u_ListenerByEvent,
  u_MapType,
  u_Synchronizer;

constructor TMapTypeProxy.Create(
  const ALanguageManager: ILanguageManager;
  const AZmp: IZmpInfoProxy;
  const AMapVersionFactoryList: IMapVersionFactoryList;
  const AMainMemCacheConfig: IMainMemCacheConfig;
  const AGlobalCacheConfig: IGlobalCacheConfig;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AGCNotifier: INotifierTime;
  const AAppClosingNotifier: INotifierOneOperation;
  const AInetConfig: IInetConfig;
  const AResamplerLoad: IImageResamplerFactoryChangeable;
  const AResamplerGetPrev: IImageResamplerFactoryChangeable;
  const AResamplerChangeProjection: IImageResamplerFactoryChangeable;
  const AResamplerDownload: IImageResamplerFactoryChangeable;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AHashFunction: IHashFunction;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloaderThreadConfig: IThreadConfig;
  const ADownloaderFactory: IDownloaderFactory;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AInvisibleBrowser: IInvisibleBrowser;
  const AProjFactory: IProjConverterFactory;
  const AConfig: IConfigDataProvider;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;

  FLanguageManager := ALanguageManager;
  FMapVersionFactoryList := AMapVersionFactoryList;
  FMainMemCacheConfig := AMainMemCacheConfig;
  FGlobalCacheConfig := AGlobalCacheConfig;
  FTileStorageTypeList := ATileStorageTypeList;
  FGCNotifier := AGCNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FInetConfig := AInetConfig;
  FResamplerLoad := AResamplerLoad;
  FResamplerGetPrev := AResamplerGetPrev;
  FResamplerChangeProjection := AResamplerChangeProjection;
  FResamplerDownload := AResamplerDownload;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FHashFunction := AHashFunction;
  FDownloadConfig := ADownloadConfig;
  FDownloaderThreadConfig := ADownloaderThreadConfig;
  FDownloaderFactory := ADownloaderFactory;
  FContentTypeManager := AContentTypeManager;
  FProjectionSetFactory := AProjectionSetFactory;
  FInvisibleBrowser := AInvisibleBrowser;
  FProjFactory := AProjFactory;
  FLocalMapConfig := AConfig;
  FPerfCounterList := APerfCounterList;

  FZmpProxy := AZmp;
  FGuiConfigProxy := TMapTypeGUIConfigProxy.Create(ALanguageManager, FZmpProxy.GUI as IZmpInfoGuiProxy);
  FAbilitiesConfig := TMapAbilitiesConfig.Create(FZmpProxy.Abilities);
end;

procedure TMapTypeProxy.Initialize(const AZmpMapConfig: IConfigDataProvider);
var
  VMapType: IMapType;
begin
  FZmpProxy.Initialize(AZmpMapConfig);

  VMapType :=
    TMapType.Create(
      FLanguageManager,
      FZmpProxy,
      FMapVersionFactoryList,
      FMainMemCacheConfig,
      FGlobalCacheConfig,
      FTileStorageTypeList,
      FGCNotifier,
      FAppClosingNotifier,
      FInetConfig,
      FResamplerLoad,
      FResamplerGetPrev,
      FResamplerChangeProjection,
      FResamplerDownload,
      FBitmap32StaticFactory,
      FHashFunction,
      FDownloadConfig,
      FDownloaderThreadConfig,
      FDownloaderFactory,
      FContentTypeManager,
      FProjectionSetFactory,
      FInvisibleBrowser,
      FProjFactory,
      FLocalMapConfig,
      FPerfCounterList
    );
  SetMapType(VMapType);

  FGuiConfigProxy.Initialize;
end;

procedure TMapTypeProxy.Reset;
begin
  SetMapType(nil);
  FZmpProxy.Reset;
  FGuiConfigProxy.Reset;
end;

function TMapTypeProxy.GetIsInitialized: Boolean;
begin
  Result := GetMapType <> nil;
end;

procedure TMapTypeProxy.ClearMemCache;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    VMapType.ClearMemCache;
  end;
end;

procedure TMapTypeProxy.SaveConfig(const ALocalConfig: IConfigDataWriteProvider);
begin
  // nothing to do
end;

function TMapTypeProxy.GetShortFolderName: string;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.GetShortFolderName;
    Exit;
  end;

  Result := '';
end;

function TMapTypeProxy.GetStorageConfig: ISimpleTileStorageConfig;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.StorageConfig;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetTileDownloaderConfig: ITileDownloaderConfig;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.TileDownloaderConfig;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.TileDownloadRequestBuilderConfig;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetTileDownloadSubsystem: ITileDownloadSubsystem;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.TileDownloadSubsystem;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetTileShowName(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersion: IMapVersionInfo
): string;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.GetTileShowName(AXY, AZoom, AVersion);
    Exit;
  end;

  Result := '';
end;

function TMapTypeProxy.GetAbilities: IMapAbilitiesConfig;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.Abilities;
    Exit;
  end;

  Result := FAbilitiesConfig;
end;

function TMapTypeProxy.GetCacheBitmap: ITileObjCacheBitmap;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.CacheBitmap;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetCacheVector: ITileObjCacheVector;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.CacheVector;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetCacheTileInfo: ITileInfoBasicMemCache;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.CacheTileInfo;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetContentType: IContentTypeInfoBasic;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.ContentType;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetProjectionSet: IProjectionSet;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.ProjectionSet;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetGUIConfig: IMapTypeGUIConfig;
begin
  Result := FGuiConfigProxy;
end;

function TMapTypeProxy.GetGUID: TGUID;
begin
  Result := FZmpProxy.GUID;
end;

function TMapTypeProxy.GetTileStorage: ITileStorage;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.TileStorage;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetVersionFactory: IMapVersionFactoryChangeable;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.VersionFactory;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetVersionRequest: IMapVersionRequestChangeable;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.VersionRequest;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetVersionRequestConfig: IMapVersionRequestConfig;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.VersionRequestConfig;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetViewProjectionSet: IProjectionSet;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.ViewProjectionSet;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetZmp: IZmpInfo;
begin
  Result := FZmpProxy;
end;

function TMapTypeProxy.GetIsBitmapTiles: Boolean;
begin
  Result := FZmpProxy.GetIsBitmapTiles;
end;

function TMapTypeProxy.GetIsKmlTiles: Boolean;
begin
  Result := FZmpProxy.GetIsKmlTiles;
end;

function TMapTypeProxy.GetLayerDrawConfig: ILayerDrawConfig;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.LayerDrawConfig;
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.LoadTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersion: IMapVersionRequest;
  const AIgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.LoadTile(AXY, AZoom, AVersion, AIgnoreError, ACache);
    Exit;
  end;

  Result := nil;
end;

procedure TMapTypeProxy.NextVersion(const AView: ILocalCoordConverter; AStep: Integer);
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    VMapType.NextVersion(AView, AStep);
  end;
end;

function TMapTypeProxy.LoadBitmap(
  const APixelRectTarget: TRect;
  const AZoom: Byte;
  const AVersion: IMapVersionRequest;
  const AUsePre, AAllowPartial, AIgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.LoadBitmap(APixelRectTarget, AZoom, AVersion, AUsePre, AAllowPartial, AIgnoreError, ACache);
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.LoadBitmapUni(
  const APixelRectTarget: TRect;
  const AProjection: IProjection;
  const AVersion: IMapVersionRequest;
  const AUsePre, AAllowPartial, AIgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.LoadBitmapUni(APixelRectTarget, AProjection, AVersion, AUsePre, AAllowPartial, AIgnoreError, ACache);
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.LoadTileUni(
  const AXY: TPoint;
  const AProjection: IProjection;
  const AVersion: IMapVersionRequest;
  const AUsePre, AAllowPartial, AIgnoreError: Boolean;
  const ACache: ITileObjCacheBitmap
): IBitmap32Static;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.LoadTileUni(AXY, AProjection, AVersion, AUsePre, AAllowPartial, AIgnoreError, ACache);
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.LoadTileVector(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersion: IMapVersionRequest;
  const AUsePre, AIgnoreError: Boolean;
  const ACache: ITileObjCacheVector
): IVectorItemSubset;
var
  VMapType: IMapType;
begin
  VMapType := GetMapType;
  if VMapType <> nil then begin
    Result := VMapType.LoadTileVector(AXY, AZoom, AVersion, AUsePre, AIgnoreError, ACache);
    Exit;
  end;

  Result := nil;
end;

function TMapTypeProxy.GetMapType: IMapType;
begin
  FLock.BeginRead;
  try
    Result := FMapType;
  finally
    FLock.EndRead;
  end;
end;

procedure TMapTypeProxy.SetMapType(const AValue: IMapType);
begin
  FLock.BeginWrite;
  try
    FMapType := AValue;
  finally
    FLock.EndWrite;
  end;
end;

initialization
  TMapTypeProxy.FLock := GSync.SyncVariable.Make('TMapTypeProxy');

finalization
  TMapTypeProxy.FLock := nil;

end.
