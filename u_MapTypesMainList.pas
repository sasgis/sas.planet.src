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

unit u_MapTypesMainList;

interface

uses
  ActiveX,
  SysUtils,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_HashFunction,
  i_ThreadConfig,
  i_CoordConverterFactory,
  i_ZmpInfoSet,
  i_NotifierTime,
  i_InetConfig,
  i_Bitmap32StaticFactory,
  i_ImageResamplerFactoryChangeable,
  i_GlobalDownloadConfig,
  i_ContentTypeManager,
  i_InvisibleBrowser,
  i_ProjConverter,
  i_MapVersionFactoryList,
  i_MainMemCacheConfig,
  i_MapTypeGUIConfigList,
  i_MapTypes,
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  i_MapTypeSetChangeable,
  i_TileStorageTypeList,
  i_GlobalCacheConfig,
  u_MapTypeSetChangeableSimple;

type
  EMapTypesNoMaps = class(Exception);

  TMapTypesMainList = class
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FGUIConfigList: IMapTypeGUIConfigList;
    FZmpInfoSet: IZmpInfoSet;
    FPerfCounterList: IInternalPerformanceCounterList;

    FTileLoadResampler: IImageResamplerFactoryChangeable;
    FTileGetPrevResampler: IImageResamplerFactoryChangeable;
    FTileReprojectResampler: IImageResamplerFactoryChangeable;
    FTileDownloadResampler: IImageResamplerFactoryChangeable;

    FFullMapsSet: IMapTypeSet;
    FFullMapsSetChangeable: IMapTypeSetChangeable;
    FFullMapsSetChangeableInternal: IMapTypeSetChangeableSimpleInternal;
    FMapsSet: IMapTypeSet;
    FLayersSet: IMapTypeSet;

    procedure BuildMapsLists;
    function GetFirstMainMapGUID: TGUID;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const AZmpInfoSet: IZmpInfoSet;
      const ATileLoadResamplerConfig: IImageResamplerFactoryChangeable;
      const ATileGetPrevResamplerConfig: IImageResamplerFactoryChangeable;
      const ATileReprojectResamplerConfig: IImageResamplerFactoryChangeable;
      const ATileDownloadResamplerConfig: IImageResamplerFactoryChangeable;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    property FullMapsSetChangeable: IMapTypeSetChangeable read FFullMapsSetChangeable;
    property FullMapsSet: IMapTypeSet read FFullMapsSet;
    property MapsSet: IMapTypeSet read FMapsSet;
    property LayersSet: IMapTypeSet read FLayersSet;
    property FirstMainMapGUID: TGUID read GetFirstMainMapGUID;

    procedure LoadMaps(
      const ALanguageManager: ILanguageManager;
      const AMapVersionFactoryList: IMapVersionFactoryList;
      const AMainMemCacheConfig: IMainMemCacheConfig;
      const AGlobalCacheConfig: IGlobalCacheConfig;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const AHashFunction: IHashFunction;
      const AGCNotifier: INotifierTime;
      const AAppClosingNotifier: INotifierOneOperation;
      const AInetConfig: IInetConfig;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloaderThreadConfig: IThreadConfig;
      const ABitmapFactory: IBitmap32BufferFactory;
      const AContentTypeManager: IContentTypeManager;
      const ACoordConverterFactory: ICoordConverterFactory;
      const AInvisibleBrowser: IInvisibleBrowser;
      const AProjFactory: IProjConverterFactory;
      const ALocalMapsConfig: IConfigDataProvider
    );
    procedure SaveMaps(const ALocalMapsConfig: IConfigDataWriteProvider);

    function GetGUIConfigList: IMapTypeGUIConfigList;
    property GUIConfigList: IMapTypeGUIConfigList read GetGUIConfigList;
  end;

implementation

uses
  Dialogs,
  c_ZeroGUID,
  i_GUIDListStatic,
  i_ZmpInfo,
  u_MapTypeGUIConfigList,
  u_MapType,
  u_ResStrings;

{ TMapTypesMainList }

constructor TMapTypesMainList.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AZmpInfoSet: IZmpInfoSet;
  const ATileLoadResamplerConfig: IImageResamplerFactoryChangeable;
  const ATileGetPrevResamplerConfig: IImageResamplerFactoryChangeable;
  const ATileReprojectResamplerConfig: IImageResamplerFactoryChangeable;
  const ATileDownloadResamplerConfig: IImageResamplerFactoryChangeable;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  FZmpInfoSet := AZmpInfoSet;
  FTileLoadResampler := ATileLoadResamplerConfig;
  FTileGetPrevResampler := ATileGetPrevResamplerConfig;
  FTileReprojectResampler := ATileReprojectResamplerConfig;
  FTileDownloadResampler := ATileDownloadResamplerConfig;
  FPerfCounterList := APerfCounterList;
  FFullMapsSetChangeableInternal :=
    TMapTypeSetChangeableSimple.Create(
      AMapTypeSetBuilderFactory,
      nil
    );
  FFullMapsSetChangeable := FFullMapsSetChangeableInternal;
end;

function TMapTypesMainList.GetFirstMainMapGUID: TGUID;
var
  i: integer;
  VGUID: TGUID;
  VGUIDList: IGUIDListStatic;
begin
  Result := CGUID_Zero;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  for i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    if FMapsSet.GetMapTypeByGUID(VGUID) <> nil then begin
      result := VGUID;
      exit;
    end;
  end;
end;

function TMapTypesMainList.GetGUIConfigList: IMapTypeGUIConfigList;
begin
  Result := FGUIConfigList;
end;

procedure TMapTypesMainList.BuildMapsLists;
var
  i: Integer;
  VMapType: IMapType;
  VMapsList: IMapTypeSetBuilder;
  VLayersList: IMapTypeSetBuilder;
begin
  VMapsList := FMapTypeSetBuilderFactory.Build(False);
  VLayersList := FMapTypeSetBuilderFactory.Build(False);
  for i := 0 to FFullMapsSet.Count - 1 do begin
    VMapType := FFullMapsSet.Items[i];
    if VMapType.Zmp.IsLayer then begin
      VLayersList.Add(VMapType);
    end else begin
      VMapsList.Add(VMapType);
    end;
  end;
  FMapsSet := VMapsList.MakeAndClear;
  FLayersSet := VLayersList.MakeAndClear;
  FFullMapsSetChangeableInternal.SetStatic(FFullMapsSet);
end;

procedure TMapTypesMainList.LoadMaps(
  const ALanguageManager: ILanguageManager;
  const AMapVersionFactoryList: IMapVersionFactoryList;
  const AMainMemCacheConfig: IMainMemCacheConfig;
  const AGlobalCacheConfig: IGlobalCacheConfig;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AHashFunction: IHashFunction;
  const AGCNotifier: INotifierTime;
  const AAppClosingNotifier: INotifierOneOperation;
  const AInetConfig: IInetConfig;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloaderThreadConfig: IThreadConfig;
  const ABitmapFactory: IBitmap32BufferFactory;
  const AContentTypeManager: IContentTypeManager;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AInvisibleBrowser: IInvisibleBrowser;
  const AProjFactory: IProjConverterFactory;
  const ALocalMapsConfig: IConfigDataProvider
);
var
  VMapType: IMapType;
  VMapOnlyCount: integer;
  VLocalMapConfig: IConfigDataProvider;
  VMapTypeCount: integer;
  VZmp: IZmpInfo;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VGetCount: Cardinal;
  VGUIDList: IGUIDListStatic;
  i: Integer;
  VFullMapsList: IMapTypeSetBuilder;
begin
  VMapOnlyCount := 0;
  VMapTypeCount := 0;

  VEnum := FZmpInfoSet.GetIterator;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    VZmp := FZmpInfoSet.GetZmpByGUID(VGUID);
    if not VZmp.IsLayer then begin
      Inc(VMapOnlyCount);
    end;
    Inc(VMapTypeCount);
  end;

  if VMapTypeCount = 0 then begin
    raise EMapTypesNoMaps.Create(SAS_ERR_NoMaps);
  end;
  if VMapOnlyCount = 0 then begin
    raise Exception.Create(SAS_ERR_MainMapNotExists);
  end;
  VEnum.Reset;
  VFullMapsList := FMapTypeSetBuilderFactory.Build(False);
  VFullMapsList.Capacity := VMapTypeCount;

  VMapOnlyCount := 0;
  VMapTypeCount := 0;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    try
      VZmp := FZmpInfoSet.GetZmpByGUID(VGUID);
      VLocalMapConfig := ALocalMapsConfig.GetSubItem(GUIDToString(VZmp.GUID));
      VMapType :=
        TMapType.Create(
          ALanguageManager,
          VZmp,
          AMapVersionFactoryList,
          AMainMemCacheConfig,
          AGlobalCacheConfig,
          ATileStorageTypeList,
          AGCNotifier,
          AAppClosingNotifier,
          AInetConfig,
          FTileLoadResampler,
          FTileGetPrevResampler,
          FTileReprojectResampler,
          FTileDownloadResampler,
          ABitmapFactory,
          AHashFunction,
          ADownloadConfig,
          ADownloaderThreadConfig,
          AContentTypeManager,
          ACoordConverterFactory,
          AInvisibleBrowser,
          AProjFactory,
          VLocalMapConfig,
          FPerfCounterList
        );
      VFullMapsList.Add(VMapType);
    except
      if ExceptObject <> nil then begin
        ShowMessage((ExceptObject as Exception).Message);
      end;
      VMapType := nil;
    end;
    if VMapType <> nil then begin
      if not VMapType.Zmp.IsLayer then begin
        Inc(VMapOnlyCount);
      end;
      inc(VMapTypeCount);
    end;
  end;

  if VMapTypeCount = 0 then begin
    raise EMapTypesNoMaps.Create(SAS_ERR_NoMaps);
  end;
  if VMapOnlyCount = 0 then begin
    raise Exception.Create(SAS_ERR_MainMapNotExists);
  end;
  FFullMapsSet := VFullMapsList.MakeAndClear;

  BuildMapsLists;
  FGUIConfigList :=
    TMapTypeGUIConfigList.Create(
      ALanguageManager,
      FFullMapsSet
    );

  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  FGUIConfigList.LockWrite;
  try
    for i := 0 to VGUIDList.Count - 1 do begin
      VGUID := VGUIDList.Items[i];
      VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID);
      VMapType.GUIConfig.SortIndex := i + 1;
    end;
  finally
    FGUIConfigList.UnlockWrite;
  end;
end;

procedure TMapTypesMainList.SaveMaps(
  const ALocalMapsConfig: IConfigDataWriteProvider
);
var
  i: integer;
  VGUIDString: string;
  VMapType: IMapType;
  VSubItem: IConfigDataWriteProvider;
  VGUID: TGUID;
  VGUIDList: IGUIDListStatic;
begin
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  for i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID);
    VGUIDString := GUIDToString(VGUID);
    VSubItem := ALocalMapsConfig.GetOrCreateSubItem(VGUIDString);
    VMapType.SaveConfig(VSubItem);
  end;
end;

end.
