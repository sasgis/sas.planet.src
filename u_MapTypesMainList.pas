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

unit u_MapTypesMainList;

interface

uses
  ActiveX,
  SysUtils,
  i_JclNotify,
  i_InternalPerformanceCounter,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_ThreadConfig,
  i_CoordConverterFactory,
  i_ZmpInfoSet,
  i_TTLCheckNotifier,
  i_InetConfig,
  i_ImageResamplerConfig,
  i_GlobalDownloadConfig,
  i_ContentTypeManager,
  i_InvisibleBrowser,
  i_DownloadResultTextProvider,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_ProjConverter,
  i_MainMemCacheConfig,
  i_MapTypeGUIConfigList,
  i_MapTypes,
  u_GlobalCahceConfig,
  u_MapType;

type
  EMapTypesNoMaps = class(Exception);

  TMapTypesMainList = class
  private
    FGUIConfigList: IMapTypeGUIConfigList;
    FZmpInfoSet: IZmpInfoSet;
    FPerfCounterList: IInternalPerformanceCounterList;

    FMapType: array of TMapType;
    FFullMapsSet: IMapTypeSet;
    FMapsSet: IMapTypeSet;
    FLayersSet: IMapTypeSet;

    procedure BuildMapsLists;
    function GetFirstMainMapGUID: TGUID;
  public
    constructor Create(
      const AZmpInfoSet: IZmpInfoSet;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
    property FullMapsSet: IMapTypeSet read FFullMapsSet;
    property MapsSet: IMapTypeSet read FMapsSet;
    property LayersSet: IMapTypeSet read FLayersSet;
    property FirstMainMapGUID: TGUID read GetFirstMainMapGUID;

    procedure LoadMaps(
      const ALanguageManager: ILanguageManager;
      const AMainMemCacheConfig: IMainMemCacheConfig;
      const AGlobalCacheConfig: TGlobalCahceConfig;
      const ATileNameGeneratorList: ITileFileNameGeneratorsList;
      const ATileNameParserList: ITileFileNameParsersList;
      const AGCList: ITTLCheckNotifier;
      const AAppClosingNotifier: IJclNotifier;
      const AInetConfig: IInetConfig;
      const AImageResamplerConfig: IImageResamplerConfig;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloaderThreadConfig: IThreadConfig;
      const AContentTypeManager: IContentTypeManager;
      const ADownloadResultTextProvider: IDownloadResultTextProvider;
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
  u_MapTypeBasic,
  u_MapTypeSet,
  u_ResStrings;

{ TMapTypesMainList }

constructor TMapTypesMainList.Create(
  const AZmpInfoSet: IZmpInfoSet;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FZmpInfoSet := AZmpInfoSet;
  FPerfCounterList := APerfCounterList;
end;

destructor TMapTypesMainList.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FMapType) - 1 do begin
    FreeAndNil(FMapType[i]);
  end;
  FMapType := nil;
  inherited;
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
  VMap: TMapType;
  VMapType: IMapType;
  VFullMapsList: TMapTypeSet;
  VMapsList: TMapTypeSet;
  VLayersList: TMapTypeSet;
begin
  VFullMapsList := TMapTypeSet.Create(False);
  FFullMapsSet := VFullMapsList;
  VMapsList := TMapTypeSet.Create(False);
  FMapsSet := VMapsList;
  VLayersList := TMapTypeSet.Create(False);
  FLayersSet := VLayersList;
  for i := 0 to Length(FMapType) - 1 do begin
    VMap := FMapType[i];
    VMapType := TMapTypeBasic.Create(VMap);
    VFullMapsList.Add(VMapType);
    if VMap.Abilities.IsLayer then begin
      VLayersList.Add(VMapType);
    end else begin
      VMapsList.Add(VMapType);
    end;
  end;
end;

procedure TMapTypesMainList.LoadMaps(
  const ALanguageManager: ILanguageManager;
  const AMainMemCacheConfig: IMainMemCacheConfig;
  const AGlobalCacheConfig: TGlobalCahceConfig;
  const ATileNameGeneratorList: ITileFileNameGeneratorsList;
  const ATileNameParserList: ITileFileNameParsersList;
  const AGCList: ITTLCheckNotifier;
  const AAppClosingNotifier: IJclNotifier;
  const AInetConfig: IInetConfig;
  const AImageResamplerConfig: IImageResamplerConfig;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloaderThreadConfig: IThreadConfig;
  const AContentTypeManager: IContentTypeManager;
  const ADownloadResultTextProvider: IDownloadResultTextProvider;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AInvisibleBrowser: IInvisibleBrowser;
  const AProjFactory: IProjConverterFactory;
  const ALocalMapsConfig: IConfigDataProvider
);
var
  VMapType: TMapType;
  VMapOnlyCount: integer;
  VLocalMapConfig: IConfigDataProvider;
  VMapTypeCount: integer;
  VZmp: IZmpInfo;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VGetCount: Cardinal;
  VGUIDList: IGUIDListStatic;
  i: Integer;
begin
  SetLength(FMapType, 0);
  VMapOnlyCount := 0;
  VMapTypeCount := 0;

  VEnum := FZmpInfoSet.GetIterator;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    VZmp := FZmpInfoSet.GetZmpByGUID(VGUID);
    if not VZmp.Abilities.IsLayer then begin
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
          AMainMemCacheConfig,
          AGlobalCacheConfig,
          ATileNameGeneratorList,
          ATileNameParserList,
          AGCList,
          AAppClosingNotifier,
          AInetConfig,
          AImageResamplerConfig,
          ADownloadConfig,
          ADownloaderThreadConfig,
          AContentTypeManager,
          ACoordConverterFactory,
          ADownloadResultTextProvider,
          AInvisibleBrowser,
          AProjFactory,
          VLocalMapConfig,
          FPerfCounterList
        );
    except
      if ExceptObject <> nil then begin
        ShowMessage((ExceptObject as Exception).Message);
      end;
      VMapType := nil;
    end;
    if VMapType <> nil then begin
      SetLength(FMapType, VMapTypeCount + 1);
      FMapType[VMapTypeCount] := VMapType;
      if not VMapType.Abilities.IsLayer then begin
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
      VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
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
  VMapType: TMapType;
  VSubItem: IConfigDataWriteProvider;
  VGUID: TGUID;
  VGUIDList: IGUIDListStatic;
begin
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  for i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    VGUIDString := GUIDToString(VGUID);
    VSubItem := ALocalMapsConfig.GetOrCreateSubItem(VGUIDString);
    VMapType.SaveConfig(VSubItem);
  end;
end;

end.
