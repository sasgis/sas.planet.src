unit u_MapTypesMainList;

interface

uses
  ActiveX,
  Classes,
  SysUtils,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_CoordConverterFactory,
  i_MapTypeIconsList,
  i_ZmpInfoSet,
  i_MemObjCache,
  i_ListOfObjectsWithTTL,
  i_InetConfig,
  i_ImageResamplerConfig,
  i_GlobalDownloadConfig,
  i_ContentTypeManager,
  i_DownloadResultTextProvider,
  i_TileFileNameGeneratorsList,
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

    FMapType: array of TMapType;
    FFullMapsSet: IMapTypeSet;
    FMapsSet: IMapTypeSet;
    FLayersSet: IMapTypeSet;

    procedure BuildMapsLists;
    function GetFirstMainMapGUID: TGUID;
  public
    constructor Create(
      AZmpInfoSet: IZmpInfoSet
    );
    destructor Destroy; override;
    property FullMapsSet: IMapTypeSet read FFullMapsSet;
    property MapsSet: IMapTypeSet read FMapsSet;
    property LayersSet: IMapTypeSet read FLayersSet;
    property FirstMainMapGUID: TGUID read GetFirstMainMapGUID;

    procedure LoadMaps(
      ALanguageManager: ILanguageManager;
      AMemCacheBitmap: IMemObjCacheBitmap;
      AMemCacheVector: IMemObjCacheVector;
      AGlobalCacheConfig: TGlobalCahceConfig;
      ATileNameGeneratorList: ITileFileNameGeneratorsList;
      AGCList: IListOfObjectsWithTTL;
      AInetConfig: IInetConfig;
      AImageResamplerConfig: IImageResamplerConfig;
      ADownloadConfig: IGlobalDownloadConfig;
      AContentTypeManager: IContentTypeManager;
      ADownloadResultTextProvider: IDownloadResultTextProvider;
      ACoordConverterFactory: ICoordConverterFactory;
      ALocalMapsConfig: IConfigDataProvider
    );
    procedure SaveMaps(ALocalMapsConfig: IConfigDataWriteProvider);

    function GetGUIConfigList: IMapTypeGUIConfigList;
    property GUIConfigList: IMapTypeGUIConfigList read GetGUIConfigList;
  end;

implementation

uses
  Dialogs,
  c_ZeroGUID,
  i_FileNameIterator,
  i_GUIDListStatic,
  i_ZmpInfo,
  u_ZmpInfo,
  u_ZmpFileNamesIteratorFactory,
  u_ConfigDataProviderByFolder,
  u_ConfigDataProviderByKaZip,
  u_ConfigDataProviderZmpComplex,
  u_MapTypeGUIConfigList,
  u_MapTypeBasic,
  u_MapTypeIconsList,
  u_MapTypeSet,
  u_ResStrings;

{ TMapTypesMainList }

constructor TMapTypesMainList.Create(AZmpInfoSet: IZmpInfoSet);
begin
  FZmpInfoSet := AZmpInfoSet;
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
  ALanguageManager: ILanguageManager;
  AMemCacheBitmap: IMemObjCacheBitmap;
  AMemCacheVector: IMemObjCacheVector;
  AGlobalCacheConfig: TGlobalCahceConfig;
  ATileNameGeneratorList: ITileFileNameGeneratorsList;
  AGCList: IListOfObjectsWithTTL;
  AInetConfig: IInetConfig;
  AImageResamplerConfig: IImageResamplerConfig;
  ADownloadConfig: IGlobalDownloadConfig;
  AContentTypeManager: IContentTypeManager;
  ADownloadResultTextProvider: IDownloadResultTextProvider;
  ACoordConverterFactory: ICoordConverterFactory;
  ALocalMapsConfig: IConfigDataProvider
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
          AMemCacheBitmap,
          AMemCacheVector,
          AGlobalCacheConfig,
          ATileNameGeneratorList,
          AGCList,
          AInetConfig,
          AImageResamplerConfig,
          ADownloadConfig,
          AContentTypeManager,
          ACoordConverterFactory,
          ADownloadResultTextProvider,
          VLocalMapConfig
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
  FGUIConfigList := TMapTypeGUIConfigList.Create(FFullMapsSet);

  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  FGUIConfigList.LockWrite;
  try
    for i := 0 to VGUIDList.Count - 1 do begin
      VGUID := VGUIDList.Items[i];
      VMapType :=FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
      VMapType.GUIConfig.SortIndex := i + 1;
    end;
  finally
    FGUIConfigList.UnlockWrite;
  end;
end;

procedure TMapTypesMainList.SaveMaps(ALocalMapsConfig: IConfigDataWriteProvider);
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
    VMapType :=FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    VGUIDString := GUIDToString(VGUID);
    VSubItem := ALocalMapsConfig.GetOrCreateSubItem(VGUIDString);
    VMapType.SaveConfig(VSubItem);
  end;
end;

end.
