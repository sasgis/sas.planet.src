unit u_MapTypesMainList;

interface

uses
  Classes,
  SysUtils,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_CoordConverterFactory,
  i_MapTypeIconsList,
  i_MemObjCache,
  i_ListOfObjectsWithTTL,
  i_InetConfig,
  i_ImageResamplerConfig,
  i_GlobalDownloadConfig,
  i_ContentTypeManager,
  i_DownloadResultTextProvider,
  i_TileFileNameGeneratorsList,
  i_MapTypes,
  u_GlobalCahceConfig,
  u_MapType;

type
  EMapTypesNoMaps = class(Exception);

  TMapTypesMainList = class
  private
    FMapType: array of TMapType;
    FFullMapsSet: IMapTypeSet;
    FMapsSet: IMapTypeSet;
    FLayersSet: IMapTypeSet;

    FMapTypeIcons18List: IMapTypeIconsList;
    FMapTypeIcons24List: IMapTypeIconsList;

    function GetMapType(Index: Integer): TMapType;
    function GetCount: Integer;
    procedure BuildMapsLists;
    function GetFirstMainMap: TMapType;
  public
    destructor Destroy; override;
    property Items[Index : Integer]: TMapType read GetMapType; default;
    property Count: Integer read GetCount;
    property FullMapsSet: IMapTypeSet read FFullMapsSet;
    property MapsSet: IMapTypeSet read FMapsSet;
    property LayersSet: IMapTypeSet read FLayersSet;
    property FirstMainMap: TMapType read GetFirstMainMap;

    property MapTypeIcons18List: IMapTypeIconsList read FMapTypeIcons18List;
    property MapTypeIcons24List: IMapTypeIconsList read FMapTypeIcons24List;

    procedure SaveMaps(ALocalMapsConfig: IConfigDataWriteProvider);
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
      ALocalMapsConfig: IConfigDataProvider;
      AMapsPath: string
    );
    function GetMapFromID(id: TGUID): TMapType;
    procedure SortList;
    procedure LoadMapIconsList;
    function GetMapTypeByHotKey(AHotKey: TShortCut): TMapType;
  end;

implementation

uses
  Dialogs,
  i_FileNameIterator,
  i_ZmpInfo,
  u_ZmpInfo,
  u_ZmpFileNamesIteratorFactory,
  u_ConfigDataProviderByFolder,
  u_ConfigDataProviderByKaZip,
  u_ConfigDataProviderZmpComplex,
  u_MapTypeBasic,
  u_MapTypeIconsList,
  u_MapTypeList,
  u_ResStrings;

{ TMapTypesMainList }

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

function TMapTypesMainList.GetCount: Integer;
begin
  Result := Length(FMapType);
end;

function TMapTypesMainList.GetFirstMainMap: TMapType;
var
  i: integer;
  VMapType: TMapType;
begin
  Result := nil;
  for i := 0 to length(FMapType) - 1 do begin
    VMapType := FMapType[i];
    if not VMapType.Abilities.IsLayer then begin
      result := VMapType;
      exit;
    end;
  end;
end;

function TMapTypesMainList.GetMapFromID(id: TGUID): TMapType;
var
  i: integer;
  VMapType: TMapType;
begin
  Result := nil;
  for i := 0 to length(FMapType) - 1 do begin
    VMapType := FMapType[i];
    if IsEqualGUID(VMapType.Zmp.GUID, id) then begin
      result := VMapType;
      exit;
    end;
  end;
end;

function TMapTypesMainList.GetMapType(Index: Integer): TMapType;
begin
  Result := FMapType[index];
end;

function TMapTypesMainList.GetMapTypeByHotKey(AHotKey: TShortCut): TMapType;
var
  i: Integer;
  VMap: TMapType;
begin
  Result := nil;
  for i := 0 to Length(FMapType) - 1 do begin
    VMap := FMapType[i];
    if VMap.GUIConfig.HotKey = AHotKey then begin
      Result := VMap;
      Break;
    end;
  end;
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

procedure TMapTypesMainList.LoadMapIconsList;
var
  i: Integer;
  VMapType: TMapType;
  VList18: TMapTypeIconsList;
  VList24: TMapTypeIconsList;
begin
  VList18 := TMapTypeIconsList.Create(18, 18);
  FMapTypeIcons18List := VList18;

  VList24 := TMapTypeIconsList.Create(24, 24);
  FMapTypeIcons24List := VList24;

  for i := 0 to GetCount - 1 do begin
    VMapType := Items[i];
    VList18.Add(VMapType.Zmp.GUID, VMapType.Zmp.GUI.Bmp18);
    VList24.Add(VMapType.Zmp.GUID, VMapType.Zmp.GUI.Bmp24);
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
  ALocalMapsConfig: IConfigDataProvider;
  AMapsPath: string
);
var
  VMapType: TMapType;
  VMapTypeLoaded: TMapType;
  VMapOnlyCount: integer;

  VZmpMapConfig: IConfigDataProvider;
  VLocalMapConfig: IConfigDataProvider;
  VMapConfig: IConfigDataProvider;
  VFileName: WideString;
  VFullFileName: string;
  VMapTypeCount: integer;
  VFilesIteratorFactory: IFileNameIteratorFactory;
  VFilesIterator: IFileNameIterator;
  VZmp: IZmpInfo;
begin
  SetLength(FMapType, 0);
  VMapOnlyCount := 0;
  VMapTypeCount := 0;
  VFilesIteratorFactory := TZmpFileNamesIteratorFactory.Create;
  VFilesIterator := VFilesIteratorFactory.CreateIterator(AMapsPath, '');
  while VFilesIterator.Next(VFileName) do begin
    VFullFileName := VFilesIterator.GetRootFolderName + VFileName;
    try
      if FileExists(VFullFileName) then begin
        VZmpMapConfig := TConfigDataProviderByKaZip.Create(VFullFileName);
      end else begin
        VZmpMapConfig := TConfigDataProviderByFolder.Create(VFullFileName);
      end;
      try
        VZmp := TZmpInfo.Create(
          ALanguageManager,
          ACoordConverterFactory,
          VFileName,
          VZmpMapConfig,
          VMapTypeCount
        );
      except
        on E: EZmpError do begin
          raise Exception.CreateResFmt(@SAS_ERR_MapGUIDError, [VFileName, E.Message]);
        end;
      end;

      VMapTypeLoaded := GetMapFromID(VZmp.GUID);
      if VMapTypeLoaded <> nil then begin
        raise Exception.CreateFmt(SAS_ERR_MapGUIDDuplicate, [VMapTypeLoaded.Zmp.FileName, VFullFileName]);
      end;

      VLocalMapConfig := ALocalMapsConfig.GetSubItem(GUIDToString(VZmp.GUID));
      VMapConfig := TConfigDataProviderZmpComplex.Create(VZmpMapConfig, VLocalMapConfig);
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
          VMapConfig
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

  if Length(FMapType) = 0 then begin
    raise EMapTypesNoMaps.Create(SAS_ERR_NoMaps);
  end;
  if VMapOnlyCount = 0 then begin
    raise Exception.Create(SAS_ERR_MainMapNotExists);
  end;
  SortList;
  BuildMapsLists;
end;

procedure TMapTypesMainList.SaveMaps(ALocalMapsConfig: IConfigDataWriteProvider);
var
  i: integer;
  VGUIDString: string;
  VMapType: TMapType;
  VSubItem: IConfigDataWriteProvider;
begin
  for i := 0 to length(FMapType) - 1 do begin
    VMapType := FMapType[i];
    VGUIDString := GUIDToString(VMapType.Zmp.GUID);
    VSubItem := ALocalMapsConfig.GetOrCreateSubItem(VGUIDString);
    VMapType.SaveConfig(VSubItem);
  end;
end;

procedure TMapTypesMainList.SortList;
var
  i, j, k: integer;
  MTb: TMapType;
begin
  k := length(FMapType) shr 1;
  while k > 0 do begin
    for i := 0 to length(FMapType) - k - 1 do begin
      j := i;
      while (j >= 0) and (FMapType[j].GUIConfig.SortIndex > FMapType[j + k].GUIConfig.SortIndex) do begin
        MTb := FMapType[j];
        FMapType[j] := FMapType[j + k];
        FMapType[j + k] := MTb;
        if j > k then begin
          Dec(j, k);
        end else begin
          j := 0;
        end;
      end;
    end;
    k := k shr 1;
  end;
  for i := 0 to length(FMapType) - 1 do begin
    FMapType[i].GUIConfig.SortIndex := i + 1;
  end;
end;

end.
