unit u_MapTypesMainList;

interface

uses
  i_IConfigDataProvider,
  UMapType;

type
  TMapTypesMainList = class
  private
    FMapType: array of TMapType;
    function GetMapType(Index: Integer): TMapType;
    function GetCount: Integer;
    function LoadGUID(AConfig : IConfigDataProvider): TGUID;
  public
    destructor Destroy; override;
    property Items[Index : Integer]: TMapType read GetMapType; default;
    property Count: Integer read GetCount;
    procedure SaveMaps;
    procedure LoadMaps;
    function GetMapFromID(id: TGUID): TMapType;
    procedure SortList;
  end;
implementation

uses
  SysUtils,
  IniFiles,
  Dialogs,
  i_IFileNameIterator,
  u_ZmpFileNamesIteratorFactory,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataProviderByFolder,
  u_ConfigDataProviderByKaZip,
  u_GlobalState,
  UResStrings;

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

function TMapTypesMainList.GetMapFromID(id: TGUID): TMapType;
var
  i: integer;
  VMapType: TMapType;
begin
  Result := nil;
  for i := 0 to length(FMapType) - 1 do begin
    VMapType := FMapType[i];
    if IsEqualGUID(VMapType.GUID, id) then begin
      result := VMapType;
      exit;
    end;
  end;
end;

function TMapTypesMainList.GetMapType(Index: Integer): TMapType;
begin
  Result := FMapType[index];
end;

function TMapTypesMainList.LoadGUID(AConfig: IConfigDataProvider): TGUID;
var
  VGUIDStr: String;
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt');
  if VParams <> nil then begin
    VParams := VParams.GetSubItem('PARAMS');
    if VParams <> nil then begin
      VGUIDStr := VParams.ReadString('GUID', '');
      if Length(VGUIDStr) > 0 then begin
        try
          Result := StringToGUID(VGUIDStr);
        except
          raise EBadGUID.CreateResFmt(@SAS_ERR_MapGUIDBad, [VGUIDStr]);
        end;
      end else begin
        raise EBadGUID.CreateRes(@SAS_ERR_MapGUIDEmpty);
      end;
    end else begin
      raise EBadGUID.CreateRes(@SAS_ERR_MapGUIDEmpty);
    end;
  end else begin
    raise EBadGUID.CreateRes(@SAS_ERR_MapGUIDEmpty);
  end;
end;

procedure TMapTypesMainList.LoadMaps;
var
  Ini: TMeminifile;
  VMapType: TMapType;
  VMapTypeLoaded: TMapType;
  VMapOnlyCount: integer;
  VMapConfig: IConfigDataProvider;
  VLocalMapsConfig: IConfigDataProvider;
  VFileName: WideString;
  VFullFileName: string;
  VMapTypeCount: integer;
  VFilesIteratorFactory: IFileNameIteratorFactory;
  VFilesIterator: IFileNameIterator;
  VGUID: TGUID;
begin
  SetLength(FMapType, 0);
  CreateDir(GState.MapsPath);
  Ini := TMeminiFile.Create(GState.MapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataProviderByIniFile.Create(Ini);
  VMapOnlyCount := 0;
  VMapTypeCount := 0;
  VFilesIteratorFactory := TZmpFileNamesIteratorFactory.Create;
  VFilesIterator := VFilesIteratorFactory.CreateIterator(GState.MapsPath, '');
  while VFilesIterator.Next(VFileName) do begin
    VFullFileName := VFilesIterator.GetRootFolderName + VFileName;
    try
      if FileExists(VFullFileName) then begin
        VMapConfig := TConfigDataProviderByKaZip.Create(VFullFileName);
      end else begin
        VMapConfig := TConfigDataProviderByFolder.Create(VFullFileName);
      end;
      try
        VGUID := LoadGUID(VMapConfig);
      except
        on E: EBadGUID do begin
          raise Exception.CreateResFmt(@SAS_ERR_MapGUIDError, [VFileName, E.Message]);
        end;
      end;
      VMapTypeLoaded := GetMapFromID(VGUID);
      if VMapTypeLoaded <> nil then begin
        raise Exception.CreateFmt(SAS_ERR_MapGUIDDuplicate, [VMapTypeLoaded.ZmpFileName, VFullFileName]);
      end;
      VMapType := TMapType.Create(VGUID, VMapConfig, VLocalMapsConfig, VMapTypeCount);
    except
      if ExceptObject <> nil then begin
        ShowMessage((ExceptObject as Exception).Message);
      end;
      VMapType := nil;
    end;
    if VMapType <> nil then begin
      SetLength(FMapType, VMapTypeCount + 1);
      FMapType[VMapTypeCount] := VMapType;
      if not VMapType.asLayer then begin
        Inc(VMapOnlyCount);
      end;
      inc(VMapTypeCount);
    end;
  end;

  if Length(FMapType) = 0 then begin
    raise Exception.Create(SAS_ERR_NoMaps);
  end;
  if VMapOnlyCount = 0 then begin
    raise Exception.Create(SAS_ERR_MainMapNotExists);
  end;
  SortList;
end;

procedure TMapTypesMainList.SaveMaps;
var
  Ini: TMeminifile;
  i: integer;
  VGUIDString: string;
  VMapType: TMapType;
begin
  Ini := TMeminiFile.Create(GState.MapsPath + 'Maps.ini');
  try
    for i := 0 to length(FMapType) - 1 do begin
      VMapType := FMapType[i];
      VGUIDString := VMapType.GUIDString;
      ini.WriteInteger(VGUIDString, 'pnum', VMapType.id);


      if VMapType.UrlGenerator.URLBase <> VMapType.UrlGenerator.DefURLBase then begin
        ini.WriteString(VGUIDString, 'URLBase', VMapType.UrlGenerator.URLBase);
      end else begin
        Ini.DeleteKey(VGUIDString, 'URLBase');
      end;

      if VMapType.HotKey <> VMapType.DefHotKey then begin
        ini.WriteInteger(VGUIDString, 'HotKey', VMapType.HotKey);
      end else begin
        Ini.DeleteKey(VGUIDString, 'HotKey');
      end;

      if VMapType.TileStorage.CacheConfig.cachetype <> VMapType.TileStorage.CacheConfig.defcachetype then begin
        ini.WriteInteger(VGUIDString, 'CacheType', VMapType.TileStorage.CacheConfig.CacheType);
      end else begin
        Ini.DeleteKey(VGUIDString, 'CacheType');
      end;

      if VMapType.separator <> VMapType.Defseparator then begin
        ini.WriteBool(VGUIDString, 'separator', VMapType.separator);
      end else begin
        Ini.DeleteKey(VGUIDString, 'separator');
      end;

      if VMapType.TileStorage.CacheConfig.NameInCache <> VMapType.TileStorage.CacheConfig.DefNameInCache then begin
        ini.WriteString(VGUIDString, 'NameInCache', VMapType.TileStorage.CacheConfig.NameInCache);
      end else begin
        Ini.DeleteKey(VGUIDString, 'NameInCache');
      end;

      if VMapType.DownloaderFactory.WaitInterval <> VMapType.DefSleep then begin
        ini.WriteInteger(VGUIDString, 'Sleep', VMapType.DownloaderFactory.WaitInterval);
      end else begin
        Ini.DeleteKey(VGUIDString, 'Sleep');
      end;

      if VMapType.ParentSubMenu <> VMapType.DefParentSubMenu then begin
        ini.WriteString(VGUIDString, 'ParentSubMenu', VMapType.ParentSubMenu);
      end else begin
        Ini.DeleteKey(VGUIDString, 'ParentSubMenu');
      end;
    end;
    Ini.UpdateFile;
  finally
    ini.Free;
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
      while (j >= 0) and (FMapType[j].id > FMapType[j + k].id) do begin
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
    FMapType[i].id := i + 1;
  end;
end;

end.
