unit u_MapTypesMainList;

interface

uses
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapTypes,
  UMapType;

type
  TMapTypesMainList = class
  private
    FMapType: array of TMapType;
    FMapsList: IMapTypeList;
    function GetMapType(Index: Integer): TMapType;
    function GetCount: Integer;
    function LoadGUID(AConfig : IConfigDataProvider): TGUID;
  public
    destructor Destroy; override;
    property Items[Index : Integer]: TMapType read GetMapType; default;
    property Count: Integer read GetCount;
    property MapsList: IMapTypeList read FMapsList;
    procedure SaveMaps(ALocalMapsConfig: IConfigDataWriteProvider);
    procedure LoadMaps(ALocalMapsConfig: IConfigDataProvider; AMapsPath: string);
    function GetMapFromID(id: TGUID): TMapType;
    procedure SortList;
  end;
implementation

uses
  SysUtils,
  Dialogs,
  i_IFileNameIterator,
  u_ZmpFileNamesIteratorFactory,
  u_ConfigDataProviderByFolder,
  u_ConfigDataProviderByKaZip,
  u_ConfigDataProviderZmpComplex,
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

procedure TMapTypesMainList.LoadMaps(ALocalMapsConfig: IConfigDataProvider; AMapsPath: string);
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
  VGUID: TGUID;
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
        VGUID := LoadGUID(VZmpMapConfig);
      except
        on E: EBadGUID do begin
          raise Exception.CreateResFmt(@SAS_ERR_MapGUIDError, [VFileName, E.Message]);
        end;
      end;
      VMapTypeLoaded := GetMapFromID(VGUID);
      if VMapTypeLoaded <> nil then begin
        raise Exception.CreateFmt(SAS_ERR_MapGUIDDuplicate, [VMapTypeLoaded.ZmpFileName, VFullFileName]);
      end;

      VLocalMapConfig := ALocalMapsConfig.GetSubItem(GUIDToString(VGUID));
      VMapConfig := TConfigDataProviderZmpComplex.Create(VZmpMapConfig, VLocalMapConfig);
      VMapType := TMapType.Create(VGUID, VMapConfig, VMapTypeCount);
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

procedure TMapTypesMainList.SaveMaps(ALocalMapsConfig: IConfigDataWriteProvider);
var
  i: integer;
  VGUIDString: string;
  VMapType: TMapType;
  VSubItem: IConfigDataWriteProvider;
begin
  for i := 0 to length(FMapType) - 1 do begin
    VMapType := FMapType[i];
    VGUIDString := VMapType.GUIDString;
    VSubItem := ALocalMapsConfig.GetOrCreateSubItem(VGUIDString);
    VSubItem.WriteInteger('pnum', VMapType.FSortIndex);


    if VMapType.UrlGenerator.URLBase <> VMapType.UrlGenerator.DefURLBase then begin
      VSubItem.WriteString('URLBase', VMapType.UrlGenerator.URLBase);
    end else begin
      VSubItem.DeleteValue('URLBase');
    end;

    if VMapType.HotKey <> VMapType.DefHotKey then begin
      VSubItem.WriteInteger('HotKey', VMapType.HotKey);
    end else begin
      VSubItem.DeleteValue('HotKey');
    end;

    if VMapType.TileStorage.CacheConfig.cachetype <> VMapType.TileStorage.CacheConfig.defcachetype then begin
      VSubItem.WriteInteger('CacheType', VMapType.TileStorage.CacheConfig.CacheType);
    end else begin
      VSubItem.DeleteValue('CacheType');
    end;

    if VMapType.separator <> VMapType.Defseparator then begin
      VSubItem.WriteBool('separator', VMapType.separator);
    end else begin
      VSubItem.DeleteValue('separator');
    end;

    if VMapType.TileStorage.CacheConfig.NameInCache <> VMapType.TileStorage.CacheConfig.DefNameInCache then begin
      VSubItem.WriteString('NameInCache', VMapType.TileStorage.CacheConfig.NameInCache);
    end else begin
      VSubItem.DeleteValue('NameInCache');
    end;

    if VMapType.DownloaderFactory.WaitInterval <> VMapType.DefSleep then begin
      VSubItem.WriteInteger('Sleep', VMapType.DownloaderFactory.WaitInterval);
    end else begin
      VSubItem.DeleteValue('Sleep');
    end;

    if VMapType.ParentSubMenu <> VMapType.DefParentSubMenu then begin
      VSubItem.WriteString('ParentSubMenu', VMapType.ParentSubMenu);
    end else begin
      VSubItem.DeleteValue('ParentSubMenu');
    end;
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
      while (j >= 0) and (FMapType[j].FSortIndex > FMapType[j + k].FSortIndex) do begin
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
    FMapType[i].FSortIndex := i + 1;
  end;
end;

end.
