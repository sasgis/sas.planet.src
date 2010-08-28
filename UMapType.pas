unit UMapType;

interface

uses
  Windows,
  sysutils,
  Classes,
  IniFiles,
  Dialogs,
  Graphics,
  SyncObjs,
  VCLZip,
  GR32,
  t_GeoTypes,
  i_ITileObjCache,
  i_ICoordConverter,
  i_ITileDownlodSession,
  i_IPoolOfObjectsSimple,
  i_IBitmapTypeExtManager,
  i_BitmapTileSaveLoad,
  i_IAntiBan,
  u_KmlInfoSimple,
  u_UrlGenerator,
  u_MapTypeCacheConfig,
  UResStrings;

type
  EBadGUID = class(Exception);

 TMapType = class
   private
    FGuid: TGUID;
    FName: string;
    FasLayer: boolean;
    FTileRect: TRect;
    Fpos: integer;
    FFileName: string;
    FTileFileExt: string;
    FMapInfo: string;
    FDefHotKey: TShortCut;
    FDefSleep: Integer;
    FDefseparator: boolean;
    FDefParentSubMenu: string;
    FUseDwn: boolean;
    FUseDel: boolean;
    FIsStoreReadOnly: Boolean;
    FUseSave: boolean;
    FIsCanShowOnSmMap: Boolean;
    FUseStick: boolean;
    FUseGenPrevious: boolean;
    Fbmp18: TBitmap;
    Fbmp24: TBitmap;
    FMaxConnectToServerCount: Cardinal;
    FRadiusA: extended;
    FRadiusB: extended;
    FAntiBan: IAntiBan;
    FMimeTypeSubstList: TStringList;
    FPNum: integer;
    FCache: ITileObjCache;
    FIcon24Index: Integer;
    FIcon18Index: Integer;
    FCacheConfig: TMapTypeCacheConfig;
    FUrlGenerator : TUrlGeneratorBasic;
    FBitmapLoaderFromStorage: IBitmapTileLoader;
    FBitmapSaverToStorage: IBitmapTileSaver;
    FInitDownloadCS: TCriticalSection;
    FCSSaveTile: TCriticalSection;
    FCSSaveTNF: TCriticalSection;
    FCoordConverter : ICoordConverter;
    FConverterForUrlGenerator: ICoordConverterSimple;
    FPoolOfDownloaders: IPoolOfObjectsSimple;
    function GetCoordConverter: ICoordConverter;
    function GetIsStoreFileCache: Boolean;
    function GetUseDwn: Boolean;
    function GetUseDel: boolean;
    function GetUseSave: boolean;
    function GetZmpFileName: string;
    function GetIsStoreReadOnly: boolean;
    function GetIsCanShowOnSmMap: boolean;
    function GetUseStick: boolean;
    function GetBitmapTypeManager: IBitmapTypeExtManager;
    function GetIsCropOnDownload: Boolean;
    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
    function GetIsHybridLayer: Boolean;
    function GetGUIDString: string;
    function GetMIMETypeSubst(AMimeType: string): string;
    procedure LoadMimeTypeSubstList(AIniFile: TCustomIniFile);
    procedure LoadGUIDFromIni(AIniFile: TCustomIniFile);
    procedure LoadMapIcons(AUnZip: TVCLZip);
    procedure LoadUrlScript(AUnZip: TVCLZip; AIniFile: TCustomIniFile);
    procedure LoadProjectionInfo(AIniFile: TCustomIniFile);
    procedure LoadStorageParams(AUnZip: TVCLZip; AIniFile: TCustomIniFile);
    procedure LoadWebSourceParams(AIniFile: TCustomIniFile);
    procedure LoadUIParams(AIniFile: TCustomIniFile);
    procedure LoadMapInfo(AUnZip: TVCLZip);
    procedure SaveTileDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileNotExists(AXY: TPoint; Azoom: byte);
    procedure CropOnDownload(ABtm: TCustomBitmap32; ATileSize: TPoint);
    function LoadFile(btm: TCustomBitmap32; APath: string; caching: boolean): boolean; overload;
    function LoadFile(btm: TKmlInfoSimple; APath: string; caching: boolean): boolean; overload;
    procedure CreateDirIfNotExists(APath: string);
    procedure SaveTileInCache(btm: TCustomBitmap32; path: string); overload;
    procedure SaveTileInCache(btm: TStream; path: string); overload;
    procedure SaveTileKmlDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileBitmapDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; AMimeType: string);
    function GetUseGenPrevious: boolean;
   public
    id: integer;

    HotKey: TShortCut;

    Sleep: Integer;

    separator: boolean;

    ParentSubMenu: string;

    showinfo: boolean;

    function GetLink(AXY: TPoint; Azoom: byte): string;

    function GetTileFileName(AXY: TPoint; Azoom: byte): string;

    function GetTileShowName(AXY: TPoint; Azoom: byte): string;

    function TileExists(AXY: TPoint; Azoom: byte): Boolean;

    function TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;

    function LoadTile(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte; caching: boolean): boolean; overload;
    function LoadTile(btm: TKmlInfoSimple; AXY: TPoint; Azoom: byte; caching: boolean): boolean; overload;

    function LoadTileFromPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte; caching: boolean): boolean;

    function LoadTileOrPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte; caching: boolean; IgnoreError: Boolean): boolean;

    function LoadTileUni(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte; caching: boolean; ACoordConverterTarget: ICoordConverter; AUsePre, AAllowPartial, IgnoreError: Boolean): boolean;

    function LoadBtimap(spr: TCustomBitmap32; APixelRectTarget: TRect; Azoom: byte; caching: boolean; AUsePre, AAllowPartial, IgnoreError: Boolean): boolean;

    function LoadBtimapUni(spr: TCustomBitmap32; APixelRectTarget: TRect; Azoom: byte; caching: boolean; ACoordConverterTarget: ICoordConverter; AUsePre, AAllowPartial, IgnoreError: Boolean): boolean;

    function DeleteTile(AXY: TPoint; Azoom: byte): Boolean;

    procedure SaveTileSimple(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);

    function TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;

    function TileSize(AXY: TPoint; Azoom: byte): integer;

    function TileExportToFile(AXY: TPoint; Azoom: byte; AFileName: string; OverWrite: boolean): boolean;

    // Строит карту заполнения дл тайла на уровне AZoom тайлами уровня ASourceZoom
    // Должна регулярно проверять по указателю IsStop не нужно ли прерваться
    function LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte; ASourceZoom: byte; IsStop: PBoolean): boolean;

    function GetShortFolderName: string;

    function DownloadTile(AThread: TThread; ATile: TPoint; AZoom: byte; ACheckTileSize: Boolean; AOldTileSize: Integer; out AUrl: string; out AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult; overload;

    property GeoConvert: ICoordConverter read GetCoordConverter;
    property GUID: TGUID read FGuid;
    property GUIDString: string read GetGUIDString;
    property IsStoreFileCache: Boolean read GetIsStoreFileCache;
    property IsBitmapTiles: Boolean read GetIsBitmapTiles;
    property IsKmlTiles: Boolean read GetIsKmlTiles;
    property IsHybridLayer: Boolean read GetIsHybridLayer;
    property UseDwn: Boolean read GetUseDwn;
    property UseDel: boolean read GetUseDel;
    property UseSave: boolean read GetUseSave;
    property IsStoreReadOnly: boolean read GetIsStoreReadOnly;
    property IsCanShowOnSmMap: boolean read GetIsCanShowOnSmMap;
    property UseStick: boolean read GetUseStick;
    property IsCropOnDownload: Boolean read GetIsCropOnDownload;
    property ZmpFileName: string read GetZmpFileName;
    property BitmapTypeManager: IBitmapTypeExtManager read GetBitmapTypeManager;
    property Icon24Index: Integer read FIcon24Index;
    property Icon18Index: Integer read FIcon18Index;
    property bmp18: TBitmap read Fbmp18;
    property bmp24: TBitmap read Fbmp24;
    property CacheConfig: TMapTypeCacheConfig read FCacheConfig;
    property UrlGenerator : TUrlGeneratorBasic read FUrlGenerator;
    property TileFileExt: string read FTileFileExt;
    property MapInfo: string read FMapInfo;
    property asLayer: boolean read FasLayer;
    property Name: string read FName;
    property UseGenPrevious: boolean read GetUseGenPrevious;
    property DefHotKey: TShortCut read FDefHotKey;
    property DefSleep: Integer read FDefSleep;
    property Defseparator: boolean read FDefseparator;
    property DefParentSubMenu: string read FDefParentSubMenu;

    constructor Create;
    destructor Destroy; override;
    procedure LoadMapTypeFromZipFile(AZipFileName : string; Apnum : Integer);
 end;


  procedure LoadMaps;
  procedure SaveMaps;
  function GetMapFromID(id: TGUID): TMapType;

implementation

uses
  Types,
  GR32_Resamplers,
  VCLUnZip,
  u_GlobalState,
  u_GeoToStr,
  UIMGFun,
  i_IObjectWithTTL,
  i_IPoolElement,
  u_PoolOfObjectsSimple,
  u_TileDownloaderBaseFactory,
  u_AntiBanStuped,
  u_TileCacheSimpleGlobal,
  u_GECache,
  u_CoordConverterBasic,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid,
  u_CoordConverterSimpleLonLat;

function GetMapFromID(id: TGUID): TMapType;
var
  i: integer;
  VMapType: TMapType;
begin
  Result:=nil;
  for i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if IsEqualGUID(VMapType.GUID, id) then begin
      result:=VMapType;
      exit;
    end;
  end;
end;

function FindGUIDInFirstMaps(AGUID: TGUID; Acnt: Cardinal; out AMapType: TMapType): Boolean;
var
  i: Integer;
  VMapType: TMapType;
begin
  AMapType := nil;
  Result := false;
  if Acnt > 0 then begin
    for i := 0 to Acnt - 1 do begin
      VMapType := GState.MapType[i];
      if IsEqualGUID(AGUID, VMapType.GUID) then begin
        Result := True;
        AMapType := VMapType;
        Break;
      end;
    end;
  end;
end;

procedure LoadMaps;
var
  Ini: TMeminifile;
  i,j,k,pnum: integer;
  startdir : string;
  SearchRec: TSearchRec;
  MTb: TMapType;
  VGUIDString: String;
  VMapType: TMapType;
  VMapTypeLoaded: TMapType;
  VMapOnlyCount: integer;
begin
  SetLength(GState.MapType,0);
  CreateDir(GState.MapsPath);
  Ini:=TMeminiFile.Create(GState.MapsPath + 'Maps.ini');
  i:=0;
  pnum:=0;
  startdir:=GState.MapsPath;
  if FindFirst(startdir+'*.zmp', faAnyFile, SearchRec) = 0 then begin
    repeat
      inc(i);
    until FindNext(SearchRec) <> 0;
  end;
  VMapOnlyCount := 0;
  SysUtils.FindClose(SearchRec);
  SetLength(GState.MapType,i);
  if FindFirst(startdir+'*.zmp', faAnyFile, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then continue;
      try
        VMapType := TMapType.Create;
        try
          VMapType.LoadMapTypeFromZipFile(startdir+SearchRec.Name, pnum);
        except
          on E: EBadGUID do begin
            raise Exception.CreateResFmt(@SAS_ERR_MapGUIDError, [startdir+SearchRec.Name, E.Message]);
          end;
        end;
        VGUIDString := VMapType.GUIDString;
        if FindGUIDInFirstMaps(VMapType.GUID, pnum, VMapTypeLoaded) then begin
          raise Exception.CreateFmt('В файлах %0:s и %1:s одинаковые GUID', [VMapTypeLoaded.FFileName, startdir+SearchRec.Name ]);
        end;
        if Ini.SectionExists(VGUIDString)then begin
          With VMapType do begin
            id:=Ini.ReadInteger(VGUIDString,'pnum',0);
            FUrlGenerator.URLBase:=ini.ReadString(VGUIDString,'URLBase',FUrlGenerator.URLBase);
            CacheConfig.CacheType:=ini.ReadInteger(VGUIDString,'CacheType',CacheConfig.cachetype);
            CacheConfig.NameInCache:=ini.ReadString(VGUIDString,'NameInCache',CacheConfig.NameInCache);
            HotKey:=ini.ReadInteger(VGUIDString,'HotKey',HotKey);
            ParentSubMenu:=ini.ReadString(VGUIDString,'ParentSubMenu',ParentSubMenu);
            Sleep:=ini.ReadInteger(VGUIDString,'Sleep',Sleep);
            separator:=ini.ReadBool(VGUIDString,'separator',separator);
          end;
        end else begin
          With VMapType do begin
            showinfo:=true;
            if Fpos < 0 then Fpos := i;
            id := Fpos;
            dec(i);
          end;
        end;
      except
        if ExceptObject <> nil then begin
          ShowMessage((ExceptObject as Exception).Message);
        end;
        FreeAndNil(VMapType);
      end;
      if VMapType <> nil then begin
        GState.MapType[pnum]:= VMapType;
        if not VMapType.asLayer then begin
          Inc(VMapOnlyCount);
        end;
        inc(pnum);
      end;
    until FindNext(SearchRec) <> 0;
    SetLength(GState.MapType, pnum);
  end;
  SysUtils.FindClose(SearchRec);
  ini.Free;
  if Length(GState.MapType) = 0 then begin
    raise Exception.Create(SAS_ERR_NoMaps);
  end;
  if VMapOnlyCount = 0 then begin
    raise Exception.Create('Среди ZMP должна быть хотя бы одна карта');
  end;

  k := length(GState.MapType) shr 1;
  while k>0 do begin
    for i:=0 to length(GState.MapType)-k-1 do begin
      j:=i;
      while (j>=0)and(GState.MapType[j].id>GState.MapType[j+k].id) do begin
        MTb:=GState.MapType[j];
        GState.MapType[j]:=GState.MapType[j+k];
        GState.MapType[j+k]:=MTb;
        if j>k then begin
          Dec(j,k);
        end else begin
          j:=0;
        end;
      end;
    end;
    k:=k shr 1;
  end;
  for i:=0 to length(GState.MapType)-1 do begin
    GState.MapType[i].id:=i+1;
    GState.MapType[i].FIcon24Index := i;
    GState.MapType[i].FIcon18Index := i;
  end;
end;

procedure SaveMaps;
var
  Ini: TMeminifile;
  i: integer;
  VGUIDString: string;
  VMapType: TMapType;
begin
  Ini:=TMeminiFile.Create(GState.MapsPath + 'Maps.ini');
  try
    for i:=0 to length(GState.MapType)-1 do begin
      VMapType := GState.MapType[i];
      VGUIDString := VMapType.GUIDString;
      ini.WriteInteger(VGUIDString,'pnum',VMapType.id);


      if VMapType.FUrlGenerator.URLBase<>VMapType.FUrlGenerator.DefURLBase then begin
        ini.WriteString(VGUIDString,'URLBase',VMapType.FUrlGenerator.URLBase);
      end else begin
        Ini.DeleteKey(VGUIDString,'URLBase');
      end;

      if VMapType.HotKey<>VMapType.DefHotKey then begin
        ini.WriteInteger(VGUIDString,'HotKey',VMapType.HotKey);
      end else begin
        Ini.DeleteKey(VGUIDString,'HotKey');
      end;

      if VMapType.CacheConfig.cachetype<>VMapType.CacheConfig.defcachetype then begin
        ini.WriteInteger(VGUIDString,'CacheType',VMapType.CacheConfig.CacheType);
      end else begin
        Ini.DeleteKey(VGUIDString,'CacheType');
      end;

      if VMapType.separator<>VMapType.Defseparator then begin
        ini.WriteBool(VGUIDString,'separator',VMapType.separator);
      end else begin
        Ini.DeleteKey(VGUIDString,'separator');
      end;

      if VMapType.CacheConfig.NameInCache<>VMapType.CacheConfig.DefNameInCache then begin
        ini.WriteString(VGUIDString,'NameInCache',VMapType.CacheConfig.NameInCache);
      end else begin
        Ini.DeleteKey(VGUIDString,'NameInCache');
      end;

      if VMapType.Sleep<>VMapType.DefSleep then begin
        ini.WriteInteger(VGUIDString,'Sleep',VMapType.sleep);
      end else begin
        Ini.DeleteKey(VGUIDString,'Sleep');
      end;

      if VMapType.ParentSubMenu<>VMapType.DefParentSubMenu then begin
        ini.WriteString(VGUIDString,'ParentSubMenu',VMapType.ParentSubMenu);
      end else begin
        Ini.DeleteKey(VGUIDString,'ParentSubMenu');
      end;
    end;
    Ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TMapType.LoadGUIDFromIni(AIniFile: TCustomIniFile);
var
  VGUIDStr: String;
begin
  VGUIDStr := AIniFile.ReadString('PARAMS','GUID', '');
  if Length(VGUIDStr) > 0 then begin
    try
      FGuid := StringToGUID(VGUIDStr);
    except
      raise EBadGUID.CreateResFmt(@SAS_ERR_MapGUIDBad, [VGUIDStr]);
    end;
  end else begin
    raise EBadGUID.CreateRes(@SAS_ERR_MapGUIDEmpty);
  end;
end;

procedure TMapType.LoadMapIcons(AUnZip: TVCLZip);
var
  MapParams:TMemoryStream;
begin
  Fbmp24:=TBitmap.create;
  MapParams:=TMemoryStream.Create;
  try
    try
      AUnZip.UnZipToStream(MapParams,'24.bmp');
      MapParams.Position:=0;
      Fbmp24.LoadFromStream(MapParams);
    except
      Fbmp24.Canvas.FillRect(Fbmp24.Canvas.ClipRect);
      Fbmp24.Width:=24;
      Fbmp24.Height:=24;
      Fbmp24.Canvas.TextOut(7,3,copy(name,1,1));
    end;
  finally
    FreeAndNil(MapParams);
  end;
  Fbmp18:=TBitmap.create;
  MapParams:=TMemoryStream.Create;
  try
    try
      AUnZip.UnZipToStream(MapParams,'18.bmp');
      MapParams.Position:=0;
      Fbmp18.LoadFromStream(MapParams);
    except
      Fbmp18.Canvas.FillRect(Fbmp18.Canvas.ClipRect);
      Fbmp18.Width:=18;
      Fbmp18.Height:=18;
      Fbmp18.Canvas.TextOut(3,2,copy(name,1,1));
    end;
  finally
    FreeAndNil(MapParams);
  end;
end;

procedure TMapType.LoadUrlScript(AUnZip: TVCLZip; AIniFile: TCustomIniFile);
begin
  if FUseDwn then begin
    try
      FUrlGenerator := TUrlGenerator.Create(AUnZip, AIniFile, FConverterForUrlGenerator);
      //GetLink(0,0,0);
    except
      on E: Exception do begin
        ShowMessage('Ошибка скрипта карты '+name+' :'+#13#10+ E.Message);
        FUrlGenerator := nil;
      end;
     else
      ShowMessage('Ошибка скрипта карты '+name+' :'+#13#10+'Неожиданная ошибка');
      FUrlGenerator := nil;
    end;
  end;
  if FUrlGenerator = nil then begin
    FUrlGenerator := TUrlGeneratorBasic.Create(AUnZip, AIniFile);
  end;
end;

procedure TMapType.LoadMapInfo(AUnZip: TVCLZip);
var
  MapParams:TMemoryStream;
begin
  MapParams:=TMemoryStream.Create;
  try
  if (AUnZip.UnZipToStream(MapParams,'info_'+inttostr(GState.Localization)+'.txt')>0)or(AUnZip.UnZipToStream(MapParams,'info.txt')>0) then
   begin
    SetLength(FMapInfo, MapParams.size);
    MapParams.Position:=0;
    MapParams.ReadBuffer(FMapinfo[1],MapParams.size);
   end;
  finally
    FreeAndNil(MapParams);
  end;
end;

procedure TMapType.LoadStorageParams(AUnZip: TVCLZip; AIniFile: TCustomIniFile);
begin
  FUseDel:=AIniFile.ReadBool('PARAMS','Usedel',true);
  FIsStoreReadOnly:=AIniFile.ReadBool('PARAMS','ReadOnly', false);
  FUseSave:=AIniFile.ReadBool('PARAMS','Usesave',true);
  FTileFileExt:=LowerCase(AIniFile.ReadString('PARAMS','Ext','.jpg'));
  FCacheConfig := TMapTypeCacheConfig.Create(AUnZip, AIniFile);
  FCache := TTileCacheSimpleGlobal.Create(Self);
  if GetIsBitmapTiles then begin
    FBitmapLoaderFromStorage := GState.BitmapTypeManager.GetBitmapLoaderForExt(FTileFileExt);
    FBitmapSaverToStorage := GState.BitmapTypeManager.GetBitmapSaverForExt(FTileFileExt);
  end;
end;

procedure TMapType.LoadProjectionInfo(AIniFile: TCustomIniFile);
var
  bfloat:string;
  projection: byte;
  VConverter: TCoordConverterBasic;
begin
  projection:=AIniFile.ReadInteger('PARAMS','projection',1);
  bfloat:=AIniFile.ReadString('PARAMS','sradiusa','6378137');
  FRadiusA:=str2r(bfloat);
  bfloat:=AIniFile.ReadString('PARAMS','sradiusb',FloatToStr(FRadiusA));
  FRadiusB:=str2r(bfloat);
  case projection of
    1: VConverter := TCoordConverterMercatorOnSphere.Create(FRadiusA);
    2: VConverter := TCoordConverterMercatorOnEllipsoid.Create(FRadiusA, FRadiusB);
    3: VConverter := TCoordConverterSimpleLonLat.Create(FRadiusA, FRadiusB);
    else raise Exception.Create('Ошибочный тип проэкции карты ' + IntToStr(projection));
  end;
  FCoordConverter := VConverter;
  FConverterForUrlGenerator := VConverter;
end;

procedure TMapType.LoadMimeTypeSubstList(AIniFile: TCustomIniFile);
var
  VMimeTypeSubstText: string;
begin
  VMimeTypeSubstText := AIniFile.ReadString('PARAMS', 'MimeTypeSubst', '');
  if Length(VMimeTypeSubstText) > 0 then begin
    FMimeTypeSubstList := TStringList.Create;
    FMimeTypeSubstList.Delimiter := ';';
    FMimeTypeSubstList.DelimitedText := VMimeTypeSubstText;
    if FMimeTypeSubstList.Count = 0 then begin
      FreeAndNil(FMimeTypeSubstList);
    end;
  end;
end;

procedure TMapType.LoadWebSourceParams(AIniFile: TCustomIniFile);
begin
  FTileRect.Left:=AIniFile.ReadInteger('PARAMS','TileRLeft',0);
  FTileRect.Top:=AIniFile.ReadInteger('PARAMS','TileRTop',0);
  FTileRect.Right:=AIniFile.ReadInteger('PARAMS','TileRRight',0);
  FTileRect.Bottom:=AIniFile.ReadInteger('PARAMS','TileRBottom',0);

  Sleep:=AIniFile.ReadInteger('PARAMS','Sleep',0);
  FDefSleep:=Sleep;
  FUseDwn:=AIniFile.ReadBool('PARAMS','UseDwn',true);
end;

procedure TMapType.LoadUIParams(AIniFile: TCustomIniFile);
begin
  FName:=AIniFile.ReadString('PARAMS','name','map#'+inttostr(FPNum));
  FName:=AIniFile.ReadString('PARAMS','name_'+inttostr(GState.Localization),FName);
  FIsCanShowOnSmMap := AIniFile.ReadBool('PARAMS','CanShowOnSmMap', true);
  HotKey:=AIniFile.ReadInteger('PARAMS','DefHotKey',0);
  FDefHotKey:=HotKey;
  ParentSubMenu:=AIniFile.ReadString('PARAMS','ParentSubMenu','');
  ParentSubMenu:=AIniFile.ReadString('PARAMS','ParentSubMenu_'+inttostr(GState.Localization),ParentSubMenu);
  FDefParentSubMenu:=ParentSubMenu;
  separator:=AIniFile.ReadBool('PARAMS','separator',false);
  FDefseparator:=separator;
  Fpos:=AIniFile.ReadInteger('PARAMS','pnum',-1);
end;

procedure TMapType.LoadMapTypeFromZipFile(AZipFileName: string; Apnum : Integer);
var
  MapParams:TMemoryStream;
  IniStrings:TStringList;
  iniparams: TMeminifile;
  UnZip:TVCLZip;
begin
  FPNum := Apnum;
  if AZipFileName = '' then begin
    raise Exception.Create('Пустое имя файла с настройками карты');
  end;
  if not FileExists(AZipFileName) then begin
    raise Exception.Create('Файл ' + AZipFileName + ' не найден');
  end;
  Ffilename := AZipFileName;
  UnZip:=TVCLZip.Create(nil);
  try
    UnZip.ZipName:=AZipFileName;
    MapParams:=TMemoryStream.Create;
    IniStrings:=TStringList.Create;
    try
      UnZip.UnZip;
      UnZip.UnZipToStream(MapParams,'params.txt');
      MapParams.Position:=0;
      iniparams:=TMemIniFile.Create('');
      IniStrings.LoadFromStream(MapParams);
      iniparams.SetStrings(IniStrings);
    finally
      FreeAndNil(IniStrings);
      FreeAndNil(MapParams);
    end;
    try
      LoadGUIDFromIni(iniparams);
      FasLayer:=iniparams.ReadBool('PARAMS','asLayer',false);
      LoadUIParams(iniparams);
      LoadMapInfo(UnZip);
      LoadStorageParams(UnZip, iniparams);
      LoadMapIcons(UnZip);
      LoadWebSourceParams(iniparams);
      FUsestick:=iniparams.ReadBool('PARAMS','Usestick',true);
      FUseGenPrevious:=iniparams.ReadBool('PARAMS','UseGenPrevious',true);
      LoadMimeTypeSubstList(iniparams);
      LoadProjectionInfo(iniparams);
      LoadUrlScript(UnZip, iniparams);
      if FUseDwn then begin
        try
          FMaxConnectToServerCount := iniparams.ReadInteger('PARAMS','MaxConnectToServerCount', 1);
          if FMaxConnectToServerCount > 64 then begin
            FMaxConnectToServerCount := 64;
          end;
          if FMaxConnectToServerCount <= 0 then begin
            FMaxConnectToServerCount := 1;
          end;
          FPoolOfDownloaders := TPoolOfObjectsSimple.Create(FMaxConnectToServerCount, TTileDownloaderBaseFactory.Create(Self, UnZip, iniparams), 60000, 60000);
          GState.GCThread.List.AddObject(FPoolOfDownloaders as IObjectWithTTL);
          FAntiBan := TAntiBanStuped.Create(UnZip, iniparams);
        except
          if ExceptObject <> nil then begin
            ShowMessageFmt('Для карты %0:s отключена загрузка тайлов из-за ошибки: %1:s',[AZipFileName, (ExceptObject as Exception).Message]);
          end;
          FUseDwn := false;
        end;
      end;
    finally
      FreeAndNil(iniparams);
    end;
  finally
    FreeAndNil(UnZip);
  end;
end;

function TMapType.GetLink(AXY: TPoint; Azoom: byte): string;
begin
  FCoordConverter.CheckTilePosStrict(AXY, Azoom, True);
  Result:=FUrlGenerator.GenLink(AXY.X, AXY.Y, Azoom);
end;

function TMapType.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  if IsStoreFileCache then begin
    Result := FCacheConfig.GetTileFileName(AXY, Azoom);
  end else begin
    raise Exception.Create('Ошибка. Это не файловый кеш');
  end;
end;

function TMapType.TileExists(AXY: TPoint; Azoom: byte): Boolean;
var
  VPath: String;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    result:=GETileExists(FCacheConfig.BasePath+'dbCache.dat.index', AXY.X, AXY.Y, Azoom + 1,self);
  end else begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    Result := Fileexists(VPath);
  end;
end;

function GetFileSize(namefile: string): Integer;
var
  InfoFile: TSearchRec;
begin
  if FindFirst(namefile, faAnyFile, InfoFile) <> 0 then begin
    Result := -1;
  end else begin
    Result := InfoFile.Size;
  end;
  SysUtils.FindClose(InfoFile);
end;

function TMapType.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
var
  VPath: string;
begin
  Result := false;
  if UseDel then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
      FCSSaveTile.Acquire;
      try
        if FileExists(VPath) then begin
          result := DeleteFile(PChar(VPath));
        end;
      finally
        FCSSaveTile.Release;
      end;
      FCache.DeleteTileFromCache(AXY,Azoom);

      VPath := ChangeFileExt(VPath, '.tne');
      FCSSaveTNF.Acquire;
      try
        if FileExists(VPath) then begin
          result := DeleteFile(PChar(VPath));
        end;
      finally
        FCSSaveTNF.Release;
      end;
    except
      Result := false;
    end;
  end else begin
    Exception.Create('Для этой карты запрещено удаление тайлов.');
  end;
end;

function TMapType.TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;
var
  VPath: String;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    Result := False;
  end else begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    Result := Fileexists(ChangeFileExt(VPath, '.tne'));
  end;
end;

procedure TMapType.CreateDirIfNotExists(APath:string);
var i:integer;
begin
 i := LastDelimiter(PathDelim, Apath);
 Apath:=copy(Apath, 1, i);
 if not(DirectoryExists(Apath)) then ForceDirectories(Apath);
end;

procedure TMapType.SaveTileBitmapDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; AMimeType: string);
var
  VPath: String;
  btmSrc:TCustomBitmap32;
  VManager: IBitmapTypeExtManager;
  VMimeType: String;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  CreateDirIfNotExists(VPath);
  VManager := BitmapTypeManager;
  VMimeType := GetMIMETypeSubst(AMimeType);
  if VManager.GetIsBitmapType(VMimeType) then begin
    if not IsCropOnDownload and SameText(TileFileExt, VManager.GetExtForType(VMimeType)) then begin
      SaveTileInCache(ATileStream,Vpath);
    end else begin
      btmsrc := TCustomBitmap32.Create;
      try
        ATileStream.Position := 0;
        VManager.GetBitmapLoaderForType(VMimeType).LoadFromStream(ATileStream, btmSrc);

        if IsCropOnDownload then begin
          CropOnDownload(btmSrc, FCoordConverter.GetTileSize(AXY, Azoom));
        end;
        SaveTileInCache(btmSrc, VPath);
      finally
        FreeAndNil(btmSrc);
      end;
    end;
    FCache.DeleteTileFromCache(AXY, Azoom);
  end else begin
    SaveTileInCache(ATileStream, ChangeFileExt(Vpath, '.err'));
    raise Exception.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AMimeType]);
  end;
end;

procedure TMapType.SaveTileKmlDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
var
  VPath: String;
  UnZip:TVCLUnZip;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  CreateDirIfNotExists(VPath);
  if (ty='application/vnd.google-earth.kmz') then begin
    try
      UnZip:=TVCLUnZip.Create(nil);
      try
        UnZip.ArchiveStream:=TMemoryStream.Create;
        try
          ATileStream.SaveToStream(UnZip.ArchiveStream);
          UnZip.ReadZip;
          ATileStream.Position:=0;
          UnZip.UnZipToStream(ATileStream,UnZip.Filename[0]);
        finally
          UnZip.ArchiveStream.Free;
          UnZip.ArchiveStream := nil;
        end;
      finally
        UnZip.Free;
      end;
      SaveTileInCache(ATileStream,Vpath);
    except
      try
        SaveTileInCache(ATileStream,Vpath);
      except
      end;
    end;
  end else if (copy(ty,1,8)='text/xml')or(ty='application/vnd.google-earth.kml+xml') then begin
    SaveTileInCache(ATileStream,Vpath);
  end;
end;

procedure TMapType.SaveTileDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
begin
  if UseSave then begin
    if TileFileExt='.kml' then begin
      SaveTileKmlDownload(AXY, Azoom, ATileStream, ty);
    end else begin
      SaveTileBitmapDownload(AXY, Azoom, ATileStream, ty);
    end;
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

procedure TMapType.SaveTileInCache(btm: TCustomBitmap32; path: string);
begin
  FCSSaveTile.Acquire;
  try
    FBitmapSaverToStorage.SaveToFile(btm, path);
  finally
    FCSSaveTile.Release;
  end;
end;

procedure TMapType.SaveTileInCache(btm: TStream; path: string);
var
  VStream: TMemoryStream;
begin
  FCSSaveTile.Acquire;
  try
    if btm is TMemoryStream then begin
      TMemoryStream(btm).SaveToFile(path);
    end else begin
      VStream := TMemoryStream.Create();
      try
        VStream.LoadFromStream(btm);
        VStream.SaveToFile(path);
      finally
        VStream.Free;
      end;
    end;
  finally
    FCSSaveTile.Release;
  end;
end;

function TMapType.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
var
  VPath: String;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    Result := 0;
  end else begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    Result := FileDateToDateTime(FileAge(VPath));
  end;
end;

function TMapType.TileSize(AXY: TPoint; Azoom: byte): integer;
var
  VPath: String;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    Result := 0;
  end else begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    Result := GetFileSize(VPath);
  end;
end;

procedure TMapType.SaveTileNotExists(AXY: TPoint; Azoom: byte);
var
  VPath: String;
  F:textfile;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
  end else begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    VPath := ChangeFileExt(VPath, '.tne');
    FCSSaveTNF.Acquire;
    try
    if not FileExists(VPath) then begin
      CreateDirIfNotExists(VPath);
      AssignFile(f,VPath);
      Rewrite(F);
      Writeln(f,DateTimeToStr(now));
      CloseFile(f);
    end;
    finally
      FCSSaveTNF.Release;
    end;
  end;
end;

procedure TMapType.SaveTileSimple(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
var
  VPath: String;
begin
  if UseSave then begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    CreateDirIfNotExists(VPath);
    DeleteFile(ChangeFileExt(Vpath,'.tne'));
    SaveTileInCache(btm, Vpath);
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

function TMapType.TileExportToFile(AXY: TPoint; Azoom: byte;
  AFileName: string; OverWrite: boolean): boolean;
var
  VPath: String;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    Result := false;
  end else begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    CreateDirIfNotExists(AFileName);
    Result := CopyFile(PChar(VPath), PChar(AFileName), not OverWrite);
  end;
end;

function TMapType.LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint; Azoom,
  ASourceZoom: byte; IsStop: PBoolean): boolean;
var
  VPixelsRect: TRect;
  VRelativeRect: TExtendedRect;
  VSourceTilesRect: TRect;
  VCurrTile: TPoint;
  VTileSize: TPoint;
  VSourceTilePixels: TRect;
  i, j: Integer;
  VClMZ: TColor32;
  VClTne: TColor32;
  VSolidDrow: Boolean;
begin
  Result := true;
  try
    GeoConvert.CheckTilePosStrict(AXY, Azoom, True);
    GeoConvert.CheckZoom(ASourceZoom);

    VPixelsRect := GeoConvert.TilePos2PixelRect(AXY, Azoom);

    VTileSize := Point(VPixelsRect.Right - VPixelsRect.Left + 1, VPixelsRect.Bottom - VPixelsRect.Top + 1);

    btm.Width := VTileSize.X;
    btm.Height := VTileSize.Y;
    btm.Clear(clBlack);

    VRelativeRect := GeoConvert.TilePos2RelativeRect(AXY, Azoom);
    VSourceTilesRect := GeoConvert.RelativeRect2TileRect(VRelativeRect, ASourceZoom);
   { if (VTileSize.X >= (VSourceTilesRect.Right - VSourceTilesRect.Left + 1)) and
      (VTileSize.Y >= (VSourceTilesRect.Right - VSourceTilesRect.Left + 1)) then  }
    begin
      VSolidDrow := (VTileSize.X <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left + 1))
        or (VTileSize.Y <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left + 1));
      VClMZ := SetAlpha(Color32(GState.MapZapColor), GState.MapZapAlpha);
      VClTne := SetAlpha(Color32(GState.MapZapTneColor), GState.MapZapAlpha);

      for i := VSourceTilesRect.Top to VSourceTilesRect.Bottom do begin
        VCurrTile.Y := i;
        if IsStop^ then break;
        for j := VSourceTilesRect.Left to VSourceTilesRect.Right do begin
          VCurrTile.X := j;
          if IsStop^ then break;
          if not TileExists(VCurrTile, ASourceZoom) then begin
            if IsStop^ then break;
            VRelativeRect := GeoConvert.TilePos2RelativeRect(VCurrTile, ASourceZoom);
            VSourceTilePixels := GeoConvert.RelativeRect2PixelRect(VRelativeRect, Azoom);
            if VSourceTilePixels.Left < VPixelsRect.Left then begin
              VSourceTilePixels.Left := VPixelsRect.Left;
            end;
            if VSourceTilePixels.Top < VPixelsRect.Top then begin
              VSourceTilePixels.Top := VPixelsRect.Top;
            end;
            if VSourceTilePixels.Right > VPixelsRect.Right then begin
              VSourceTilePixels.Right := VPixelsRect.Right;
            end;
            if VSourceTilePixels.Bottom > VPixelsRect.Bottom then begin
              VSourceTilePixels.Bottom := VPixelsRect.Bottom;
            end;
            VSourceTilePixels.Left := VSourceTilePixels.Left - VPixelsRect.Left;
            VSourceTilePixels.Top := VSourceTilePixels.Top - VPixelsRect.Top;
            VSourceTilePixels.Right := VSourceTilePixels.Right - VPixelsRect.Left;
            VSourceTilePixels.Bottom := VSourceTilePixels.Bottom - VPixelsRect.Top;
            if VSolidDrow then begin
              Inc(VSourceTilePixels.Right);
              Inc(VSourceTilePixels.Bottom);
            end;
            if ((VSourceTilePixels.Right-VSourceTilePixels.Left)=1)and
               ((VSourceTilePixels.Bottom-VSourceTilePixels.Top)=1)then begin
              if GState.MapZapShowTNE and TileNotExistsOnServer(VCurrTile, ASourceZoom) then begin
                btm.Pixel[VSourceTilePixels.Left,VSourceTilePixels.Top]:=VClTne;
              end else begin
                btm.Pixel[VSourceTilePixels.Left,VSourceTilePixels.Top]:=VClMZ;
              end;
            end else begin
              if GState.MapZapShowTNE and TileNotExistsOnServer(VCurrTile, ASourceZoom) then begin
                btm.FillRect(VSourceTilePixels.Left,VSourceTilePixels.Top,VSourceTilePixels.Right,VSourceTilePixels.Bottom, VClTne);
              end else begin
                btm.FillRect(VSourceTilePixels.Left,VSourceTilePixels.Top,VSourceTilePixels.Right,VSourceTilePixels.Bottom, VClMZ);
              end;
            end;
          end;
          if IsStop^ then break;
        end;
        if IsStop^ then break;
      end;
    end;
    if IsStop^ then begin
      Result := false;
    end;
  except
    Result := false;
  end;
end;

function TMapType.GetShortFolderName: string;
begin
  Result := ExtractFileName(ExtractFileDir(IncludeTrailingPathDelimiter(FCacheConfig.NameInCache)));
end;


function TMapType.GetCoordConverter: ICoordConverter;
begin
 if Self=nil
  then Result:= nil
  else Result:= FCoordConverter;
end;

constructor TMapType.Create;
begin
  FInitDownloadCS := TCriticalSection.Create;
  FCSSaveTile := TCriticalSection.Create;
  FCSSaveTNF := TCriticalSection.Create;
  FMimeTypeSubstList := nil;
end;

destructor TMapType.Destroy;
begin
  FreeAndNil(FMimeTypeSubstList);
  FreeAndNil(FInitDownloadCS);
  FreeAndNil(FCSSaveTile);
  FreeAndNil(FCSSaveTNF);
  FreeAndNil(FUrlGenerator);
  FreeAndNil(Fbmp18);
  FreeAndNil(Fbmp24);
  FCoordConverter := nil;
  FPoolOfDownloaders := nil;
  FCache := nil;
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TMapType.DownloadTile(AThread: TThread; ATile: TPoint;
  AZoom: byte; ACheckTileSize: Boolean; AOldTileSize: Integer; out AUrl,
  AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;
var
  StatusCode: Cardinal;
  VPoolElement: IPoolElement;
  VDownloader: ITileDownlodSession;
begin
  if Self.UseDwn then begin
    AUrl := GetLink(ATile, AZoom);
    VPoolElement := FPoolOfDownloaders.TryGetPoolElement(60000);
    if VPoolElement = nil then begin
      raise Exception.Create('No free connections');
    end;
    VDownloader := VPoolElement.GetObject as ITileDownlodSession;
    if FAntiBan <> nil then begin
      FAntiBan.PreDownload(VDownloader, ATile, AZoom, AUrl);
    end;
    Result := VDownloader.DownloadTile(AUrl, ACheckTileSize, AOldTileSize, fileBuf, StatusCode, AContentType);
    if FAntiBan <> nil then begin
      Result := FAntiBan.PostCheckDownload(VDownloader, ATile, AZoom, AUrl, Result, StatusCode, AContentType, fileBuf.Memory, fileBuf.Size);
    end;
    if Result = dtrOK then begin
      SaveTileDownload(ATile, AZoom, fileBuf, AContentType);
    end else if Result = dtrTileNotExists then begin
      if GState.SaveTileNotExists then begin
        SaveTileNotExists(ATile, AZoom);
      end;
    end;
  end else begin
    raise Exception.Create('Для этой карты загрузка запрещена.');
  end;
end;

function TMapType.GetTileShowName(AXY: TPoint; Azoom: byte): string;
begin
  if Self.IsStoreFileCache then begin
    Result := FCacheConfig.GetTileFileName(AXY, Azoom)
  end else begin
    Result := 'z' + IntToStr(Azoom + 1) + 'x' + IntToStr(AXY.X) + 'y' + IntToStr(AXY.Y);
  end;
end;

function TMapType.GetIsStoreFileCache: Boolean;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    Result := false;
  end else begin
    Result := true;
  end;
end;

function TMapType.GetUseDwn: Boolean;
begin
  if Self.UseSave then begin
    Result := FUseDwn;
  end else begin
    Result := false;
  end;
end;

function TMapType.GetUseGenPrevious: boolean;
begin
  Result := False;
  if GetUseSave then begin
    if GetIsBitmapTiles then begin
      Result := FUseGenPrevious;
    end;
  end;
end;

function TMapType.GetUseDel: boolean;
begin
  if IsStoreReadOnly then begin
    Result := false;
  end else begin
    Result := FUseDel;
  end;
end;

function TMapType.GetUseSave: boolean;
begin
  if IsStoreReadOnly then begin
    Result := false;
  end else begin
    Result := FUseSave;
  end;
end;

function TMapType.GetZmpFileName: string;
begin
  Result := ExtractFileName(FFileName);
end;

function TMapType.GetIsStoreReadOnly: boolean;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    Result := True;
  end else begin
    Result := FIsStoreReadOnly;
  end;
end;

function TMapType.GetUseStick: boolean;
begin
  if GetIsBitmapTiles then begin
    Result := FUseStick;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetIsCanShowOnSmMap: boolean;
begin
  if GetIsBitmapTiles then begin
    Result := FIsCanShowOnSmMap;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetBitmapTypeManager: IBitmapTypeExtManager;
begin
  Result := GState.BitmapTypeManager;
end;

procedure TMapType.CropOnDownload(ABtm: TCustomBitmap32; ATileSize: TPoint);
var
  VBtmSrc: TCustomBitmap32;
  VBtmDest: TCustomBitmap32;
begin
  VBtmSrc := TCustomBitmap32.Create;
  try
    VBtmSrc.Assign(ABtm);
    VBtmSrc.Resampler := TLinearResampler.Create;
    VBtmDest := TCustomBitmap32.Create;
    try
      VBtmDest.SetSize(ATileSize.X, ATileSize.Y);
      VBtmDest.Draw(Bounds(0, 0, ATileSize.X, ATileSize.Y), FTileRect, VBtmSrc);
      ABtm.Assign(VBtmDest);
    finally
      VBtmDest.Free;
    end;
  finally
    VBtmSrc.Free;
  end;
end;

function TMapType.GetIsCropOnDownload: Boolean;
begin
  if (FTileRect.Left<>0)
    or (FTileRect.Top<>0)
    or (FTileRect.Right<>0)
    or (FTileRect.Bottom<>0)
  then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetIsBitmapTiles: Boolean;
begin
  if SameText(TileFileExt, '.jpg')
    or SameText(TileFileExt, '.png')
    or SameText(TileFileExt, '.gif')
    or SameText(TileFileExt, '.bmp')
  then begin
    Result := true;
  end else begin
    Result := false;
  end;
end;

function TMapType.GetIsKmlTiles: Boolean;
begin
  if SameText(TileFileExt, '.kml')
    or SameText(TileFileExt, '.kmz')
  then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetIsHybridLayer: Boolean;
begin
  if asLayer and SameText(TileFileExt, '.png') then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetGUIDString: string;
begin
  Result := GUIDToString(FGuid);
end;

function TMapType.GetMIMETypeSubst(AMimeType: string): string;
var
  VNewMimeType: string;
begin
  Result := AMimeType;
  if FMimeTypeSubstList <> nil then begin
    VNewMimeType := FMimeTypeSubstList.Values[AMimeType];
    if Length(VNewMimeType) > 0 then begin
      Result := VNewMimeType;
    end;
  end;
end;

function TMapType.LoadFile(btm: TKmlInfoSimple; APath: string; caching:boolean): boolean;
begin
  Result := false;
  if GetFileSize(Apath)<=0 then begin
    exit;
  end;
  try
    GState.KmlLoader.LoadFromFile(Apath,  btm);
    Result := True;
  except
    Assert(False, 'Ошибка загрузки kml из файла:' + APath);
  end;
end;

function TMapType.LoadFile(btm: TCustomBitmap32; APath: string; caching:boolean): boolean;
begin
  Result := false;
  if GetFileSize(Apath)<=0 then begin
    exit;
  end;
  try
    if FBitmapLoaderFromStorage <> nil then begin
      FBitmapLoaderFromStorage.LoadFromFile(APath, btm);
    end else begin
      raise Exception.Create('У этой карты не растровые тайлы');
    end;
    result:=true;
  except
  end;
end;

function TMapType.LoadTileFromPreZ(spr: TCustomBitmap32; AXY: TPoint;
  Azoom: byte; caching: boolean): boolean;
var
  i: integer;
  bmp: TCustomBitmap32;
  VTileExists: Boolean;
  VTileTargetBounds:TRect;
  VTileSourceBounds:TRect;
  VTileParent: TPoint;
  VTargetTilePixelRect: TRect;
  VSourceTilePixelRect: TRect;
  VRelative: TExtendedPoint;
  VRelativeRect: TExtendedRect;
  VParentZoom: Byte;
begin
  result:=false;
  VTargetTilePixelRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
  VRelativeRect := FCoordConverter.PixelRect2RelativeRect(VTargetTilePixelRect, Azoom);
  VTileTargetBounds.Left := 0;
  VTileTargetBounds.Top := 0;
  VTileTargetBounds.Right := VTargetTilePixelRect.Right - VTargetTilePixelRect.Left + 1;
  VTileTargetBounds.Bottom := VTargetTilePixelRect.Bottom - VTargetTilePixelRect.Top + 1;
  spr.SetSize(VTileTargetBounds.Right, VTileTargetBounds.Bottom);
  if asLayer then spr.Clear(SetAlpha(Color32(GState.BGround),0))
             else spr.Clear(Color32(GState.BGround));
  if (not(GState.UsePrevZoom) and (asLayer=false)) or
  (not(GState.UsePrevZoomLayer) and (asLayer=true)) then
  begin
    exit;
  end;
  VTileExists := false;
  VRelative := FCoordConverter.TilePos2Relative(AXY, Azoom);
  VParentZoom := 0;
  for i:=Azoom - 1 downto 0 do begin
    VTileParent := FCoordConverter.Relative2Tile(VRelative, i);
    if TileExists(VTileParent, i) then begin
      VParentZoom := i;
      VTileExists := true;
      break;
    end;
  end;
  if not(VTileExists)or(Azoom - VParentZoom > 8) then begin
    if asLayer then spr.Clear(SetAlpha(Color32(GState.BGround),0))
               else spr.Clear(Color32(GState.BGround));
  end else begin
    if (not caching)or(not FCache.TryLoadTileFromCache(spr, AXY, Azoom)) then begin
      bmp:=TCustomBitmap32.Create;
      try
        if not(LoadTile(bmp, VTileParent, VParentZoom, true))then begin
          if asLayer then spr.Clear(SetAlpha(Color32(GState.BGround),0))
                     else spr.Clear(Color32(GState.BGround));
        end else begin
          bmp.Resampler := CreateResampler(GState.Resampling);
          VSourceTilePixelRect := FCoordConverter.TilePos2PixelRect(VTileParent, VParentZoom);
          VTargetTilePixelRect := FCoordConverter.RelativeRect2PixelRect(VRelativeRect, VParentZoom);
          VTileSourceBounds.Left := VTargetTilePixelRect.Left - VSourceTilePixelRect.Left;
          VTileSourceBounds.Top := VTargetTilePixelRect.Top - VSourceTilePixelRect.Top;
          VTileSourceBounds.Right := VTargetTilePixelRect.Right - VSourceTilePixelRect.Left + 1;
          VTileSourceBounds.Bottom := VTargetTilePixelRect.Bottom - VSourceTilePixelRect.Top + 1;
          try
            spr.Draw(VTileTargetBounds, VTileSourceBounds, bmp);
            FCache.AddTileToCache(spr, AXY, Azoom);
          except
            Result := false;
            Assert(False, 'Ошибка в рисовании из предыдущего уровня'+name);
            Exit;
          end;
        end;
      finally
        FreeAndNil(bmp);
      end;
    end;
    Result := true;
  end;
end;

function TMapType.LoadTile(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean): boolean;
var
  Path: string;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    if (not caching)or(not FCache.TryLoadTileFromCache(btm, AXY, Azoom)) then begin
      result:=GetGETile(btm, FCacheConfig.BasePath+'dbCache.dat',AXY.X, AXY.Y, Azoom + 1, Self);
      if ((result)and(caching)) then FCache.AddTileToCache(btm, AXY, Azoom);
    end else begin
      result:=true;
    end;
  end else begin
    path := FCacheConfig.GetTileFileName(AXY, Azoom);
    if (not caching)or(not FCache.TryLoadTileFromCache(btm, AXY, Azoom)) then begin
     result:=LoadFile(btm, path, caching);
     if ((result)and(caching)) then FCache.AddTileToCache(btm, AXY, Azoom);
    end else begin
      result:=true;
    end;
  end;
end;

function TMapType.LoadTile(btm: TKmlInfoSimple; AXY: TPoint; Azoom: byte;
  caching: boolean): boolean;
var path: string;
begin
  if FCacheConfig.EffectiveCacheType = 5 then begin
    raise Exception.Create('Из GE кеша можно получать только растры');
  end else begin
    path := FCacheConfig.GetTileFileName(AXY, Azoom);
    if (not caching)or(not FCache.TryLoadTileFromCache(btm, AXY, Azoom)) then begin
     result:=LoadFile(btm, path, caching);
     if ((result)and(caching)) then FCache.AddTileToCache(btm, AXY, Azoom);
    end else begin
      result:=true;
    end;
  end;
end;

function TMapType.LoadTileOrPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean; IgnoreError: Boolean): boolean;
var
  VRect: TRect;
  VSize: TPoint;
  bSpr:TCustomBitmap32;
  VBmp: TBitmap32;
begin
  if TileExists(AXY, Azoom) then begin
    VRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
    VSize := Point(VRect.Right - VRect.Left + 1, VRect.Bottom - VRect.Top + 1);
    Result := LoadTile(spr, AXY, Azoom, caching);
    if Result then begin
      if (spr.Width < VSize.X) or
        (spr.Height < VSize.Y) then begin
        bSpr:=TCustomBitmap32.Create;
        bSpr.Assign(spr);
        spr.SetSize(VSize.X, VSize.Y);
        spr.Draw(0,0,bSpr);
        bSpr.Free;
      end;
    end;
    if not Result then begin
      if IgnoreError then begin
        Result := LoadTileFromPreZ(spr, AXY, Azoom, caching);
      end else begin
        VBmp := TBitmap32.Create;
        try
          VBmp.SetSize(VSize.X, VSize.Y);
          if asLayer then begin
            VBmp.Clear(SetAlpha(Color32(GState.BGround),0));
          end else begin
            VBmp.Clear(Color32(GState.BGround));
          end;
          VBmp.RenderText(87,120,SAS_ERR_BadFile,0,clBlack32);
          spr.Assign(VBmp);
        finally
          VBmp.Free;
        end;
      end;
    end;
  end else begin
    Result := LoadTileFromPreZ(spr, AXY, Azoom, caching);
  end;
end;

function TMapType.LoadTileUni(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean; ACoordConverterTarget: ICoordConverter; AUsePre, AAllowPartial,
  IgnoreError: Boolean): boolean;
var
  VPixelRect: TRect;
begin
  VPixelRect := ACoordConverterTarget.TilePos2PixelRect(AXY, Azoom);
  Result := LoadBtimapUni(spr, VPixelRect, Azoom, caching, FCoordConverter, AUsePre, AAllowPartial, IgnoreError);
end;

function TMapType.LoadBtimap(spr: TCustomBitmap32; APixelRectTarget: TRect;
  Azoom: byte; caching, AUsePre, AAllowPartial,
  IgnoreError: Boolean): boolean;
var
  VTileRect: TRect;
  VTargetImageSize: TPoint;
  VPixelRectCurrTile: TRect;
  VAllTilesExits: Boolean;
  i, j: Integer;
  VTile: TPoint;
  VSpr:TCustomBitmap32;
  VLoadResult: Boolean;
  VSourceBounds: TRect;
  VTargetBounds: TRect;
begin
  Result := False;

  VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left + 1;
  VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top + 1;

  spr.SetSize(VTargetImageSize.X, VTargetImageSize.Y);

  VTileRect := FCoordConverter.PixelRect2TileRect(APixelRectTarget, Azoom);
  if (VTileRect.Left = VTileRect.Right) and
    (VTileRect.Top = VTileRect.Bottom)
  then begin
    VPixelRectCurrTile := FCoordConverter.TilePos2PixelRect(VTileRect.TopLeft, Azoom);
    if
      (VPixelRectCurrTile.Left = APixelRectTarget.Left) and
      (VPixelRectCurrTile.Top = APixelRectTarget.Top) and
      (VPixelRectCurrTile.Right = APixelRectTarget.Right) and
      (VPixelRectCurrTile.Bottom = APixelRectTarget.Bottom)
    then begin
      if AUsePre then begin
        Result := LoadTileOrPreZ(spr, VTileRect.TopLeft, Azoom, caching, IgnoreError);
      end else begin
        Result := LoadTile(spr, VTileRect.TopLeft, Azoom, caching);
      end;
      exit;
    end;
  end;

  if asLayer then begin
    spr.Clear(SetAlpha(Color32(GState.BGround),0));
  end else begin
    spr.Clear(Color32(GState.BGround));
  end;

  if not AAllowPartial and not AUsePre then begin
    VAllTilesExits := True;
    for i := VTileRect.Top to VTileRect.Bottom do begin
      VTile.Y := i;
      for j := VTileRect.Left to VTileRect.Right do begin
        VTile.X := j;
        VAllTilesExits := TileExists(VTile, Azoom);
        if not VAllTilesExits then Break;
      end;
      if not VAllTilesExits then Break;
    end;
    if not VAllTilesExits then begin
      Exit;
    end;
  end;
  VSpr := TCustomBitmap32.Create;
  try
    for i := VTileRect.Top to VTileRect.Bottom do begin
      VTile.Y := i;
      for j := VTileRect.Left to VTileRect.Right do begin
        VTile.X := j;
        if AUsePre then begin
          VLoadResult := LoadTileOrPreZ(VSpr, VTile, Azoom, caching, IgnoreError);
        end else begin
          VLoadResult := LoadTile(VSpr, VTile, Azoom, caching);
        end;
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
          Inc(VSourceBounds.Right);
          Inc(VSourceBounds.Bottom);

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
          Inc(VTargetBounds.Right);
          Inc(VTargetBounds.Bottom);

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

function TMapType.LoadBtimapUni(spr: TCustomBitmap32; APixelRectTarget: TRect;
  Azoom: byte; caching: boolean; ACoordConverterTarget: ICoordConverter;
  AUsePre, AAllowPartial, IgnoreError: Boolean): boolean;
var
  VSameCoordConvert: Boolean;
  VLonLatRectTarget: TExtendedRect;
  VTileRectInSource: TRect;
  VPixelRectOfTargetPixelRectInSource: TRect;
  VAllTilesExits: Boolean;
  i, j: Integer;
  VTile: TPoint;
  VSpr:TCustomBitmap32;
  VLoadResult: Boolean;
  VPixelRectCurTileInSource:  TRect;
  VLonLatRectCurTile:  TExtendedRect;
  VPixelRectCurTileInTarget:  TRect;
  VSourceBounds: TRect;
  VTargetBounds: TRect;
  VTargetImageSize: TPoint;
begin
  Result := False;
  VSameCoordConvert :=
    (ACoordConverterTarget.GetDatumEPSG <> 0) and
    (FCoordConverter.GetDatumEPSG <> 0) and
    (ACoordConverterTarget.GetProjectionEPSG <> 0) and
    (FCoordConverter.GetProjectionEPSG <> 0) and
    (ACoordConverterTarget.GetTileSplitCode <> 0) and
    (FCoordConverter.GetTileSplitCode <> 0) and
    (ACoordConverterTarget.GetDatumEPSG = FCoordConverter.GetDatumEPSG) and
    (ACoordConverterTarget.GetProjectionEPSG = FCoordConverter.GetProjectionEPSG) and
    (ACoordConverterTarget.GetTileSplitCode = FCoordConverter.GetTileSplitCode);

  if VSameCoordConvert then begin
    Result := LoadBtimap(spr, APixelRectTarget, Azoom, caching, AUsePre, AAllowPartial, IgnoreError);
  end else begin
    VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left + 1;
    VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top + 1;

    spr.SetSize(VTargetImageSize.X, VTargetImageSize.Y);

    if asLayer then begin
      spr.Clear(SetAlpha(Color32(GState.BGround),0));
    end else begin
      spr.Clear(Color32(GState.BGround));
    end;

    VLonLatRectTarget := ACoordConverterTarget.PixelRect2LonLatRect(APixelRectTarget, Azoom);
    VPixelRectOfTargetPixelRectInSource := FCoordConverter.LonLatRect2PixelRect(VLonLatRectTarget, Azoom);
    VTileRectInSource := FCoordConverter.PixelRect2TileRect(VPixelRectOfTargetPixelRectInSource, Azoom);

    if not AAllowPartial and not AUsePre then begin
      VAllTilesExits := True;
      for i := VTileRectInSource.Top to VTileRectInSource.Bottom do begin
        VTile.Y := i;
        for j := VTileRectInSource.Left to VTileRectInSource.Right do begin
          VTile.X := j;
          VAllTilesExits := TileExists(VTile, Azoom);
          if not VAllTilesExits then Break;
        end;
        if not VAllTilesExits then Break;
      end;
      if not VAllTilesExits then begin
        Exit;
      end;
    end;
    VSpr := TCustomBitmap32.Create;
    try
      for i := VTileRectInSource.Top to VTileRectInSource.Bottom do begin
        VTile.Y := i;
        for j := VTileRectInSource.Left to VTileRectInSource.Right do begin
          VTile.X := j;
          if AUsePre then begin
            VLoadResult := LoadTileOrPreZ(VSpr, VTile, Azoom, caching, IgnoreError);
          end else begin
            VLoadResult := LoadTile(VSpr, VTile, Azoom, caching);
          end;
          if VLoadResult then begin
            VPixelRectCurTileInSource := FCoordConverter.TilePos2PixelRect(VTile, Azoom);
            VLonLatRectCurTile := FCoordConverter.PixelRect2LonLatRect(VPixelRectCurTileInSource, Azoom);
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
            Inc(VSourceBounds.Right);
            Inc(VSourceBounds.Bottom);

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
            Inc(VTargetBounds.Right);
            Inc(VTargetBounds.Bottom);

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

end.
