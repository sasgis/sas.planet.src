unit UMapType;

interface

uses
  Windows,
  sysutils,
  Classes,
  Dialogs,
  Graphics,
  SyncObjs,
  GR32,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_ITileObjCache,
  i_ICoordConverter,
  i_ITileDownlodSession,
  i_IPoolOfObjectsSimple,
  i_IBitmapTypeExtManager,
  i_BitmapTileSaveLoad,
  i_IKmlInfoSimpleLoader,
  i_IAntiBan,
  u_KmlInfoSimple,
  u_UrlGenerator,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract,
  UResStrings;

type
  EBadGUID = class(Exception);

 TMapType = class
   private
    FGuid: TGUID;
    FName: string;
    FasLayer: boolean;
    FTileRect: TRect;
    FZMPFileName: string;
    FMapInfo: string;
    FDefHotKey: TShortCut;
    FDefSleep: Integer;
    FDefseparator: boolean;
    FDefParentSubMenu: string;
    FUseDwn: boolean;
    FIsCanShowOnSmMap: Boolean;
    FUseStick: boolean;
    FUseGenPrevious: boolean;
    Fbmp18: TBitmap;
    Fbmp24: TBitmap;
    FMaxConnectToServerCount: Cardinal;
    FAntiBan: IAntiBan;
    FMimeTypeSubstList: TStringList;
    FCache: ITileObjCache;
    FStorage: TTileStorageAbstract;
    FUrlGenerator : TUrlGeneratorBasic;
    FBitmapLoaderFromStorage: IBitmapTileLoader;
    FBitmapSaverToStorage: IBitmapTileSaver;
    FKmlLoaderFromStorage: IKmlInfoSimpleLoader;
    FInitDownloadCS: TCriticalSection;
    FCSSaveTile: TCriticalSection;
    FCSSaveTNF: TCriticalSection;
    FCoordConverter : ICoordConverter;
    FConverterForUrlGenerator: ICoordConverterSimple;
    FPoolOfDownloaders: IPoolOfObjectsSimple;
    FTileDownlodSessionFactory: ITileDownlodSessionFactory;
    function GetCoordConverter: ICoordConverter;
    function GetUseDwn: Boolean;
    function GetZmpFileName: string;
    function GetIsCanShowOnSmMap: boolean;
    function GetUseStick: boolean;
    function GetIsCropOnDownload: Boolean;
    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
    function GetIsHybridLayer: Boolean;
    function GetGUIDString: string;
    function GetMIMETypeSubst(AMimeType: string): string;
    procedure LoadMimeTypeSubstList(AConfig : IConfigDataProvider);
    procedure LoadGUIDFromIni(AConfig : IConfigDataProvider);
    procedure LoadMapIcons(AConfig : IConfigDataProvider);
    procedure LoadUrlScript(AConfig : IConfigDataProvider);
    procedure LoadDownloader(AConfig : IConfigDataProvider);
    procedure LoadProjectionInfo(AConfig : IConfigDataProvider);
    procedure LoadStorageParams(AConfig : IConfigDataProvider);
    procedure LoadWebSourceParams(AConfig : IConfigDataProvider);
    procedure LoadUIParams(AConfig : IConfigDataProvider);
    procedure LoadMapInfo(AConfig : IConfigDataProvider);
    procedure LoadGlobalConfig(AConfig : IConfigDataProvider);
    procedure SaveTileDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileNotExists(AXY: TPoint; Azoom: byte);
    procedure CropOnDownload(ABtm: TCustomBitmap32; ATileSize: TPoint);
    procedure SaveBitmapTileToStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
    function LoadBitmapTileFromStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32): Boolean;
    function LoadKmlTileFromStorage(AXY: TPoint; Azoom: byte; AKml: TKmlInfoSimple): boolean;

    procedure SaveTileKmlDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileBitmapDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; AMimeType: string);
    function GetUseGenPrevious: boolean;
   public
    id: integer;
    HotKey: TShortCut;
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
    function LoadTileOrPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte; caching: boolean; IgnoreError: Boolean; AUsePre: Boolean): boolean;
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

    property asLayer: boolean read FasLayer;
    property IsBitmapTiles: Boolean read GetIsBitmapTiles;
    property IsKmlTiles: Boolean read GetIsKmlTiles;
    property IsHybridLayer: Boolean read GetIsHybridLayer;
    property UseDwn: Boolean read GetUseDwn;
    property UseGenPrevious: boolean read GetUseGenPrevious;
    property IsCanShowOnSmMap: boolean read GetIsCanShowOnSmMap;
    property UseStick: boolean read GetUseStick;
    property IsCropOnDownload: Boolean read GetIsCropOnDownload;

    property ZmpFileName: string read GetZmpFileName;
    property bmp18: TBitmap read Fbmp18;
    property bmp24: TBitmap read Fbmp24;
    property TileStorage: TTileStorageAbstract read FStorage;
    property UrlGenerator : TUrlGeneratorBasic read FUrlGenerator;
    property MapInfo: string read FMapInfo;
    property Name: string read FName;
    property DefHotKey: TShortCut read FDefHotKey;
    property DefSleep: Integer read FDefSleep;
    property Defseparator: boolean read FDefseparator;
    property DefParentSubMenu: string read FDefParentSubMenu;
    property DownloaderFactory: ITileDownlodSessionFactory read FTileDownlodSessionFactory;

    constructor Create;
    destructor Destroy; override;
    procedure LoadMapType(AConfig, AAllMapsConfig : IConfigDataProvider; Apnum : Integer);
 end;

implementation

uses
  Types,
  GR32_Resamplers,
  KAZip,
  u_GlobalState,
  u_GeoToStr,
  UIMGFun,
  i_IObjectWithTTL,
  i_IPoolElement,
  u_PoolOfObjectsSimple,
  u_TileDownloaderBaseFactory,
  u_AntiBanStuped,
  u_TileCacheSimpleGlobal,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_GECache,
  u_TileStorageGEStuped,
  u_TileStorageFileSystem,
  u_CoordConverterBasic,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid,
  u_CoordConverterSimpleLonLat;

procedure TMapType.LoadGlobalConfig(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem(GUIDString);
  if VParams <> nil then begin
      id:=VParams.ReadInteger('pnum',id);
      FUrlGenerator.URLBase:=VParams.ReadString('URLBase',FUrlGenerator.URLBase);
      TileStorage.CacheConfig.CacheType:=VParams.ReadInteger('CacheType',TileStorage.CacheConfig.cachetype);
      TileStorage.CacheConfig.NameInCache:=VParams.ReadString('NameInCache',TileStorage.CacheConfig.NameInCache);
      HotKey:=VParams.ReadInteger('HotKey',HotKey);
      ParentSubMenu:=VParams.ReadString('ParentSubMenu',ParentSubMenu);
      DownloaderFactory.WaitInterval:=VParams.ReadInteger('Sleep',DownloaderFactory.WaitInterval);
      separator:=VParams.ReadBool('separator',separator);
      showinfo:=false;
  end else begin
      showinfo:=true;
      if id < 0 then id := 1000;
  end;
end;

procedure TMapType.LoadGUIDFromIni(AConfig: IConfigDataProvider);
var
  VGUIDStr: String;
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  VGUIDStr := VParams.ReadString('GUID', '');
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

procedure TMapType.LoadMapIcons(AConfig: IConfigDataProvider);
var
  VStream:TMemoryStream;
begin
  Fbmp24:=TBitmap.create;
  VStream:=TMemoryStream.Create;
  try
    try
      AConfig.ReadBinaryStream('24.bmp', VStream);
      VStream.Position:=0;
      Fbmp24.LoadFromStream(VStream);
    except
      Fbmp24.Canvas.FillRect(Fbmp24.Canvas.ClipRect);
      Fbmp24.Width:=24;
      Fbmp24.Height:=24;
      Fbmp24.Canvas.TextOut(7,3,copy(name,1,1));
    end;
  finally
    FreeAndNil(VStream);
  end;
  Fbmp18:=TBitmap.create;
  VStream:=TMemoryStream.Create;
  try
    try
      AConfig.ReadBinaryStream('18.bmp', VStream);
      VStream.Position:=0;
      Fbmp18.LoadFromStream(VStream);
    except
      Fbmp18.Canvas.FillRect(Fbmp18.Canvas.ClipRect);
      Fbmp18.Width:=18;
      Fbmp18.Height:=18;
      Fbmp18.Canvas.TextOut(3,2,copy(name,1,1));
    end;
  finally
    FreeAndNil(VStream);
  end;
end;

procedure TMapType.LoadUrlScript(AConfig: IConfigDataProvider);
begin
  if FUseDwn then begin
    try
      FUrlGenerator := TUrlGenerator.Create(AConfig, FConverterForUrlGenerator);
      //GetLink(0,0,0);
    except
      on E: Exception do begin
        ShowMessage('Ошибка скрипта карты '+name+' :'+#13#10+ E.Message);
        FUrlGenerator := nil;
        FUseDwn := False;
      end;
     else
      ShowMessage('Ошибка скрипта карты '+name+' :'+#13#10+'Неожиданная ошибка');
      FUrlGenerator := nil;
      FUseDwn := False;
    end;
  end;
  if FUrlGenerator = nil then begin
    FUrlGenerator := TUrlGeneratorBasic.Create(AConfig);
  end;
end;

procedure TMapType.LoadMapInfo(AConfig: IConfigDataProvider);
begin
  FMapinfo := AConfig.ReadString('info_'+GState.LanguageManager.GetCurrentLanguageCode+'.txt', '');
  if FMapInfo = '' then begin
    FMapinfo := AConfig.ReadString('info.txt', '');
  end;
end;

procedure TMapType.LoadStorageParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  if VParams.ReadInteger('CacheType', 0) = 5  then begin
    FStorage := TTileStorageGEStuped.Create(FCoordConverter);
  end else begin
    FStorage := TTileStorageFileSystem.Create(FCoordConverter, AConfig);
  end;

  FCache := TTileCacheSimpleGlobal.Create(Self);
  if GetIsBitmapTiles then begin
    FBitmapLoaderFromStorage := GState.BitmapTypeManager.GetBitmapLoaderForExt(TileStorage.TileFileExt);
    FBitmapSaverToStorage := GState.BitmapTypeManager.GetBitmapSaverForExt(TileStorage.TileFileExt);
  end else begin
    if GetIsKmlTiles then begin
      if TileStorage.TileFileExt = '.kmz' then begin
        FKmlLoaderFromStorage := TKmzInfoSimpleParser.Create;
      end else begin
        FKmlLoaderFromStorage := TKmlInfoSimpleParser.Create;
      end;
    end;
  end;
end;

procedure TMapType.LoadProjectionInfo(AConfig: IConfigDataProvider);
var
  bfloat:string;
  projection: byte;
  VConverter: TCoordConverterBasic;
  VParams: IConfigDataProvider;
  VRadiusA: extended;
  VRadiusB: extended;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  projection:=VParams.ReadInteger('projection',1);
  bfloat:=VParams.ReadString('sradiusa','6378137');
  VRadiusA:=str2r(bfloat);
  bfloat:=VParams.ReadString('sradiusb',FloatToStr(VRadiusA));
  VRadiusB:=str2r(bfloat);
  case projection of
    1: VConverter := TCoordConverterMercatorOnSphere.Create(VRadiusA);
    2: VConverter := TCoordConverterMercatorOnEllipsoid.Create(VRadiusA, VRadiusB);
    3: VConverter := TCoordConverterSimpleLonLat.Create(VRadiusA, VRadiusB);
    else raise Exception.Create('Ошибочный тип проэкции карты ' + IntToStr(projection));
  end;
  FCoordConverter := VConverter;
  FConverterForUrlGenerator := VConverter;
end;

procedure TMapType.LoadMimeTypeSubstList(AConfig: IConfigDataProvider);
var
  VMimeTypeSubstText: string;
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  VMimeTypeSubstText := VParams.ReadString('MimeTypeSubst', '');
  if Length(VMimeTypeSubstText) > 0 then begin
    FMimeTypeSubstList := TStringList.Create;
    FMimeTypeSubstList.Delimiter := ';';
    FMimeTypeSubstList.DelimitedText := VMimeTypeSubstText;
    if FMimeTypeSubstList.Count = 0 then begin
      FreeAndNil(FMimeTypeSubstList);
    end;
  end;
end;

procedure TMapType.LoadWebSourceParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FTileRect.Left:=VParams.ReadInteger('TileRLeft',0);
  FTileRect.Top:=VParams.ReadInteger('TileRTop',0);
  FTileRect.Right:=VParams.ReadInteger('TileRRight',0);
  FTileRect.Bottom:=VParams.ReadInteger('TileRBottom',0);

  FDefSleep:=VParams.ReadInteger('Sleep',0);
  FUseDwn:=VParams.ReadBool('UseDwn',true);
end;

procedure TMapType.LoadUIParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FName:=VParams.ReadString('name',FName);
  FName:=VParams.ReadString('name_'+GState.LanguageManager.GetCurrentLanguageCode,FName);
  FIsCanShowOnSmMap := VParams.ReadBool('CanShowOnSmMap', true);
  HotKey:=VParams.ReadInteger('DefHotKey',0);
  FDefHotKey:=HotKey;
  ParentSubMenu:=VParams.ReadString('ParentSubMenu','');
  ParentSubMenu:=VParams.ReadString('ParentSubMenu_'+GState.LanguageManager.GetCurrentLanguageCode,ParentSubMenu);
  FDefParentSubMenu:=ParentSubMenu;
  separator:=VParams.ReadBool('separator',false);
  FDefseparator:=separator;
  id:=VParams.ReadInteger('pnum',-1);
end;

procedure TMapType.LoadDownloader(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
  VDownloader: TTileDownloaderFactory;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  if FUseDwn then begin
    try
      FMaxConnectToServerCount := VParams.ReadInteger('MaxConnectToServerCount', 1);
      if FMaxConnectToServerCount > 64 then begin
        FMaxConnectToServerCount := 64;
      end;
      if FMaxConnectToServerCount <= 0 then begin
        FMaxConnectToServerCount := 1;
      end;
      VDownloader := TTileDownloaderFactory.Create(AConfig);
      FTileDownlodSessionFactory := VDownloader;
      FPoolOfDownloaders := TPoolOfObjectsSimple.Create(FMaxConnectToServerCount, VDownloader, 60000, 60000);
      GState.GCThread.List.AddObject(FPoolOfDownloaders as IObjectWithTTL);
      FAntiBan := TAntiBanStuped.Create(AConfig);
    except
      if ExceptObject <> nil then begin
        ShowMessageFmt('Для карты %0:s отключена загрузка тайлов из-за ошибки: %1:s',[FZMPFileName, (ExceptObject as Exception).Message]);
      end;
      FTileDownlodSessionFactory := nil;
      FUseDwn := false;
    end;
  end;
  if FTileDownlodSessionFactory = nil then begin
    FTileDownlodSessionFactory := TTileDownloaderFactoryBase.Create(AConfig);
  end;
end;

procedure TMapType.LoadMapType(AConfig, AAllMapsConfig: IConfigDataProvider; Apnum: Integer);
var
  VParams: IConfigDataProvider;
begin
  FName:='map#'+inttostr(Apnum);
  FZMPFileName := AConfig.ReadString('::FileName', FName);
  LoadGUIDFromIni(AConfig);
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FasLayer:= VParams.ReadBool('asLayer', false);
  LoadUIParams(AConfig);
  LoadMapInfo(AConfig);
  LoadProjectionInfo(AConfig);
  LoadStorageParams(AConfig);
  LoadMapIcons(AConfig);
  LoadWebSourceParams(AConfig);
  FUsestick:=VParams.ReadBool('Usestick',true);
  FUseGenPrevious:=VParams.ReadBool('UseGenPrevious',true);
  LoadMimeTypeSubstList(AConfig);
  LoadUrlScript(AConfig);
  LoadDownloader(AConfig);
  LoadGlobalConfig(AAllMapsConfig);
end;

function TMapType.GetLink(AXY: TPoint; Azoom: byte): string;
begin
  FCoordConverter.CheckTilePosStrict(AXY, Azoom, True);
  Result:=FUrlGenerator.GenLink(AXY.X, AXY.Y, Azoom);
end;

function TMapType.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Result := FStorage.GetTileFileName(AXY, Azoom);
end;

function TMapType.TileExists(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := FStorage.ExistsTile(AXY, Azoom);
end;

function TMapType.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := FStorage.DeleteTile(AXY, Azoom);
end;

function TMapType.TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := FStorage.ExistsTNE(AXY, Azoom);
end;

procedure TMapType.SaveBitmapTileToStorage(AXY: TPoint; Azoom: byte;
  btm: TCustomBitmap32);
var
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    FBitmapSaverToStorage.SaveToStream(btm, VMemStream);
    FStorage.SaveTile(AXY, Azoom, VMemStream);
  finally
    VMemStream.Free;
  end;
end;

function TMapType.LoadBitmapTileFromStorage(AXY: TPoint; Azoom: byte;
  btm: TCustomBitmap32): Boolean;
var
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    Result := FStorage.LoadTile(AXY, Azoom, VMemStream);
    if Result then begin
      FBitmapLoaderFromStorage.LoadFromStream(VMemStream, btm);
    end;
  finally
    VMemStream.Free;
  end;
end;

function TMapType.LoadKmlTileFromStorage(AXY: TPoint; Azoom: byte;
  AKml: TKmlInfoSimple): boolean;
var
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    Result := FStorage.LoadTile(AXY, Azoom, VMemStream);
    if Result then  begin
      FKmlLoaderFromStorage.LoadFromStream(VMemStream, AKml);
    end;
  finally
    VMemStream.Free;
  end;
end;

procedure TMapType.SaveTileBitmapDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; AMimeType: string);
var
  btmSrc:TCustomBitmap32;
  VManager: IBitmapTypeExtManager;
  VMimeType: String;
begin
  VManager := GState.BitmapTypeManager;
  VMimeType := GetMIMETypeSubst(AMimeType);
  if VManager.GetIsBitmapType(VMimeType) then begin
    if not IsCropOnDownload and SameText(FStorage.TileFileExt, VManager.GetExtForType(VMimeType)) then begin
      FStorage.SaveTile(AXY, Azoom, ATileStream);
    end else begin
      btmsrc := TCustomBitmap32.Create;
      try
        ATileStream.Position := 0;
        VManager.GetBitmapLoaderForType(VMimeType).LoadFromStream(ATileStream, btmSrc);

        if IsCropOnDownload then begin
          CropOnDownload(btmSrc, FCoordConverter.GetTileSize(AXY, Azoom));
        end;
        SaveBitmapTileToStorage(AXY, Azoom, btmSrc);
      finally
        FreeAndNil(btmSrc);
      end;
    end;
    FCache.DeleteTileFromCache(AXY, Azoom);
  end else begin
    raise Exception.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AMimeType]);
  end;
end;

procedure TMapType.SaveTileKmlDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
var
  UnZip:TKAZip;
  VMemStream: TMemoryStream;
begin
  if (ty='application/vnd.google-earth.kmz') then begin
    try
      UnZip:=TKAZip.Create(nil);
      try
        UnZip.Open(ATileStream);
        VMemStream := TMemoryStream.Create;
        try
          UnZip.Entries.Items[0].ExtractToStream(VMemStream);
          FStorage.SaveTile(AXY, Azoom, VMemStream);
        finally
          VMemStream.Free;
        end;
      finally
        UnZip.Free;
      end;
    except
      try
        FStorage.SaveTile(AXY, Azoom, ATileStream);
      except
      end;
    end;
  end else if (copy(ty,1,8)='text/xml')or(ty='application/vnd.google-earth.kml+xml') then begin
    FStorage.SaveTile(AXY, Azoom, ATileStream);
  end;
end;

procedure TMapType.SaveTileDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
begin
  if FStorage.GetUseSave then begin
    if GetIsKmlTiles then begin
      SaveTileKmlDownload(AXY, Azoom, ATileStream, ty);
    end else begin
      SaveTileBitmapDownload(AXY, Azoom, ATileStream, ty);
    end;
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

function TMapType.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
begin
  Result := FStorage.TileLoadDate(AXY, Azoom);
end;

function TMapType.TileSize(AXY: TPoint; Azoom: byte): integer;
begin
  Result := FStorage.TileSize(AXY, Azoom);
end;

procedure TMapType.SaveTileNotExists(AXY: TPoint; Azoom: byte);
begin
  FStorage.SaveTNE(AXY, Azoom);
end;

procedure TMapType.SaveTileSimple(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
begin
  SaveBitmapTileToStorage(AXY, Azoom, btm);
end;

function TMapType.TileExportToFile(AXY: TPoint; Azoom: byte;
  AFileName: string; OverWrite: boolean): boolean;
var
  VTileTime: TDateTime;
  VFileStream: TFileStream;
  VFileExists: Boolean;
  VExportPath: string;
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
    VTileTime := FStorage.TileLoadDate(AXY, Azoom);
    VFileStream := TFileStream.Create(AFileName, fmCreate);
    try
      FileSetDate(VFileStream.Handle, DateTimeToFileDate(VTileTime));
      Result := FStorage.LoadTile(AXY, Azoom, VFileStream);
    finally
      VFileStream.Free;
    end;
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
  Result := ExtractFileName(ExtractFileDir(IncludeTrailingPathDelimiter(FStorage.CacheConfig.NameInCache)));
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
  FreeAndNil(FStorage);
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
  if FStorage.GetIsStoreFileCache then begin
    Result := FStorage.CacheConfig.GetTileFileName(AXY, Azoom)
  end else begin
    Result := 'z' + IntToStr(Azoom + 1) + 'x' + IntToStr(AXY.X) + 'y' + IntToStr(AXY.Y);
  end;
end;

function TMapType.GetUseDwn: Boolean;
begin
  if FStorage.GetUseSave then begin
    Result := FUseDwn;
  end else begin
    Result := false;
  end;
end;

function TMapType.GetUseGenPrevious: boolean;
begin
  Result := False;
  if FStorage.GetUseSave then begin
    if GetIsBitmapTiles then begin
      Result := FUseGenPrevious;
    end;
  end;
end;

function TMapType.GetZmpFileName: string;
begin
  Result := ExtractFileName(FZMPFileName);
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
  if SameText(FStorage.TileFileExt, '.jpg')
    or SameText(FStorage.TileFileExt, '.png')
    or SameText(FStorage.TileFileExt, '.gif')
    or SameText(FStorage.TileFileExt, '.bmp')
  then begin
    Result := true;
  end else begin
    Result := false;
  end;
end;

function TMapType.GetIsKmlTiles: Boolean;
begin
  if SameText(FStorage.TileFileExt, '.kml')
    or SameText(FStorage.TileFileExt, '.kmz')
  then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetIsHybridLayer: Boolean;
begin
  if asLayer and SameText(FStorage.TileFileExt, '.png') then begin
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

function TMapType.LoadTile(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean): boolean;
begin
  if (not caching)or(not FCache.TryLoadTileFromCache(btm, AXY, Azoom)) then begin
    if FStorage.CacheConfig.EffectiveCacheType = 5 then begin
      result:=GetGETile(btm, FStorage.CacheConfig.BasePath+'dbCache.dat',AXY.X, AXY.Y, Azoom + 1, FCoordConverter);
    end else begin
      result:=LoadBitmapTileFromStorage(AXY, Azoom, btm);
    end;
    if ((result)and(caching)) then FCache.AddTileToCache(btm, AXY, Azoom);
  end else begin
    result:=true;
  end;
end;

function TMapType.LoadTile(btm: TKmlInfoSimple; AXY: TPoint; Azoom: byte;
  caching: boolean): boolean;
begin
  if (not caching)or(not FCache.TryLoadTileFromCache(btm, AXY, Azoom)) then begin
    result:=LoadKmlTileFromStorage(AXY, Azoom, btm);
    if ((result)and(caching)) then FCache.AddTileToCache(btm, AXY, Azoom);
  end else begin
    result:=true;
  end;
end;

function TMapType.LoadTileFromPreZ(spr: TCustomBitmap32; AXY: TPoint;
  Azoom: byte; caching: boolean): boolean;
var
  i: integer;
  VBmp: TCustomBitmap32;
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
      VBmp:=TCustomBitmap32.Create;
      try
        if not(LoadTile(VBmp, VTileParent, VParentZoom, true))then begin
          if asLayer then spr.Clear(SetAlpha(Color32(GState.BGround),0))
                     else spr.Clear(Color32(GState.BGround));
        end else begin
          VBmp.Resampler := CreateResampler(GState.Resampling);
          VSourceTilePixelRect := FCoordConverter.TilePos2PixelRect(VTileParent, VParentZoom);
          VTargetTilePixelRect := FCoordConverter.RelativeRect2PixelRect(VRelativeRect, VParentZoom);
          VTileSourceBounds.Left := VTargetTilePixelRect.Left - VSourceTilePixelRect.Left;
          VTileSourceBounds.Top := VTargetTilePixelRect.Top - VSourceTilePixelRect.Top;
          VTileSourceBounds.Right := VTargetTilePixelRect.Right - VSourceTilePixelRect.Left + 1;
          VTileSourceBounds.Bottom := VTargetTilePixelRect.Bottom - VSourceTilePixelRect.Top + 1;
          try
            spr.Draw(VTileTargetBounds, VTileSourceBounds, VBmp);
            FCache.AddTileToCache(spr, AXY, Azoom);
          except
            Result := false;
            Assert(False, 'Ошибка в рисовании из предыдущего уровня'+name);
            Exit;
          end;
        end;
      finally
        FreeAndNil(VBmp);
      end;
    end;
    Result := true;
  end;
end;

function TMapType.LoadTileOrPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean; IgnoreError: Boolean; AUsePre: Boolean): boolean;
var
  VRect: TRect;
  VSize: TPoint;
  bSpr:TCustomBitmap32;
  VBmp: TBitmap32;
begin
  Result := False;
  VRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
  VSize := Point(VRect.Right - VRect.Left + 1, VRect.Bottom - VRect.Top + 1);
  if TileExists(AXY, Azoom) then begin
    Result := LoadTile(spr, AXY, Azoom, caching);
    if Result then begin
      if (spr.Width < VSize.X) or
        (spr.Height < VSize.Y) then begin
        bSpr:=TCustomBitmap32.Create;
        try
          bSpr.Assign(spr);
          spr.SetSize(VSize.X, VSize.Y);
          if asLayer then begin
            spr.Clear(SetAlpha(Color32(GState.BGround),0));
          end else begin
            spr.Clear(Color32(GState.BGround));
          end;
          spr.Draw(0,0,bSpr);
        finally
          bSpr.Free;
        end;
      end;
    end;
    if not Result then begin
      if IgnoreError then begin
        if AUsePre then begin
          Result := LoadTileFromPreZ(spr, AXY, Azoom, caching);
        end else begin
          spr.SetSize(VSize.X, VSize.Y);
          if asLayer then begin
            spr.Clear(SetAlpha(Color32(GState.BGround),0));
          end else begin
            spr.Clear(Color32(GState.BGround));
          end;
        end;
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
    if AUsePre then begin
      Result := LoadTileFromPreZ(spr, AXY, Azoom, caching);
    end else begin
      spr.SetSize(VSize.X, VSize.Y);
      if asLayer then begin
        spr.Clear(SetAlpha(Color32(GState.BGround),0));
      end else begin
        spr.Clear(Color32(GState.BGround));
      end;
    end;
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
      Result := LoadTileOrPreZ(spr, VTileRect.TopLeft, Azoom, caching, IgnoreError, AUsePre);
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
        VLoadResult := LoadTileOrPreZ(VSpr, VTile, Azoom, caching, IgnoreError, AUsePre);
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
          VLoadResult := LoadTileOrPreZ(VSpr, VTile, Azoom, caching, IgnoreError, AUsePre);
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
