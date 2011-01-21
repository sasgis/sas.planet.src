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
    FVersion: Variant;
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
    FLoadPrevMaxZoomDelta: Integer;

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
    procedure LoadMapIcons(AConfig : IConfigDataProvider);
    procedure LoadUrlScript(AConfig : IConfigDataProvider);
    procedure LoadDownloader(AConfig : IConfigDataProvider);
    procedure LoadProjectionInfo(AConfig : IConfigDataProvider);
    procedure LoadStorageParams(AConfig : IConfigDataProvider);
    procedure LoadWebSourceParams(AConfig : IConfigDataProvider);
    procedure LoadUIParams(AConfig : IConfigDataProvider);
    procedure LoadMapInfo(AConfig : IConfigDataProvider);
    procedure SaveTileDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileNotExists(AXY: TPoint; Azoom: byte);
    procedure CropOnDownload(ABtm: TCustomBitmap32; ATileSize: TPoint);
    procedure SaveBitmapTileToStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
    function LoadBitmapTileFromStorage(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32): Boolean;
    function LoadKmlTileFromStorage(AXY: TPoint; Azoom: byte; AKml: TKmlInfoSimple): boolean;
    procedure LoadMapType(AConfig : IConfigDataProvider; Apnum : Integer);

    procedure SaveTileKmlDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileBitmapDownload(AXY: TPoint; Azoom: byte; ATileStream: TCustomMemoryStream; AMimeType: string);
    function GetUseGenPrevious: boolean;
    function LoadTileFromPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte; caching: boolean; IgnoreError: Boolean): boolean;
   public
    FSortIndex: integer;
    HotKey: TShortCut;
    separator: boolean;
    ParentSubMenu: string;
    showinfo: boolean;

    function GetLink(AXY: TPoint; Azoom: byte): string;
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
    function GetTileShowName(AXY: TPoint; Azoom: byte): string;
    function TileExists(AXY: TPoint; Azoom: byte): Boolean;
    function TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;
    function LoadTile(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte; caching: boolean; IgnoreError: Boolean): boolean; overload;
    function LoadTile(btm: TKmlInfoSimple; AXY: TPoint; Azoom: byte; caching: boolean; IgnoreError: Boolean): boolean; overload;
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
    property Cache: ITileObjCache read FCache;

    constructor Create(AGUID: TGUID; AConfig: IConfigDataProvider; Apnum: Integer);
    destructor Destroy; override;
 end;

type
  TMapUpdateEvent = procedure(AMapType: TMapType) of object;
  TMapTileUpdateEvent = procedure(AMapType: TMapType; AZoom: Byte;
    ATile: TPoint) of object;

implementation

uses
  Types,
  GR32_Resamplers,
  KAZip,
  u_GlobalState,
  i_IObjectWithTTL,
  i_IPoolElement,
  i_ITileInfoBasic,
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
        ShowMessageFmt(SAS_ERR_UrlScriptError, [name, E.Message, ZmpFileName]);
        FUrlGenerator := nil;
        FUseDwn := False;
      end;
     else
      ShowMessageFmt(SAS_ERR_UrlScriptUnexpectedError, [name, ZmpFileName]);
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
    FStorage := TTileStorageGEStuped.Create(AConfig);
  end else begin
    FStorage := TTileStorageFileSystem.Create(AConfig);
  end;

  FCache := TTileCacheSimpleGlobal.Create(Self, GState.MainMemCache);
  FBitmapLoaderFromStorage := GState.BitmapTypeManager.GetBitmapLoaderForExt(TileStorage.TileFileExt);
  if FBitmapLoaderFromStorage <> nil then begin
    if FStorage.GetUseSave then begin
      FBitmapSaverToStorage := GState.BitmapTypeManager.GetBitmapSaverForExt(TileStorage.TileFileExt);
    end;
  end else begin
    if TileStorage.TileFileExt = '.kmz' then begin
      FKmlLoaderFromStorage := TKmzInfoSimpleParser.Create;
    end else if TileStorage.TileFileExt = '.kml' then begin
      FKmlLoaderFromStorage := TKmlInfoSimpleParser.Create;
    end;
  end;
end;

procedure TMapType.LoadProjectionInfo(AConfig: IConfigDataProvider);
var
  projection: byte;
  VConverter: TCoordConverterBasic;
  VParams: IConfigDataProvider;
  VRadiusA: Double;
  VRadiusB: Double;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  projection:= VParams.ReadInteger('projection',1);
  VRadiusA := VParams.ReadFloat('sradiusa',6378137);
  VRadiusB := VParams.ReadFloat('sradiusb',VRadiusA);
  case projection of
    1: VConverter := TCoordConverterMercatorOnSphere.Create(VRadiusA);
    2: VConverter := TCoordConverterMercatorOnEllipsoid.Create(VRadiusA, VRadiusB);
    3: VConverter := TCoordConverterSimpleLonLat.Create(VRadiusA, VRadiusB);
    else raise Exception.CreateFmt(SAS_ERR_MapProjectionUnexpectedType, [IntToStr(projection)]);
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

  FDefSleep:=VParams.ReadInteger('MAIN:Sleep',0);
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
  HotKey:=VParams.ReadInteger('HotKey',0);
  FDefHotKey := VParams.ReadInteger('MAIN:HotKey',0);
  ParentSubMenu:=VParams.ReadString('ParentSubMenu','');
  ParentSubMenu:=VParams.ReadString('ParentSubMenu_'+GState.LanguageManager.GetCurrentLanguageCode,ParentSubMenu);
  FDefParentSubMenu:=VParams.ReadString('MAIN:ParentSubMenu','');
  FDefParentSubMenu:=VParams.ReadString('MAIN:ParentSubMenu_'+GState.LanguageManager.GetCurrentLanguageCode, FDefParentSubMenu);
  separator:=VParams.ReadBool('separator',false);
  FDefseparator:=VParams.ReadBool('MAIN:separator',false);
  FSortIndex:=VParams.ReadInteger('pnum',-1);
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
        ShowMessageFmt(SAS_ERR_MapDownloadByError,[FZMPFileName, (ExceptObject as Exception).Message]);
      end;
      FTileDownlodSessionFactory := nil;
      FUseDwn := false;
    end;
  end;
  if FTileDownlodSessionFactory = nil then begin
    FTileDownlodSessionFactory := TTileDownloaderFactoryBase.Create(AConfig);
  end;
end;

procedure TMapType.LoadMapType(AConfig: IConfigDataProvider; Apnum: Integer);
var
  VParams: IConfigDataProvider;
begin
  FName:='map#'+inttostr(Apnum);
  FZMPFileName := AConfig.ReadString('::FileName', FName);
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FasLayer:= VParams.ReadBool('asLayer', false);
  LoadUIParams(AConfig);
  if FSortIndex < 0 then begin
    FSortIndex := 1000;
    showinfo := True;
  end else begin
    showinfo := False;
  end;

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
end;

function TMapType.GetLink(AXY: TPoint; Azoom: byte): string;
begin
  FCoordConverter.CheckTilePosStrict(AXY, Azoom, True);
  Result:=FUrlGenerator.GenLink(AXY.X, AXY.Y, Azoom);
end;

function TMapType.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Result := FStorage.GetTileFileName(AXY, Azoom, FVersion);
end;

function TMapType.TileExists(AXY: TPoint; Azoom: byte): Boolean;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersion);
  Result := VTileInfo.GetIsExists;
end;

function TMapType.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := FStorage.DeleteTile(AXY, Azoom, FVersion);
end;

function TMapType.TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersion);
  Result := VTileInfo.GetIsExistsTNE;
end;

procedure TMapType.SaveBitmapTileToStorage(AXY: TPoint; Azoom: byte;
  btm: TCustomBitmap32);
var
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    FBitmapSaverToStorage.SaveToStream(btm, VMemStream);
    FStorage.SaveTile(AXY, Azoom, FVersion, VMemStream);
  finally
    VMemStream.Free;
  end;
end;

function TMapType.LoadBitmapTileFromStorage(AXY: TPoint; Azoom: byte;
  btm: TCustomBitmap32): Boolean;
var
  VTileInfo: ITileInfoBasic;
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    Result := FStorage.LoadTile(AXY, Azoom, FVersion, VMemStream, VTileInfo);
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
  VTileInfo: ITileInfoBasic;
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    Result := FStorage.LoadTile(AXY, Azoom, FVersion, VMemStream, VTileInfo);
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
      FStorage.SaveTile(AXY, Azoom, FVersion, ATileStream);
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
          FStorage.SaveTile(AXY, Azoom, FVersion, VMemStream);
        finally
          VMemStream.Free;
        end;
      finally
        UnZip.Free;
      end;
    except
      try
        FStorage.SaveTile(AXY, Azoom, FVersion, ATileStream);
      except
      end;
    end;
  end else if (copy(ty,1,8)='text/xml')or(ty='application/vnd.google-earth.kml+xml') then begin
    FStorage.SaveTile(AXY, Azoom, FVersion, ATileStream);
  end;
end;

procedure TMapType.SaveTileDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
begin
  if FStorage.GetUseSave then begin
    if GetIsKmlTiles then begin
      SaveTileKmlDownload(AXY, Azoom, ATileStream, ty);
    end else if GetIsBitmapTiles then begin
      SaveTileBitmapDownload(AXY, Azoom, ATileStream, ty);
    end else begin
      raise Exception.Create('В этой карте неизвестный тип тайлов.');
    end;
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

function TMapType.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersion);
  Result := VTileInfo.GetLoadDate;
end;

function TMapType.TileSize(AXY: TPoint; Azoom: byte): integer;
var
  VTileInfo: ITileInfoBasic;
begin
  VTileInfo := FStorage.GetTileInfo(AXY, Azoom, FVersion);
  Result := VTileInfo.GetSize;
end;

procedure TMapType.SaveTileNotExists(AXY: TPoint; Azoom: byte);
begin
  FStorage.SaveTNE(AXY, Azoom, FVersion);
end;

procedure TMapType.SaveTileSimple(AXY: TPoint; Azoom: byte; btm: TCustomBitmap32);
begin
  SaveBitmapTileToStorage(AXY, Azoom, btm);
end;

function TMapType.TileExportToFile(AXY: TPoint; Azoom: byte;
  AFileName: string; OverWrite: boolean): boolean;
var
  VFileStream: TFileStream;
  VFileExists: Boolean;
  VExportPath: string;
  VTileInfo: ITileInfoBasic;
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
    VFileStream := TFileStream.Create(AFileName, fmCreate);
    try
      Result := FStorage.LoadTile(AXY, Azoom, FVersion, VFileStream, VTileInfo);
      if Result then begin
        FileSetDate(VFileStream.Handle, DateTimeToFileDate(VTileInfo.GetLoadDate));
      end;
    finally
      VFileStream.Free;
    end;
  end;
end;

function TMapType.LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint; Azoom,
  ASourceZoom: byte; IsStop: PBoolean): boolean;
begin
  Result := FStorage.LoadFillingMap(btm, AXY, Azoom, ASourceZoom, FVersion, IsStop);
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

constructor TMapType.Create(AGUID: TGUID; AConfig: IConfigDataProvider; Apnum: Integer);
begin
  FGuid := AGUID;
  FInitDownloadCS := TCriticalSection.Create;
  FCSSaveTile := TCriticalSection.Create;
  FCSSaveTNF := TCriticalSection.Create;
  FMimeTypeSubstList := nil;
  LoadMapType(AConfig, Apnum);
  if FasLayer then begin
    FLoadPrevMaxZoomDelta := 4;
  end else begin
    FLoadPrevMaxZoomDelta := 6;
  end;
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
  Result := FBitmapLoaderFromStorage <> nil;
end;

function TMapType.GetIsKmlTiles: Boolean;
begin
  Result := FKmlLoaderFromStorage <> nil;
end;

function TMapType.GetIsHybridLayer: Boolean;
begin
  Result := IsBitmapTiles and asLayer;
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
  caching: boolean; IgnoreError: Boolean): boolean;
begin
  try
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
  except
    if not IgnoreError then begin
      raise;
    end else begin
      Result := False;
    end;
  end;
end;

function TMapType.LoadTile(btm: TKmlInfoSimple; AXY: TPoint; Azoom: byte;
  caching: boolean; IgnoreError: Boolean): boolean;
begin
  try
    if (not caching)or(not FCache.TryLoadTileFromCache(btm, AXY, Azoom)) then begin
      result:=LoadKmlTileFromStorage(AXY, Azoom, btm);
      if ((result)and(caching)) then FCache.AddTileToCache(btm, AXY, Azoom);
    end else begin
      result:=true;
    end;
  except
    if not IgnoreError then begin
      raise;
    end else begin
      Result := False;
    end;
  end;
end;

function TMapType.LoadTileFromPreZ(spr: TCustomBitmap32; AXY: TPoint;
  Azoom: byte; caching: boolean; IgnoreError: Boolean): boolean;
var
  i: integer;
  VBmp: TCustomBitmap32;
  VTileTargetBounds:TRect;
  VTileSourceBounds:TRect;
  VTileParent: TPoint;
  VTargetTilePixelRect: TRect;
  VSourceTilePixelRect: TRect;
  VRelative: TDoublePoint;
  VRelativeRect: TDoubleRect;
  VParentZoom: Byte;
  VMinZoom: Integer;
begin
  result:=false;
  VRelative := FCoordConverter.TilePos2Relative(AXY, Azoom);
  VMinZoom :=  Azoom - FLoadPrevMaxZoomDelta;
  if VMinZoom < 0 then begin
    VMinZoom := 0;
  end;
  if Azoom - 1 > VMinZoom then begin
    VBmp:=TCustomBitmap32.Create;
    try
      for i := Azoom - 1 downto VMinZoom do begin
        VParentZoom := i;
        VTileParent := FCoordConverter.Relative2Tile(VRelative, i);
        if LoadTile(VBmp, VTileParent, VParentZoom, caching, IgnoreError)then begin
          VTargetTilePixelRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
          VRelativeRect := FCoordConverter.PixelRect2RelativeRect(VTargetTilePixelRect, Azoom);
          VTileTargetBounds.Left := 0;
          VTileTargetBounds.Top := 0;
          VTileTargetBounds.Right := VTargetTilePixelRect.Right - VTargetTilePixelRect.Left;
          VTileTargetBounds.Bottom := VTargetTilePixelRect.Bottom - VTargetTilePixelRect.Top;

          VBmp.Resampler := GState.ImageResamplerConfig.GetActiveFactory.CreateResampler;

          VSourceTilePixelRect := FCoordConverter.TilePos2PixelRect(VTileParent, VParentZoom);
          VTargetTilePixelRect := FCoordConverter.RelativeRect2PixelRect(VRelativeRect, VParentZoom);
          VTileSourceBounds.Left := VTargetTilePixelRect.Left - VSourceTilePixelRect.Left;
          VTileSourceBounds.Top := VTargetTilePixelRect.Top - VSourceTilePixelRect.Top;
          VTileSourceBounds.Right := VTargetTilePixelRect.Right - VSourceTilePixelRect.Left;
          VTileSourceBounds.Bottom := VTargetTilePixelRect.Bottom - VSourceTilePixelRect.Top;
          try
            spr.SetSize(VTileTargetBounds.Right, VTileTargetBounds.Bottom);
            spr.Draw(VTileTargetBounds, VTileSourceBounds, VBmp);
            Result := true;
            if caching then begin
              FCache.AddTileToCache(spr, AXY, Azoom);
            end;
            Break;
          except
            if not IgnoreError then begin
              raise
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(VBmp);
    end;
  end;
end;

function TMapType.LoadTileOrPreZ(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean; IgnoreError: Boolean; AUsePre: Boolean): boolean;
var
  VRect: TRect;
  VSize: TPoint;
  bSpr:TCustomBitmap32;
begin
  VRect := FCoordConverter.TilePos2PixelRect(AXY, Azoom);
  VSize := Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
  Result := LoadTile(spr, AXY, Azoom, caching, IgnoreError);
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
    if AUsePre then begin
      Result := LoadTileFromPreZ(spr, AXY, Azoom, caching, IgnoreError);
    end;
  end;
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

  VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
  VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

  VTileRect := FCoordConverter.PixelRect2TileRect(APixelRectTarget, Azoom);
  if (VTileRect.Left = VTileRect.Right - 1) and
    (VTileRect.Top = VTileRect.Bottom - 1)
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

  spr.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
  if asLayer then begin
    spr.Clear(SetAlpha(Color32(GState.BGround),0));
  end else begin
    spr.Clear(Color32(GState.BGround));
  end;

  VSpr := TCustomBitmap32.Create;
  try
    for i := VTileRect.Top to VTileRect.Bottom - 1 do begin
      VTile.Y := i;
      for j := VTileRect.Left to VTileRect.Right - 1 do begin
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
  VLonLatRectTarget: TDoubleRect;
  VTileRectInSource: TRect;
  VPixelRectOfTargetPixelRectInSource: TRect;
  VAllTilesExits: Boolean;
  i, j: Integer;
  VTile: TPoint;
  VSpr:TCustomBitmap32;
  VLoadResult: Boolean;
  VPixelRectCurTileInSource:  TRect;
  VLonLatRectCurTile:  TDoubleRect;
  VPixelRectCurTileInTarget:  TRect;
  VSourceBounds: TRect;
  VTargetBounds: TRect;
  VTargetImageSize: TPoint;
begin
  Result := False;

  if FCoordConverter.IsSameConverter(ACoordConverterTarget) then begin
    Result := LoadBtimap(spr, APixelRectTarget, Azoom, caching, AUsePre, AAllowPartial, IgnoreError);
  end else begin
    VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
    VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

    spr.SetSize(VTargetImageSize.X, VTargetImageSize.Y);

    if asLayer then begin
      spr.Clear(SetAlpha(Color32(GState.BGround),0));
    end else begin
      spr.Clear(Color32(GState.BGround));
    end;

    VLonLatRectTarget := ACoordConverterTarget.PixelRect2LonLatRect(APixelRectTarget, Azoom);
    VPixelRectOfTargetPixelRectInSource := FCoordConverter.LonLatRect2PixelRect(VLonLatRectTarget, Azoom);
    VTileRectInSource := FCoordConverter.PixelRect2TileRect(VPixelRectOfTargetPixelRectInSource, Azoom);

    VSpr := TCustomBitmap32.Create;
    try
      for i := VTileRectInSource.Top to VTileRectInSource.Bottom - 1 do begin
        VTile.Y := i;
        for j := VTileRectInSource.Left to VTileRectInSource.Right - 1 do begin
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

function TMapType.LoadTileUni(spr: TCustomBitmap32; AXY: TPoint; Azoom: byte;
  caching: boolean; ACoordConverterTarget: ICoordConverter; AUsePre, AAllowPartial,
  IgnoreError: Boolean): boolean;
var
  VPixelRect: TRect;
begin
  VPixelRect := ACoordConverterTarget.TilePos2PixelRect(AXY, Azoom);
  Result := LoadBtimapUni(spr, VPixelRect, Azoom, caching, ACoordConverterTarget, AUsePre, AAllowPartial, IgnoreError);
end;

end.


