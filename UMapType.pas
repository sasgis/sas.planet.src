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
    FZMPFileName: string;
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
    FAntiBan: IAntiBan;
    FMimeTypeSubstList: TStringList;
    FCache: ITileObjCache;
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
    procedure LoadProjectionInfo(AConfig : IConfigDataProvider);
    procedure LoadStorageParams(AConfig : IConfigDataProvider);
    procedure LoadWebSourceParams(AConfig : IConfigDataProvider);
    procedure LoadUIParams(AConfig : IConfigDataProvider);
    procedure LoadMapInfo(AConfig : IConfigDataProvider);
    procedure LoadGlobalConfig(AConfig : IConfigDataProvider);
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

    property asLayer: boolean read FasLayer;
    property IsStoreFileCache: Boolean read GetIsStoreFileCache;
    property IsBitmapTiles: Boolean read GetIsBitmapTiles;
    property IsKmlTiles: Boolean read GetIsKmlTiles;
    property IsHybridLayer: Boolean read GetIsHybridLayer;
    property UseDwn: Boolean read GetUseDwn;
    property UseDel: boolean read GetUseDel;
    property UseSave: boolean read GetUseSave;
    property UseGenPrevious: boolean read GetUseGenPrevious;
    property IsStoreReadOnly: boolean read GetIsStoreReadOnly;
    property IsCanShowOnSmMap: boolean read GetIsCanShowOnSmMap;
    property UseStick: boolean read GetUseStick;
    property IsCropOnDownload: Boolean read GetIsCropOnDownload;

    property ZmpFileName: string read GetZmpFileName;
    property bmp18: TBitmap read Fbmp18;
    property bmp24: TBitmap read Fbmp24;
    property CacheConfig: TMapTypeCacheConfig read FCacheConfig;
    property UrlGenerator : TUrlGeneratorBasic read FUrlGenerator;
    property TileFileExt: string read FTileFileExt;
    property MapInfo: string read FMapInfo;
    property Name: string read FName;
    property DefHotKey: TShortCut read FDefHotKey;
    property DefSleep: Integer read FDefSleep;
    property Defseparator: boolean read FDefseparator;
    property DefParentSubMenu: string read FDefParentSubMenu;

    constructor Create;
    destructor Destroy; override;
    procedure LoadMapType(AConfig, AAllMapsConfig : IConfigDataProvider; Apnum : Integer);
 end;

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

procedure TMapType.LoadGlobalConfig(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem(GUIDString);
  if VParams <> nil then begin
      id:=VParams.ReadInteger('pnum',id);
      FUrlGenerator.URLBase:=VParams.ReadString('URLBase',FUrlGenerator.URLBase);
      CacheConfig.CacheType:=VParams.ReadInteger('CacheType',CacheConfig.cachetype);
      CacheConfig.NameInCache:=VParams.ReadString('NameInCache',CacheConfig.NameInCache);
      HotKey:=VParams.ReadInteger('HotKey',HotKey);
      ParentSubMenu:=VParams.ReadString('ParentSubMenu',ParentSubMenu);
      Sleep:=VParams.ReadInteger('Sleep',Sleep);
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
      end;
     else
      ShowMessage('Ошибка скрипта карты '+name+' :'+#13#10+'Неожиданная ошибка');
      FUrlGenerator := nil;
    end;
  end;
  if FUrlGenerator = nil then begin
    FUrlGenerator := TUrlGeneratorBasic.Create(AConfig);
  end;
end;

procedure TMapType.LoadMapInfo(AConfig: IConfigDataProvider);
begin
  FMapinfo := AConfig.ReadString('info_'+inttostr(GState.Localization)+'.txt', '');
  if FMapInfo = '' then begin
    FMapinfo := AConfig.ReadString('info.txt', '');
  end;
end;

procedure TMapType.LoadStorageParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUseDel:=VParams.ReadBool('Usedel',true);
  FIsStoreReadOnly:=VParams.ReadBool('ReadOnly', false);
  FUseSave:=VParams.ReadBool('Usesave',true);
  FTileFileExt:=LowerCase(VParams.ReadString('Ext','.jpg'));
  FCacheConfig := TMapTypeCacheConfig.Create(AConfig);
  FCache := TTileCacheSimpleGlobal.Create(Self);
  if GetIsBitmapTiles then begin
    FBitmapLoaderFromStorage := GState.BitmapTypeManager.GetBitmapLoaderForExt(FTileFileExt);
    FBitmapSaverToStorage := GState.BitmapTypeManager.GetBitmapSaverForExt(FTileFileExt);
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

  Sleep:=VParams.ReadInteger('Sleep',0);
  FDefSleep:=Sleep;
  FUseDwn:=VParams.ReadBool('UseDwn',true);
end;

procedure TMapType.LoadUIParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FName:=VParams.ReadString('name',FName);
  FName:=VParams.ReadString('name_'+inttostr(GState.Localization),FName);
  FIsCanShowOnSmMap := VParams.ReadBool('CanShowOnSmMap', true);
  HotKey:=VParams.ReadInteger('DefHotKey',0);
  FDefHotKey:=HotKey;
  ParentSubMenu:=VParams.ReadString('ParentSubMenu','');
  ParentSubMenu:=VParams.ReadString('ParentSubMenu_'+inttostr(GState.Localization),ParentSubMenu);
  FDefParentSubMenu:=ParentSubMenu;
  separator:=VParams.ReadBool('separator',false);
  FDefseparator:=separator;
  id:=VParams.ReadInteger('pnum',-1);
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
  LoadStorageParams(AConfig);
  LoadMapIcons(AConfig);
  LoadWebSourceParams(AConfig);
  FUsestick:=VParams.ReadBool('Usestick',true);
  FUseGenPrevious:=VParams.ReadBool('UseGenPrevious',true);
  LoadMimeTypeSubstList(AConfig);
  LoadProjectionInfo(AConfig);
  LoadUrlScript(AConfig);
  if FUseDwn then begin
    try
      FMaxConnectToServerCount := VParams.ReadInteger('MaxConnectToServerCount', 1);
      if FMaxConnectToServerCount > 64 then begin
        FMaxConnectToServerCount := 64;
      end;
      if FMaxConnectToServerCount <= 0 then begin
        FMaxConnectToServerCount := 1;
      end;
      FPoolOfDownloaders := TPoolOfObjectsSimple.Create(FMaxConnectToServerCount, TTileDownloaderBaseFactory.Create(Self, AConfig), 60000, 60000);
      GState.GCThread.List.AddObject(FPoolOfDownloaders as IObjectWithTTL);
      FAntiBan := TAntiBanStuped.Create(AConfig);
    except
      if ExceptObject <> nil then begin
        ShowMessageFmt('Для карты %0:s отключена загрузка тайлов из-за ошибки: %1:s',[FZMPFileName, (ExceptObject as Exception).Message]);
      end;
      FUseDwn := false;
    end;
  end;
  LoadGlobalConfig(AAllMapsConfig);
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
  VManager := GState.BitmapTypeManager;
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
  Result := ExtractFileName(FZMPFileName);
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
