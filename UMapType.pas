unit UMapType;

interface

uses
  Windows,
  Forms,
  sysutils,
  Classes,
  IniFiles,
  Dialogs,
  Graphics,
  StdCtrls,
  ComCtrls,
  SyncObjs,
  Menus,
  math,
  ExtCtrls,
  TBX,
  VCLZip,
  GR32,
  t_GeoTypes,
  i_ICoordConverter,
  i_ITileDownlodSession,
  i_IPoolOfObjectsSimple,
  i_IBitmapTypeExtManager,
  u_TileDownloaderBase,
  u_UrlGenerator,
  UResStrings;

type
 TMapType = class
   private
    FTileRect:TRect;
    Fpos: integer;
    FFileName: string;
    FUseDwn: boolean;
    FUseDel: boolean;
    FIsStoreReadOnly: Boolean;
    FUseSave: boolean;
    FShowOnSmMap: boolean;
    FIsCanShowOnSmMap: Boolean;
    FUseStick: boolean;
    FGetURLScript: string;
    Fbmp18: TBitmap;
    Fbmp24: TBitmap;
    FContent_Type: string;
    FStatus_Code: string;
    FBanIfLen: integer;
    FUseAntiBan: integer;
    FMaxConnectToServerCount: Cardinal;
    function GetCoordConverter: ICoordConverter;
    function GetIsStoreFileCache: Boolean;
    function GetUseDwn: Boolean;
    function GetUseDel: boolean;
    function GetUseSave: boolean;
    function GetZmpFileName: string;
    function GetIsStoreReadOnly: boolean;
    function GetIsCanShowOnSmMap: boolean;
    function GetUseStick: boolean;
    function GetShowOnSmMap: boolean;
    procedure SetShowOnSmMap(const Value: boolean);
    function GetBitmapTypeManager: IBitmapTypeExtManager;
    function GetIsCropOnDownload: Boolean;
   public
    id: integer;
    guids: string;
    TileFileExt: string;
    MapInfo: string;
    asLayer: boolean;
    name: string;
    DelAfterShow: boolean;
    projection: byte;
    radiusa: extended;
    radiusb: extended;
    exct: extended;
    UseGenPrevious: boolean;

    DefHotKey: TShortCut;
    HotKey: TShortCut;

    DefURLBase: string;
    URLBase: string;

    DefSleep: Integer;
    Sleep: Integer;

    Defseparator: boolean;
    separator: boolean;

    defcachetype: byte;
    cachetype: byte;

    DefParentSubMenu:string;
    ParentSubMenu: string;

    DefNameInCache:string;
    NameInCache:string;

    NSmItem: TTBXItem;
    TBItem: TTBXItem;
    NLayerParamsItem: TTBXItem;
    TBFillingItem: TTBXItem;
    TBSubMenuItem: TTBXSubmenuItem;
    NDwnItem: TMenuItem;
    NDelItem: TMenuItem;
    active: boolean;
    showinfo: boolean;

    function GetLink(x,y:longint;Azoom:byte):string;overload;
    function GetLink(AXY: TPoint;Azoom:byte):string;overload;

    procedure SaveTileDownload(x,y:longint;Azoom:byte; ATileStream:TCustomMemoryStream; ty: string); overload;
    procedure SaveTileDownload(AXY: TPoint;Azoom:byte; ATileStream:TCustomMemoryStream; ty: string); overload;

    function GetTileFileName(x,y:longint;Azoom:byte):string; overload;
    function GetTileFileName(AXY: TPoint;Azoom:byte):string; overload;

    function GetTileShowName(x,y:longint;Azoom:byte):string; overload;
    function GetTileShowName(AXY: TPoint;Azoom:byte):string; overload;

    function TileExists(x,y:longint;Azoom:byte): Boolean; overload;
    function TileExists(AXY: TPoint;Azoom:byte): Boolean; overload;

    function TileNotExistsOnServer(x,y:longint;Azoom:byte): Boolean; overload;
    function TileNotExistsOnServer(AXY: TPoint;Azoom:byte): Boolean; overload;

    function LoadTile(btm:TObject; x,y:longint;Azoom:byte; caching:boolean):boolean; overload;
    function LoadTile(btm:TObject; AXY: TPoint; Azoom:byte; caching:boolean):boolean; overload;
{
    function LoadTile(btm:TStream; x,y:longint;Azoom:byte; caching:boolean):boolean; overload;
    function LoadTile(btm:TStream; AXY: TPoint; Azoom:byte; caching:boolean):boolean; overload;
    function LoadTile(btm:TBitmap32; x,y:longint;Azoom:byte; caching:boolean):boolean; overload;
    function LoadTile(btm:TBitmap32; AXY: TPoint; Azoom:byte; caching:boolean):boolean; overload;
}
    function LoadTileFromPreZ(spr:TBitmap32;x,y:integer;Azoom:byte; caching:boolean):boolean; overload;
    function LoadTileFromPreZ(spr:TBitmap32; AXY: TPoint; Azoom:byte; caching:boolean):boolean; overload;

    function DeleteTile(x,y:longint; Azoom:byte): Boolean; overload;
    function DeleteTile(AXY: TPoint; Azoom:byte): Boolean; overload;

    procedure SaveTileSimple(x,y:longint;Azoom:byte; btm: TBitmap32); overload;
    procedure SaveTileSimple(AXY: TPoint; Azoom:byte; btm: TBitmap32); overload;

    procedure SaveTileNotExists(x,y:longint;Azoom:byte); overload;
    procedure SaveTileNotExists(AXY: TPoint; Azoom:byte); overload;

    function TileLoadDate(x,y:longint;Azoom:byte): TDateTime; overload;
    function TileLoadDate(AXY: TPoint; Azoom:byte): TDateTime; overload;

    function TileSize(x,y:longint;Azoom:byte): integer; overload;
    function TileSize(AXY: TPoint; Azoom:byte): integer; overload;

    function TileExportToFile(x,y:longint;Azoom:byte; AFileName: string; OverWrite: boolean): boolean; overload;
    function TileExportToFile(AXY: TPoint; Azoom:byte; AFileName: string; OverWrite: boolean): boolean; overload;

    // Строит карту заполнения дл тайла на уровне AZoom тайлами уровня ASourceZoom
    // Должна регулярно проверять по указателю IsStop не нужно ли прерваться
    function LoadFillingMap(btm:TBitmap32; x,y:longint;Azoom:byte;ASourceZoom: byte; IsStop: PBoolean):boolean; overload;
    function LoadFillingMap(btm:TBitmap32; AXY: TPoint; Azoom:byte;ASourceZoom: byte; IsStop: PBoolean):boolean; overload;

    function GetShortFolderName: string;

    function IncDownloadedAndCheckAntiBan: Boolean;
    procedure addDwnforban;
    procedure ExecOnBan(ALastUrl: string);
    function DownloadTile(AXY: TPoint; AZoom: byte; ACheckTileSize: Boolean; AOldTileSize: Integer; out AUrl: string; out AContentType: string; fileBuf:TMemoryStream): TDownloadTileResult;

    property GeoConvert: ICoordConverter read GetCoordConverter;
    property IsStoreFileCache: Boolean read GetIsStoreFileCache;
    property UseDwn: Boolean read GetUseDwn;
    property UseDel: boolean read GetUseDel;
    property UseSave: boolean read GetUseSave;
    property IsStoreReadOnly: boolean read GetIsStoreReadOnly;
    property IsCanShowOnSmMap: boolean read GetIsCanShowOnSmMap;
    property UseStick: boolean read GetUseStick;
    property ContentType: string read FContent_Type;
    property IsCropOnDownload: Boolean read GetIsCropOnDownload;
    property ShowOnSmMap: boolean read GetShowOnSmMap write SetShowOnSmMap;
    property ZmpFileName: string read GetZmpFileName;
    property BitmapTypeManager: IBitmapTypeExtManager read GetBitmapTypeManager;
    constructor Create;
    procedure LoadMapTypeFromZipFile(AZipFileName : string; pnum : Integer);
    destructor Destroy; override;
  private
    FDownloadTilesCount: Longint;
    FInitDownloadCS: TCriticalSection;
    FCSSaveTile: TCriticalSection;
    FCSSaveTNF: TCriticalSection;
    FUrlGenerator : TUrlGenerator;
    FCoordConverter : ICoordConverter;
    FPoolOfDownloaders: IPoolOfObjectsSimple;
    //Для борьбы с капчей
    ban_pg_ld: Boolean;
    procedure CropOnDownload(ABtm: TBitmap32; ATileSize: TPoint);
    function LoadFile(btm:Tobject; APath: string; caching:boolean):boolean;
    procedure CreateDirIfNotExists(APath:string);
    procedure SaveTileInCache(btm: TBitmap32; path: string); overload;
    procedure SaveTileInCache(btm: TStream; path: string); overload;
    function CheckIsBan(AXY: TPoint; AZoom: byte; StatusCode: Cardinal; ty: string; fileBuf: TMemoryStream): Boolean;
    function GetBasePath: string;
    procedure SaveTileKmlDownload(AXY: TPoint;Azoom:byte; ATileStream: TCustomMemoryStream; ty: string);
    procedure SaveTileBitmapDownload(AXY: TPoint;Azoom:byte; ATileStream: TCustomMemoryStream; ty: string);
 end;

var
  MapType:array of TMapType;
  MapsEdit:boolean;
  sat_map_both:TMapType;

  procedure LoadMaps;
  procedure SaveMaps;
  procedure CreateMapUI;
  function GetMapFromID(id:string):TMapType;

implementation

uses
  Types,
  pngimage,
  IJL,
  jpeg,
  gifimage,
  GR32_Resamplers,
  VCLUnZip,
  u_GlobalState,
  Usettings,
  unit1,
  UGeoFun,
  UFillingMap,
  UIMGFun,
  DateUtils,
  i_IObjectWithTTL,
  i_IPoolElement,
  u_PoolOfObjectsSimple,
  u_TileDownloaderBaseFactory,
  ImgMaker,
  UKmlParse,
  u_MiniMap,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid,
  u_CoordConverterSimpleLonLat;

function GetMapFromID(id:string):TMapType;
var
  i:integer;
begin
  Result:=nil;
  for i:=0 to length(MapType)-1 do begin
    if MapType[i].guids=id then begin
      result:=MapType[i];
      exit;
    end;
  end;
end;

procedure CreateMapUI;
var
  i,j: integer;
begin
  FSettings.MapList.Clear;
  Fmain.MapIcons24.Clear;
  Fmain.MapIcons18.Clear;
  Fmain.TBSMB.Clear;
  Fmain.NSMB.Clear;
  Fmain.ldm.Clear;
  Fmain.dlm.Clear;
  Fmain.NLayerParams.Clear;
  Fmain.NSubMenuSmItem.Clear;
  for i:=0 to Fmain.NLayerSel.Count-1 do Fmain.NLayerSel.Items[0].Free;
  for i:=0 to Fmain.TBLayerSel.Count-1 do Fmain.TBLayerSel.Items[0].Free;
  for i:=0 to Fmain.TBFillingTypeMap.Count-2 do Fmain.TBFillingTypeMap.Items[1].Free;
  for i:=0 to GMiniMapPopupMenu.Items.Count-3 do GMiniMapPopupMenu.Items.Items[2].Free;

  GMiniMap.maptype:=nil;
  fillingmaptype:=nil;
  i:=length(MapType)-1;

  if i>0 then begin
    for i:=0 to length(MapType)-1 do begin
      With MapType[i] do begin
        TBItem:=TTBXItem.Create(Fmain.TBSMB);
        if ParentSubMenu='' then begin
          if asLayer then begin
            Fmain.TBLayerSel.Add(TBItem);
          end else begin
            Fmain.TBSMB.Add(TBItem);
          end;
        end else begin
          j:=0;
          While MapType[j].ParentSubMenu<>ParentSubMenu do inc(j);
          TBSubMenuItem:=TTBXSubmenuItem.Create(Fmain.TBSMB);
          TBSubMenuItem.caption:=ParentSubMenu;
          TBSubMenuItem.Images:=Fmain.MapIcons18;
          if j=i then begin
            if asLayer then begin
              Fmain.TBLayerSel.Add(TBSubMenuItem)
            end else begin
              Fmain.TBSMB.Add(TBSubMenuItem);
            end;
          end;
          MapType[j].TBSubMenuItem.Add(TBItem);
        end;
        Fmain.MapIcons24.AddMasked(Fbmp24,RGB(255,0,255));
        Fmain.MapIcons18.AddMasked(Fbmp18,RGB(255,0,255));
        TBItem.Name:='TBMapN'+inttostr(id);
        TBItem.ShortCut:=HotKey;
        TBItem.ImageIndex:=i;
        TBItem.Caption:=name;
        TBItem.OnAdjustFont:=Fmain.AdjustFont;
        TBItem.OnClick:=Fmain.TBmap1Click;

        TBFillingItem:=TTBXItem.Create(Fmain.TBFillingTypeMap);
        TBFillingItem.name:='TBMapFM'+inttostr(id);
        TBFillingItem.ImageIndex:=i;
        TBFillingItem.Caption:=name;
        TBFillingItem.OnAdjustFont:=Fmain.AdjustFont;
        TBFillingItem.OnClick:=Fmain.TBfillMapAsMainClick;
        Fmain.TBFillingTypeMap.Add(TBFillingItem);

        if IsCanShowOnSmMap then begin
          if not(asLayer) then begin
            NSmItem:=TTBXITem.Create(GMiniMapPopupMenu);
            GMiniMapPopupMenu.Items.Add(NSmItem)
          end else begin
            NSmItem:=TTBXITem.Create(Fmain.NSubMenuSmItem);
            Fmain.NSubMenuSmItem.Add(NSmItem);
          end;
          NSmItem.Name:='NSmMapN'+inttostr(id);
          NSmItem.ImageIndex:=i;
          NSmItem.Caption:=name;
          NSmItem.OnAdjustFont:=Fmain.AdjustFont;
          NSmItem.OnClick:=Fmain.NMMtype_0Click;
          if ShowOnSmMap then begin
            NSmItem.Checked:=true;
          end;
        end;
        if asLayer then begin
          NDwnItem:=TMenuItem.Create(nil);
          NDwnItem.Caption:=name;
          NDwnItem.ImageIndex:=i;
          NDwnItem.OnClick:=Fmain.N21Click;
          Fmain.ldm.Add(NDwnItem);
          NDelItem:=TMenuItem.Create(nil);
          NDelItem.Caption:=name;
          NDelItem.ImageIndex:=i;
          NDelItem.OnClick:=Fmain.NDelClick;
          Fmain.dlm.Add(NDelItem);
          NLayerParamsItem:=TTBXItem.Create(Fmain.NLayerParams);
          NLayerParamsItem.Caption:=name;
          NLayerParamsItem.ImageIndex:=i;
          NLayerParamsItem.OnClick:=Fmain.NMapParamsClick;
          Fmain.NLayerParams.Add(NLayerParamsItem);
        end;
        if (asLayer)and(active) then begin
          TBItem.Checked:=true;
        end;
        if separator then begin
          TBItem.Parent.Add(TTBXSeparatorItem.Create(Fmain.TBSMB));
          if NSmItem<>NIL  then begin
            NSmItem.Parent.Add(TTBXSeparatorItem.Create(Fmain.NSubMenuSmItem));
          end;
          TBFillingItem.Parent.Add(TTBXSeparatorItem.Create(Fmain.NSubMenuSmItem));
        end;
        if (active)and(MapType[i].asLayer=false) then begin
          sat_map_both:=MapType[i];
        end;
        if (ShowOnSmMap)and(not(asLayer)) then begin
          GMiniMap.maptype:=MapType[i];
        end;
        TBItem.Tag:=Longint(MapType[i]);
        TBFillingItem.Tag:=Longint(MapType[i]);
        if IsCanShowOnSmMap then begin
          NSmItem.Tag:=Longint(MapType[i]);
        end;
        if asLayer then begin
          NDwnItem.Tag:=longint(MapType[i]);
          NDelItem.Tag:=longint(MapType[i]);
          NLayerParamsItem.Tag:=longint(MapType[i]);
        end;
        FSettings.MapList.AddItem(MapType[i].name,nil);
        FSettings.MapList.Items.Item[i].Data:=MapType[i];
        FSettings.MapList.Items.Item[i].SubItems.Add(MapType[i].NameInCache);
        if MapType[i].asLayer then begin
          FSettings.MapList.Items.Item[i].SubItems.Add(SAS_STR_Layers+'\'+MapType[i].ParentSubMenu);
        end else begin
          FSettings.MapList.Items.Item[i].SubItems.Add(SAS_STR_Maps+'\'+MapType[i].ParentSubMenu);
        end;
        FSettings.MapList.Items.Item[i].SubItems.Add(ShortCutToText(MapType[i].HotKey));
        FSettings.MapList.Items.Item[i].SubItems.Add(MapType[i].FFilename);
      end;
    end;
  end;
  if FSettings.MapList.Items.Count>0 then begin
    FSettings.MapList.Items.Item[0].Selected:=true;
  end;
  if GMiniMap.maptype=nil then begin
    Fmain.NMMtype_0.Checked:=true;
  end;
  if (sat_map_both=nil)and(MapType[0]<>nil) then begin
    sat_map_both:=MapType[0];
  end;
end;

procedure LoadMaps;
var
  Ini: TMeminifile;
  i,j,k,pnum: integer;
  startdir : string;
  SearchRec: TSearchRec;
  MTb: TMapType;
begin
  SetLength(MapType,0);
  CreateDir(GState.MapsPath);
  Ini:=TMeminiFile.Create(GState.ProgramPath+'Maps\Maps.ini');
  i:=0;
  pnum:=0;
  startdir:=GState.MapsPath;
  if FindFirst(startdir+'*.zmp', faAnyFile, SearchRec) = 0 then begin
    repeat
      inc(i);
    until FindNext(SearchRec) <> 0;
  end;
  SysUtils.FindClose(SearchRec);
  SetLength(MapType,i);
  if FindFirst(startdir+'*.zmp', faAnyFile, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then continue;
      MapType[pnum]:=TMapType.Create;
      MapType[pnum].LoadMapTypeFromZipFile(startdir+SearchRec.Name, pnum);
      MapType[pnum].ban_pg_ld := true;
      if Ini.SectionExists(MapType[pnum].GUIDs)then begin
        With MapType[pnum] do begin
          id:=Ini.ReadInteger(GUIDs,'pnum',0);
          active:=ini.ReadBool(GUIDs,'active',false);
          ShowOnSmMap:=ini.ReadBool(GUIDs,'ShowOnSmMap',true);
          URLBase:=ini.ReadString(GUIDs,'URLBase',URLBase);
          CacheType:=ini.ReadInteger(GUIDs,'CacheType',cachetype);
          NameInCache:=ini.ReadString(GUIDs,'NameInCache',NameInCache);
          HotKey:=ini.ReadInteger(GUIDs,'HotKey',HotKey);
          ParentSubMenu:=ini.ReadString(GUIDs,'ParentSubMenu',ParentSubMenu);
          Sleep:=ini.ReadInteger(GUIDs,'Sleep',Sleep);
          separator:=ini.ReadBool(GUIDs,'separator',separator);
        end;
      end else begin
        With MapType[pnum] do begin
          showinfo:=true;
          if Fpos < 0 then Fpos := i;
          id := Fpos;
          dec(i);
          active:=false;
          ShowOnSmMap:=false;
        end;
      end;
      inc(pnum);
    until FindNext(SearchRec) <> 0;
  end;
  SysUtils.FindClose(SearchRec);
  ini.Free;

  k := length(MapType) shr 1;
  while k>0 do begin
    for i:=0 to length(MapType)-k-1 do begin
      j:=i;
      while (j>=0)and(MapType[j].id>MapType[j+k].id) do begin
        MTb:=MapType[j];
        MapType[j]:=MapType[j+k];
        MapType[j+k]:=MTb;
        if j>k then begin
          Dec(j,k);
        end else begin
          j:=0;
        end;
      end;
    end;
    k:=k shr 1;
  end;
  MTb:=nil;
  MTb.Free;
  for i:=0 to length(MapType)-1 do begin
    MapType[i].id:=i+1;
  end;
end;

procedure SaveMaps;
var
  Ini: TMeminifile;
  i: integer;
begin
  Ini:=TMeminiFile.Create(GState.ProgramPath+'Maps\Maps.ini');
  try
    for i:=0 to length(MapType)-1 do begin
      ini.WriteInteger(MapType[i].guids,'pnum',MapType[i].id);
      ini.WriteBool(MapType[i].guids,'active',MapType[i].active);
      ini.WriteBool(MapType[i].guids,'ShowOnSmMap',MapType[i].ShowOnSmMap);

      if MapType[i].URLBase<>MapType[i].DefURLBase then begin
        ini.WriteString(MapType[i].guids,'URLBase',MapType[i].URLBase);
      end else begin
        Ini.DeleteKey(MapType[i].guids,'URLBase');
      end;

      if MapType[i].HotKey<>MapType[i].DefHotKey then begin
        ini.WriteInteger(MapType[i].guids,'HotKey',MapType[i].HotKey);
      end else begin
        Ini.DeleteKey(MapType[i].guids,'HotKey');
      end;

      if MapType[i].cachetype<>MapType[i].defcachetype then begin
        ini.WriteInteger(MapType[i].guids,'CacheType',MapType[i].CacheType);
      end else begin
        Ini.DeleteKey(MapType[i].guids,'CacheType');
      end;

      if MapType[i].separator<>MapType[i].Defseparator then begin
        ini.WriteBool(MapType[i].guids,'separator',MapType[i].separator);
      end else begin
        Ini.DeleteKey(MapType[i].guids,'separator');
      end;

      if MapType[i].NameInCache<>MapType[i].DefNameInCache then begin
        ini.WriteString(MapType[i].guids,'NameInCache',MapType[i].NameInCache);
      end else begin
        Ini.DeleteKey(MapType[i].guids,'NameInCache');
      end;

      if MapType[i].Sleep<>MapType[i].DefSleep then begin
        ini.WriteInteger(MapType[i].guids,'Sleep',MapType[i].sleep);
      end else begin
        Ini.DeleteKey(MapType[i].guids,'Sleep');
      end;

      if MapType[i].ParentSubMenu<>MapType[i].DefParentSubMenu then begin
        ini.WriteString(MapType[i].guids,'ParentSubMenu',MapType[i].ParentSubMenu);
      end else begin
        Ini.DeleteKey(MapType[i].guids,'ParentSubMenu');
      end;
    end;
    Ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TMapType.LoadMapTypeFromZipFile(AZipFileName: string; pnum : Integer);
var
  MapParams:TMemoryStream;
  AZipFile:TFileStream;
  IniStrings:TStringList;
  iniparams: TMeminifile;
  GUID:TGUID;
  guidstr : string;
  bfloat:string;
  bb:array [1..2048] of char;
  NumRead : integer;
  UnZip:TVCLZip;
begin
  if AZipFileName = '' then begin
    raise Exception.Create('Пустое имя файла с настройками карты');
  end;
  if not FileExists(AZipFileName) then begin
    raise Exception.Create('Файл ' + AZipFileName + ' не найден');
  end;
  Ffilename := AZipFileName;
  UnZip:=TVCLZip.Create(nil);
  try
    AZipFile:=TFileStream.Create(AZipFileName,fmOpenRead or fmShareDenyNone);
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
      guidstr:=iniparams.ReadString('PARAMS','ID',GUIDToString(GUID));
      name:=iniparams.ReadString('PARAMS','name','map#'+inttostr(pnum));
      name:=iniparams.ReadString('PARAMS','name_'+inttostr(GState.Localization),name);

      MapParams:=TMemoryStream.Create;
      try
      if (UnZip.UnZipToStream(MapParams,'info_'+inttostr(GState.Localization)+'.txt')>0)or(UnZip.UnZipToStream(MapParams,'info.txt')>0) then
       begin
        SetLength(MapInfo, MapParams.size);
        MapParams.Position:=0;
        MapParams.ReadBuffer(Mapinfo[1],MapParams.size);
       end;
      finally
        FreeAndNil(MapParams);
      end;

      MapParams:=TMemoryStream.Create;
      try
        UnZip.UnZipToStream(MapParams,'GetUrlScript.txt');
        MapParams.Position:=0;
        repeat
          NumRead:=MapParams.Read(bb,SizeOf(bb));
          FGetURLScript:=FGetURLScript+copy(bb,1, NumRead);
        until (NumRead = 0);
      finally
        FreeAndNil(MapParams);
      end;
      Fbmp24:=TBitmap.create;
      MapParams:=TMemoryStream.Create;
      try
        try
          UnZip.UnZipToStream(MapParams,'24.bmp');
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
          UnZip.UnZipToStream(MapParams,'18.bmp');
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
      GUIDs:=iniparams.ReadString('PARAMS','GUID',GUIDstr);
      asLayer:=iniparams.ReadBool('PARAMS','asLayer',false);
      URLBase:=iniparams.ReadString('PARAMS','DefURLBase','http://maps.google.com/');
      DefUrlBase:=URLBase;
      FTileRect.Left:=iniparams.ReadInteger('PARAMS','TileRLeft',0);
      FTileRect.Top:=iniparams.ReadInteger('PARAMS','TileRTop',0);
      FTileRect.Right:=iniparams.ReadInteger('PARAMS','TileRRight',0);
      FTileRect.Bottom:=iniparams.ReadInteger('PARAMS','TileRBottom',0);
      FUseDwn:=iniparams.ReadBool('PARAMS','UseDwn',true);
      FUsestick:=iniparams.ReadBool('PARAMS','Usestick',true);
      UseGenPrevious:=iniparams.ReadBool('PARAMS','UseGenPrevious',true);
      FUseDel:=iniparams.ReadBool('PARAMS','Usedel',true);
      FIsStoreReadOnly:=iniparams.ReadBool('PARAMS','ReadOnly', false);
      FIsCanShowOnSmMap := iniparams.ReadBool('PARAMS','CanShowOnSmMap', true);
      DelAfterShow:=iniparams.ReadBool('PARAMS','DelAfterShow',false);
      FUseSave:=iniparams.ReadBool('PARAMS','Usesave',true);
      FUseAntiBan:=iniparams.ReadInteger('PARAMS','UseAntiBan',0);
      CacheType:=iniparams.ReadInteger('PARAMS','CacheType',0);
      DefCacheType:=CacheType;
      Sleep:=iniparams.ReadInteger('PARAMS','Sleep',0);
      DefSleep:=Sleep;
      FBanIfLen:=iniparams.ReadInteger('PARAMS','BanIfLen',0);
      FContent_Type:=iniparams.ReadString('PARAMS','ContentType','image\jpg');
      FStatus_Code:=iniparams.ReadString('PARAMS','ValidStatusCode','200');
      TileFileExt:=LowerCase(iniparams.ReadString('PARAMS','Ext','.jpg'));
      NameInCache:=iniparams.ReadString('PARAMS','NameInCache','Sat');
      DefNameInCache:=NameInCache;
      projection:=iniparams.ReadInteger('PARAMS','projection',1);
      bfloat:=iniparams.ReadString('PARAMS','sradiusa','6378137');
      radiusa:=str2r(bfloat);
      bfloat:=iniparams.ReadString('PARAMS','sradiusb',FloatToStr(radiusa));
      radiusb:=str2r(bfloat);
      HotKey:=iniparams.ReadInteger('PARAMS','DefHotKey',0);
      DefHotKey:=HotKey;
      ParentSubMenu:=iniparams.ReadString('PARAMS','ParentSubMenu','');
      ParentSubMenu:=iniparams.ReadString('PARAMS','ParentSubMenu_'+inttostr(GState.Localization),ParentSubMenu);
      DefParentSubMenu:=ParentSubMenu;
      separator:=iniparams.ReadBool('PARAMS','separator',false);
      Defseparator:=separator;
      exct:=sqrt(radiusa*radiusa-radiusb*radiusb)/radiusa;
      Fpos:=iniparams.ReadInteger('PARAMS','pnum',-1);
      FMaxConnectToServerCount := iniparams.ReadInteger('PARAMS','MaxConnectToServerCount', 1);
      if FMaxConnectToServerCount > 64 then begin
        FMaxConnectToServerCount := 64;
      end;
      if FMaxConnectToServerCount <= 0 then begin
        FMaxConnectToServerCount := 1;
      end;
      FPoolOfDownloaders := TPoolOfObjectsSimple.Create(FMaxConnectToServerCount, TTileDownloaderBaseFactory.Create(Self), 60000, 60000);
      GState.GCThread.List.AddObject(FPoolOfDownloaders as IObjectWithTTL);
      case projection of
        1: FCoordConverter := TCoordConverterMercatorOnSphere.Create(radiusa);
        2: FCoordConverter := TCoordConverterMercatorOnEllipsoid.Create(Exct,radiusa,radiusb);
        3: FCoordConverter := TCoordConverterSimpleLonLat.Create(radiusa);
        else raise Exception.Create('Ошибочный тип проэкции карты ' + IntToStr(projection));
      end;
      try
      FUrlGenerator := TUrlGenerator.Create('procedure Return(Data: string); begin ResultURL := Data; end; ' + FGetURLScript, FCoordConverter);
      FUrlGenerator.GetURLBase := URLBase;
      //GetLink(0,0,0);
      except
        on E: Exception do begin
          ShowMessage('Ошибка скрипта карты '+name+' :'+#13#10+ E.Message);
        end;
       else
        ShowMessage('Ошибка скрипта карты '+name+' :'+#13#10+'Неожиданная ошибка');
      end;
    finally
      FreeAndNil(iniparams);
    end;
  finally
    FreeAndNil(AZipFile);
    FreeAndNil(UnZip);
  end;
end;

function TMapType.GetLink(AXY: TPoint; Azoom: byte): string;
begin
  if (FUrlGenerator = nil) then result:='';
  FCoordConverter.CheckTilePosStrict(AXY, Azoom, True);
  FUrlGenerator.GetURLBase:=URLBase;
  Result:=FUrlGenerator.GenLink(AXY.X, AXY.Y, Azoom);
end;

function TMapType.GetLink(x,y:Integer;Azoom:byte): string;
begin
  Result := Self.GetLink(FCoordConverter.PixelPos2TilePos(Point(x, y), Azoom - 1), Azoom - 1)
end;

function TMapType.GetBasePath: string;
var
  ct:byte;
begin
  if (CacheType=0) then begin
    ct:=GState.DefCache;
  end else begin
    ct:=CacheType;
  end;
  result := NameInCache;
  if (length(result)<2)or((result[2]<>'\')and(system.pos(':',result)=0)) then begin
    case ct of
      1: begin
        result:=GState.OldCpath_ + Result;
      end;
      2: begin
        result:=GState.NewCpath_+Result;
      end;
      3: begin
        result:=GState.ESCpath_+Result;
      end;
      4,41: begin
        result:=GState.GMTilespath_+Result;
      end;
      5: begin
        result:=GState.GECachepath_+Result;
      end;
    end;
  end;
  if (length(result)<2)or((result[2]<>'\')and(system.pos(':',result)=0))then begin
    result:=GState.ProgramPath+result;
  end;
end;

function TMapType.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  if IsStoreFileCache then begin
    Result := GetBasePath;
    if Result <> '' then begin
      Result := IncludeTrailingPathDelimiter(Result);
    end;
    Result := Result + GState.TileNameGenerator.GetGenerator(cachetype).GetTileFileName(AXY, Azoom) + TileFileExt;
  end else begin
    raise Exception.Create('Ошибка. Это не файловый кеш');
  end;
end;

function TMapType.GetTileFileName(x, y: Integer; Azoom: byte): string;
begin
  Result := GetTileFileName(Point(x shr 8, y shr 8), Azoom - 1);
end;

function TMapType.TileExists(AXY: TPoint; Azoom: byte): Boolean;
var
  VPath: String;
begin
  if ((CacheType=0)and(GState.DefCache=5))or(CacheType=5) then begin
    result:=GETileExists(GetBasePath+'\dbCache.dat.index', AXY.X, AXY.Y, Azoom + 1,self);
  end else begin
    VPath := GetTileFileName(AXY, Azoom);
    Result := Fileexists(VPath);
  end;
end;

function TMapType.TileExists(x, y: Integer; Azoom: byte): Boolean;
begin
  Result := Self.TileExists(FCoordConverter.PixelPos2TilePos(Point(x, y), Azoom - 1), Azoom - 1);
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

function TMapType.LoadTileFromPreZ(spr: TBitmap32; AXY: TPoint;
  Azoom: byte; caching: boolean): boolean;
begin
  Result := Self.LoadTileFromPreZ(spr, AXY.X shl 8, AXY.Y shl 8, Azoom + 1, caching);
end;

function TMapType.LoadTileFromPreZ(spr:TBitmap32;x,y:integer;Azoom:byte; caching:boolean):boolean;
var
  i,c_x,c_y,dZ: integer;
  bmp: TBitmap32;
  VTileExists: Boolean;
  key: string;
begin
  result:=false;
  if (not(GState.UsePrevZoom) and (asLayer=false)) or
  (not(GState.UsePrevZoomLayer) and (asLayer=true)) then
  begin
    if asLayer then spr.Clear(SetAlpha(Color32(GState.BGround),0))
               else spr.Clear(Color32(GState.BGround));
    exit;
  end;
  VTileExists := false;
  for i:=(Azoom-1) downto 1 do begin
    dZ:=(Azoom-i);
    if TileExists(x shr dZ,y shr dZ,i) then begin
      VTileExists := true;
      break;
    end;
  end;
  if not(VTileExists)or(dZ>8) then begin
    if asLayer then spr.Clear(SetAlpha(Color32(GState.BGround),0))
               else spr.Clear(Color32(GState.BGround));
    exit;
  end;
  key:=guids+'-'+inttostr(x shr 8)+'-'+inttostr(y shr 8)+'-'+inttostr(Azoom);
  if (not caching)or(not GState.MainFileCache.TryLoadFileFromCache(TBitmap32(spr), key)) then begin
    bmp:=TBitmap32.Create;
    try
      if not(LoadTile(bmp,x shr dZ,y shr dZ, Azoom - dZ,true))then begin
        if asLayer then spr.Clear(SetAlpha(Color32(GState.BGround),0))
                   else spr.Clear(Color32(GState.BGround));
        exit;
      end;
      bmp.Resampler := CreateResampler(GState.Resampling);
      c_x:=((x-(x mod 256))shr dZ)mod 256;
      c_y:=((y-(y mod 256))shr dZ)mod 256;
      try
        spr.Draw(bounds(-c_x shl dZ,-c_y shl dZ,256 shl dZ,256 shl dZ),bounds(0,0,256,256),bmp);
        GState.MainFileCache.AddTileToCache(TBitmap32(spr), key );
      except
      end;
    finally
      FreeAndNil(bmp);
    end;
  end;
  Result := true;
end;

function TMapType.LoadTile(btm: Tobject; AXY: TPoint; Azoom: byte;
  caching: boolean): boolean;
begin
  Result := Self.LoadTile(btm, AXY.X shl 8, AXY.Y shl 8, Azoom + 1, caching);
end;

function TMapType.LoadTile(btm: Tobject; x,y:longint;Azoom:byte;
  caching: boolean): boolean;
var path: string;
begin
  if ((CacheType=0)and(GState.DefCache=5))or(CacheType=5) then begin
    if (not caching)or(not GState.MainFileCache.TryLoadFileFromCache(TBitmap32(btm), guids+'-'+inttostr(x shr 8)+'-'+inttostr(y shr 8)+'-'+inttostr(Azoom))) then begin
      result:=GetGETile(TBitmap32(btm),GetBasePath+'\dbCache.dat',x shr 8,y shr 8,Azoom, Self);
      if ((result)and(caching)) then GState.MainFileCache.AddTileToCache(TBitmap32(btm), guids+'-'+inttostr(x shr 8)+'-'+inttostr(y shr 8)+'-'+inttostr(Azoom) );
    end else begin
      result:=true;
    end;
  end else begin
    path := GetTileFileName(x, y, Azoom);
    result:= LoadFile(btm, path, caching);
  end;
end;

function TMapType.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
var
  VPath: string;
begin
  Result := false;
  if UseDel then begin
    try
      VPath := GetTileFileName(AXY, Azoom);
      FCSSaveTile.Acquire;
      try
        result := DeleteFile(PChar(VPath));
      finally
        FCSSaveTile.Release;
      end;
    except
      Result := false;
    end;
  end else begin
    Exception.Create('Для этой карты запрещено удаление тайлов.');
  end;
end;

function TMapType.DeleteTile(x, y: Integer; Azoom: byte): Boolean;
begin
  Result := Self.DeleteTile(Point(x shr 8, y shr 8), Azoom - 1);
end;

function TMapType.LoadFile(btm: Tobject; APath: string; caching:boolean): boolean;
var
  VManager: IBitmapTypeExtManager;
begin
  Result := false;
  if GetFileSize(Apath)=0 then begin
    exit;
  end;
  try
    if (btm is TBitmap32) then begin
      VManager := BitmapTypeManager;
      if VManager.GetIsBitmapExt(TileFileExt) then begin
        VManager.GetBitmapLoaderForExt(TileFileExt).LoadFromFile(APath, TBitmap32(btm));
      end else begin
        raise Exception.Create('У этой карты не растровые тайлы');
      end;
    end else begin
      if (btm is TPicture) then
        TPicture(btm).LoadFromFile(Apath)
      else if (btm is TJPEGimage) then
        TJPEGimage(btm).LoadFromFile(Apath)
      else if (btm is TPNGObject) then begin
       if not(caching) then begin
         TPNGObject(btm).LoadFromFile(Apath)
       end else begin
         if not GState.MainFileCache.TryLoadFileFromCache(btm, Apath) then begin
          TPNGObject(btm).LoadFromFile(Apath);
          GState.MainFileCache.AddTileToCache(btm, Apath);
         end;
       end
      end
      else if (btm is TKML) then
        TKML(btm).LoadFromFile(Apath)
      else if (btm is TGraphic) then
        TGraphic(btm).LoadFromFile(Apath);
    end;
    result:=true;
  except
  end;
end;

function TMapType.TileNotExistsOnServer(AXY: TPoint; Azoom: byte): Boolean;
var
  VPath: String;
begin
  VPath := GetTileFileName(AXY, Azoom);
  Result := Fileexists(ChangeFileExt(VPath, '.tne'));
end;

function TMapType.TileNotExistsOnServer(x, y: Integer;
  Azoom: byte): Boolean;
begin
  Result := Self.TileNotExistsOnServer(Point(x shr 8, y shr 8), Azoom - 1);
end;

procedure TMapType.CreateDirIfNotExists(APath:string);
var i:integer;
begin
 i := LastDelimiter('\', Apath);
 Apath:=copy(Apath, 1, i);
 if not(DirectoryExists(Apath)) then ForceDirectories(Apath);
end;

procedure TMapType.SaveTileBitmapDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
var
  VPath: String;
  btmSrc:TBitmap32;
  VManager: IBitmapTypeExtManager;
begin
  VPath := GetTileFileName(AXY, Azoom);
  CreateDirIfNotExists(VPath);
  VManager := BitmapTypeManager;
  if VManager.GetIsBitmapType(ty) then begin
    if not IsCropOnDownload and SameText(TileFileExt, VManager.GetExtForType(ty)) then begin
      SaveTileInCache(ATileStream,Vpath);
    end else begin
      btmsrc := TBitmap32.Create;
      VManager.GetBitmapLoaderForType(ty).LoadFromStream(ATileStream, btmSrc);

      if IsCropOnDownload then begin
        CropOnDownload(btmSrc, FCoordConverter.GetTileSize(AXY, Azoom));
      end;
      SaveTileInCache(btmSrc, VPath);
    end;
    ban_pg_ld:=true;
    GState.MainFileCache.DeleteFileFromCache(Vpath);
  end else begin
    SaveTileInCache(ATileStream, ChangeFileExt(Vpath, '.err'));
  end;
end;

procedure TMapType.SaveTileKmlDownload(AXY: TPoint; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
var
  VPath: String;
  UnZip:TVCLUnZip;
begin
  VPath := GetTileFileName(AXY, Azoom);
  CreateDirIfNotExists(VPath);
  if (ty='application/vnd.google-earth.kmz') then begin
    try
      UnZip:=TVCLUnZip.Create(Fmain);
      UnZip.ArchiveStream:=TMemoryStream.Create;
      ATileStream.SaveToStream(UnZip.ArchiveStream);
      UnZip.ReadZip;
      ATileStream.Position:=0;
      UnZip.UnZipToStream(ATileStream,UnZip.Filename[0]);
      UnZip.Free;
      SaveTileInCache(ATileStream,Vpath);
      ban_pg_ld:=true;
    except
      try
        SaveTileInCache(ATileStream,Vpath);
      except
      end;
    end;
  end else if (copy(ty,1,8)='text/xml')or(ty='application/vnd.google-earth.kml+xml') then begin
    SaveTileInCache(ATileStream,Vpath);
    ban_pg_ld:=true;
  end;
  GState.MainFileCache.DeleteFileFromCache(Vpath);
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


procedure TMapType.SaveTileDownload(x, y: Integer; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
begin
  Self.SaveTileDownload(Point(X shr 8, Y shr 8), Azoom - 1, ATileStream, ty);
end;

procedure TMapType.SaveTileInCache(btm: TBitmap32; path: string);
var
  VManager: IBitmapTypeExtManager;
begin
  VManager := BitmapTypeManager;
  FCSSaveTile.Acquire;
  try
    VManager.GetBitmapSaverForExt(TileFileExt).SaveToFile(btm, path);
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
  VPath := GetTileFileName(AXY, Azoom);
  Result := FileDateToDateTime(FileAge(VPath));
end;

function TMapType.TileLoadDate(x, y: Integer; Azoom: byte): TDateTime;
begin
  Result := Self.TileLoadDate(Point(x shr 8, y shr 8), Azoom - 1);
end;

function TMapType.TileSize(AXY: TPoint; Azoom: byte): integer;
var
  VPath: String;
begin
  VPath := GetTileFileName(AXY, Azoom);
  Result := GetFileSize(VPath);
end;

function TMapType.TileSize(x, y: Integer; Azoom: byte): integer;
begin
  Result := Self.TileSize(Point(x shr 8, y shr 8), Azoom - 1);
end;

procedure TMapType.SaveTileNotExists(AXY: TPoint; Azoom: byte);
var
  VPath: String;
  F:textfile;
begin
  VPath := GetTileFileName(AXY, Azoom);
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

procedure TMapType.SaveTileNotExists(x, y: Integer; Azoom: byte);
begin
  Self.SaveTileNotExists(Point(x shr 8, y shr 8), Azoom - 1);
end;

procedure TMapType.SaveTileSimple(AXY: TPoint; Azoom: byte; btm: TBitmap32);
var
  VPath: String;
begin
  if UseSave then begin
    VPath := GetTileFileName(AXY, Azoom);
    CreateDirIfNotExists(VPath);
    DeleteFile(ChangeFileExt(Vpath,'.tne'));
    SaveTileInCache(btm, Vpath);
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

procedure TMapType.SaveTileSimple(x, y: Integer; Azoom: byte;
  btm: TBitmap32);
begin
  Self.SaveTileSimple(Point(x shr 8, y shr 8), Azoom - 1, btm);
end;

function TMapType.TileExportToFile(AXY: TPoint; Azoom: byte;
  AFileName: string; OverWrite: boolean): boolean;
var
  VPath: String;
begin
  VPath := GetTileFileName(AXY, Azoom);
  CreateDirIfNotExists(AFileName);
  Result := CopyFile(PChar(VPath), PChar(AFileName), not OverWrite);
end;

function TMapType.TileExportToFile(x, y: Integer; Azoom: byte;
  AFileName: string; OverWrite: boolean): boolean;
begin
  Result := Self.TileExportToFile(Point(x shr 8, y shr 8), Azoom - 1, AFileName, OverWrite);
end;

function TMapType.LoadFillingMap(btm: TBitmap32; AXY: TPoint; Azoom,
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
begin
  Result := true;
  try
    GeoConvert.CheckTilePosStrict(AXY, Azoom, GState.CiclMap);
    GeoConvert.CheckZoom(ASourceZoom);

    VPixelsRect := GeoConvert.TilePos2PixelRect(AXY, Azoom);

    VTileSize := Point(VPixelsRect.Right - VPixelsRect.Left + 1, VPixelsRect.Bottom - VPixelsRect.Top + 1);

    btm.Width := VTileSize.X;
    btm.Height := VTileSize.Y;
    btm.Clear(clBlack);

    VRelativeRect := GeoConvert.TilePos2RelativeRect(AXY, Azoom);
    VSourceTilesRect := GeoConvert.RelativeRect2TileRect(VRelativeRect, ASourceZoom);
    if (VTileSize.X >= (VSourceTilesRect.Right - VSourceTilesRect.Left + 1)) and
      (VTileSize.Y >= (VSourceTilesRect.Right - VSourceTilesRect.Left + 1)) then
    begin
      VClMZ := SetAlpha(Color32(GState.MapZapColor), GState.MapZapAlpha);
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
            VSourceTilesRect.Left := VSourceTilesRect.Left - VPixelsRect.Left;
            VSourceTilesRect.Top := VSourceTilesRect.Top - VPixelsRect.Top;
            VSourceTilesRect.Right := VSourceTilesRect.Right - VPixelsRect.Left;
            VSourceTilesRect.Bottom := VSourceTilesRect.Bottom - VPixelsRect.Top;
            btm.FillRectS(VSourceTilesRect, VClMZ);
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

function TMapType.LoadFillingMap(btm: TBitmap32; x, y: Integer; Azoom,
  ASourceZoom: byte; IsStop: PBoolean): boolean;
begin
  Result := Self.LoadFillingMap(btm, Point(x shr 8, y shr 8), Azoom - 1, ASourceZoom - 1, IsStop);
end;

function TMapType.GetShortFolderName: string;
begin
  Result := ExtractFileName(ExtractFileDir(IncludeTrailingPathDelimiter(NameInCache)));
end;


function TMapType.GetCoordConverter: ICoordConverter;
begin
 if Self=nil
  then Result:= nil
  else Result:= FCoordConverter;
end;

function TMapType.CheckIsBan(AXY: TPoint; AZoom: byte;
  StatusCode: Cardinal; ty: string; fileBuf: TMemoryStream): Boolean;
begin
  Result := false;
  if (ty <> FContent_type)
    and(fileBuf.Size <> 0)
    and(FBanIfLen <> 0)
    and(fileBuf.Size < (FBanIfLen + 50))
    and(fileBuf.Size >(FBanIfLen-50)) then
  begin
    result := true;
  end;
end;

function TMapType.IncDownloadedAndCheckAntiBan: Boolean;
var
  cnt: Integer;
begin
  cnt := InterlockedIncrement(FDownloadTilesCount);
  if (FUseAntiBan > 1) then begin
    Result := (cnt mod FUseAntiBan) = 0;
  end else begin
    Result := (FUseAntiBan > 0) and  (cnt = 1);
  end;
end;

procedure TMapType.addDwnforban;
begin
  if (FUseAntiBan>0) then begin
    Fmain.WebBrowser1.Navigate('http://maps.google.com/?ie=UTF8&ll='+inttostr(random(100)-50)+','+inttostr(random(300)-150)+'&spn=1,1&t=k&z=8');
  end;
end;

procedure TMapType.ExecOnBan(ALastUrl: string);
begin
  if ban_pg_ld then begin
    Fmain.ShowCaptcha(ALastUrl);
    ban_pg_ld:=false;
  end;
end;

constructor TMapType.Create;
begin
  FInitDownloadCS := TCriticalSection.Create;
  FCSSaveTile := TCriticalSection.Create;
  FCSSaveTNF := TCriticalSection.Create;
end;

destructor TMapType.Destroy;
begin
  FreeAndNil(FInitDownloadCS);
  FreeAndNil(FCSSaveTile);
  FreeAndNil(FCSSaveTNF);
  inherited;
end;

function TMapType.DownloadTile(AXY: TPoint; AZoom: byte;
  ACheckTileSize: Boolean; AOldTileSize: Integer;
  out AUrl: string; out AContentType: string;
  fileBuf: TMemoryStream): TDownloadTileResult;
var
  StatusCode: Cardinal;
  VPoolElement: IPoolElement;
  VDownloader: ITileDownlodSession;
begin
  if Self.UseDwn then begin
    AUrl := GetLink(AXY.X, AXY.Y, AZoom);
    VPoolElement := FPoolOfDownloaders.TryGetPoolElement(60000);
    if VPoolElement = nil then begin
      raise Exception.Create('No free connections');
    end;
    VDownloader := VPoolElement.GetObject as ITileDownlodSession;
    Result := VDownloader.DownloadTile(AUrl, ACheckTileSize, AOldTileSize, fileBuf, StatusCode, AContentType);
    if CheckIsBan(AXY, AZoom, StatusCode, AContentType, fileBuf) then begin
      result := dtrBanError;
    end;
  end else begin
    raise Exception.Create('Для этой карты загрузка запрещена.');
  end;
end;

function TMapType.GetTileShowName(x, y: Integer; Azoom: byte): string;
begin
  Result := Self.GetTileShowName(Point(x shr 8, y shr 8), Azoom - 1);
end;

function TMapType.GetTileShowName(AXY: TPoint; Azoom: byte): string;
begin
  if Self.IsStoreFileCache then begin
    Result := GetTileFileName(AXY, Azoom)
  end else begin
    Result := 'z' + IntToStr(Azoom + 1) + 'x' + IntToStr(AXY.X) + 'y' + IntToStr(AXY.Y);
  end;
end;

function TMapType.GetIsStoreFileCache: Boolean;
begin
  if ((CacheType=0)and(GState.DefCache=5))or(CacheType=5) then begin
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
  if ((CacheType=0)and(GState.DefCache=5))or(CacheType=5) then begin
    Result := True;
  end else begin
    Result := FIsStoreReadOnly;
  end;
end;

function TMapType.GetUseStick: boolean;
begin
  if TileFileExt<>'.kml' then begin
    Result := FUseStick;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetIsCanShowOnSmMap: boolean;
begin
  if TileFileExt<>'.kml' then begin
    Result := FIsCanShowOnSmMap;
  end else begin
    Result := False;
  end;
end;

function TMapType.GetShowOnSmMap: boolean;
begin
  if Self.IsCanShowOnSmMap then begin
    Result := FShowOnSmMap;
  end else begin
    Result := False
  end;
end;

procedure TMapType.SetShowOnSmMap(const Value: boolean);
begin
  FShowOnSmMap := Value;
end;

function TMapType.GetBitmapTypeManager: IBitmapTypeExtManager;
begin
  Result := GState.BitmapTypeManager;
end;

procedure TMapType.CropOnDownload(ABtm: TBitmap32; ATileSize: TPoint);
var
  VBtmSrc: TBitmap32;
  VBtmDest: TBitmap32;
begin
  VBtmSrc := TBitmap32.Create;
  try
    VBtmSrc.Assign(ABtm);
    VBtmSrc.Resampler := TLinearResampler.Create;
    VBtmDest := TBitmap32.Create;
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

end.
