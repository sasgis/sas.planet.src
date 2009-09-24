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
  Menus,
  math,
  ExtCtrls,
  Uprogress,
  TBX,
  VCLZip,
  GR32,
  u_CoordConverterAbstract,
  u_UrlGenerator,
  UResStrings;

type
 TMapType = class
   protected
    FCoordConverter : ICoordConverter;
    FUrlGenerator : TUrlGenerator;
    TileRect:TRect;
    pos: integer;
    filename: string;
   public
    id: integer;
    guids: string;
    active:boolean;
    info:string;
    showinfo:boolean;
    ShowOnSmMap:boolean;
    asLayer:boolean;
    name: string;
    Icon24Name,Icon18Name:string;
    HotKey:TShortCut;
    DefHotKey:TShortCut;
    URLBase: string;
    DefURLBase: string;
    UseDwn,Usestick,UseGenPrevious,Usedel,Usesave:boolean;
    UseSubDomain:boolean;
    UseAntiBan:integer;
    Sleep,DefSleep:Integer;
    separator:boolean;
    Defseparator:boolean;
    DelAfterShow:boolean;
    GetURLScript:string;
    projection:byte;
    cachetype:byte;
    defcachetype:byte;
    CONTENT_TYPE:string;
    BanIfLen:integer;
    radiusa,radiusb,exct:extended;
    ext,ParentSubMenu:string;
    DefParentSubMenu:string;
    NSmItem,TBItem,NLayerParamsItem,TBFillingItem:TTBXItem;
    TBSubMenuItem:TTBXSubmenuItem;
    NDwnItem,NDelItem:TMenuItem;
    NameInCache:string;
    DefNameInCache:string;
    bmp18,bmp24:TBitmap;
    function GetLink(x,y:longint;Azoom:byte):string;
    function GetMapSize(zoom:byte):longint;
    procedure LoadMapTypeFromZipFile(AZipFileName : string; pnum : Integer);
    function GetTileFileName(x,y:longint;Azoom:byte):string;
    function TileExists(x,y:longint;Azoom:byte): Boolean;
    function TileNotExistsOnServer(x,y:longint;Azoom:byte): Boolean;
    function LoadTile(btm:Tobject; x,y:longint;Azoom:byte; caching:boolean):boolean;
    // Строит карту заполнения дл тайла на уровне AZoom тайлами уровня ASourceZoom
    // Должна регулярно проверять по указателю IsStop не нужно ли прерваться
    function LoadFillingMap(btm:TBitmap32; x,y:longint;Azoom:byte;ASourceZoom: byte; IsStop: PBoolean):boolean;
    function DeleteTile(x,y:longint;Azoom:byte): Boolean;
    procedure SaveTileDownload(x,y:longint;Azoom:byte; ATileStream:TCustomMemoryStream; ty: string);
    procedure SaveTileSimple(x,y:longint;Azoom:byte; btm:TObject);
    procedure SaveTileNotExists(x,y:longint;Azoom:byte);
    function TileLoadDate(x,y:longint;Azoom:byte): TDateTime;
    function TileSize(x,y:longint;Azoom:byte): integer;
    function TileExportToFile(x,y:longint;Azoom:byte; AFileName: string; OverWrite: boolean): boolean;
    property GeoConvert: ICoordConverter read FCoordConverter;
  private
    err: string;
    function LoadFile(btm:Tobject; APath: string; caching:boolean):boolean;
    procedure CreateDirIfNotExists(APath:string);
    procedure SaveTileInCache(btm:TObject;path:string);
  end;
var
  MapType:array of TMapType;
  MapsEdit:boolean;
  procedure LoadMaps;
  procedure SaveMaps;
  procedure CreateMapUI;
  function GetMapFromID(id:string):TMapType;

implementation

uses
  pngimage,
  IJL,
  jpeg,
  RxGIF,
  GR32_Resamplers,
  VCLUnZip,
  Usettings,
  unit1,
  UGeoFun,
  UFillingMap,
  DateUtils,
  ImgMaker,
  UKmlParse,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid,
  u_CoordConverterSimpleLonLat;

function GetMapFromID(id:string):TMapType;
var i:integer;
begin
 for i:=0 to length(MapType)-1 do
  if MapType[i].guids=id then begin
                               result:=MapType[i];
                               exit;
                              end;
 result:=nil;
end;

procedure CreateMapUI;
var i,j:integer;
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
 for i:=0 to Fmain.PopupMSmM.Items.Count-3 do Fmain.PopupMSmM.Items.Items[2].Free;

 sm_map.maptype:=nil;
 fillingmaptype:=nil;
 i:=length(MapType)-1;
 if i>0 then
 for i:=0 to length(MapType)-1 do
  With MapType[i] do
  begin
   TBItem:=TTBXItem.Create(Fmain.TBSMB);
   if ParentSubMenu=''
    then if asLayer then Fmain.TBLayerSel.Add(TBItem)
                    else Fmain.TBSMB.Add(TBItem)
    else begin
          j:=0;
          While MapType[j].ParentSubMenu<>ParentSubMenu do inc(j);
          TBSubMenuItem:=TTBXSubmenuItem.Create(Fmain.TBSMB);
          TBSubMenuItem.caption:=ParentSubMenu;
          TBSubMenuItem.Images:=Fmain.MapIcons18;
          if j=i then
          begin
          if asLayer then Fmain.TBLayerSel.Add(TBSubMenuItem)
                     else Fmain.TBSMB.Add(TBSubMenuItem);
          end;
          MapType[j].TBSubMenuItem.Add(TBItem);
         end;
   Fmain.MapIcons24.AddMasked(bmp24,RGB(255,0,255));
   Fmain.MapIcons18.AddMasked(bmp18,RGB(255,0,255));
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

   if ext<>'.kml' then
    begin
     if not(asLayer) then begin
                           NSmItem:=TTBXITem.Create(Fmain.PopupMSmM);
                           Fmain.PopupMSmM.Items.Add(NSmItem)
                          end
                     else begin
                           NSmItem:=TTBXITem.Create(Fmain.NSubMenuSmItem);
                           Fmain.NSubMenuSmItem.Add(NSmItem);
                          end;
     NSmItem.Name:='NSmMapN'+inttostr(id);
     NSmItem.ImageIndex:=i;
     NSmItem.Caption:=name;
     NSmItem.OnAdjustFont:=Fmain.AdjustFont;
     NSmItem.OnClick:=Fmain.NMMtype_0Click;
     if ShowOnSmMap then NSmItem.Checked:=true;
    end;
   if asLayer then
    begin
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
   if (asLayer)and(active) then TBItem.Checked:=true;
   if separator then
    begin
     TBItem.Parent.Add(TTBXSeparatorItem.Create(Fmain.TBSMB));
     if NSmItem<>NIL  then  NSmItem.Parent.Add(TTBXSeparatorItem.Create(Fmain.NSubMenuSmItem));
     TBFillingItem.Parent.Add(TTBXSeparatorItem.Create(Fmain.NSubMenuSmItem));
    end;
   if (active)and(MapType[i].asLayer=false) then sat_map_both:=MapType[i];
   if (ShowOnSmMap)and(not(asLayer)) then sm_map.maptype:=MapType[i];
   TBItem.Tag:=Longint(MapType[i]);
   TBFillingItem.Tag:=Longint(MapType[i]);
   if ext<>'.kml' then NSmItem.Tag:=Longint(MapType[i]);
   if asLayer then
    begin
     NDwnItem.Tag:=longint(MapType[i]);
     NDelItem.Tag:=longint(MapType[i]);
     NLayerParamsItem.Tag:=longint(MapType[i]);
    end;
   FSettings.MapList.AddItem(MapType[i].name,nil);
   FSettings.MapList.Items.Item[i].Data:=MapType[i];
   FSettings.MapList.Items.Item[i].SubItems.Add(MapType[i].NameInCache);
   if MapType[i].asLayer then FSettings.MapList.Items.Item[i].SubItems.Add(SAS_STR_Layers+'\'+MapType[i].ParentSubMenu)
                         else FSettings.MapList.Items.Item[i].SubItems.Add(SAS_STR_Maps+'\'+MapType[i].ParentSubMenu);
   FSettings.MapList.Items.Item[i].SubItems.Add(ShortCutToText(MapType[i].HotKey));
   FSettings.MapList.Items.Item[i].SubItems.Add(MapType[i].filename);
  end;
 if FSettings.MapList.Items.Count>0 then FSettings.MapList.Items.Item[0].Selected:=true;
 if longint(sm_map.maptype)=0 then Fmain.NMMtype_0.Checked:=true;
 if (sat_map_both=nil)and(MapType[0]<>nil) then sat_map_both:=MapType[0];
end;

procedure LoadMaps;
var  Ini: TMeminifile;
     i,j,k,pnum: integer;
     startdir : string;
     SearchRec: TSearchRec;
     MTb:TMapType;
begin
 SetLength(MapType,0);
 sm_map.maptype:=nil;
 CreateDir(extractfilepath(paramstr(0))+'Maps');
 Ini:=TMeminiFile.Create(extractfilepath(paramstr(0))+'Maps\Maps.ini');
 i:=0;
 pnum:=0;
 startdir:=extractfilepath(paramstr(0))+'maps\';
 if FindFirst(startdir+'*.zmp', faAnyFile, SearchRec) = 0 then
  repeat
   inc(i);
  until FindNext(SearchRec) <> 0;
 SysUtils.FindClose(SearchRec);
 SetLength(MapType,i);
 if FindFirst(startdir+'*.zmp', faAnyFile, SearchRec) = 0 then
  repeat
   if (SearchRec.Attr and faDirectory) = faDirectory then continue;
   MapType[pnum]:=TMapType.Create;
   MapType[pnum].LoadMapTypeFromZipFile(startdir+SearchRec.Name, pnum);
   if Ini.SectionExists(MapType[pnum].GUIDs)
    then With MapType[pnum] do
          begin
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
          end
    else With MapType[pnum] do
          begin
           showinfo:=true;
           if pos < 0 then pos := i;
           id := pos;
           dec(i);
           active:=false;
           ShowOnSmMap:=false;
          end;
   inc(pnum);
  until FindNext(SearchRec) <> 0;
 SysUtils.FindClose(SearchRec);
 ini.Free;

 k := length(MapType) shr 1;
 while k>0 do
  begin
   for i:=0 to length(MapType)-k-1 do
    begin
      j:=i;
      while (j>=0)and(MapType[j].id>MapType[j+k].id) do
      begin
        MTb:=MapType[j];
        MapType[j]:=MapType[j+k];
        MapType[j+k]:=MTb;
        if j>k then Dec(j,k)
               else j:=0;
      end;
    end;
   k:=k shr 1;
  end;
 MTb:=nil;
 MTb.Free;
 for i:=0 to length(MapType)-1 do begin MapType[i].id:=i+1; end;
end;

procedure SaveMaps;
var  Ini: TMeminifile;
       i: integer;
begin
 Ini:=TMeminiFile.Create(extractfilepath(paramstr(0))+'Maps\Maps.ini');
 for i:=0 to length(MapType)-1 do
       begin
         ini.WriteInteger(MapType[i].guids,'pnum',MapType[i].id);
         ini.WriteBool(MapType[i].guids,'active',MapType[i].active);
         ini.WriteBool(MapType[i].guids,'ShowOnSmMap',MapType[i].ShowOnSmMap);
         if MapType[i].URLBase<>MapType[i].DefURLBase then
          ini.WriteString(MapType[i].guids,'URLBase',MapType[i].URLBase)
          else Ini.DeleteKey(MapType[i].guids,'URLBase');
         if MapType[i].HotKey<>MapType[i].DefHotKey then
          ini.WriteInteger(MapType[i].guids,'HotKey',MapType[i].HotKey)
          else Ini.DeleteKey(MapType[i].guids,'HotKey');
         if MapType[i].cachetype<>MapType[i].defcachetype then
          ini.WriteInteger(MapType[i].guids,'CacheType',MapType[i].CacheType)
          else Ini.DeleteKey(MapType[i].guids,'CacheType');
         if MapType[i].separator<>MapType[i].Defseparator then
          ini.WriteBool(MapType[i].guids,'separator',MapType[i].separator)
          else Ini.DeleteKey(MapType[i].guids,'separator');
         if MapType[i].NameInCache<>MapType[i].DefNameInCache then
          ini.WriteString(MapType[i].guids,'NameInCache',MapType[i].NameInCache)
          else Ini.DeleteKey(MapType[i].guids,'NameInCache');
         if MapType[i].Sleep<>MapType[i].DefSleep then
          ini.WriteInteger(MapType[i].guids,'Sleep',MapType[i].sleep)
          else Ini.DeleteKey(MapType[i].guids,'Sleep');
         if MapType[i].ParentSubMenu<>MapType[i].DefParentSubMenu then
          ini.WriteString(MapType[i].guids,'ParentSubMenu',MapType[i].ParentSubMenu)
          else Ini.DeleteKey(MapType[i].guids,'ParentSubMenu');
       end;
 Ini.UpdateFile;
 ini.Free;
end;

procedure TMapType.LoadMapTypeFromZipFile(AZipFileName: string; pnum : Integer);
var
  MapParams:TMemoryStream;
  AZipFile:TFileStream;
  ParamsTempFile : String;
  iniparams: TMeminifile;
  GUID:TGUID;
  guidstr : string;
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
  filename := AZipFileName;
  UnZip:=TVCLZip.Create(nil);
  try
    AZipFile:=TFileStream.Create(AZipFileName,fmOpenRead or fmShareDenyNone);
    UnZip.ZipName:=AZipFileName;
    MapParams:=TMemoryStream.Create;
    try
      UnZip.UnZip;
      UnZip.UnZipToStream(MapParams,'params.txt');
      ParamsTempFile := c_GetTempPath+'params.~txt';
      MapParams.SaveToFile(ParamsTempFile);
      iniparams:=TMemIniFile.Create(ParamsTempFile);
    finally
      FreeAndNil(MapParams);
    end;
    try
      guidstr:=iniparams.ReadString('PARAMS','ID',GUIDToString(GUID));
      name:=iniparams.ReadString('PARAMS','name','map#'+inttostr(pnum));
      name:=iniparams.ReadString('PARAMS','name_'+inttostr(localization),name);


      MapParams:=TMemoryStream.Create;
      try
      if (UnZip.UnZipToStream(MapParams,'info_'+inttostr(localization)+'.txt')>0)or(UnZip.UnZipToStream(MapParams,'info.txt')>0) then
       begin
        SetLength(info,MapParams.size);
        MapParams.Position:=0;
        MapParams.ReadBuffer(info[1],MapParams.size);
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
          GetURLScript:=GetURLScript+copy(bb,1, NumRead);
        until (NumRead = 0);
      finally
        FreeAndNil(MapParams);
      end;
      bmp24:=TBitmap.create;
      MapParams:=TMemoryStream.Create;
      try
        try
          UnZip.UnZipToStream(MapParams,'24.bmp');
          MapParams.Position:=0;
          bmp24.LoadFromStream(MapParams);
        except
          bmp24.Canvas.FillRect(bmp24.Canvas.ClipRect); bmp24.Width:=24; bmp24.Height:=24; bmp24.Canvas.TextOut(7,3,copy(name,1,1))
        end;
      finally
        FreeAndNil(MapParams);
      end;
      bmp18:=TBitmap.create;
      MapParams:=TMemoryStream.Create;
      try
        try
          UnZip.UnZipToStream(MapParams,'18.bmp');
          MapParams.Position:=0;
          bmp18.LoadFromStream(MapParams);
        except
          bmp18.Canvas.FillRect(bmp18.Canvas.ClipRect); bmp18.Width:=18; bmp18.Height:=18; bmp18.Canvas.TextOut(3,2,copy(name,1,1))
        end;
      finally
        FreeAndNil(MapParams);
      end;
      GUIDs:=iniparams.ReadString('PARAMS','GUID',GUIDstr);
      asLayer:=iniparams.ReadBool('PARAMS','asLayer',false);
      URLBase:=iniparams.ReadString('PARAMS','DefURLBase','http://maps.google.com/');
      DefUrlBase:=URLBase;
      TileRect.Left:=iniparams.ReadInteger('PARAMS','TileRLeft',0);
      TileRect.Top:=iniparams.ReadInteger('PARAMS','TileRTop',0);
      TileRect.Right:=iniparams.ReadInteger('PARAMS','TileRRight',0);
      TileRect.Bottom:=iniparams.ReadInteger('PARAMS','TileRBottom',0);
      UseDwn:=iniparams.ReadBool('PARAMS','UseDwn',true);
      Usestick:=iniparams.ReadBool('PARAMS','Usestick',true);
      UseGenPrevious:=iniparams.ReadBool('PARAMS','UseGenPrevious',true);
      Usedel:=iniparams.ReadBool('PARAMS','Usedel',true);
      DelAfterShow:=iniparams.ReadBool('PARAMS','DelAfterShow',false);
      Usesave:=iniparams.ReadBool('PARAMS','Usesave',true);
      UseAntiBan:=iniparams.ReadInteger('PARAMS','UseAntiBan',0);
      CacheType:=iniparams.ReadInteger('PARAMS','CacheType',0);
      DefCacheType:=CacheType;
      Sleep:=iniparams.ReadInteger('PARAMS','Sleep',0);
      DefSleep:=Sleep;
      BanIfLen:=iniparams.ReadInteger('PARAMS','BanIfLen',0);
      CONTENT_TYPE:=iniparams.ReadString('PARAMS','ContentType','image\jpg');
      Ext:=LowerCase(iniparams.ReadString('PARAMS','Ext','.jpg'));
      NameInCache:=iniparams.ReadString('PARAMS','NameInCache','Sat');
      DefNameInCache:=NameInCache;
      projection:=iniparams.ReadInteger('PARAMS','projection',1);
      radiusa:=iniparams.ReadFloat('PARAMS','sradiusa',1);
      radiusb:=iniparams.ReadFloat('PARAMS','sradiusb',radiusa);
      HotKey:=iniparams.ReadInteger('PARAMS','DefHotKey',0);
      DefHotKey:=HotKey;
      ParentSubMenu:=iniparams.ReadString('PARAMS','ParentSubMenu','');
      ParentSubMenu:=iniparams.ReadString('PARAMS','ParentSubMenu_'+inttostr(localization),ParentSubMenu);
      DefParentSubMenu:=ParentSubMenu;
      separator:=iniparams.ReadBool('PARAMS','separator',false);
      Defseparator:=separator;
      exct:=sqrt(radiusa*radiusa-radiusb*radiusb)/radiusa;
      pos:=iniparams.ReadInteger('PARAMS','pnum',-1);
      case projection of
        1: FCoordConverter := TCoordConverterMercatorOnSphere.Create(radiusa);
        2: FCoordConverter := TCoordConverterMercatorOnEllipsoid.Create(Exct,radiusa,radiusb);
        3: FCoordConverter := TCoordConverterSimpleLonLat.Create(radiusa);
        else raise Exception.Create('Ошибочный тип проэкции карты ' + IntToStr(projection));
      end;
      try
      FUrlGenerator := TUrlGenerator.Create('procedure Return(Data: string); begin ResultURL := Data; end; ' + GetURLScript, FCoordConverter);
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

function TMapType.GetLink(x,y:Integer;Azoom:byte): string;
begin
  if (FUrlGenerator = nil) then result:='';
  if not(Azoom in [1..24]) then raise Exception.Create('Ошибочный Zoom');
  if x>=0 then x:=x mod zoom[Azoom]
          else x:=zoom[Azoom]+(x mod zoom[Azoom]);
  if y>=0 then y:=y mod zoom[Azoom]
              else y:=zoom[Azoom]+(y mod zoom[Azoom]);

  FUrlGenerator.GetURLBase:=URLBase;
  Result:=FUrlGenerator.GenLink(x,y,Azoom-1);
end;

function TMapType.GetMapSize(zoom:byte):longint;
begin
 result:=round(intpower(2,zoom))*256;
end;

function TMapType.GetTileFileName(x, y: Integer; Azoom: byte): string;
function full(int,z:integer):string;
var s,s1:string;
    i:byte;
begin
 result:='';
 s:=inttostr(int);
 s1:=inttostr(zoom[z] div 256);
 for i:=length(s) to length(s1)-1 do result:=result+'0';
 result:=result+s;
end;
var os,prer:TPoint;
    i,ct:byte;
    sbuf,name,fname:String;
    ms:TMemoryStream;
    SearchRec:TSearchRec;
begin

 if (CacheType=0) then ct:=DefCache
                       else ct:=CacheType;
 if x>=0 then x:=x mod zoom[Azoom]
         else x:=zoom[Azoom]+(x mod zoom[Azoom]);
 case ct of
 1:
 begin
   sbuf:=Format('%.*d', [2, Azoom]);
   result:=OldCpath_;
   result:=result + NameInCache+'\'+sbuf+'\t';
   os.X:=zoom[Azoom]shr 1;
   os.Y:=zoom[Azoom]shr 1;
   prer:=os;
   for i:=2 to Azoom do
    begin
    prer.X:=prer.X shr 1;
    prer.Y:=prer.Y shr 1;
    if x<os.X
     then begin
           os.X:=os.X-prer.X;
           if y<os.y then begin
                            os.Y:=os.Y-prer.Y;
                            result:=result+'q';
                           end
                      else begin
                            os.Y:=os.Y+prer.Y;
                            result:=result+'t';
                           end;
          end
     else begin
           os.X:=os.X+prer.X;
           if y<os.y then begin
                           os.Y:=os.Y-prer.Y;
                           result:=result+'r';
                          end
                     else begin
                           os.Y:=os.Y+prer.Y;
                           result:=result+'s';
                          end;
         end;
    end;
  result:=result + ext;
 end;
 2:
 begin
  result:=NewCpath_;
  x:=x shr 8;
  y:=y shr 8;
  result:=result+NameInCache+format('\z%d\%d\x%d\%d\y%d',[Azoom,x shr 10,x,y shr 10,y])+ext;
 end;
 3:
 begin
   sbuf:=Format('%.*d', [2, Azoom]);
   result:=ESCpath_;
   name:=sbuf+'-'+full(x shr 8,Azoom)+'-'+full(y shr 8,Azoom);
   if Azoom<7
    then result:=result+NameInCache+'\'+sbuf+'\'
    else if Azoom<11
          then result:=result+NameInCache+'\'+sbuf+'\'+Chr(59+Azoom)+
                       full((x shr 8)shr 5,Azoom-5)+full((y shr 8)shr 5,Azoom-5)+'\'
          else result:=result+NameInCache+'\'+'10'+'-'+full((x shr (Azoom-10))shr 8,10)+'-'+
                       full((y shr (Azoom-10))shr 8,10)+'\'+sbuf+'\'+Chr(59+Azoom)+
                       full((x shr 8)shr 5,Azoom-5)+full((y shr 8)shr 5,Azoom-5)+'\';
   result:=result+name+ext;
 end;
 4,41:
 begin
  result:=GMTilespath_;
  x:=x shr 8;
  y:=y shr 8;
  if ct=4 then result:=result+NameInCache+format('\z%d\%d\%d'+ext,[Azoom-1,Y,X])
          else result:=result+NameInCache+format('\z%d\%d_%d'+ext,[Azoom-1,Y,X]);
 end;
 5:
 begin
  result:=GECachepath_;
  result:=result+NameInCache;
  fname:='buf'+GEXYZtoTileName(x,y,Azoom)+'.jpg';
  if (result[2]<>'\')and(system.pos(':',result)=0)
   then result:=ProgrammPath+result;
  if (not FileExists(result+'\'+fname))and(FileExists(result+'\dbCache.dat'))and(FileExists(result+'\dbCache.dat.index'))then
  try
   x:=x shr 8;
   y:=y shr 8;
   if FindFirst(result+'\*.jpg', faAnyFile, SearchRec) = 0 then
    repeat
     if (SearchRec.Attr and faDirectory) <> faDirectory then
       DeleteFile(result+'\'+SearchRec.Name);
    until FindNext(SearchRec) <> 0;
   FindClose(SearchRec);

   if GetMerkatorGETile(ms,result+'\dbCache.dat',x,y,Azoom, Self)
    then ms.SaveToFile(result+'\'+fname);
   FreeAndNil(ms);
  except
  end;
  result:=result+'\'+fname;
 end;
 end;
 if (result[2]<>'\')and(system.pos(':',result)=0)
   then result:=ProgrammPath+result;
end;

function TMapType.TileExists(x, y: Integer; Azoom: byte): Boolean;
var
  VPath: String;
begin
  VPath := GetTileFileName(x, y, Azoom);
  Result := Fileexists(VPath);
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

function LoadJPG32(FileName: string; Btm: TBitmap32): boolean;
  procedure RGBA2BGRA2(pData : Pointer; Width, Height : Integer);
  var W, H : Integer;
      p : PInteger;
  begin
    p := PInteger(pData);
    for H := 0 to Height-1 do
    begin
      for W := 0 to Width-1 do
      begin
        p^:= (byte(p^ shr 24) shl 24) or byte(p^ shr 16) or
            (integer(byte(p^ shr 8)) shl 8) or (integer(byte(p^)) shl 16);
        Inc(p);
      end;
    end;
  end;
const
  sRead : array [Boolean] of String = ('JFILE_READ = ','JBUFF_READ = ');
var
  iWidth, iHeight, iNChannels : Integer;
  iStatus : Integer;
  jcprops : TJPEG_CORE_PROPERTIES;
begin
 try
    result:=true;
    iStatus := ijlInit(@jcprops);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    jcprops.JPGFile := PChar(FileName);
    iStatus := ijlRead(@jcprops,IJL_JFILE_READPARAMS);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    iWidth := jcprops.JPGWidth;
    iHeight := jcprops.JPGHeight;
    iNChannels := 4;
    Btm.SetSize(iWidth,iHeight);
    jcprops.DIBWidth := iWidth;
    jcprops.DIBHeight := iHeight;
    jcprops.DIBChannels := iNChannels;
    jcprops.DIBColor := IJL_RGBA_FPX;
    jcprops.DIBPadBytes := ((((iWidth*iNChannels)+3) div 4)*4)-(iWidth*iNChannels);
    jcprops.DIBBytes := PByte(Btm.Bits);
    if (jcprops.JPGChannels = 3) then
      jcprops.JPGColor := IJL_YCBCR
    else if (jcprops.JPGChannels = 4) then
      jcprops.JPGColor := IJL_YCBCRA_FPX
    else if (jcprops.JPGChannels = 1) then
      jcprops.JPGColor := IJL_G
    else
    begin
      jcprops.DIBColor := TIJL_COLOR (IJL_OTHER);
      jcprops.JPGColor := TIJL_COLOR (IJL_OTHER);
    end;
    iStatus := ijlRead(@jcprops,IJL_JFILE_READWHOLEIMAGE);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    if jcprops.DIBColor = IJL_RGBA_FPX then
      RGBA2BGRA2(jcprops.DIBBytes,iWidth,iHeight);
    ijlFree(@jcprops);
  except
    on E: Exception do
    begin
      result:=false;
      ijlFree(@jcprops);
    end;
  end;
end;

function TMapType.LoadTile(btm: Tobject; x,y:longint;Azoom:byte;
  caching: boolean): boolean;
var
  path: string;
begin
  path := GetTileFileName(x, y, Azoom);
  result:= LoadFile(btm, path, caching);
end;

function TMapType.DeleteTile(x, y: Integer; Azoom: byte): Boolean;
var
  VPath: string;
begin
  try
    VPath := GetTileFileName(x, y, Azoom);
    result:=DeleteFile(PChar(VPath));
  except
    Result := false;
  end;
end;

function TMapType.LoadFile(btm: Tobject; APath: string; caching:boolean): boolean;
begin
  Result := false;
  if GetFileSize(Apath)=0 then begin
    exit;
  end;
  try
    if (btm is TBitmap32) then begin
      if not(caching) then begin
        if ExtractFileExt(Apath)='.jpg' then begin
          if not(LoadJPG32(Apath,TBitmap32(btm))) then begin
            result:=false;
            exit;
          end;
        end else begin
          TBitmap32(btm).LoadFromFile(Apath);
        end;
        result:=true;
      end else begin
        if not MainFileCache.TryLoadFileFromCache(btm, Apath) then begin
          if ExtractFileExt(Apath)='.jpg' then begin
            if not(LoadJPG32(Apath,TBitmap32(btm))) then begin
              result:=false;
              exit;
            end
          end else begin
            TBitmap32(btm).LoadFromFile(Apath);
          end;
          MainFileCache.AddTileToCache(btm, Apath);
        end;
      end;
    end else begin
      if (btm is TPicture) then
        TPicture(btm).LoadFromFile(Apath)
      else if (btm is TJPEGimage) then
        TJPEGimage(btm).LoadFromFile(Apath)
      else if (btm is TPNGObject) then
        TPNGObject(btm).LoadFromFile(Apath)
      else if (btm is TKML) then
        TKML(btm).LoadFromFile(Apath)
      else if (btm is TGraphic) then
        TGraphic(btm).LoadFromFile(Apath);
    end;
    result:=true;
  except
  end;
end;

function TMapType.TileNotExistsOnServer(x, y: Integer;
  Azoom: byte): Boolean;
var
  VPath: String;
begin
  VPath := GetTileFileName(x, y, Azoom);
  Result := Fileexists(copy(VPath,1,length(VPath)-3)+'tne');
end;

procedure TMapType.CreateDirIfNotExists(APath:string);
var i:integer;
begin
 i := LastDelimiter('\', Apath);
 Apath:=copy(Apath, 1, i);
 if not(DirectoryExists(Apath)) then ForceDirectories(Apath);
end;


procedure TMapType.SaveTileDownload(x, y: Integer; Azoom: byte;
  ATileStream: TCustomMemoryStream; ty: string);
var
  VPath: String;
    jpg:TJPEGImage;
    btm:TBitmap;
    png:TBitmap32;
    btmSrc:TBitmap32;
    btmDest:TBitmap32;
    UnZip:TVCLUnZip;
begin
  VPath := GetTileFileName(x, y, Azoom);
  CreateDirIfNotExists(VPath);
  DeleteFile(copy(Vpath,1,length(Vpath)-3)+'tne');
  if ((copy(ty,1,8)='text/xml')or(ty='application/vnd.google-earth.kmz'))and(ext='.kml')then begin
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
        err:=SAS_ERR_BadFile;
      end;
    end;
  end;

  SaveTileInCache(ATileStream,Vpath);
  if (TileRect.Left<>0)or(TileRect.Top<>0)or
    (TileRect.Right<>0)or(TileRect.Bottom<>0) then begin
    btmsrc:=TBitmap32.Create;
    btmDest:=TBitmap32.Create;
    try
      btmSrc.Resampler:=TLinearResampler.Create;
      if LoadFile(btmsrc,Vpath,false) then begin
        btmDest.SetSize(256,256);
        btmdest.Draw(bounds(0,0,256,256),TileRect,btmSrc);
        SaveTileInCache(btmDest,Vpath);
      end;
    except
    end;
    btmSrc.Free;
    btmDest.Free;
  end;

  ban_pg_ld:=true;
  if (ty='image/png')and(ext='.jpg') then begin
    btm:=TBitmap.Create;
    png:=TBitmap32.Create;
    jpg:=TJPEGImage.Create;
    RenameFile(Vpath,copy(Vpath,1,length(Vpath)-4)+'.png');
    if LoadFile(png,copy(Vpath,1,length(Vpath)-4)+'.png',false) then begin
      btm.Assign(png);
      jpg.Assign(btm);
      SaveTileInCache(jpg,Vpath);
      DeleteFile(copy(Vpath,1,length(Vpath)-4)+'.png');
      btm.Free;
      jpg.Free;
      png.Free;
    end;
  end;
end;

procedure TMapType.SaveTileInCache(btm:TObject;path:string);
var
    Jpg_ex:TJpegImage;
    png_ex:TPNGObject;
    Gif_ex:TGIFImage;
    btm_ex:TBitmap;
begin
 if (btm is TBitmap32) then
  begin
   btm_ex:=TBitmap.Create;
   btm_ex.Assign(btm as TBitmap32);
   if UpperCase(ExtractFileExt(path))='.JPG' then
    begin
     Jpg_ex:=TJpegImage.Create;
     Jpg_ex.CompressionQuality:=85;
     Jpg_ex.Assign(btm_ex);
     Jpg_ex.SaveToFile(path);
     Jpg_ex.Free;
    end;
   if UpperCase(ExtractFileExt(path))='.GIF' then
    begin
     Gif_ex:=TGifImage.Create;
     Gif_ex.Assign(btm_ex);
     Gif_ex.SaveToFile(path);
     Gif_ex.Free;
    end;
   if UpperCase(ExtractFileExt(path))='.PNG' then
    begin
     PNG_ex:=TPNGObject.Create;
     PNG_ex.Assign(btm_ex);
     PNG_ex.SaveToFile(path);
     PNG_ex.Free;
    end;
   if UpperCase(ExtractFileExt(path))='.BMP' then btm_ex.SaveToFile(path);
   btm_ex.Free;
  end;
 if (btm is TJPEGimage) then TJPEGimage(btm).SaveToFile(path) else
 if (btm is TPNGObject) then TPNGObject(btm).SaveToFile(path) else
 if (btm is TMemoryStream) then TMemoryStream(btm).SaveToFile(path) else
 if (btm is TPicture) then TPicture(btm).SaveToFile(path);
end;


function TMapType.TileLoadDate(x, y: Integer; Azoom: byte): TDateTime;
var
  VPath: String;
begin
  VPath := GetTileFileName(x, y, Azoom);
  Result := FileDateToDateTime(FileAge(VPath));
end;

function TMapType.TileSize(x, y: Integer; Azoom: byte): integer;
var
  VPath: String;
begin
  VPath := GetTileFileName(x, y, Azoom);
  Result := GetFileSize(VPath);
end;

procedure TMapType.SaveTileNotExists(x, y: Integer; Azoom: byte);
var
  VPath: String;
  F:textfile;
begin
  VPath := GetTileFileName(x, y, Azoom);
 if not(FileExists(copy(Vpath,1,length(Vpath)-3)+'tne')) then
  begin
   CreateDirIfNotExists(copy(Vpath,1,length(Vpath)-3)+'tne');
   AssignFile(f,copy(Vpath,1,length(Vpath)-3)+'tne');
   Rewrite(F);
   Writeln(f,DateTimeToStr(now));
   CloseFile(f);
  end;
end;

procedure TMapType.SaveTileSimple(x, y: Integer; Azoom: byte;
  btm:TObject);
var
  VPath: String;
begin
  VPath := GetTileFileName(x, y, Azoom);
  CreateDirIfNotExists(VPath);
  DeleteFile(copy(Vpath,1,length(Vpath)-3)+'tne');
  SaveTileInCache(btm,Vpath);
end;

function TMapType.TileExportToFile(x, y: Integer; Azoom: byte;
  AFileName: string; OverWrite: boolean): boolean;
var
  VPath: String;
begin
  VPath := GetTileFileName(x, y, Azoom);
  CreateDirIfNotExists(AFileName);
  Result := CopyFile(PChar(VPath), PChar(AFileName), not OverWrite);
end;

function TMapType.LoadFillingMap(btm: TBitmap32; x, y: Integer; Azoom,
  ASourceZoom: byte; IsStop: PBoolean): boolean;
begin

end;

end.
