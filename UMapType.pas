unit UMapType;

interface
uses Forms,sysutils,Classes,iniFiles,Windows,Uprogress,Dialogs,Menus,TB2Toolbar,
     TB2Item,Graphics,uPSCompiler,uPSRuntime,StdCtrls,ComCtrls,
     uPSR_std,uPSR_forms,uPSUtils,math,ExtCtrls, VCLZip,
     u_CoordConverterAbstract,u_UrlGenerator;

type
 PMapType = ^TMapType;
 TMapType = class
   protected
    FCoordConverter : ICoordConverter;
    FUrlGenerator : TUrlGenerator;
   public
    TileRect:TRect;
    id,pos:integer;
    filename,guids:string;
    active:boolean;
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
    TBItem:TTBItem;
    TBSubMenuItem:TTBSubmenuItem;
    NItem,NSmItem,NDwnItem,NDelItem:TMenuItem;
    NSubMenuItem:TMenuItem;
    NameInCache:string;
    DefNameInCache:string;
    bmp18,bmp24:TBitmap;
    function GetLink(x,y:longint;Azoom:byte):string;
    function GetMapSize(zoom:byte):longint;
    procedure LoadMapTypeFromZipFile(AZipFileName : string; pnum : Integer);
  end;
var
  MapType:array of TMapType;
  MapsEdit:boolean;
  procedure LoadMaps;
  procedure SaveMaps;
  procedure CreateMapUI;

implementation
uses Usettings,unit1,UGeoFun, DateUtils, u_CoordConverterMercatorOnSphere,
     u_CoordConverterMercatorOnEllipsoid, u_CoordConverterSimpleLonLat;

procedure CreateMapUI;
var i,j:integer;
begin
 fmain.XPMenu.Active:=false;
 FSettings.MapList.Clear;
 Fmain.MapIcons24.Clear;
 Fmain.MapIcons18.Clear;
 Fmain.TBSMB.Clear;
 Fmain.NSMB.Clear;
 Fmain.ldm.Clear;
 Fmain.dlm.Clear;
 Fmain.NSubMenuSmItem.Clear;
 for i:=0 to Fmain.NLayerSel.Count-3 do Fmain.NLayerSel.Items[2].Free;
 for i:=0 to Fmain.TBLayerSel.Count-3 do Fmain.TBLayerSel.Items[2].Free;
 for i:=0 to Fmain.PopupMSmM.Items.Count-3 do Fmain.PopupMSmM.Items.Items[2].Free;

 sm_map.maptype:=nil;
 for i:=0 to length(MapType)-1 do
  With MapType[i] do
  begin
   TBItem:=TTBItem.Create(Fmain.TBSMB);
   NItem:=TMenuItem.Create(Fmain.NSMB);
   if ParentSubMenu=''
    then begin
          if asLayer then begin
                            Fmain.NLayerSel.Add(NItem);
                            Fmain.TBLayerSel.Add(TBItem)
                          end
                     else begin
                            Fmain.NSMB.Add(NItem);
                            Fmain.TBSMB.Add(TBItem)
                          end;
         end
    else begin
          j:=0;
          While MapType[j].ParentSubMenu<>ParentSubMenu do inc(j);
          TBSubMenuItem:=TTBSubmenuItem.Create(Fmain.TBSMB);
          NSubMenuItem:=TMenuItem.Create(Fmain.NSMB);
          TBSubMenuItem.caption:=ParentSubMenu;
          NSubMenuItem.caption:=ParentSubMenu;
          TBSubMenuItem.Images:=Fmain.MapIcons18;
          NSubMenuItem.SubMenuImages:=Fmain.MapIcons18;
          if j=i then
          begin
          if asLayer then begin
                            Fmain.NLayerSel.Add(NSubMenuItem);
                            Fmain.TBLayerSel.Add(TBSubMenuItem)
                          end
                     else begin
                            Fmain.TBSMB.Add(TBSubMenuItem);
                            Fmain.NSMB.Add(NSubMenuItem);
                          end;
          end;
          MapType[j].TBSubMenuItem.Add(TBItem);
          MapType[j].NSubMenuItem.Add(NItem);
         end;
   Fmain.MapIcons24.AddMasked(bmp24,RGB(255,0,255));
   Fmain.MapIcons18.AddMasked(bmp18,RGB(255,0,255));
   TBItem.Name:='TBMapN'+ inttostr(id);
   TBItem.ImageIndex:=i;
   TBItem.Caption:=name;
   TBItem.OnClick:=Fmain.TBmap1Click;
   NItem.Name:='NMapN'+inttostr(id);
   NItem.ImageIndex:=i;
   NItem.Caption:=name;
   NItem.ShortCut:=HotKey;
   NItem.OnClick:=Fmain.TBmap1Click;
   if ext<>'.kml' then
    begin
     if not(asLayer) then begin
                           NSmItem:=TMenuItem.Create(Fmain.PopupMSmM);
                           Fmain.PopupMSmM.Items.Add(NSmItem)
                          end
                     else begin
                           NSmItem:=TMenuItem.Create(Fmain.NSubMenuSmItem);
                           Fmain.NSubMenuSmItem.Add(NSmItem);
                          end;
     NSmItem.Name:='NSmMapN'+inttostr(id);
     NSmItem.ImageIndex:=i;
     NSmItem.Caption:=name;
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
    end;
   if (asLayer)and(active) then begin
                                  TBItem.Checked:=true;
                                  NItem.Checked:=true;
                                end;
   if separator then
    begin
     TBItem.Parent.Add(TTBSeparatorItem.Create(Fmain.TBSMB));
     NItem.Parent.InsertNewLineAfter(NItem);
     if NSmItem<>NIL  then  NSmItem.Parent.InsertNewLineAfter(NSmItem);
    end;
   if (active)and(MapType[i].asLayer=false) then sat_map_both:=@MapType[i];
   if (ShowOnSmMap)and(not(asLayer)) then sm_map.maptype:=@MapType[i];
   TBItem.Tag:=Longint(@MapType[i]);
   NItem.Tag:=Longint(@MapType[i]);
   if ext<>'.kml' then NSmItem.Tag:=Longint(@MapType[i]);
   if asLayer then
    begin
     NDwnItem.Tag:=longint(@MapType[i]);
     NDelItem.Tag:=longint(@MapType[i]);
    end;
   FSettings.MapList.AddItem(MapType[i].name,nil);
   FSettings.MapList.Items.Item[i].Data:=@MapType[i];
   FSettings.MapList.Items.Item[i].SubItems.Add(MapType[i].NameInCache);
   if MapType[i].asLayer then FSettings.MapList.Items.Item[i].SubItems.Add('Слои\'+MapType[i].ParentSubMenu)
                         else FSettings.MapList.Items.Item[i].SubItems.Add('Карты\'+MapType[i].ParentSubMenu);
   FSettings.MapList.Items.Item[i].SubItems.Add(ShortCutToText(MapType[i].HotKey));
   FSettings.MapList.Items.Item[i].SubItems.Add(MapType[i].filename);
  end;
 if FSettings.MapList.Items.Count>0 then FSettings.MapList.Items.Item[0].Selected:=true;
 if longint(sm_map.maptype)=0 then Fmain.NMMtype_0.Checked:=true;
 if (sat_map_both=nil)and(@MapType[0]<>nil) then sat_map_both:=@MapType[0];
 fmain.XPMenu.Active:=true;
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
           sleep:=ini.ReadInteger(GUIDs,'sleep',sleep);
           ParentSubMenu:=ini.ReadString(GUIDs,'ParentSubMenu',ParentSubMenu);
           separator:=ini.ReadBool(GUIDs,'separator',separator);
          end
    else With MapType[pnum] do
          begin
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
 CreateMapUI;
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
         if MapType[i].Sleep<>MapType[i].defSleep then
          ini.WriteInteger(MapType[i].guids,'sleep',MapType[i].sleep)
          else Ini.DeleteKey(MapType[i].guids,'sleep');
         if MapType[i].separator<>MapType[i].Defseparator then
          ini.WriteBool(MapType[i].guids,'separator',MapType[i].separator)
          else Ini.DeleteKey(MapType[i].guids,'separator');
         if MapType[i].NameInCache<>MapType[i].DefNameInCache then
          ini.WriteString(MapType[i].guids,'NameInCache',MapType[i].NameInCache)
          else Ini.DeleteKey(MapType[i].guids,'NameInCache');
         if MapType[i].ParentSubMenu<>MapType[i].DefParentSubMenu then
          ini.WriteString(MapType[i].guids,'ParentSubMenu',MapType[i].ParentSubMenu)
          else Ini.DeleteKey(MapType[i].guids,'ParentSubMenu');
       end;
 Ini.UpdateFile;
 ini.Free;
end;

procedure TMapType.LoadMapTypeFromZipFile(AZipFileName: string; pnum : Integer);
var
//  KaZip:TKaZip;
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
  //KaZip:=TKaZip.Create(nil);
  UnZip:=TVCLZip.Create(nil);
  try
    AZipFile:=TFileStream.Create(AZipFileName,fmOpenRead or fmShareDenyNone);
    //KaZip.Open(AZipFile);
    UnZip.ZipName:=AZipFileName;
    MapParams:=TMemoryStream.Create;
    try
//      KaZip.ExtractToStream(KaZip.Entries.Items[KaZip.Entries.IndexOf('params.txt')],MapParams);
//      UnZip.UnZipToStream(MapParams,'params.txt');
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
        //KaZip.ExtractToStream(KaZip.Entries.Items[KaZip.Entries.IndexOf('GetUrlScript.txt')],MapParams);
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
         // KaZip.ExtractToStream(KaZip.Entries.Items[KaZip.Entries.IndexOf('24.bmp')],MapParams);
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
         // KaZip.ExtractToStream(KaZip.Entries.Items[KaZip.Entries.IndexOf('18.bmp')],MapParams);
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
        1: FCoordConverter := TCoordConverterMercatorOnSphere.Create();
        2: FCoordConverter := TCoordConverterMercatorOnEllipsoid.Create(exct);
        3: FCoordConverter := TCoordConverterSimpleLonLat.Create();
        else raise Exception.Create('Ошибочный тип проэкции карты ' + IntToStr(projection));
      end;
      try
      FUrlGenerator := TUrlGenerator.Create('procedure Return(Data: string); begin ResultURL := Data; end; ' + GetURLScript, FCoordConverter);
      FUrlGenerator.GetURLBase := URLBase;
      GetLink(0,0,1);
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
//    FreeAndNil(KaZip);
    FreeAndNil(UnZip);
  end;
end;

function TMapType.GetLink(x,y:Integer;Azoom:byte): string;
var xx,yy:integer;
    zz:byte;
begin
  if (FUrlGenerator = nil) then result:='';
  xx:=x div 256;
  yy:=y div 256;
  if not(Azoom in [1..24]) then raise Exception.Create('Ошибочный Zoom');
  zz:=Azoom-1;
  FUrlGenerator.GetURLBase:=URLBase;
  Result:=FUrlGenerator.GenLink(xx,yy,zz);
end;

function TMapType.GetMapSize(zoom:byte):longint;
begin
 result:=round(intpower(2,zoom))*256;
end;

end.
