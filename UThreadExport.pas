unit UThreadExport;
interface

uses Windows,Forms,SysUtils,Classes,UMapType,UImgFun,UGeoFun,unit4, VCLZIp, Graphics,
     DISQLite3Database, DISQLite3Api, PNGImage, JPEG, GR32, UResStrings;
type
  ThreadExport = class(TThread)
    PolygLL:array of TExtendedpoint;
    Zoomarr:array [0..23] of boolean;
    typemaparr:array of PMapType;
    format:byte;
    Fprogress: TFprogress2;
    Move,ziped:boolean;
    Replace:boolean;
    Path:string;
    DISQLite3Database:TDISQLite3Database;
    Zip:TVCLZip;
    Zippu:boolean;
    RelativePath:boolean;
    csat,cmap,chib:byte;
  private
  protected
    procedure savefilesREG(APolyLL:array of TExtendedPoint);
    procedure Execute; override;
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
    procedure export2iMaps(APolyLL:array of TExtendedPoint);
    procedure Export2KML(APolyLL:array of TExtendedPoint);
    function Write_Stream_to_Blob_Traditional(const AStream: TStream; Azoom,Ax,Ay,Aflags,Alength:integer): Int64;
  public
    constructor Create(CrSusp:Boolean;APath:string; APolygon_:array of TExtendedPoint;Azoomarr:array of boolean;Atypemaparr:array of PMapType; Amove,Areplace,Aziped:boolean; Aformat,Acsat,Acmap,Achib:byte;ARelativePath:boolean);
  end;

implementation
uses unit1,usaveas, Math;

procedure ThreadExport.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
 if Zippu then Zip.CancelTheOperation;
end;

constructor ThreadExport.Create(CrSusp:Boolean;APath:string; APolygon_:array of TExtendedPoint;Azoomarr:array of boolean;Atypemaparr:array of PMapType; Amove,Areplace,Aziped:boolean; Aformat,Acsat,Acmap,Achib:byte;ARelativePath:boolean);
var i:integer;
begin
  inherited Create(CrSusp);
  Application.CreateForm(TFProgress2, FProgress);
  Zippu:=false;
  cSat:=Acsat;
  cMap:=Acmap;
  cHib:=Achib;
  FProgress.OnClose:=CloseFProgress;
  FProgress.Visible:=true;
  Path:=APath;
  Move:=AMove;
  format:=AFormat+1;
  ziped:=Aziped;
  Replace:=AReplace;
  RelativePath:=ARelativePath;
  setlength(PolygLL,length(APolygon_));
  for i:=1 to length(APolygon_) do
    PolygLL[i-1]:=Apolygon_[i-1];
  for i:=0 to 23 do
    zoomarr[i]:=Azoomarr[i];
  setlength(typemaparr,length(Atypemaparr));
  for i:=1 to length(Atypemaparr) do
    typemaparr[i-1]:=Atypemaparr[i-1];
end;


procedure ThreadExport.Execute;
begin
 Zippu:=false;
 case format of
  5,6:export2iMaps(PolygLL);
    7:export2KML(PolygLL);
  else savefilesREG(PolygLL);
 end;
 FProgress.Close;
end;

function ThreadExport.Write_Stream_to_Blob_Traditional(const AStream: TStream; Azoom,Ax,Ay,Aflags,Alength:integer): Int64;
var l: Integer;
    p: Pointer;
    Stmt: TDISQLite3Statement;
begin
  Stmt:=DISQLite3Database.Prepare('INSERT INTO Images (data,zoom,x,y,flags,length) VALUES (?,"'+inttostr(Azoom)+'","'+inttostr(Ax)+'","'+inttostr(Ay)+'","'+inttostr(AFlags)+'","'+inttostr(ALength)+'")');
  try
    if AStream is TCustomMemoryStream
     then with AStream as TCustomMemoryStream do
           Stmt.Bind_Blob(1, Memory, Size, SQLITE_STATIC)
     else begin
           l:=AStream.Size;
           GetMem(p, l);
           AStream.Seek(0, soFromBeginning);
           AStream.Read(p^, l);
           Stmt.Bind_Blob(1, p, l, sqlite3_Destroy_Mem);
          end;
    Stmt.Step;
    Result := DISQLite3Database.LastInsertRowID;
  finally
    Stmt.Free;
  end;
end;

procedure UniLoadTile(var bmp:TBitmap32; TypeMapArr:PmapType; MapTypeMerS:TMapType;p_h:TPoint;p_x,p_y:integer; zoom:byte);
var bmp322:TBitmap32;
    png:TPngObject;
begin
 bmp322:=TBitmap32.Create;
 bmp322.DrawMode:=dmBlend;
 png:=TPngObject.Create;
            try
             if TypeMapArr.ext='.png' then
              begin
               TypeMapArr.LoadTile(png,p_h.x,p_h.y,zoom+1,false);
               PNGintoBitmap32(bmp,png);
              end
              else
                TypeMapArr.LoadTile(bmp,p_h.x,p_h.y,zoom+1,false);
            except
             bmp.width:=256;
             bmp.Height:=256;
             bmp.Clear(clSilver);
            end;
            if MapTypeMerS.projection<>TypeMapArr.projection then
             begin
              try
               if TypeMapArr.ext='.png' then
                begin
                 TypeMapArr.LoadTile(png,p_h.x,p_h.y+256,zoom+1,false);
                 PNGintoBitmap32(bmp322,png);
                end
               else TypeMapArr.LoadTile(bmp322,p_h.x,p_h.y+256,zoom+1,false);
              except
               bmp322.width:=256;
               bmp322.Height:=256;
               bmp322.Clear(clSilver);
              end;
              bmp.Roll(0,(((p_Y-(p_y mod 256)) mod 256)-(p_h.Y mod 256)),false,clBlack);
              bmp.Draw(0,((((p_Y-(p_y mod 256)) mod 256)+256)-(p_h.Y mod 256)),bmp322);
             end;
 png.Free;
 bmp322.Free;
end;

procedure ThreadExport.export2iMaps(APolyLL:array of TExtendedPoint);
var p_x,p_y,p_xd256,p_yd256,i,j,xi,yi,hxyi,sizeim,cri,crj:integer;
    num_dwn,scachano,obrab,alpha:integer;
    polyg:array of TPoint;
    pathto,persl,perzoom,kti:string;
    max,min,p_h:TPoint;
    MapTypeMerS:TMapType;
    png:TPngObject;
    Color32arr:PColor32Array;
    bmp32,bmp322,bmp32crop:TBitmap32;
    jpg:TJpegImage;
    bmp:TBitmap;
    TileStream : TMemoryStream;
    PList:Text;
    LLCenter:TExtendedPoint;
begin
 try
 if (TypeMapArr[0]=nil)and(TypeMapArr[1]=nil)and(TypeMapArr[2]=nil) then exit;
 i:=0;
 While not(zoomarr[i]) do inc(i);
 SetLength(polyg,length(APolyLL));
 if TypeMapArr[0]<>nil then Fsaveas.formatepoligon(TypeMapArr[0],i+1,APolyLL,polyg)
  else if TypeMapArr[1]<>nil then Fsaveas.formatepoligon(TypeMapArr[1],i+1,APolyLL,polyg)
        else if TypeMapArr[2]<>nil then Fsaveas.formatepoligon(TypeMapArr[2],i+1,APolyLL,polyg);
 Fsaveas.GetMinMax(min,max,polyg,true);
 if TypeMapArr[0]<>nil then LLCenter:=GPos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i+1,TypeMapArr[0])
  else if TypeMapArr[1]<>nil then LLCenter:=GPos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i+1,TypeMapArr[1])
        else if TypeMapArr[2]<>nil then LLCenter:=GPos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i+1,TypeMapArr[2]);
 AssignFile(Plist,PATH+'com.apple.Maps.plist');
 Rewrite(PList);
 Writeln(PList,'<plist>');
 Writeln(PList,'<dict>');
 Writeln(PList,'<key>LastViewMode</key>');
 if (TypeMapArr[0]<>nil)and(TypeMapArr[0].active) then Writeln(PList,'<integer>0</integer>');
 if (TypeMapArr[1]<>nil)and(TypeMapArr[1].active) then Writeln(PList,'<integer>1</integer>');
 if (TypeMapArr[2]<>nil)and(TypeMapArr[2].active) then Writeln(PList,'<integer>2</integer>');
 Writeln(PList,'<key>LastViewedLatitude</key>');
 Writeln(PList,'<real>'+R2StrPoint(LLCenter.y)+'</real>');
 Writeln(PList,'<key>LastViewedLongitude</key>');
 Writeln(PList,'<real>'+R2StrPoint(LLCenter.x)+'</real>');
 Writeln(PList,'<key>LastViewedZoomScale</key>');
 Writeln(PList,'<real>'+inttostr(i+1)+'</real>');
 Writeln(PList,'</dict>');
 Writeln(PList,'</plist>');
 CloseFile(PList);

 if format=5 then begin
                   hxyi:=1;
                   sizeim:=128;
                  end
             else begin
                   hxyi:=4;
                   sizeim:=64;
                  end;

 try
 jpg:=TJpegImage.Create;
 bmp:=TBitmap.Create;
 bmp.Width:=sizeim;
 bmp.Height:=sizeim;
 png:=TPngObject.Create;
 png.Assign(bmp);
 TileStream:=TMemoryStream.Create;
 bmp32:=TBitmap32.Create;
 bmp32.DrawMode:=dmBlend;
// bmp32.DrawMode:=dmTransparent;
 bmp322:=TBitmap32.Create;
 bmp322.DrawMode:=dmBlend;

 bmp32crop:=TBitmap32.Create;
 bmp32crop.Width:=sizeim;
 bmp32crop.Height:=sizeim;

 MapTypeMerS:=TMapType.Create;
 MapTypeMerS.projection:=1;
 MapTypeMerS.radiusa:=6378137;
 MapTypeMerS.radiusb:=6356752;
 MapTypeMerS.exct:=sqrt(sqr(MapTypeMerS.radiusa)-sqr(MapTypeMerS.radiusb))/MapTypeMerS.radiusa;
 num_dwn:=0;
 SetLength(polyg,length(APolyLL));
 persl:='';
 kti:='';
 for i:=0 to length(TypeMapArr)-1 do
  if TypeMapArr[i]<>nil then
  begin
   persl:=persl+TypeMapArr[i].NameInCache+'_';
   perzoom:='';
   for j:=0 to 23 do
    if zoomarr[j] then
     begin
      Fsaveas.formatepoligon(TypeMapArr[i],j+1,APolyLL,polyg);
      num_dwn:=num_dwn+Fsaveas.GetDwnlNum(min,max,Polyg,true);
      perzoom:=perzoom+inttostr(j+1)+'_';
      kti:=RoundEx(GPos2LonLat(min,j+1,TypeMapArr[i]).x,4);
      kti:=kti+'_'+RoundEx(GPos2LonLat(min,j+1,TypeMapArr[i]).y,4);
      kti:=kti+'_'+RoundEx(GPos2LonLat(max,j+1,TypeMapArr[i]).x,4);
      kti:=kti+'_'+RoundEx(GPos2LonLat(max,j+1,TypeMapArr[i]).y,4);
     end;
  end;
 persl:=copy(persl,1,length(persl)-1);
 perzoom:=copy(perzoom,1,length(perzoom)-1);
 fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles;
 fprogress.Caption:=SAS_STR_AllSaves+' '+inttostr(num_dwn)+' '+SAS_STR_files;
 FProgress.ProgressBar1.Progress1:=0;
 FProgress.ProgressBar1.Max:=100;
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1)+'%';
 obrab:=0;
 scachano:=0;

 sqlite3_initialize;
 DISQLite3Database:=TDISQLite3Database.Create(nil);
 DISQLite3Database.DatabaseName:=PATH+'MapTiles.sqlitedb';
 if not(FileExists(PATH+'MapTiles.sqlitedb'))then Replace:=true;
 If Replace then
  begin
   If FileExists(PATH+'MapTiles.sqlitedb') then DeleteFile(PATH+'MapTiles.sqlitedb');
   DISQLite3Database.CreateDatabase;
   DISQLite3Database.Execute('CREATE TABLE version(version int)');
   DISQLite3Database.Execute('CREATE TABLE images(zoom int, x int, y int, flags int, length int, data blob);');
   DISQLite3Database.Execute('CREATE INDEX index1 on images (zoom,x,y,flags)');
  end
  else DISQLite3Database.Open;
 DISQLite3Database.Execute('PRAGMA locking_mode=EXCLUSIVE');
 DISQLite3Database.Execute('PRAGMA cache_size=100000');
 DISQLite3Database.Execute('PRAGMA synchronous=OFF');
 DISQLite3Database.Connected:=true;
 If Replace then
  begin
   if format=5 then DISQLite3Database.Execute('INSERT INTO version (version) VALUES ("5")')
               else DISQLite3Database.Execute('INSERT INTO version (version) VALUES ("4")');
   DISQLite3Database.Execute('INSERT INTO version (version) VALUES ("0")');
  end;
 DISQLite3Database.Execute('BEGIN TRANSACTION');
 for i:=0 to 23 do //по масштабу
  if zoomarr[i] then
   for j:=0 to 2 do //по типу
    if TypeMapArr[j]<>nil then
     begin
      Fsaveas.formatepoligon(@MapTypeMerS,i+1,APolyLL,polyg);
      Fsaveas.GetDwnlNum(min,max,Polyg,false);

      p_x:=min.x;
      while p_x<max.x do
       begin
        p_y:=min.Y;
        while p_y<max.Y do
         begin
          p_xd256:=p_x div 256;
          p_yd256:=p_y div 256;
          if (FProgress.Visible=false)or(not(RgnAndRgn(Polyg,p_x,p_y,false)))
                                           then begin
                                                 inc(p_y,256);
                                                 CONTINUE;
                                                end;
          bmp322.Clear;
          if (j=2)and(TypeMapArr[0]<>nil) then
           begin
            p_h:=ConvertPosM2M(Point(p_x,p_y-(p_y mod 256)),i+1,@MapTypeMerS,TypeMapArr[0]);
            if TypeMapArr[0].TileExists(p_h.x,p_h.y,i+1) then UniLoadTile(bmp322,TypeMapArr[0],MapTypeMerS,p_h,p_x,p_y,i);
            bmp322.SaveToFile('c:\123.bmp');
           end;
          bmp32.Clear;
          p_h:=ConvertPosM2M(Point(p_x,p_y-(p_y mod 256)),i+1,@MapTypeMerS,TypeMapArr[j]);
          if TypeMapArr[j].TileExists(p_h.x,p_h.y,i+1) then
           begin
            UniLoadTile(bmp32,TypeMapArr[j],MapTypeMerS,p_h,p_x,p_y,i);
            if (j=2)and(TypeMapArr[0]<>nil) then
              begin
               bmp322.SaveToFile('c:\123.bmp');
               bmp322.Draw(0,0,bmp32);
               bmp32.SaveToFile('c:\123.bmp');
               bmp32.Draw(0,0,bmp322);
              end;
            if j=2 then
              for xi:=0 to hxyi do
               for yi:=0 to hxyi do
                begin
                  bmp32crop.Clear; 
                  bmp32crop.Draw(0,0,bounds(sizeim*xi,sizeim*yi,sizeim,sizeim),bmp32);
                  bmp.Assign(bmp32Crop);
                  jpg.Assign(bmp);
                  TileStream.Clear;
                  jpg.CompressionQuality:=chib;
                  jpg.SaveToStream(TileStream);
                  jpg.SaveToFile('c:\123.jpg');
                  Write_Stream_to_Blob_Traditional(TileStream, i+1,((p_xd256)*(hxyi+1))+xi,((p_yd256)*(hxyi+1))+yi,6,TileStream.Size);
                end;
            if j=1 then
              for xi:=0 to hxyi do
               for yi:=0 to hxyi do
                begin      
                  png.Canvas.CopyRect(Bounds(0,0,sizeim,sizeim),bmp32.Canvas,bounds(sizeim*xi,sizeim*yi,sizeim,sizeim));
                  TileStream.Clear;
                  png.CompressionLevel:=cMap;
                  png.SaveToStream(TileStream);
                  Write_Stream_to_Blob_Traditional(TileStream, i+1,((p_xd256)*(hxyi+1))+xi,((p_yd256)*(hxyi+1))+yi,2,TileStream.Size);
                end;
            if j=0 then
              for xi:=0 to hxyi do
               for yi:=0 to hxyi do
                begin
                  bmp32crop.Draw(0,0,bounds(sizeim*xi,sizeim*yi,sizeim,sizeim),bmp32);
                  bmp.Assign(bmp32crop);
                  jpg.Assign(bmp);
                  TileStream.Clear;
                  jpg.CompressionQuality:=cSat;
                  jpg.SaveToStream(TileStream);
                  Write_Stream_to_Blob_Traditional(TileStream, i+1,((p_xd256)*(hxyi+1))+xi,((p_yd256)*(hxyi+1))+yi,3,TileStream.Size);
                end;
            inc(scachano);
           end;
          inc(obrab);
          if ((num_dwn<100)and(obrab mod 5 = 0))or
             ((num_dwn>=100)and(obrab mod 50 = 0)) then
           begin
            FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
            fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1)+'%';
           end;
          if (obrab mod 500 = 0) then
           begin
            DISQLite3Database.Execute('COMMIT');
            DISQLite3Database.Execute('BEGIN TRANSACTION');
           end;
          inc(p_y,256);
         end;
        inc(p_x,256);
       end;
     end;
 DISQLite3Database.Execute('COMMIT');
 FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
 finally
  sqlite3_shutdown;
  FProgress.Close;
  DISQLite3Database.Free;
  png.Free;
  jpg.Free;
  bmp.Free;
  bmp32.Free;
  bmp322.Free;
 end;
 except
  on e:Exception do
   Application.ShowException(e);
 end;
end;

function RetDate(inDate: TDateTime; inTip: integer): integer;
var xYear, xMonth, xDay: word;
begin
  DecodeDate(inDate, xYear, xMonth, xDay);
  case inTip of
   1: Result := xYear;  // год
   2: Result := xMonth; // месяц
   3: Result := xDay;   // день
  end;
end;

procedure ThreadExport.savefilesREG(APolyLL:array of TExtendedPoint);
var p_x,p_y,i,j:integer;
    num_dwn,scachano,obrab:integer;
    polyg:array of TPoint;
    pathfrom,pathto,persl,perzoom,kti,datestr:string;
    max,min:TPoint;
    AMapType:TMapType;
begin
 AMapType:=TMapType.Create;
 num_dwn:=0;
 SetLength(polyg,length(APolyLL));
 persl:='';
 kti:='';
 datestr:=inttostr(RetDate(now,3))+'.'+inttostr(RetDate(now,2))+'.'+inttostr(RetDate(now,1));
 for i:=0 to length(TypeMapArr)-1 do
  begin
   persl:=persl+TypeMapArr[i].NameInCache+'_';
   perzoom:='';
   for j:=0 to 23 do
    if zoomarr[j] then
     begin
      Fsaveas.formatepoligon(TypeMapArr[i],j+1,APolyLL,polyg);
      num_dwn:=num_dwn+Fsaveas.GetDwnlNum(min,max,Polyg,true);
      perzoom:=perzoom+inttostr(j+1)+'_';
      kti:=RoundEx(GPos2LonLat(min,j+1,TypeMapArr[i]).x,4);
      kti:=kti+'_'+RoundEx(GPos2LonLat(min,j+1,TypeMapArr[i]).y,4);
      kti:=kti+'_'+RoundEx(GPos2LonLat(max,j+1,TypeMapArr[i]).x,4);
      kti:=kti+'_'+RoundEx(GPos2LonLat(max,j+1,TypeMapArr[i]).y,4);
     end;
  end;
 persl:=copy(persl,1,length(persl)-1);
 perzoom:=copy(perzoom,1,length(perzoom)-1);
 if ziped then begin
                fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles+' '+SAS_STR_CreateArhList;
                Zip:=TVCLZip.Create(Fmain);
                Zippu:=true;
                Zip.Recurse := False;
                Zip.StorePaths := true; // Путь не сохраняем
                Zip.PackLevel := 0; // Уровень сжатия
                Zip.ZipName := path+'SG-'+persl+'-'+perzoom+'-'+kti+'-'+datestr+'.ZIP';
                //Zip.ZipComment:='Дата создания: '+DateTimeToStr(now);
                               // 'Количество карт: '+inttostr(length(TypeMapArr))+#13#10+
                                //'Типы карт: '+persl+#13#10+
                                //'Масштабы: '+perzoom+#13#10+
                                //'Граничные координаты: '+kti+#13#10+
                                //'Тип кэша: '+CacheTypeStr[format];
               end
          else fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles;
 fprogress.Caption:=SAS_STR_AllSaves+' '+inttostr(num_dwn)+' '+SAS_STR_Files;
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1);
 FProgress.ProgressBar1.Max:=100;
 FProgress.ProgressBar1.Progress1:=0;
 obrab:=0;
 scachano:=0;
 for i:=0 to 23 do //по масштабу
  if zoomarr[i] then
   for j:=0 to length(TypeMapArr)-1 do //по типу
     begin
      Fsaveas.formatepoligon(TypeMapArr[j],i+1,APolyLL,polyg);
      AMapType.ext:=TypeMapArr[j].ext;
      AMapType.NameInCache:=TypeMapArr[j].NameInCache;
      AMapType.CacheType:=format;
      Fsaveas.GetDwnlNum(min,max,Polyg,false);
      p_x:=min.x;
      while p_x<max.x do
       begin
        p_y:=min.Y;
        while p_y<max.Y do
         begin
          if FProgress.Visible=false then
           begin
            Unit1.m_up:=Unit1.move;
            Fmain.generate_im(nilLastLoad,'');
            exit;
           end;
          if not(RgnAndRgn(Polyg,p_x,p_y,false)) then begin
                                                 inc(p_y,256);
                                                 CONTINUE;
                                                end;
//TODO: Разобраться и избавиться от путей.
          pathfrom:=TypeMapArr[j].GetTileFileName(p_x,p_y,i+1);
          if TypeMapArr[j].TileExists(p_x,p_y,i+1) then
           begin
            inc(scachano);
            if ziped then begin
                           Zip.FilesList.Add(pathfrom);
                          end
                     else begin
//TODO: Разобраться и избавиться от путей.
                           pathto:=PATH+AMapType.GetTileFileName(p_x,p_y,i+1);
                           Fmain.createdirif(pathto);
                           Copy_File(Pchar(pathfrom),PChar(pathto),not(replace));
                           if move then DelFile(pathfrom);
                          end;
           end;
          inc(obrab);
          if obrab mod 100 = 0 then
           begin
            FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
            fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
           end;
          inc(p_y,256);
         end;
        inc(p_x,256);
       end;
     end;
 if ziped then
  begin
   fprogress.MemoInfo.Lines[0]:=SAS_STR_Pack+' '+'SG-'+persl+'-'+perzoom+'-'+kti+'-'+datestr+'.ZIP';
   if FileExists(Zip.ZipName) then DeleteFile(Zip.ZipName);
   If Zip.Zip=0 then
    Application.MessageBox(PChar(SAS_ERR_CreateArh),PChar(SAS_MSG_coution),48);
   Zip.free;
   Zippu:=false;
  end;
 FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
 AMapType.Destroy;
 FProgress.Close;
end;


procedure ThreadExport.Export2KML(APolyLL:array of TExtendedPoint);
var p_x,p_y,i,j:integer;
    num_dwn,scachano,obrab:integer;
    polyg:array of TPoint;
    persl,perzoom,kti,ToFile,datestr:string;
    max,min:TPoint;
    AMapType:TMapType;
    KMLFile:TextFile;

procedure KmlFileWrite(x,y:integer;z,level:byte);
var xym256lt,xym256rb:TPoint;
    i,nxy,xi,yi:integer;
    savepath,north,south,east,west:string;
begin
  //TODO: Нужно думать на случай когда тайлы будут в базе данных
  savepath:=TypeMapArr[0].GetTileFileName(x,y,z);
  if (Replace)and(not(TypeMapArr[0].TileExists(x,y,z))) then exit;
  if RelativePath then savepath:= ExtractRelativePath(ExtractFilePath(path), savepath);
  xym256lt:=Point(x-(x mod 256),y-(y mod 256));
  xym256rb:=Point(256+x-(x mod 256),256+y-(y mod 256));
  north:=R2StrPoint(GPos2LonLat(xym256lt,z,TypeMapArr[0]).y);
  south:=R2StrPoint(GPos2LonLat(xym256rb,z,TypeMapArr[0]).y);
  east:=R2StrPoint(GPos2LonLat(xym256rb,z,TypeMapArr[0]).x);
  west:=R2StrPoint(GPos2LonLat(xym256lt,z,TypeMapArr[0]).x);
  ToFile:=#13#10+'<Folder>'+#13#10+{'  <name></name>'+#13#10+}'  <Region>'+#13#10+'    <LatLonAltBox>'+#13#10+
          '      <north>'+north+'</north>'+#13#10+'      <south>'+south+'</south>'+#13#10+'      <east>'+east+'</east>'+#13#10+
          '      <west>'+west+'</west>'+#13#10+'    </LatLonAltBox>'+#13#10+'    <Lod>';
  if level>1 then ToFile:=ToFile+#13#10+'      <minLodPixels>128</minLodPixels>'
             else ToFile:=ToFile+#13#10+'      <minLodPixels>16</minLodPixels>';
  ToFile:=ToFile+#13#10+'      <maxLodPixels>-1</maxLodPixels>'+#13#10+'    </Lod>'+#13#10+'  </Region>'+#13#10+
          '  <GroundOverlay>'+#13#10+'    <drawOrder>'+inttostr(level)+'</drawOrder>'+#13#10+'    <Icon>'+#13#10+
          '      <href>'+savepath+'</href>'+#13#10+'    </Icon>'+#13#10+'    <LatLonBox>'+#13#10+'      <north>'+north+'</north>'+#13#10+
          '      <south>'+south+'</south>'+#13#10+'      <east>'+east+'</east>'+#13#10+'      <west>'+west+'</west>'+#13#10+
          '    </LatLonBox>'+#13#10+'  </GroundOverlay>';
  ToFile:=AnsiToUtf8(ToFile);
  Write(KMLFile,ToFile);
  inc(obrab);
  if obrab mod 100 = 0 then
   begin
    FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
    fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
   end;
  i:=z;
  while (not(zoomarr[i]))and(i<24) do inc(i);
  if i<24 then
   begin
    nxy:=round(intpower(2,(i+1)-z));
    for xi:=1 to nxy do
     for yi:=1 to nxy do
      KmlFileWrite(xym256lt.x*nxy+(256*(xi-1)),xym256lt.y*nxy+(256*(yi-1)),i+1,level+1);
   end;
  ToFile:=AnsiToUtf8(#13#10+'</Folder>');
  Write(KMLFile,ToFile);
end;

begin
 AMapType:=TMapType.Create;
 num_dwn:=0;
 SetLength(polyg,length(APolyLL));
 persl:='';
 kti:='';
 datestr:=inttostr(RetDate(now,3))+'.'+inttostr(RetDate(now,2))+'.'+inttostr(RetDate(now,1));
 persl:=persl+TypeMapArr[0].NameInCache+'_';
 perzoom:='';
 for j:=0 to 23 do
  if zoomarr[j] then
   begin
    Fsaveas.formatepoligon(TypeMapArr[0],j+1,APolyLL,polyg);
    num_dwn:=num_dwn+Fsaveas.GetDwnlNum(min,max,Polyg,true);
    perzoom:=perzoom+inttostr(j+1)+'_';
    kti:=RoundEx(GPos2LonLat(min,j+1,TypeMapArr[0]).x,4);
    kti:=kti+'_'+RoundEx(GPos2LonLat(min,j+1,TypeMapArr[0]).y,4);
    kti:=kti+'_'+RoundEx(GPos2LonLat(max,j+1,TypeMapArr[0]).x,4);
    kti:=kti+'_'+RoundEx(GPos2LonLat(max,j+1,TypeMapArr[0]).y,4);
   end;
 persl:=copy(persl,1,length(persl)-1);
 perzoom:=copy(perzoom,1,length(perzoom)-1);
 if ziped then begin
                fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles+' '+SAS_STR_CreateArhList;
                Zip:=TVCLZip.Create(Fmain);
                Zippu:=true;
                Zip.Recurse := False;
                Zip.StorePaths := true; // Путь не сохраняем
                Zip.PackLevel := 0; // Уровень сжатия
                Zip.ZipName := path+'SG-'+persl+'-'+perzoom+'-'+kti+'-'+datestr+'.ZIP';
                //Zip.ZipComment:='Дата создания: '+DateTimeToStr(now);
                               // 'Количество карт: '+inttostr(length(TypeMapArr))+#13#10+
                                //'Типы карт: '+persl+#13#10+
                                //'Масштабы: '+perzoom+#13#10+
                                //'Граничные координаты: '+kti+#13#10+
                                //'Тип кэша: '+CacheTypeStr[format];
               end
          else fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles;
 fprogress.Caption:=SAS_STR_AllSaves+' '+inttostr(num_dwn)+' '+SAS_STR_Files;
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1);
 FProgress.ProgressBar1.Max:=100;
 FProgress.ProgressBar1.Progress1:=0;
 obrab:=0;
 scachano:=0;
 i:=0;
 AssignFile(KMLFile,path);
 Rewrite(KMLFile);
 ToFile:=AnsiToUtf8('<?xml version="1.0" encoding="UTF-8"?>'+#13#10+'<kml xmlns="http://earth.google.com/kml/2.1">'+#13#10);
 ToFile:=ToFile+AnsiToUtf8('<Document>'+#13#10+'<name>'+ExtractFileName(path)+'</name>');
 Write(KMLFile,ToFile);

 while not(zoomarr[i])or(i>23) do inc(i);
 Fsaveas.formatepoligon(TypeMapArr[0],i+1,APolyLL,polyg);
 AMapType.ext:=TypeMapArr[0].ext;
 AMapType.NameInCache:=TypeMapArr[0].NameInCache;
 AMapType.CacheType:=format;
 Fsaveas.GetDwnlNum(min,max,Polyg,false);
 p_x:=min.x;
 while p_x<max.x do
  begin
   p_y:=min.Y;
   while p_y<max.Y do
    begin
     if FProgress.Visible=false then
      begin
        Unit1.m_up:=Unit1.move;
        Fmain.generate_im(nilLastLoad,'');
        exit;
      end;
     if not(RgnAndRgn(Polyg,p_x,p_y,false)) then begin
                                                  inc(p_y,256);
                                                  CONTINUE;
                                                 end;
     KmlFileWrite(p_x,p_y,i+1,1);
     inc(p_y,256);
    end;
    inc(p_x,256);
   end;
 if ziped then
  begin
   fprogress.MemoInfo.Lines[0]:=SAS_STR_Pack+' '+'SG-'+persl+'-'+perzoom+'-'+kti+'-'+datestr+'.ZIP';
   if FileExists(Zip.ZipName) then DeleteFile(Zip.ZipName);
   If Zip.Zip=0 then
    Application.MessageBox(PChar(SAS_ERR_CreateArh),PChar(SAS_MSG_coution),48);
   Zip.free;
   Zippu:=false;
  end;
 ToFile:=AnsiToUtf8(#13#10+'</Document>'+#13#10+'</kml>');
 Write(KMLFile,ToFile);
 CloseFile(KMLFile);
 FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
 AMapType.Destroy;
 FProgress.Close;
end;


end.
