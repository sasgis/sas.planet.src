unit UThreadExport;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Classes,
  Graphics,
  gifimage,
  VCLZIp,
  DISQLite3Database,
  DISQLite3Api,
  PNGImage,
  JPEG,
  GR32,
  UMapType,
  UGeoFun,
  unit4,
  UResStrings,
  UYaMobile,
  t_GeoTypes;

type
  ThreadExport = class(TThread)
    PolygLL:TExtendedPointArray;
    Zoomarr:array [0..23] of boolean;
    typemaparr:array of TMapType;
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
    procedure savefilesREG(APolyLL:TExtendedPointArray);
    procedure Execute; override;
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
    procedure export2iMaps(APolyLL:TExtendedPointArray);
    procedure export2YaMaps(APolyLL:TExtendedPointArray);
    procedure Export2KML(APolyLL:TExtendedPointArray);
    function Write_Stream_to_Blob_Traditional(const AStream: TStream; Azoom,Ax,Ay,Aflags,Alength:integer): Int64;
  public
    constructor Create(CrSusp:Boolean;APath:string; APolygon_:TExtendedPointArray;Azoomarr:array of boolean;Atypemaparr:array of TMapType; Amove,Areplace,Aziped:boolean; Aformat,Acsat,Acmap,Achib:byte;ARelativePath:boolean);
  end;

implementation

uses
  Math,
  u_GeoToStr,
  unit1,
  i_ITileFileNameGenerator,
  i_ICoordConverter,
  u_GlobalState,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid;

procedure ThreadExport.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
 if Zippu then Zip.CancelTheOperation;
end;

constructor ThreadExport.Create(CrSusp:Boolean;APath:string; APolygon_:TExtendedPointArray;Azoomarr:array of boolean;Atypemaparr:array of TMapType; Amove,Areplace,Aziped:boolean; Aformat,Acsat,Acmap,Achib:byte;ARelativePath:boolean);
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
    8:export2YaMaps(PolygLL);
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

function UniLoadTile(var bmp:TBitmap32; ATypeMap: TmapType; ATargetProjection: byte; p_h:TPoint;p_x,p_y:integer; zoom:byte):boolean;
var
  bmp2,bmp1:TBitmap32;
  res1,res2:boolean;
begin
  res2:=false;
  bmp.width:=256;
  bmp.Height:=256;
  bmp.Clear(Color32(GState.BGround));
  bmp1:=TBitmap32.Create;
  try
    bmp1.DrawMode:=dmBlend;
    bmp2:=TBitmap32.Create;
    try
      bmp2.DrawMode:=dmBlend;
      res1:=true;
      if (not(ATypeMap.LoadTile(bmp1,p_h.x, p_h.y, zoom+1,false))) then begin
        res1:=false;
        bmp1.width:=256;
        bmp1.Height:=256;
        bmp1.Clear(Color32(GState.BGround));
      end;
      if p_h.Y<0 then begin
        bmp.Draw(0,((((p_Y-(p_y mod 256)) mod 256)+256)-(p_h.Y mod 256)),bmp1);
      end else begin
        bmp.Draw(0,(((p_Y-(p_y mod 256)) mod 256)-(p_h.Y mod 256)),bmp1);
      end;

      if ATargetProjection<>ATypeMap.projection then begin
        res2:=true;
        if (not(ATypeMap.LoadTile(bmp2,p_h.x,p_h.y+256,zoom+1,false))) then begin
          res2:=false;
          bmp2.width:=256;
          bmp2.Height:=256;
          bmp2.Clear(Color32(GState.BGround));
        end;
        if p_h.Y<0 then begin
          bmp.Draw(0,(((p_Y-(p_y mod 256)) mod 256)-(p_h.Y mod 256)),bmp2);
        end else begin
          bmp.Draw(0,((((p_Y-(p_y mod 256)) mod 256)+256)-(p_h.Y mod 256)),bmp2);
        end;
      end;
      result:=(res1 or res2);
    finally
      bmp2.Free;
    end;
  finally
    bmp1.Free;
  end;
end;

procedure ThreadExport.export2iMaps(APolyLL:TExtendedPointArray);
var p_x,p_y,p_xd256,p_yd256,i,j,xi,yi,hxyi,sizeim,cri,crj:integer;
    num_dwn,scachano,obrab,alpha:integer;
    polyg: TPointArray;
    perzoom,kti:string;
    max,min,p_h:TPoint;
    png:TPngObject;
    Color32arr:PColor32Array;
    bmp32,bmp322,bmp32crop:TBitmap32;
    jpg:TJpegImage;
    bmp:TBitmap;
    TileStream : TMemoryStream;
    PList:Text;
    LLCenter:TExtendedPoint;
    Vprojection: byte;
    Vradiusa: double;
    Vradiusb: double;
    Vexct: double;
    VGeoConvert: ICoordConverter;
begin
 try
 if (TypeMapArr[0]=nil)and(TypeMapArr[1]=nil)and(TypeMapArr[2]=nil) then exit;
 i:=0;
 While not(zoomarr[i]) do inc(i);
 if TypeMapArr[0]<>nil then polyg := TypeMapArr[0].GeoConvert.PoligonProject(i + 8, APolyLL)
  else if TypeMapArr[1]<>nil then polyg := TypeMapArr[1].GeoConvert.PoligonProject(i + 8, APolyLL)
        else if TypeMapArr[2]<>nil then polyg := TypeMapArr[2].GeoConvert.PoligonProject(i + 8, APolyLL);
 GetMinMax(min,max,polyg,true);
 if TypeMapArr[0]<>nil then LLCenter:= TypeMapArr[0].GeoConvert.Pos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i + 8)
  else if TypeMapArr[1]<>nil then LLCenter:=TypeMapArr[1].GeoConvert.Pos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i + 8)
        else if TypeMapArr[2]<>nil then LLCenter:=TypeMapArr[2].GeoConvert.Pos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i + 8);
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
 bmp322:=TBitmap32.Create;
 bmp322.DrawMode:=dmBlend;

 bmp32crop:=TBitmap32.Create;
 bmp32crop.Width:=sizeim;
 bmp32crop.Height:=sizeim;

 Vprojection:=1;
 Vradiusa:=6378137;
 Vradiusb:=6378137;
 Vexct:=sqrt(sqr(Vradiusa)-sqr(Vradiusb))/Vradiusa;
 VGeoConvert:=TCoordConverterMercatorOnSphere.Create(Vradiusa);

 num_dwn:=0;
 kti:='';
 for i:=0 to length(TypeMapArr)-1 do
  if TypeMapArr[i]<>nil then
  begin
   perzoom:='';
   for j:=0 to 23 do
    if zoomarr[j] then
     begin
      polyg := TypeMapArr[i].GeoConvert.PoligonProject(j + 8, APolyLL);
      num_dwn:=num_dwn+GetDwnlNum(min,max,Polyg,true);
      perzoom:=perzoom+inttostr(j+1)+'_';
      kti:=RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(min,j + 8).x,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(min,j + 8).y,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(max,j + 8).x,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(max,j + 8).y,4);
     end;
  end;
 perzoom:=copy(perzoom,1,length(perzoom)-1);
 fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles;
 fprogress.Caption:=SAS_STR_AllSaves+' '+inttostr(num_dwn)+' '+SAS_STR_files;
 FProgress.ProgressBar1.Progress1:=0;
 FProgress.ProgressBar1.Max:=100;
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1)+'%';
 obrab:=0;

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
      Polyg := VGeoConvert.PoligonProject(i + 8, APolyLL);
      GetDwnlNum(min,max,Polyg,false);

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
            p_h := VGeoConvert.Pos2OtherMap(Point(p_x,p_y-(p_y mod 256)), i + 8, TypeMapArr[0].GeoConvert);
            if TypeMapArr[0].TileExists(p_h.x,p_h.y,i+1) then UniLoadTile(bmp322,TypeMapArr[0],Vprojection,p_h,p_x,p_y,i);
           end;
          bmp32.Clear;
          p_h := VGeoConvert.Pos2OtherMap(Point(p_x,p_y-(p_y mod 256)), i + 8, TypeMapArr[j].GeoConvert);
          if TypeMapArr[j].TileExists(p_h.x,p_h.y,i+1) then
           begin
            UniLoadTile(bmp32,TypeMapArr[j],Vprojection,p_h,p_x,p_y,i);
            if (j=2)and(TypeMapArr[0]<>nil) then
              begin
               bmp322.Draw(0,0,bmp32);
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

procedure ThreadExport.savefilesREG(APolyLL:TExtendedPointArray);
var p_x,p_y,i,j:integer;
    num_dwn,obrab:integer;
    polyg:TPointArray;
    pathfrom,pathto,persl,perzoom,kti,datestr:string;
    max,min:TPoint;
    VTileNameGen: ITileFileNameGenerator;
    VExt: string;
    VPath: string;
begin
 num_dwn:=0;
 SetLength(polyg,length(APolyLL));
 persl:='';
 kti:='';
 datestr:=inttostr(RetDate(now,3))+'.'+inttostr(RetDate(now,2))+'.'+inttostr(RetDate(now,1));
 for i:=0 to length(TypeMapArr)-1 do
  begin
   persl:=persl+ TypeMapArr[i].GetShortFolderName+'_';
   perzoom:='';
   for j:=0 to 23 do
    if zoomarr[j] then
     begin
      polyg := TypeMapArr[i].GeoConvert.PoligonProject(j + 8, APolyLL);
      num_dwn:=num_dwn+GetDwnlNum(min,max,Polyg,true);
      perzoom:=perzoom+inttostr(j+1)+'_';
      kti:=RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(min,j + 8).x,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(min,j + 8).y,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(max,j + 8).x,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(max,j + 8).y,4);
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
               end
          else fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles;
 fprogress.Caption:=SAS_STR_AllSaves+' '+inttostr(num_dwn)+' '+SAS_STR_Files;
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1);
 FProgress.ProgressBar1.Max:=100;
 FProgress.ProgressBar1.Progress1:=0;
 obrab:=0;
 for i:=0 to 23 do //по масштабу
  if zoomarr[i] then
   for j:=0 to length(TypeMapArr)-1 do //по типу
     begin
      polyg := TypeMapArr[j].GeoConvert.PoligonProject(i + 8, APolyLL);
      VExt := TypeMapArr[j].TileFileExt;
      VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(PATH) + TypeMapArr[j].GetShortFolderName);
      VTileNameGen := GState.TileNameGenerator.GetGenerator(format);
      GetDwnlNum(min,max,Polyg,false);
      p_x:=min.x;
      while p_x<max.x do
       begin
        p_y:=min.Y;
        while p_y<max.Y do
         begin
          if FProgress.Visible=false then
           begin
            Fmain.generate_im(nilLastLoad,'');
            exit;
           end;
          if not(RgnAndRgn(Polyg,p_x,p_y,false)) then begin
                                                 inc(p_y,256);
                                                 CONTINUE;
                                                end;
          if TypeMapArr[j].TileExists(p_x,p_y,i+1) then
           begin
            if ziped then begin
//TODO: Разобраться и избавиться от путей. Нужно предусмотреть вариант, что тайлы хранятся не в файлах, а перед зипованием сохраняются в файлы.
                            pathfrom:=TypeMapArr[j].GetTileFileName(p_x,p_y,i+1);
                            Zip.FilesList.Add(pathfrom);
                          end
                     else begin
                           pathto:= VPath + VTileNameGen.GetTileFileName(Point(p_x shr 8,p_y shr 8), i) + VExt;
                           if TypeMapArr[j].TileExportToFile(p_x,p_y,i+1, pathto, replace) then begin
                             if move then TypeMapArr[j].DeleteTile(p_x,p_y,i+1);
                           end;
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
 VTileNameGen := nil;
 FProgress.Close;
end;


procedure ThreadExport.Export2KML(APolyLL:TExtendedPointArray);
var p_x,p_y,i,j:integer;
    num_dwn,obrab:integer;
    polyg:TPointArray;
    persl,perzoom,kti,ToFile,datestr:string;
    max,min:TPoint;
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
  north:=R2StrPoint(TypeMapArr[0].GeoConvert.Pos2LonLat(xym256lt,(z - 1) + 8).y);
  south:=R2StrPoint(TypeMapArr[0].GeoConvert.Pos2LonLat(xym256rb,(z - 1) + 8).y);
  east:=R2StrPoint(TypeMapArr[0].GeoConvert.Pos2LonLat(xym256rb,(z - 1) + 8).x);
  west:=R2StrPoint(TypeMapArr[0].GeoConvert.Pos2LonLat(xym256lt,(z - 1) + 8).x);
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
 num_dwn:=0;
 SetLength(polyg,length(APolyLL));
 persl:='';
 kti:='';
 datestr:=inttostr(RetDate(now,3))+'.'+inttostr(RetDate(now,2))+'.'+inttostr(RetDate(now,1));
 persl:=persl+ TypeMapArr[0].GetShortFolderName+'_';
 perzoom:='';
 for j:=0 to 23 do
  if zoomarr[j] then
   begin
    polyg := TypeMapArr[0].GeoConvert.PoligonProject(j + 8, APolyLL);
    num_dwn:=num_dwn+GetDwnlNum(min,max,Polyg,true);
    perzoom:=perzoom+inttostr(j+1)+'_';
    kti:=RoundEx(TypeMapArr[0].GeoConvert.Pos2LonLat(min,j + 8).x,4);
    kti:=kti+'_'+RoundEx(TypeMapArr[0].GeoConvert.Pos2LonLat(min,j + 8).y,4);
    kti:=kti+'_'+RoundEx(TypeMapArr[0].GeoConvert.Pos2LonLat(max,j + 8).x,4);
    kti:=kti+'_'+RoundEx(TypeMapArr[0].GeoConvert.Pos2LonLat(max,j + 8).y,4);
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
               end
          else fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles;
 fprogress.Caption:=SAS_STR_AllSaves+' '+inttostr(num_dwn)+' '+SAS_STR_Files;
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1);
 FProgress.ProgressBar1.Max:=100;
 FProgress.ProgressBar1.Progress1:=0;
 obrab:=0;
 i:=0;
 AssignFile(KMLFile,path);
 Rewrite(KMLFile);
 ToFile:=AnsiToUtf8('<?xml version="1.0" encoding="UTF-8"?>'+#13#10+'<kml xmlns="http://earth.google.com/kml/2.1">'+#13#10);
 ToFile:=ToFile+AnsiToUtf8('<Document>'+#13#10+'<name>'+ExtractFileName(path)+'</name>');
 Write(KMLFile,ToFile);

 while not(zoomarr[i])or(i>23) do inc(i);
 polyg := TypeMapArr[0].GeoConvert.PoligonProject(i + 8, APolyLL);
 GetDwnlNum(min,max,Polyg,false);
 p_x:=min.x;
 while p_x<max.x do
  begin
   p_y:=min.Y;
   while p_y<max.Y do
    begin
     if FProgress.Visible=false then
      begin
        FMain.MouseUpPoint:= Fmain.MouseDownPoint;
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
 FProgress.Close;
end;


procedure ThreadExport.export2YaMaps(APolyLL:TExtendedPointArray);
var p_x,p_y,p_xd256,p_yd256,i,j,ci,cj,xi,yi,hxyi,sizeim,cri,crj:integer;
    num_dwn,scachano,obrab,alpha:integer;
    polyg:TPointArray;
    pathto,persl,perzoom,kti:string;
    max,min,p_h:TPoint;
    png:TPngObject;
    Color32arr:PColor32Array;
    bmp32,bmp322,bmp32crop:TBitmap32;
    jpg:TJpegImage;
    bmp,bb,bmp8b:TBitmap;
    TileStream : TMemoryStream;
    PList:Text;
    LLCenter:TExtendedPoint;
    tic:longint;
    BtmRGB,PngRGB:PByte;
    Vprojection: byte;
    Vradiusa: double;
    Vradiusb: double;
    Vexct: double;
    VGeoConvert: ICoordConverter;
begin
 try
 if (TypeMapArr[0]=nil)and(TypeMapArr[1]=nil)and(TypeMapArr[2]=nil) then exit;
 i:=0;
 While not(zoomarr[i]) do inc(i);
 SetLength(polyg,length(APolyLL));
 if TypeMapArr[0]<>nil then polyg := TypeMapArr[0].GeoConvert.PoligonProject(i + 8, APolyLL)
  else if TypeMapArr[1]<>nil then polyg := TypeMapArr[1].GeoConvert.PoligonProject(i + 8, APolyLL)
        else if TypeMapArr[2]<>nil then  polyg := TypeMapArr[2].GeoConvert.PoligonProject(i + 8, APolyLL);
 GetMinMax(min,max,polyg,true);
 if TypeMapArr[0]<>nil then LLCenter:=TypeMapArr[0].GeoConvert.Pos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i + 8)
  else if TypeMapArr[1]<>nil then LLCenter:=TypeMapArr[1].GeoConvert.Pos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i + 8)
        else if TypeMapArr[2]<>nil then LLCenter:=TypeMapArr[2].GeoConvert.Pos2LonLat(Point(min.x+(max.X-min.X)div 2,min.y+(max.y-min.y)div 2),i + 8);

 hxyi:=1;
 sizeim:=128;

 try
 jpg:=TJpegImage.Create;
 bmp:=TBitmap.Create;
 bmp.Width:=sizeim;
 bmp.Height:=sizeim;
 png:=tpngobject.createblank(COLOR_PALETTE, 8, sizeim,sizeim);
 png.CompressionLevel:=cMap;
 png.AddtEXt('src','SAS.Planet'+SASVersion);
 TileStream:=TMemoryStream.Create;
 bmp32:=TBitmap32.Create;
 bmp32.DrawMode:=dmBlend;
 bmp322:=TBitmap32.Create;
 bmp322.DrawMode:=dmBlend;

 bmp32crop:=TBitmap32.Create;
 bmp32crop.Width:=sizeim;
 bmp32crop.Height:=sizeim;

 Vprojection:=2;
 Vradiusa:=6378137;
 Vradiusb:=6356752;
 Vexct:=sqrt(sqr(Vradiusa)-sqr(Vradiusb))/Vradiusa;
 VGeoConvert := TCoordConverterMercatorOnEllipsoid.Create(VExct,Vradiusa,Vradiusb);
 num_dwn:=0;
 SetLength(polyg,length(APolyLL));
 persl:='';
 kti:='';
 for i:=0 to length(TypeMapArr)-1 do
  if TypeMapArr[i]<>nil then
  begin
   persl:=persl+TypeMapArr[i].GetShortFolderName+'_';
   perzoom:='';
   for j:=0 to 23 do
    if zoomarr[j] then
     begin
      polyg := TypeMapArr[i].GeoConvert.PoligonProject(j + 8, APolyLL);
      num_dwn:=num_dwn+GetDwnlNum(min,max,Polyg,true);
      perzoom:=perzoom+inttostr(j+1)+'_';
      kti:=RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(min,j + 8).x,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(min,j + 8).y,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(max,j + 8).x,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(max,j + 8).y,4);
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

 for i:=0 to 23 do //по масштабу
  if zoomarr[i] then
   for j:=0 to 2 do //по типу
    if (TypeMapArr[j]<>nil)and(not((j=0)and(TypeMapArr[2]<>nil))) then
     begin
      polyg := VGeoConvert.PoligonProject(i + 8, APolyLL);
      GetDwnlNum(min,max,Polyg,false);

      p_x:=min.x;
      while p_x<max.x do
       begin
        p_y:=min.Y;
        while p_y<max.Y do
         begin
          if (FProgress.Visible=false)or(not(RgnAndRgn(Polyg,p_x,p_y,false)))
                                           then begin
                                                 inc(p_y,256);
                                                 CONTINUE;
                                                end;
          bmp322.Clear;
          if (j=2)and(TypeMapArr[0]<>nil) then
           begin
            p_h := VGeoConvert.Pos2OtherMap(Point(p_x,p_y-(p_y mod 256)), i + 8, TypeMapArr[0].GeoConvert);
            UniLoadTile(bmp322,TypeMapArr[0],Vprojection,p_h,p_x,p_y,i);
           end;
          bmp32.Clear;
          p_h := VGeoConvert.Pos2OtherMap(Point(p_x,p_y-(p_y mod 256)), i + 8, TypeMapArr[j].GeoConvert);
          if UniLoadTile(bmp32,TypeMapArr[j],Vprojection,p_h,p_x,p_y,i) then
           begin
            if (j=2)and(TypeMapArr[0]<>nil) then
              begin
               bmp322.Draw(0,0,bmp32);
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
                  WriteTileInCache(p_x div 256,p_y div 256,i+1,2,(yi*2)+xi,path, TileStream,Replace)
                end;
            if j=1 then
             begin
              bmp.Assign(bmp32);
              bmp8b:=ReduceColors(bmp, rmQuantize, dmNearest, 8, bmp.Palette);
              for xi:=0 to hxyi do
               for yi:=0 to hxyi do
                begin
                  BtmRGB:=Pointer(integer(bmp8b.Scanline[(sizeim*yi)])+(sizeim*xi));
                  PngRGB:=png.Scanline[0];
                  for cj:=(sizeim*yi) to (sizeim*yi)+(sizeim-1) do
                   begin
                    CopyMemory(PngRGB,BtmRGB,sizeim);
                    DEC(BtmRGB, bmp8b.Width);
                    DEC(PngRGB, png.Width);
                   end;
                  png.Palette:=bmp8b.Palette;
                  TileStream.Clear;
                  png.SaveToStream(TileStream);
                  WriteTileInCache(p_x div 256,p_y div 256,i+1,1,(yi*2)+xi,path, TileStream,Replace)
                end;
              bmp8b.Free;
             end;
            if j=0 then
              for xi:=0 to hxyi do
               for yi:=0 to hxyi do
                begin
                  bmp32crop.Clear;
                  bmp32crop.Draw(0,0,bounds(sizeim*xi,sizeim*yi,sizeim,sizeim),bmp32);
                  bmp.Assign(bmp32crop);
                  jpg.Assign(bmp);
                  TileStream.Clear;
                  jpg.CompressionQuality:=cSat;
                  jpg.SaveToStream(TileStream);
                  WriteTileInCache(p_x div 256,p_y div 256,i+1,2,(yi*2)+xi,path, TileStream,Replace)
                end;
           end;
          inc(obrab);
          if ((num_dwn<100)and(obrab mod 5 = 0))or
             ((num_dwn>=100)and(obrab mod 50 = 0)) then
           begin
            FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
            fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1)+'%';
           end;
          inc(p_y,256);
         end;
        inc(p_x,256);
       end;
     end;
 FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
 finally
  FProgress.Close;
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

end.
