unit UThreadExportIPhone;

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
  t_GeoTypes;

type
  TThreadExportIPhone = class(TThread)
  private
    PolygLL:TExtendedPointArray;
    Zoomarr:array [0..23] of boolean;
    typemaparr:array of TMapType;
    FNewFormat: Boolean;
    Fprogress: TFprogress2;
    Replace:boolean;
    Path:string;
    DISQLite3Database:TDISQLite3Database;
    csat,cmap,chib:byte;
    procedure export2iMaps(APolyLL:TExtendedPointArray);
    function Write_Stream_to_Blob_Traditional(const AStream: TStream; Azoom,Ax,Ay,Aflags,Alength:integer): Int64;
  protected
    procedure Execute; override;
  public
    constructor Create(
      APath: string;
      APolygon_: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Areplace: boolean;
      ANewFormat: Boolean;
      Acsat: byte;
      Acmap: byte;
      Achib: byte
    );
  end;

implementation

uses
  u_GeoToStr,
  i_ICoordConverter,
  u_GlobalState,
  u_CoordConverterMercatorOnSphere;

constructor TThreadExportIPhone.Create(
  APath: string;
  APolygon_: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Areplace: boolean;
  ANewFormat: Boolean;
  Acsat, Acmap, Achib: byte
);
var i:integer;
begin
  inherited Create(false);
  Priority := tpLowest;
  FreeOnTerminate:=true;
  Application.CreateForm(TFProgress2, FProgress);
  cSat:=Acsat;
  cMap:=Acmap;
  cHib:=Achib;
  FProgress.Visible:=true;
  Path:=APath;
  FNewFormat := ANewFormat;
  Replace:=AReplace;
  setlength(PolygLL,length(APolygon_));
  for i:=1 to length(APolygon_) do
    PolygLL[i-1]:=Apolygon_[i-1];
  for i:=0 to 23 do
    zoomarr[i]:=Azoomarr[i];
  setlength(typemaparr,length(Atypemaparr));
  for i:=1 to length(Atypemaparr) do
    typemaparr[i-1]:=Atypemaparr[i-1];
end;


procedure TThreadExportIPhone.Execute;
begin
  export2iMaps(PolygLL);
  FProgress.Close;
end;

function TThreadExportIPhone.Write_Stream_to_Blob_Traditional(const AStream: TStream; Azoom,Ax,Ay,Aflags,Alength:integer): Int64;
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

procedure TThreadExportIPhone.export2iMaps(APolyLL:TExtendedPointArray);
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

 if FNewFormat then begin
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
   if FNewFormat then DISQLite3Database.Execute('INSERT INTO version (version) VALUES ("5")')
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

function RetDate(inDate: TDateTime): string;
var xYear, xMonth, xDay: word;
begin
  DecodeDate(inDate, xYear, xMonth, xDay);
  Result := inttostr(xDay)+'.'+inttostr(xMonth)+'.'+inttostr(xYear);
end;
end.
