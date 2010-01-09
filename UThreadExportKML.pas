unit UThreadExportKML;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Classes,
  Graphics,
  gifimage,
  VCLZIp,
  PNGImage,
  JPEG,
  GR32,
  UMapType,
  UGeoFun,
  unit4,
  UResStrings,
  t_GeoTypes;

type
  TThreadExportKML = class(TThread)
  private
    PolygLL:TExtendedPointArray;
    Zoomarr:array [0..23] of boolean;
    FTypemap: TMapType;
    Fprogress: TFprogress2;
    ziped:boolean;
    Replace:boolean;
    Path:string;
    Zip:TVCLZip;
    Zippu:boolean;
    RelativePath:boolean;
    procedure Export2KML(APolyLL:TExtendedPointArray);
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Execute; override;
  public
    constructor Create(
      APath: string;
      APolygon_: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemap: TMapType;
      Areplace: boolean;
      Aziped: boolean;
      ARelativePath: boolean
    );
  end;

implementation

uses
  Math,
  u_GeoToStr,
  unit1,
  i_ICoordConverter;

procedure TThreadExportKML.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
 if Zippu then Zip.CancelTheOperation;
end;

constructor TThreadExportKML.Create(
  APath: string;
  APolygon_: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemap: TMapType;
  Areplace, Aziped: boolean;
  ARelativePath: boolean
);
var i:integer;
begin
  inherited Create(false);
  Priority := tpLowest;
  FreeOnTerminate:=true;
  Application.CreateForm(TFProgress2, FProgress);
  Zippu:=false;
  FProgress.OnClose:=CloseFProgress;
  FProgress.Visible:=true;
  Path:=APath;
  ziped:=Aziped;
  Replace:=AReplace;
  RelativePath:=ARelativePath;
  setlength(PolygLL,length(APolygon_));
  for i:=1 to length(APolygon_) do
    PolygLL[i-1]:=Apolygon_[i-1];
  for i:=0 to 23 do
    zoomarr[i]:=Azoomarr[i];
  FTypemap:=Atypemap;
end;


procedure TThreadExportKML.Execute;
begin
  Zippu:=false;
  export2KML(PolygLL);
  FProgress.Close;
end;

function RetDate(inDate: TDateTime): string;
var xYear, xMonth, xDay: word;
begin
  DecodeDate(inDate, xYear, xMonth, xDay);
  Result := inttostr(xDay)+'.'+inttostr(xMonth)+'.'+inttostr(xYear);
end;

procedure TThreadExportKML.Export2KML(APolyLL:TExtendedPointArray);
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
  savepath:=FTypeMap.GetTileFileName(x,y,z);
  if (Replace)and(not(FTypeMap.TileExists(x,y,z))) then exit;
  if RelativePath then savepath:= ExtractRelativePath(ExtractFilePath(path), savepath);
  xym256lt:=Point(x-(x mod 256),y-(y mod 256));
  xym256rb:=Point(256+x-(x mod 256),256+y-(y mod 256));
  north:=R2StrPoint(FTypeMap.GeoConvert.Pos2LonLat(xym256lt,(z - 1) + 8).y);
  south:=R2StrPoint(FTypeMap.GeoConvert.Pos2LonLat(xym256rb,(z - 1) + 8).y);
  east:=R2StrPoint(FTypeMap.GeoConvert.Pos2LonLat(xym256rb,(z - 1) + 8).x);
  west:=R2StrPoint(FTypeMap.GeoConvert.Pos2LonLat(xym256lt,(z - 1) + 8).x);
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
 datestr:=RetDate(now);
 persl:=persl+ FTypeMap.GetShortFolderName+'_';
 perzoom:='';
 for j:=0 to 23 do
  if zoomarr[j] then
   begin
    polyg := FTypeMap.GeoConvert.PoligonProject(j + 8, APolyLL);
    num_dwn:=num_dwn+GetDwnlNum(min,max,Polyg,true);
    perzoom:=perzoom+inttostr(j+1)+'_';
    kti:=RoundEx(FTypeMap.GeoConvert.Pos2LonLat(min,j + 8).x,4);
    kti:=kti+'_'+RoundEx(FTypeMap.GeoConvert.Pos2LonLat(min,j + 8).y,4);
    kti:=kti+'_'+RoundEx(FTypeMap.GeoConvert.Pos2LonLat(max,j + 8).x,4);
    kti:=kti+'_'+RoundEx(FTypeMap.GeoConvert.Pos2LonLat(max,j + 8).y,4);
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
 polyg := FTypeMap.GeoConvert.PoligonProject(i + 8, APolyLL);
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

end.
