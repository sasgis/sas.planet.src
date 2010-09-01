unit UThreadExportKML;

interface

uses
  Windows,
  Types,
  Forms,
  SysUtils,
  Classes,
  GR32,
  UMapType,
  UGeoFun,
  unit4,
  UResStrings,
  t_GeoTypes,
  u_ExportThreadAbstract;

type
  TThreadExportKML = class(TExportThreadAbstract)
  private
    FMapType: TMapType;
    FProgressForm: TFprogress2;
    FIsReplace:boolean;
    FPathExport:string;
    RelativePath:boolean;
    KMLFile:TextFile;
    procedure KmlFileWrite(x,y:integer;z,level:byte);
  protected
    procedure ExportRegion; override;
  public
    constructor Create(
      APath: string;
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemap: TMapType;
      Areplace: boolean;
      ARelativePath: boolean
    );
  end;

implementation

uses
  Math,
  Dialogs,
  u_GeoToStr,
  i_ICoordConverter;

constructor TThreadExportKML.Create(
  APath: string;
  APolygon: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemap: TMapType;
  Areplace: boolean;
  ARelativePath: boolean
);
begin
  inherited Create(APolygon, Azoomarr);
  FPathExport:=APath;
  FIsReplace:=AReplace;
  RelativePath:=ARelativePath;
  FMapType:=Atypemap;
end;

procedure TThreadExportKML.KmlFileWrite(x,y:integer;z,level:byte);
var xym256lt,xym256rb:TPoint;
    i,nxy,xi,yi:integer;
    savepath,north,south,east,west:string;
    ToFile:string;
  VExtRect: TExtendedRect;
  VTile: TPoint;
begin
  VTile := Point(x shr 8, y shr 8);
  //TODO: Нужно думать на случай когда тайлы будут в базе данных
  savepath:=FMapType.GetTileFileName(VTile, z - 1);
  if (FIsReplace)and(not(FMapType.TileExists(VTile, z - 1))) then exit;
  if RelativePath then savepath:= ExtractRelativePath(ExtractFilePath(FPathExport), savepath);
  xym256lt:=Point(x-(x mod 256),y-(y mod 256));
  xym256rb:=Point(256+x-(x mod 256),256+y-(y mod 256));
  VExtRect.TopLeft := FMapType.GeoConvert.PixelPos2LonLat(xym256lt,(z - 1));
  VExtRect.BottomRight := FMapType.GeoConvert.PixelPos2LonLat(xym256rb,(z - 1));

  north:=R2StrPoint(VExtRect.Top);
  south:=R2StrPoint(VExtRect.Bottom);
  east:=R2StrPoint(VExtRect.Right);
  west:=R2StrPoint(VExtRect.Left);
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
  inc(FTilesProcessed);
  if FTilesProcessed mod 100 = 0 then
   begin
    ProgressFormUpdateOnProgress
   end;
  i:=z;
  while (not(FZoomArr[i]))and(i<24) do inc(i);
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

procedure TThreadExportKML.ExportRegion;
var p_x,p_y,i,j:integer;
    polyg:TPointArray;
    ToFile:string;
    max,min:TPoint;
begin
 FTilesToProcess:=0;
 SetLength(polyg,length(FPolygLL));
 for j:=0 to 23 do
  if FZoomArr[j] then
   begin
    polyg := FMapType.GeoConvert.LonLatArray2PixelArray(FPolygLL, j);
    FTilesToProcess:=FTilesToProcess+GetDwnlNum(min,max,Polyg,true);
   end;
  FTilesProcessed:=0;
  ProgressFormUpdateCaption(SAS_STR_ExportTiles, SAS_STR_AllSaves+' '+inttostr(FTilesToProcess)+' '+SAS_STR_Files);
  ProgressFormUpdateOnProgress;
 try
   i:=0;
   AssignFile(KMLFile,FPathExport);
   Rewrite(KMLFile);
   ToFile:=AnsiToUtf8('<?xml version="1.0" encoding="UTF-8"?>'+#13#10+'<kml xmlns="http://earth.google.com/kml/2.1">'+#13#10);
   ToFile:=ToFile+AnsiToUtf8('<Document>'+#13#10+'<name>'+ExtractFileName(FPathExport)+'</name>');
   Write(KMLFile,ToFile);

   while not(FZoomArr[i])or(i>23) do inc(i);
   polyg := FMapType.GeoConvert.LonLatArray2PixelArray(FPolygLL, i);
   GetDwnlNum(min,max,Polyg,false);
   p_x:=min.x;
   while p_x<max.x do
    begin
     p_y:=min.Y;
     while p_y<max.Y do
      begin
       if not FProgressForm.Visible then begin
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
   ToFile:=AnsiToUtf8(#13#10+'</Document>'+#13#10+'</kml>');
   Write(KMLFile,ToFile);
   CloseFile(KMLFile);
 finally
  ProgressFormUpdateOnProgress;
 end;
end;

end.
