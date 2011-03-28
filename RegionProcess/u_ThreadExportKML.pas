unit u_ThreadExportKML;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  UMapType,
  u_GeoFun,
  u_ResStrings,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExportKML = class(TThreadExportAbstract)
  private
    FMapType: TMapType;
    FNotSaveNotExists: boolean;
    FPathExport: string;
    RelativePath: boolean;
    KMLFile: TextFile;
    procedure KmlFileWrite(x, y: integer; AZoom, level: byte);
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      APath: string;
      APolygon: TArrayOfDoublePoint;
      Azoomarr: array of boolean;
      Atypemap: TMapType;
      ANotSaveNotExists: boolean;
      ARelativePath: boolean
    );
  end;

implementation

uses
  Math,
  u_GeoToStr,
  i_CoordConverter;

constructor TThreadExportKML.Create(
  APath: string;
  APolygon: TArrayOfDoublePoint;
  Azoomarr: array of boolean;
  Atypemap: TMapType;
  ANotSaveNotExists: boolean;
  ARelativePath: boolean
);
begin
  inherited Create(APolygon, Azoomarr);
  FPathExport := APath;
  FNotSaveNotExists := ANotSaveNotExists;
  RelativePath := ARelativePath;
  FMapType := Atypemap;
end;

procedure TThreadExportKML.KmlFileWrite(x, y: integer; AZoom, level: byte);
var
  xym256lt, xym256rb: TPoint;
  VZoom: Byte;
  nxy, xi, yi: integer;
  savepath, north, south, east, west: string;
  ToFile: string;
  VExtRect: TDoubleRect;
  VTile: TPoint;
begin
  VTile := Point(x shr 8, y shr 8);
  //TODO: Нужно думать на случай когда тайлы будут в базе данных
  savepath := FMapType.GetTileFileName(VTile, AZoom);
  if (FNotSaveNotExists) and (not (FMapType.TileExists(VTile, AZoom))) then begin
    exit;
  end;
  if RelativePath then begin
    savepath := ExtractRelativePath(ExtractFilePath(FPathExport), savepath);
  end;
  xym256lt := Point(x - (x mod 256), y - (y mod 256));
  xym256rb := Point(256 + x - (x mod 256), 256 + y - (y mod 256));
  VExtRect.TopLeft := FMapType.GeoConvert.PixelPos2LonLat(xym256lt, (AZoom));
  VExtRect.BottomRight := FMapType.GeoConvert.PixelPos2LonLat(xym256rb, (AZoom));

  north := R2StrPoint(VExtRect.Top);
  south := R2StrPoint(VExtRect.Bottom);
  east := R2StrPoint(VExtRect.Right);
  west := R2StrPoint(VExtRect.Left);
  ToFile := #13#10 + '<Folder>' + #13#10 +{'  <name></name>'+#13#10+}'  <Region>' + #13#10 + '    <LatLonAltBox>' + #13#10 +
    '      <north>' + north + '</north>' + #13#10 + '      <south>' + south + '</south>' + #13#10 + '      <east>' + east + '</east>' + #13#10 +
    '      <west>' + west + '</west>' + #13#10 + '    </LatLonAltBox>' + #13#10 + '    <Lod>';
  if level > 1 then begin
    ToFile := ToFile + #13#10 + '      <minLodPixels>128</minLodPixels>';
  end else begin
    ToFile := ToFile + #13#10 + '      <minLodPixels>16</minLodPixels>';
  end;
  ToFile := ToFile + #13#10 + '      <maxLodPixels>-1</maxLodPixels>' + #13#10 + '    </Lod>' + #13#10 + '  </Region>' + #13#10 +
    '  <GroundOverlay>' + #13#10 + '    <drawOrder>' + inttostr(level) + '</drawOrder>' + #13#10 + '    <Icon>' + #13#10 +
    '      <href>' + savepath + '</href>' + #13#10 + '    </Icon>' + #13#10 + '    <LatLonBox>' + #13#10 + '      <north>' + north + '</north>' + #13#10 +
    '      <south>' + south + '</south>' + #13#10 + '      <east>' + east + '</east>' + #13#10 + '      <west>' + west + '</west>' + #13#10 +
    '    </LatLonBox>' + #13#10 + '  </GroundOverlay>';
  ToFile := AnsiToUtf8(ToFile);
  Write(KMLFile, ToFile);
  inc(FTilesProcessed);
  if FTilesProcessed mod 100 = 0 then begin
    ProgressFormUpdateOnProgress;
  end;
  if level < Length(FZooms) then begin
    VZoom := FZooms[level];
    nxy := round(intpower(2, VZoom - AZoom));
    for xi := 1 to nxy do begin
      for yi := 1 to nxy do begin
        KmlFileWrite(xym256lt.x * nxy + (256 * (xi - 1)), xym256lt.y * nxy + (256 * (yi - 1)), VZoom, level + 1);
      end;
    end;
  end;
  ToFile := AnsiToUtf8(#13#10 + '</Folder>');
  Write(KMLFile, ToFile);
end;

procedure TThreadExportKML.ProcessRegion;
var
  p_x, p_y, i: integer;
  VZoom: Byte;
  polyg: TArrayOfPoint;
  ToFile: string;
  max, min: TPoint;
begin
  inherited;
  FTilesToProcess := 0;
  SetLength(polyg, length(FPolygLL));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    polyg := FMapType.GeoConvert.LonLatArray2PixelArray(FPolygLL, VZoom);
    FTilesToProcess := FTilesToProcess + GetDwnlNum(min, max, Polyg, true);
  end;
  FTilesProcessed := 0;
  ProgressFormUpdateCaption(SAS_STR_ExportTiles, SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files);
  ProgressFormUpdateOnProgress;
  try
    AssignFile(KMLFile, FPathExport);
    Rewrite(KMLFile);
    ToFile := AnsiToUtf8('<?xml version="1.0" encoding="UTF-8"?>' + #13#10 + '<kml xmlns="http://earth.google.com/kml/2.1">' + #13#10);
    ToFile := ToFile + AnsiToUtf8('<Document>' + #13#10 + '<name>' + ExtractFileName(FPathExport) + '</name>');
    Write(KMLFile, ToFile);

    VZoom := FZooms[0];
    polyg := FMapType.GeoConvert.LonLatArray2PixelArray(FPolygLL, VZoom);
    GetDwnlNum(min, max, Polyg, false);
    p_x := min.x;
    while p_x < max.x do begin
      p_y := min.Y;
      while p_y < max.Y do begin
        if not IsCancel then begin
          if (RgnAndRgn(Polyg, p_x, p_y, false)) then begin
            KmlFileWrite(p_x, p_y, VZoom, 1);
          end;
        end;
        inc(p_y, 256);
      end;
      inc(p_x, 256);
    end;
    ToFile := AnsiToUtf8(#13#10 + '</Document>' + #13#10 + '</kml>');
    Write(KMLFile, ToFile);
    CloseFile(KMLFile);
  finally
    ProgressFormUpdateOnProgress;
  end;
end;

end.
