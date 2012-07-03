unit u_ThreadExportKML;

interface

uses
  Types,
  SysUtils,
  Classes,
  GR32,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  u_MapType,
  u_ResStrings,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExportKML = class(TThreadExportAbstract)
  private
    FMapType: TMapType;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FNotSaveNotExists: boolean;
    FPathExport: string;
    RelativePath: boolean;
    KMLFile: TextFile;
    FTilesToProcess: Int64;
    FTilesProcessed: Int64;
    procedure KmlFileWrite(
      const ATile: TPoint;
      AZoom, level: byte
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APath: string;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const APolygon: ILonLatPolygon;
      const Azoomarr: TByteDynArray;
      Atypemap: TMapType;
      ANotSaveNotExists: boolean;
      ARelativePath: boolean
    );
  end;

implementation

uses
  u_GeoToStr,
  i_TileIterator,
  u_TileIteratorByPolygon,
  i_VectorItemProjected,
  i_CoordConverter;

constructor TThreadExportKML.Create(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APath: string;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const APolygon: ILonLatPolygon;
  const Azoomarr: TByteDynArray;
  Atypemap: TMapType;
  ANotSaveNotExists: boolean;
  ARelativePath: boolean
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    Azoomarr
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FPathExport := APath;
  FNotSaveNotExists := ANotSaveNotExists;
  RelativePath := ARelativePath;
  FMapType := Atypemap;
end;

procedure TThreadExportKML.KmlFileWrite(
  const ATile: TPoint;
  AZoom, level: byte
);
var
  VZoom: Byte;
  xi, yi: integer;
  savepath, north, south, east, west: string;
  ToFile: string;
  VTileRect: TRect;
  VExtRect: TDoubleRect;
begin
  //TODO: Нужно думать на случай когда тайлы будут в базе данных
  savepath := FMapType.GetTileFileName(ATile, AZoom);
  if (FNotSaveNotExists) and (not (FMapType.TileExists(ATile, AZoom))) then begin
    exit;
  end;
  if RelativePath then begin
    savepath := ExtractRelativePath(ExtractFilePath(FPathExport), savepath);
  end;
  VExtRect := FMapType.GeoConvert.TilePos2LonLatRect(ATile, AZoom);

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
    ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  end;
  if level < Length(FZooms) then begin
    VZoom := FZooms[level];
    VTileRect := FMapType.GeoConvert.LonLatRect2TileRect(VExtRect, VZoom);
    for xi := VTileRect.Left to VTileRect.Right - 1 do begin
      for yi := VTileRect.Top to VTileRect.Bottom - 1 do begin
        KmlFileWrite(Point(xi, yi), VZoom, level + 1);
      end;
    end;
  end;
  ToFile := AnsiToUtf8(#13#10 + '</Folder>');
  Write(KMLFile, ToFile);
end;

procedure TThreadExportKML.ProcessRegion;
var
  i: integer;
  VZoom: Byte;
  ToFile: string;
  VProjectedPolygon: IProjectedPolygon;
  VTempIterator: ITileIterator;
  VIterator: ITileIterator;
  VTile: TPoint;
begin
  inherited;
  FTilesToProcess := 0;
  if Length(FZooms) > 0 then begin
    VZoom := FZooms[0];
    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(FMapType.GeoConvert, VZoom),
        PolygLL
      );
    VIterator := TTileIteratorByPolygon.Create(VProjectedPolygon);
    FTilesToProcess := FTilesToProcess + VIterator.TilesTotal;
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VProjectedPolygon :=
        FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(FMapType.GeoConvert, VZoom),
          PolygLL
        );
      VTempIterator := TTileIteratorByPolygon.Create(VProjectedPolygon);
      FTilesToProcess := FTilesToProcess + VTempIterator.TilesTotal;
    end;
  end;
  FTilesProcessed := 0;
  ProgressInfo.SetCaption(SAS_STR_ExportTiles);
  ProgressInfo.SetFirstLine(
    SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
  );
  ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  try
    AssignFile(KMLFile, FPathExport);
    Rewrite(KMLFile);
    ToFile := AnsiToUtf8('<?xml version="1.0" encoding="UTF-8"?>' + #13#10 + '<kml xmlns="http://earth.google.com/kml/2.1">' + #13#10);
    ToFile := ToFile + AnsiToUtf8('<Document>' + #13#10 + '<name>' + ExtractFileName(FPathExport) + '</name>');
    Write(KMLFile, ToFile);

    VZoom := FZooms[0];
    while VIterator.Next(VTile) do begin
      if not CancelNotifier.IsOperationCanceled(OperationID) then begin
        KmlFileWrite(VTile, VZoom, 1);
      end;
    end;
    ToFile := AnsiToUtf8(#13#10 + '</Document>' + #13#10 + '</kml>');
    Write(KMLFile, ToFile);
    CloseFile(KMLFile);
  finally
    ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  end;
end;

end.
