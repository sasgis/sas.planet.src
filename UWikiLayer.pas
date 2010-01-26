unit UWikiLayer;

interface

uses
  Classes,
  GR32,
  UgeoFun,
  UMapType,
  t_GeoTypes,
  u_KmlInfoSimple;

type
  TWikiLayerElement = class
  public
    name_blok: string;
    num_blok: string;
    description: string;
    LT, RD: Tpoint;
    FPolygonOnBitmap: TPointArray;
    constructor Create;
    destructor Destroy; override;
  end;

  TWikiLayer = class
  private
    FFixedPointArray: TArrayOfFixedPoint;
    FWikiLayerElments: array of TWikiLayerElement;
    procedure addWL(var AData: TKMLData);
    procedure DrawWikiElement(var AData: TWikiLayerElement);
    procedure DrawWikiElementGR32(var AData: TWikiLayerElement);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddFromLayer(Alayer: TMapType);
    procedure MouseOnReg(var APWL: TResObj; xy: TPoint);
  end;

implementation

uses
  SysUtils,
  StrUtils,
  GR32_Polygons,
  i_ICoordConverter,
  u_GlobalState,
  unit1;

{ TWikiLayerElement }

constructor TWikiLayerElement.Create;
begin
  name_blok := '';
  num_blok := '';
  description := '';
  FPolygonOnBitmap := nil;
end;

destructor TWikiLayerElement.Destroy;
begin
  name_blok := '';
  num_blok := '';
  description := '';
  FPolygonOnBitmap := nil;
  inherited;
end;

{ TWikiLayer }

procedure TWikiLayer.AddFromLayer(Alayer: TMapType);
var
  i, j, ii: integer;
  Vzoom: byte;
  VTile: TPoint;
  VCenterTile: TPoint;
  VPos: TPoint;
  kml: TKmlInfoSimple;
  VSizeInTile: TPoint;
begin
  FMain.LayerMapWiki.Visible := true;
  VSizeInTile := Fmain.LoadedSizeInTile;
  Fmain.LayerMapWiki.Bitmap.BeginUpdate;
  try
    Vzoom := GState.zoom_size - 1;
    VPos := GState.sat_map_both.GeoConvert.Pos2OtherMap(FMain.ScreenCenterPos, Vzoom + 8, Alayer.GeoConvert);
    VCenterTile := Alayer.GeoConvert.PixelPos2TilePos(VPos, Vzoom);
    for i := 0 to VSizeInTile.X do begin
      for j := 0 to VSizeInTile.Y do begin
        VTile.X := VCenterTile.X - (VSizeInTile.X div 2) + i;
        VTile.Y := VCenterTile.Y - (VSizeInTile.Y div 2) + j;
        Alayer.GeoConvert.CheckTilePosStrict(VTile, Vzoom, GState.CiclMap);
        KML := TKmlInfoSimple.Create;
        try
          if Alayer.LoadTile(kml, VTile, Vzoom, true) then begin
            for ii := 0 to length(KML.Data) - 1 do begin
              addWL(KML.Data[ii]);
            end;
          end;
        finally
          KML.Free;
        end;
      end;
    end;
  finally
    Fmain.LayerMapWiki.Bitmap.EndUpdate;
  end;
end;

procedure TWikiLayer.Clear;
var
  i: integer;
begin
  for i := 0 to length(FWikiLayerElments) - 1 do begin
    FreeAndNil(FWikiLayerElments[i]);
  end;
  FWikiLayerElments := nil;
  FMain.LayerMapWiki.Visible := false;
end;

constructor TWikiLayer.Create;
begin
  FWikiLayerElments := nil;
  SetLength(FFixedPointArray, 256);
end;

destructor TWikiLayer.Destroy;
begin
  Clear;
  FWikiLayerElments := nil;
  FFixedPointArray := nil;
  inherited;
end;

procedure TWikiLayer.MouseOnReg(var APWL: TResObj; xy: TPoint);
var
  i, j: integer;
  VLen: integer;
begin
  for i := 0 to length(FWikiLayerElments) - 1 do begin
    if (xy.x > FWikiLayerElments[i].lt.X - 5) and (xy.x < FWikiLayerElments[i].rd.X + 5) and
      (xy.y > FWikiLayerElments[i].lt.Y - 5) and (xy.y < FWikiLayerElments[i].rd.Y + 5) then begin
      VLen := length(FWikiLayerElments[i].FPolygonOnBitmap);
      if VLen > 0 then begin
        if VLen = 1 then begin
          APWL.name := FWikiLayerElments[i].name_blok;
          APWL.descr := FWikiLayerElments[i].description;
          APWL.numid := FWikiLayerElments[i].num_blok;
          APWL.find := true;
          Break;
        end else begin
          j := 1;
          if (FWikiLayerElments[i].FPolygonOnBitmap[0].X <> FWikiLayerElments[i].FPolygonOnBitmap[VLen - 1].x) or
            (FWikiLayerElments[i].FPolygonOnBitmap[0].y <> FWikiLayerElments[i].FPolygonOnBitmap[VLen - 1].y) then begin
            while (j < length(FWikiLayerElments[i].FPolygonOnBitmap)) do begin
              if CursorOnLinie(xy.x, xy.Y, FWikiLayerElments[i].FPolygonOnBitmap[j - 1].x, FWikiLayerElments[i].FPolygonOnBitmap[j - 1].y,
                FWikiLayerElments[i].FPolygonOnBitmap[j].x, FWikiLayerElments[i].FPolygonOnBitmap[j].y, 3) then begin
                APWL.name := FWikiLayerElments[i].name_blok;
                APWL.descr := FWikiLayerElments[i].description;
                APWL.numid := FWikiLayerElments[i].num_blok;
                APWL.find := true;
                exit;
              end;
              inc(j);
            end;
          end else if PtInRgn(FWikiLayerElments[i].FPolygonOnBitmap, xy) then begin
            if (PolygonSquare(FWikiLayerElments[i].FPolygonOnBitmap) > APWL.S) and (APWL.S <> 0) then begin
              continue;
            end;
            APWL.S := PolygonSquare(FWikiLayerElments[i].FPolygonOnBitmap);
            APWL.name := FWikiLayerElments[i].name_blok;
            APWL.descr := FWikiLayerElments[i].description;
            APWL.numid := FWikiLayerElments[i].num_blok;
            APWL.find := true;
          end;
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.addWL(var AData: TKMLData);
var
  i, lenLay: integer;
  VConverter: ICoordConverter;
begin
  VConverter := GState.sat_map_both.GeoConvert;
  Delete(AData.description, posEx('#ge', AData.description, 0), 1);
  setLength(FWikiLayerElments, length(FWikiLayerElments) + 1);
  lenLay := length(FWikiLayerElments);
  FWikiLayerElments[lenLay - 1] := TWikiLayerElement.Create;
  With FWikiLayerElments[lenLay - 1] do begin
    VConverter.CheckLonLatPos(AData.coordinatesLT);
    LT := VConverter.LonLat2PixelPos(AData.coordinatesLT, GState.zoom_size - 1);
    LT := Fmain.MapPixel2LoadedPixel(LT);
    VConverter.CheckLonLatPos(AData.coordinatesRD);
    RD := VConverter.LonLat2PixelPos(AData.coordinatesRD, GState.zoom_size - 1);
    RD := Fmain.MapPixel2LoadedPixel(RD);
    if AData.coordinatesLT.X = AData.coordinatesRD.x then begin
      LT.X := LT.X - 3;
      RD.x := RD.x + 3;
    end;
    if AData.coordinatesLT.y = AData.coordinatesRD.y then begin
      LT.Y := LT.Y - 3;
      RD.Y := RD.Y + 3;
    end;
    if (((RD.x - LT.x) <= 1) or ((RD.y - LT.y) <= 1) or
      ((LT.y > ((yhgpx div 2) + mHd2)) or (RD.y < ((yhgpx div 2) - mHd2)) or (LT.x > ((xhgpx div 2) + mWd2)) or (RD.x < ((xhgpx div 2) - mWd2)))) then begin
      exit;
    end;
    name_blok := AData.name;
    num_blok := AData.PlacemarkID;
    description := AData.description;
    setLength(FPolygonOnBitmap, length(AData.coordinates));
    if length(AData.coordinates) = 1 then begin
      setLength(FPolygonOnBitmap, 1);
      VConverter.CheckLonLatPos(AData.coordinates[0]);
      FPolygonOnBitmap[0] := VConverter.LonLat2PixelPos(AData.coordinates[0], GState.zoom_size - 1);
      FPolygonOnBitmap[0] := Fmain.MapPixel2LoadedPixel(FPolygonOnBitmap[0]);
    end else begin
      for i := 0 to length(AData.coordinates) - 1 do begin
        VConverter.CheckLonLatPos(AData.coordinates[i]);
        FPolygonOnBitmap[i] := VConverter.LonLat2PixelPos(AData.coordinates[i], GState.zoom_size - 1);
        FPolygonOnBitmap[i] := Fmain.MapPixel2LoadedPixel(FPolygonOnBitmap[i]);
      end;
    end;
  end;
  DrawWikiElementGR32(FWikiLayerElments[lenLay - 1]);
end;

procedure TWikiLayer.DrawWikiElement(var AData: TWikiLayerElement);
begin
  FMain.LayerMapWiki.Bitmap.Canvas.Pen.Width := 3;
  FMain.LayerMapWiki.Bitmap.Canvas.Pen.Color := GState.WikiMapFonColor;
  if length(AData.FPolygonOnBitmap) = 1 then begin
    FMain.LayerMapWiki.Bitmap.Canvas.Ellipse(AData.FPolygonOnBitmap[0].x - 2, AData.FPolygonOnBitmap[0].y - 2, AData.FPolygonOnBitmap[0].x + 2, AData.FPolygonOnBitmap[0].y + 2);
  end else begin
    FMain.LayerMapWiki.Bitmap.Canvas.Polyline(AData.FPolygonOnBitmap);
  end;
  FMain.LayerMapWiki.Bitmap.Canvas.Pen.Width := 1;
  FMain.LayerMapWiki.Bitmap.Canvas.Pen.Color := GState.WikiMapMainColor;
  if length(AData.FPolygonOnBitmap) = 1 then begin
    FMain.LayerMapWiki.Bitmap.Canvas.Ellipse(AData.FPolygonOnBitmap[0].x - 2, AData.FPolygonOnBitmap[0].y - 2, AData.FPolygonOnBitmap[0].x + 2, AData.FPolygonOnBitmap[0].y + 2);
  end else begin
    FMain.LayerMapWiki.Bitmap.Canvas.Polyline(AData.FPolygonOnBitmap);
  end;
end;

procedure TWikiLayer.DrawWikiElementGR32(var AData: TWikiLayerElement);
var
  VPolygon: TPolygon32;
  VLen: integer;
  i: integer;
  VColorMain: TColor32;
  VColorBG: TColor32;
begin
  VColorMain := SetAlpha(GState.WikiMapMainColor, 255);
  VColorBG := SetAlpha(GState.WikiMapFonColor, 255);
  VPolygon := TPolygon32.Create;
  try
    VLen := Length(AData.FPolygonOnBitmap);
    if VLen > 1 then begin
      if Length(FFixedPointArray) < VLen then begin
        SetLength(FFixedPointArray, VLen);
      end;
      for i := 0 to VLen - 1 do begin
        FFixedPointArray[i] := FixedPoint(AData.FPolygonOnBitmap[i]);
      end;
      VPolygon.AddPoints(FFixedPointArray[0], VLen);
      VPolygon.DrawEdge(FMain.LayerMapWiki.Bitmap, VColorBG);
      VPolygon.Offset(Fixed(1), Fixed(1));
      VPolygon.DrawEdge(FMain.LayerMapWiki.Bitmap, VColorMain);
    end else begin
      FFixedPointArray[0] := FixedPoint(AData.FPolygonOnBitmap[0].X, AData.FPolygonOnBitmap[0].Y - 3);
      FFixedPointArray[1] := FixedPoint(AData.FPolygonOnBitmap[0].X + 3, AData.FPolygonOnBitmap[0].Y);
      FFixedPointArray[2] := FixedPoint(AData.FPolygonOnBitmap[0].X, AData.FPolygonOnBitmap[0].Y + 3);
      FFixedPointArray[3] := FixedPoint(AData.FPolygonOnBitmap[0].X - 3, AData.FPolygonOnBitmap[0].Y);
      VPolygon.AddPoints(FFixedPointArray[0], 4);
      VPolygon.Draw(FMain.LayerMapWiki.Bitmap, VColorBG, VColorMain);
    end;
  finally
    FreeAndNil(VPolygon);
  end;
end;

end.
