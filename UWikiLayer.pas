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
    FProjectedArr: TPointArray;
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
  FProjectedArr := nil;
end;

destructor TWikiLayerElement.Destroy;
begin
  name_blok := '';
  num_blok := '';
  description := '';
  FProjectedArr := nil;
  inherited;
end;

{ TWikiLayer }

procedure TWikiLayer.AddFromLayer(Alayer: TMapType);
var
  Ax, Ay, i, j, ii, Azoom: integer;
  APos: TPoint;
  kml: TKmlInfoSimple;
  VSizeInTile: TPoint;
begin
  FMain.LayerMapWiki.Visible := true;
  VSizeInTile := Fmain.LoadedSizeInTile;
  Fmain.LayerMapWiki.Bitmap.BeginUpdate;
  try
    for i := 0 to VSizeInTile.X do begin
      for j := 0 to VSizeInTile.Y do begin
        Azoom := GState.zoom_size;
        APos := GState.sat_map_both.GeoConvert.Pos2OtherMap(FMain.ScreenCenterPos, (Azoom - 1) + 8, Alayer.GeoConvert);
        if GState.CiclMap then begin
          Ax := Fmain.X2AbsX(APos.X - pr_x + (i shl 8), GState.zoom_size);
        end else begin
          Ax := APos.X - pr_x + (i shl 8);
        end;
        Ay := APos.y - pr_y + (j shl 8);
        KML := TKmlInfoSimple.Create;
        try
          if Alayer.LoadTile(kml, Ax, Ay, Azoom, true) then begin
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
  l: integer;
begin
  for i := 0 to length(FWikiLayerElments) - 1 do begin
    if (xy.x > FWikiLayerElments[i].lt.X - 5) and (xy.x < FWikiLayerElments[i].rd.X + 5) and
      (xy.y > FWikiLayerElments[i].lt.Y - 5) and (xy.y < FWikiLayerElments[i].rd.Y + 5) then begin
      if length(FWikiLayerElments[i].FProjectedArr) = 1 then begin
        APWL.name := FWikiLayerElments[i].name_blok;
        APWL.descr := FWikiLayerElments[i].description;
        APWL.numid := FWikiLayerElments[i].num_blok;
        APWL.find := true;
        exit;
      end;
      l := length(FWikiLayerElments[i].FProjectedArr) - 1;
      if l < 0 then begin
        continue;
      end;
      j := 1;
      if (FWikiLayerElments[i].FProjectedArr[0].X <> FWikiLayerElments[i].FProjectedArr[l].x) or
        (FWikiLayerElments[i].FProjectedArr[0].y <> FWikiLayerElments[i].FProjectedArr[l].y) then begin
        while (j < length(FWikiLayerElments[i].FProjectedArr)) do begin
          if CursorOnLinie(xy.x, xy.Y, FWikiLayerElments[i].FProjectedArr[j - 1].x, FWikiLayerElments[i].FProjectedArr[j - 1].y,
            FWikiLayerElments[i].FProjectedArr[j].x, FWikiLayerElments[i].FProjectedArr[j].y, 3) then begin
            APWL.name := FWikiLayerElments[i].name_blok;
            APWL.descr := FWikiLayerElments[i].description;
            APWL.numid := FWikiLayerElments[i].num_blok;
            APWL.find := true;
            exit;
          end;
          inc(j);
        end;
      end else if PtInRgn(FWikiLayerElments[i].FProjectedArr, xy) then begin
        if (PolygonSquare(FWikiLayerElments[i].FProjectedArr) > APWL.S) and (APWL.S <> 0) then begin
          continue;
        end;
        APWL.S := PolygonSquare(FWikiLayerElments[i].FProjectedArr);
        APWL.name := FWikiLayerElments[i].name_blok;
        APWL.descr := FWikiLayerElments[i].description;
        APWL.numid := FWikiLayerElments[i].num_blok;
        APWL.find := true;
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
      ((LT.y > (pr_y + mHd2)) or (RD.y < (pr_y - mHd2)) or (LT.x > (pr_x + mWd2)) or (RD.x < (pr_x - mWd2)))) then begin
      exit;
    end;
    name_blok := AData.name;
    num_blok := AData.PlacemarkID;
    description := AData.description;
    setLength(FProjectedArr, length(AData.coordinates));
    if length(AData.coordinates) = 1 then begin
      setLength(FProjectedArr, 1);
      VConverter.CheckLonLatPos(AData.coordinates[0]);
      FProjectedArr[0] := VConverter.LonLat2PixelPos(AData.coordinates[0], GState.zoom_size - 1);
      FProjectedArr[0] := Fmain.MapPixel2LoadedPixel(FProjectedArr[0]);
    end else begin
      for i := 0 to length(AData.coordinates) - 1 do begin
        VConverter.CheckLonLatPos(AData.coordinates[i]);
        FProjectedArr[i] := VConverter.LonLat2PixelPos(AData.coordinates[i], GState.zoom_size - 1);
        FProjectedArr[i] := Fmain.MapPixel2LoadedPixel(FProjectedArr[i]);
      end;
    end;
  end;
  DrawWikiElementGR32(FWikiLayerElments[lenLay - 1]);
end;

procedure TWikiLayer.DrawWikiElement(var AData: TWikiLayerElement);
begin
  FMain.LayerMapWiki.Bitmap.Canvas.Pen.Width := 3;
  FMain.LayerMapWiki.Bitmap.Canvas.Pen.Color := GState.WikiMapFonColor;
  if length(AData.FProjectedArr) = 1 then begin
    FMain.LayerMapWiki.Bitmap.Canvas.Ellipse(AData.FProjectedArr[0].x - 2, AData.FProjectedArr[0].y - 2, AData.FProjectedArr[0].x + 2, AData.FProjectedArr[0].y + 2);
  end else begin
    FMain.LayerMapWiki.Bitmap.Canvas.Polyline(AData.FProjectedArr);
  end;
  FMain.LayerMapWiki.Bitmap.Canvas.Pen.Width := 1;
  FMain.LayerMapWiki.Bitmap.Canvas.Pen.Color := GState.WikiMapMainColor;
  if length(AData.FProjectedArr) = 1 then begin
    FMain.LayerMapWiki.Bitmap.Canvas.Ellipse(AData.FProjectedArr[0].x - 2, AData.FProjectedArr[0].y - 2, AData.FProjectedArr[0].x + 2, AData.FProjectedArr[0].y + 2);
  end else begin
    FMain.LayerMapWiki.Bitmap.Canvas.Polyline(AData.FProjectedArr);
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
    VLen := Length(AData.FProjectedArr);
    if VLen > 1 then begin
      if Length(FFixedPointArray) < VLen then begin
        SetLength(FFixedPointArray, VLen);
      end;
      for i := 0 to VLen - 1 do begin
        FFixedPointArray[i] := FixedPoint(AData.FProjectedArr[i]);
      end;
      VPolygon.AddPoints(FFixedPointArray[0], VLen);
      VPolygon.DrawEdge(FMain.LayerMapWiki.Bitmap, VColorBG);
      VPolygon.Offset(Fixed(1), Fixed(1));
      VPolygon.DrawEdge(FMain.LayerMapWiki.Bitmap, VColorMain);
    end else begin
      FFixedPointArray[0] := FixedPoint(AData.FProjectedArr[0].X, AData.FProjectedArr[0].Y - 2);
      FFixedPointArray[1] := FixedPoint(AData.FProjectedArr[0].X + 2, AData.FProjectedArr[0].Y);
      FFixedPointArray[2] := FixedPoint(AData.FProjectedArr[0].X, AData.FProjectedArr[0].Y + 2);
      FFixedPointArray[3] := FixedPoint(AData.FProjectedArr[0].X - 2, AData.FProjectedArr[0].Y);
      VPolygon.AddPoints(FFixedPointArray[0], 4);
      VPolygon.Draw(FMain.LayerMapWiki.Bitmap, VColorBG, VColorMain);
    end;
  finally
    FreeAndNil(VPolygon);
  end;
end;

end.
