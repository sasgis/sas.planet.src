unit u_GeometryFunc;

interface

uses
  t_GeoTypes,
  i_GeometryLonLat,
  i_ProjectionInfo;

function GetGeometryLonLatNearestPoint(
  const AGeometry: IGeometryLonLat;
  const AProjection: IProjectionInfo;
  const ACurrMapPixel: TDoublePoint;
  const AMaxDistInMapPixel: Double
): TDoublePoint;

implementation

uses
  Math,
  SysUtils,
  i_CoordConverter,
  i_EnumDoublePoint,
  u_GeoFunc;

function GetGeometryLonLatPointNearestPoint(
  const AGeometry: IGeometryLonLatPoint;
  const AProjection: IProjectionInfo;
  const ACurrMapPixel: TDoublePoint;
  out APoint: TDoublePoint;
  out ADist: Double
): Boolean;
var
  VConverter: ICoordConverter;
  VZoom: byte;
  VLonLatPoint: TDoublePoint;
  VMapPoint: TDoublePoint;
  VDist: Double;
begin
  Result := False;
  APoint := CEmptyDoublePoint;
  ADist := NaN;
  VZoom := AProjection.Zoom;
  VConverter := AProjection.GeoConverter;
  VLonLatPoint := AGeometry.Point;
  if not PointIsEmpty(VLonLatPoint) then begin
    VConverter.CheckLonLatPos(VLonLatPoint);
    VMapPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
    VDist :=  Sqr(VMapPoint.X - ACurrMapPixel.X) + Sqr(VMapPoint.Y - ACurrMapPixel.Y);
    Result := True;
    APoint := VLonLatPoint;
    ADist := VDist;
  end;
end;

function GetGeometryLonLatLineNearestPoint(
  const AGeometry: IGeometryLonLatSingleLine;
  const AProjection: IProjectionInfo;
  const ACurrMapPixel: TDoublePoint;
  out APoint: TDoublePoint;
  out ADist: Double
): Boolean;
var
  VConverter: ICoordConverter;
  VZoom: byte;
  VEnum: IEnumLonLatPoint;
  VLonLatPoint: TDoublePoint;
  VMapPoint: TDoublePoint;
  VDist: Double;
begin
  Result := False;
  APoint := CEmptyDoublePoint;
  ADist := NaN;
  VZoom := AProjection.Zoom;
  VConverter := AProjection.GeoConverter;
  VEnum := AGeometry.GetEnum;
  if VEnum.Next(VLonLatPoint) then begin
    VConverter.CheckLonLatPos(VLonLatPoint);
    VMapPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
    VDist :=  Sqr(VMapPoint.X - ACurrMapPixel.X) + Sqr(VMapPoint.Y - ACurrMapPixel.Y);
    APoint := VLonLatPoint;
    ADist :=  VDist;
    Result := True;

    while VEnum.Next(VLonLatPoint) do begin
      VConverter.CheckLonLatPos(VLonLatPoint);
      VMapPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
      VDist :=  Sqr(VMapPoint.X - ACurrMapPixel.X) + Sqr(VMapPoint.Y - ACurrMapPixel.Y);
      if VDist < ADist then begin
        ADist := VDist;
        APoint := VLonLatPoint;
      end;
    end;
  end;
end;

function GetGeometryLonLatPolygonNearestPoint(
  const AGeometry: IGeometryLonLatSinglePolygon;
  const AProjection: IProjectionInfo;
  const ACurrMapPixel: TDoublePoint;
  out APoint: TDoublePoint;
  out ADist: Double
): Boolean;
var
  VConverter: ICoordConverter;
  VZoom: byte;
  VEnum: IEnumLonLatPoint;
  VLonLatPoint: TDoublePoint;
  VMapPoint: TDoublePoint;
  VDist: Double;
begin
  Result := False;
  APoint := CEmptyDoublePoint;
  ADist := NaN;
  VZoom := AProjection.Zoom;
  VConverter := AProjection.GeoConverter;
  VEnum := AGeometry.GetEnum;
  if VEnum.Next(VLonLatPoint) then begin
    VConverter.CheckLonLatPos(VLonLatPoint);
    VMapPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
    VDist :=  Sqr(VMapPoint.X - ACurrMapPixel.X) + Sqr(VMapPoint.Y - ACurrMapPixel.Y);
    APoint := VLonLatPoint;
    ADist :=  VDist;
    Result := True;

    while VEnum.Next(VLonLatPoint) do begin
      VConverter.CheckLonLatPos(VLonLatPoint);
      VMapPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
      VDist :=  Sqr(VMapPoint.X - ACurrMapPixel.X) + Sqr(VMapPoint.Y - ACurrMapPixel.Y);
      if VDist < ADist then begin
        ADist := VDist;
        APoint := VLonLatPoint;
      end;
    end;
  end;
end;

function GetGeometryLonLatMultiLineNearestPoint(
  const AGeometry: IGeometryLonLatMultiLine;
  const AProjection: IProjectionInfo;
  const ACurrMapPixel: TDoublePoint;
  out APoint: TDoublePoint;
  out ADist: Double
): Boolean;
var
  VLonLatPoint: TDoublePoint;
  VDist: Double;
  i: Integer;
begin
  Result := False;
  APoint := CEmptyDoublePoint;
  ADist := NaN;
  for i := 0 to AGeometry.Count - 1 do begin
    if GetGeometryLonLatLineNearestPoint(AGeometry.Item[i], AProjection, ACurrMapPixel, VLonLatPoint, VDist) then begin
      if Result then begin
        if VDist < ADist then begin
          APoint := VLonLatPoint;
          ADist := VDist;
        end;
      end else begin
        Result := True;
        APoint := VLonLatPoint;
        ADist := VDist;
      end;
    end;
  end;
end;

function GetGeometryLonLatMultiPolygonNearestPoint(
  const AGeometry: IGeometryLonLatMultiPolygon;
  const AProjection: IProjectionInfo;
  const ACurrMapPixel: TDoublePoint;
  out APoint: TDoublePoint;
  out ADist: Double
): Boolean;
var
  VLonLatPoint: TDoublePoint;
  VDist: Double;
  i: Integer;
begin
  Result := False;
  APoint := CEmptyDoublePoint;
  ADist := NaN;
  for i := 0 to AGeometry.Count - 1 do begin
    if GetGeometryLonLatPolygonNearestPoint(AGeometry.Item[i], AProjection, ACurrMapPixel, VLonLatPoint, VDist) then begin
      if Result then begin
        if VDist < ADist then begin
          APoint := VLonLatPoint;
          ADist := VDist;
        end;
      end else begin
        Result := True;
        APoint := VLonLatPoint;
        ADist := VDist;
      end;
    end;
  end;
end;

function GetGeometryLonLatNearestPoint(
  const AGeometry: IGeometryLonLat;
  const AProjection: IProjectionInfo;
  const ACurrMapPixel: TDoublePoint;
  const AMaxDistInMapPixel: Double
): TDoublePoint;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatSingleLine;
  VPolygon: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiLine;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
  VSqDist: Double;
  VDist: Double;
  VLonLatPoint: TDoublePoint;
begin
  VSqDist := Sqr(AMaxDistInMapPixel);
  Result := CEmptyDoublePoint;
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    if GetGeometryLonLatPointNearestPoint(VPoint, AProjection, ACurrMapPixel, VLonLatPoint, VDist) then begin
      if VDist <= VSqDist then begin
        Result := VLonLatPoint;
      end;
    end;
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    if GetGeometryLonLatMultiLineNearestPoint(VMultiLine, AProjection, ACurrMapPixel, VLonLatPoint, VDist) then begin
      if VDist <= VSqDist then begin
        Result := VLonLatPoint;
      end;
    end;
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    if GetGeometryLonLatMultiPolygonNearestPoint(VMultiPolygon, AProjection, ACurrMapPixel, VLonLatPoint, VDist) then begin
      if VDist <= VSqDist then begin
        Result := VLonLatPoint;
      end;
    end;
  end else if Supports(AGeometry, IGeometryLonLatSingleLine, VLine) then begin
    if GetGeometryLonLatLineNearestPoint(VLine, AProjection, ACurrMapPixel, VLonLatPoint, VDist) then begin
      if VDist <= VSqDist then begin
        Result := VLonLatPoint;
      end;
    end;
  end else if Supports(AGeometry, IGeometryLonLatSinglePolygon, VPolygon) then begin
    if GetGeometryLonLatPolygonNearestPoint(VPolygon, AProjection, ACurrMapPixel, VLonLatPoint, VDist) then begin
      if VDist <= VSqDist then begin
        Result := VLonLatPoint;
      end;
    end;
  end else begin
    Assert(False);
  end;
end;

end.
