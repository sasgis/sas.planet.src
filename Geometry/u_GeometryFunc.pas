unit u_GeometryFunc;

interface

uses
  GR32,
  GR32_Polygons,
  t_GeoTypes,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_LocalCoordConverter,
  i_DoublePointsAggregator,
  i_ProjectionInfo;

function GetGeometryLonLatNearestPoint(
  const AGeometry: IGeometryLonLat;
  const AProjection: IProjectionInfo;
  const ACurrMapPixel: TDoublePoint;
  const AMaxDistInMapPixel: Double
): TDoublePoint;

function GetProjectedSinglePolygonByProjectedPolygon(
  const AGeometry: IGeometryProjectedPolygon
): IGeometryProjectedSinglePolygon;

procedure ProjectedLine2GR32Polygon(
  const ALine: IGeometryProjectedLine;
  const ALocalConverter: ILocalCoordConverter;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);

procedure ProjectedPolygon2GR32Polygon(
  const ALine: IGeometryProjectedPolygon;
  const ALocalConverter: ILocalCoordConverter;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);

function IsValidLonLatLine(
  const AGeometry: IGeometryLonLatLine
): Boolean;

function IsValidLonLatPolygon(
  const AGeometry: IGeometryLonLatPolygon
): Boolean;

implementation

uses
  Math,
  SysUtils,
  i_CoordConverter,
  i_EnumDoublePoint,
  u_EnumDoublePointClosePoly,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
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

function GetProjectedSinglePolygonByProjectedPolygon(
  const AGeometry: IGeometryProjectedPolygon
): IGeometryProjectedSinglePolygon;
var
  VMulti: IGeometryProjectedMultiPolygon;
begin
  if not Supports(AGeometry, IGeometryProjectedSinglePolygon, Result) then begin
    if Supports(AGeometry, IGeometryProjectedMultiPolygon, VMulti) then begin
      Result := VMulti.Item[0];
    end else begin
      Result := nil;
    end;
  end;
end;  

procedure SingleLine2GR32Polygon(
  const ALine: IGeometryProjectedSingleLine;
  const ALocalConverter: ILocalCoordConverter;
  const ARectWithDelta: TDoubleRect;
  const AMapRect: TDoubleRect;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);
var
  VEnum: IEnumLocalPoint;
  VPoint: TDoublePoint;
  VPointsProcessedCount: Integer;
  VIndex: Integer;
  i: Integer;
begin
  if IsIntersecProjectedRect(AMapRect, ALine.Bounds) then begin
    APreparedPointsAggreagtor.Clear;
    VEnum :=
      TEnumDoublePointMapPixelToLocalPixel.Create(
        ALocalConverter,
        ALine.GetEnum
      );
    VEnum :=
      TEnumLocalPointClipByRect.Create(
        False,
        ARectWithDelta,
        VEnum
      );
    VEnum := TEnumLocalPointFilterEqual.Create(VEnum);
    while VEnum.Next(VPoint) do begin
      APreparedPointsAggreagtor.Add(VPoint);
    end;
    VPointsProcessedCount := APreparedPointsAggreagtor.Count;
    if VPointsProcessedCount > 0 then begin
      if APolygon = nil then begin
        APolygon := TPolygon32.Create;
        APolygon.Antialiased := true;
        APolygon.AntialiasMode := am4times;
        APolygon.Closed := False;
      end else begin
        APolygon.NewLine;
      end;
      if Length(AFixedPointArray) < VPointsProcessedCount then begin
        SetLength(AFixedPointArray, VPointsProcessedCount);
      end;
      VIndex := 0;
      for i := 0 to VPointsProcessedCount - 1 do begin
        VPoint := APreparedPointsAggreagtor.Points[i];
        if PointIsEmpty(VPoint) then begin
          APolygon.AddPoints(AFixedPointArray[0], VIndex);
          APolygon.NewLine;
          VIndex := 0;
        end else begin
          AFixedPointArray[VIndex] := FixedPoint(VPoint.X, VPoint.Y);
          Inc(VIndex);
        end;
      end;
      APolygon.AddPoints(AFixedPointArray[0], VIndex);
    end;
  end;
end;

procedure ProjectedLine2GR32Polygon(
  const ALine: IGeometryProjectedLine;
  const ALocalConverter: ILocalCoordConverter;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);
var
  VMapRect: TDoubleRect;
  VLocalRect: TDoubleRect;
  VRectWithDelta: TDoubleRect;
  VLineIndex: Integer;
  VSingleLine: IGeometryProjectedSingleLine;
  VMultiLine: IGeometryProjectedMultiLine;
begin
  if Assigned(APolygon) then begin
    APolygon.Clear;
  end;

  if ALine <> nil then begin
    if not ALine.IsEmpty then begin
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      if IsIntersecProjectedRect(VMapRect, ALine.Bounds) then begin
        VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
        VRectWithDelta.Left := VLocalRect.Left - 10;
        VRectWithDelta.Top := VLocalRect.Top - 10;
        VRectWithDelta.Right := VLocalRect.Right + 10;
        VRectWithDelta.Bottom := VLocalRect.Bottom + 10;
        if Supports(ALine, IGeometryProjectedSingleLine, VSingleLine) then begin
          SingleLine2GR32Polygon(
            VSingleLine,
            ALocalConverter,
            VRectWithDelta,
            VMapRect,
            APreparedPointsAggreagtor,
            AFixedPointArray,
            APolygon
          );
        end else if Supports(ALine, IGeometryProjectedMultiLine, VMultiLine) then begin
          for VLineIndex := 0 to VMultiLine.Count - 1 do begin
            VSingleLine := VMultiLine.Item[VLineIndex];
            SingleLine2GR32Polygon(
              VSingleLine,
              ALocalConverter,
              VRectWithDelta,
              VMapRect,
              APreparedPointsAggreagtor,
              AFixedPointArray,
              APolygon
            );
          end;
        end;
      end;
    end;
  end;
end;

procedure SinglePoly2GR32Polygon(
  const ALine: IGeometryProjectedSinglePolygon;
  const ALocalConverter: ILocalCoordConverter;
  const ARectWithDelta: TDoubleRect;
  const AMapRect: TDoubleRect;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);
var
  VEnum: IEnumLocalPoint;
  VPoint: TDoublePoint;
  VPointsProcessedCount: Integer;
  i: Integer;
begin
  if IsIntersecProjectedRect(AMapRect, ALine.Bounds) then begin
    APreparedPointsAggreagtor.Clear;
    VEnum :=
      TEnumDoublePointMapPixelToLocalPixel.Create(
        ALocalConverter,
        ALine.GetEnum
      );
    VEnum :=
      TEnumLocalPointClipByRect.Create(
        True,
        ARectWithDelta,
        VEnum
      );
    VEnum := TEnumLocalPointFilterEqual.Create(VEnum);
    VEnum := TEnumLocalPointClosePoly.Create(VEnum);
    while VEnum.Next(VPoint) do begin
      APreparedPointsAggreagtor.Add(VPoint);
    end;
    VPointsProcessedCount := APreparedPointsAggreagtor.Count;
    if VPointsProcessedCount > 0 then begin
      if APolygon = nil then begin
        APolygon := TPolygon32.Create;
        APolygon.Antialiased := true;
        APolygon.AntialiasMode := am4times;
        APolygon.Closed := True;
      end else begin
        APolygon.NewLine;
      end;
      if Length(AFixedPointArray) < VPointsProcessedCount then begin
        SetLength(AFixedPointArray, VPointsProcessedCount);
      end;
      for i := 0 to VPointsProcessedCount - 1 do begin
        VPoint := APreparedPointsAggreagtor.Points[i];
        AFixedPointArray[i] := FixedPoint(VPoint.X, VPoint.Y);
      end;
      APolygon.AddPoints(AFixedPointArray[0], VPointsProcessedCount);
    end;
  end;
end;

procedure ProjectedPolygon2GR32Polygon(
  const ALine: IGeometryProjectedPolygon;
  const ALocalConverter: ILocalCoordConverter;
  const APreparedPointsAggreagtor: IDoublePointsAggregator;
  var AFixedPointArray: TArrayOfFixedPoint;
  var APolygon: TPolygon32
);
var
  VMapRect: TDoubleRect;
  VLocalRect: TDoubleRect;
  VRectWithDelta: TDoubleRect;
  VProjectedMultiLine: IGeometryProjectedMultiPolygon;
  VProjectedSingleLine: IGeometryProjectedSinglePolygon;
  VLineIndex: Integer;
begin
  if Assigned(APolygon) then begin
    APolygon.Clear;
  end;

  if ALine <> nil then begin
    if not ALine.IsEmpty then begin
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      if IsIntersecProjectedRect(VMapRect, ALine.Bounds) then begin
        VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
        VRectWithDelta.Left := VLocalRect.Left - 10;
        VRectWithDelta.Top := VLocalRect.Top - 10;
        VRectWithDelta.Right := VLocalRect.Right + 10;
        VRectWithDelta.Bottom := VLocalRect.Bottom + 10;
        if Supports(ALine, IGeometryProjectedSinglePolygon, VProjectedSingleLine) then begin
          SinglePoly2GR32Polygon(
            VProjectedSingleLine,
            ALocalConverter,
            VRectWithDelta,
            VMapRect,
            APreparedPointsAggreagtor,
            AFixedPointArray,
            APolygon
          );
        end else if Supports(ALine, IGeometryProjectedMultiPolygon, VProjectedMultiLine) then begin
          for VLineIndex := 0 to VProjectedMultiLine.Count - 1 do begin
            VProjectedSingleLine := VProjectedMultiLine.Item[VLineIndex];
            SinglePoly2GR32Polygon(
              VProjectedSingleLine,
              ALocalConverter,
              VRectWithDelta,
              VMapRect,
              APreparedPointsAggreagtor,
              AFixedPointArray,
              APolygon
            );
          end;
        end;
      end;
    end;
  end;
end;

function IsValidLonLatLine(
  const AGeometry: IGeometryLonLatLine
): Boolean;
var
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  Result := False;
  if not AGeometry.IsEmpty then begin
    if Supports(AGeometry, IGeometryLonLatSingleLine, VSingleLine) then begin
      Result := VSingleLine.Count > 1;
    end else if Supports(AGeometry, IGeometryLonLatSingleLine, VMultiLine) then begin
      Result := (VMultiLine.Count > 1) or ((VMultiLine.Count > 0) and (VMultiLine.Item[0].Count > 1));
    end;
  end;
end;

function IsValidLonLatPolygon(
  const AGeometry: IGeometryLonLatPolygon
): Boolean;
var
  VSingleLine: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiPolygon;
begin
  Result := False;
  if not AGeometry.IsEmpty then begin
    if Supports(AGeometry, IGeometryLonLatSinglePolygon, VSingleLine) then begin
      Result := VSingleLine.Count > 2;
    end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiLine) then begin
      Result := (VMultiLine.Count > 1) or ((VMultiLine.Count > 0) and (VMultiLine.Item[0].Count > 2));
    end;
  end;
end;


end.
