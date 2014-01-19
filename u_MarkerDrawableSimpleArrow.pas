unit u_MarkerDrawableSimpleArrow;

interface

uses
  GR32,
  t_GeoTypes,
  u_MarkerDrawableSimpleAbstract;

type
  TMarkerDrawableSimpleArrow = class(TMarkerDrawableWithDirectionSimpleAbstract)
  protected
    function DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ): Boolean; override;
  end;

implementation

uses
  Types,
  GR32_Polygons,
  GR32_Transforms,
  u_GeoFunc;

{ TMarkerDrawableSimpleArrow }

function TMarkerDrawableSimpleArrow.DrawToBitmapWithDirection(
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint;
  const AAngle: Double
): Boolean;
var
  VPolygon: TPolygon32;
  VTransform: TAffineTransformation;
  VHalfSize: Double;
  VWidth: Double;
  VTargetRect: TRect;
  VTargetDoubleRect: TDoubleRect;
begin
  VHalfSize := Config.MarkerSize / 2;
  VWidth := Config.MarkerSize / 3;

  VTargetDoubleRect.Left := APosition.X - VHalfSize;
  VTargetDoubleRect.Top := APosition.Y - VHalfSize;
  VTargetDoubleRect.Right := APosition.X + VHalfSize;
  VTargetDoubleRect.Bottom := APosition.Y + VHalfSize;

  VTargetRect := RectFromDoubleRect(VTargetDoubleRect, rrOutside);
  Types.IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if Types.IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  if not ABitmap.MeasuringMode then begin
    ABitmap.BeginUpdate;
    try
      VTransform := TAffineTransformation.Create;
      try
        VTransform.Rotate(APosition.X, APosition.Y, -AAngle);
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Closed := True;
          VPolygon.Antialiased := true;
          VPolygon.AntialiasMode := am2times;
          VPolygon.Add(VTransform.Transform(FixedPoint(APosition.X, APosition.Y - VHalfSize)));
          VPolygon.Add(VTransform.Transform(FixedPoint(APosition.X - VWidth, APosition.Y + VHalfSize)));
          VPolygon.Add(VTransform.Transform(FixedPoint(APosition.X + VWidth, APosition.Y + VHalfSize)));

          VPolygon.DrawFill(ABitmap, Config.MarkerColor);
          VPolygon.DrawEdge(ABitmap, Config.BorderColor);
        finally
          VPolygon.Free;
        end;
      finally
        VTransform.Free;
      end;
    finally
      ABitmap.EndUpdate;
    end;
  end;
  ABitmap.Changed(VTargetRect);
  Result := True;
end;

end.
