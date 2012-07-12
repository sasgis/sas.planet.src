unit u_MarkerDrawableSimpleArrow;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  u_MarkerDrawableSimpleAbstract;

type
  TMarkerDrawableSimpleArrow = class(TMarkerDrawableWithDirectionSimpleAbstract)
  protected
    procedure DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ); override;
  end;

implementation

uses
  GR32_Polygons,
  GR32_Transforms;

{ TMarkerDrawableSimpleArrow }

procedure TMarkerDrawableSimpleArrow.DrawToBitmapWithDirection(
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint;
  const AAngle: Double
);
var
  VPolygon: TPolygon32;
  VTransform: TAffineTransformation;
  VHalfSize: Double;
  VWidth: Double;
begin
  VHalfSize := Config.MarkerSize / 2;
  VWidth := Config.MarkerSize / 3;
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
end;

end.
