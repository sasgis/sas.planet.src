unit u_MarkerDrawableSimpleCross;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  u_MarkerDrawableSimpleAbstract;

type
  TMarkerDrawableSimpleCross = class(TMarkerDrawableSimpleAbstract)
  protected
    procedure DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ); override;
  end;

implementation

uses
  GR32_Polygons;

{ TMarkerDrawableSimpleCross }

procedure TMarkerDrawableSimpleCross.DrawToBitmap(ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint);
var
  VCrossHalfWidth: Double;
  VHalfSize: Double;
  VPolygon: TPolygon32;
begin
  VCrossHalfWidth := Config.MarkerSize / 10;
  VHalfSize := Config.MarkerSize / 2;
  VPolygon := TPolygon32.Create;
  try
    VPolygon.Closed := True;
    VPolygon.Antialiased := true;
    VPolygon.AntialiasMode := am2times;
    VPolygon.Add(FixedPoint(APosition.X - VCrossHalfWidth, APosition.Y - VHalfSize));
    VPolygon.Add(FixedPoint(APosition.X + VCrossHalfWidth, APosition.Y - VHalfSize));
    VPolygon.Add(FixedPoint(APosition.X + VCrossHalfWidth, APosition.Y - VCrossHalfWidth));
    VPolygon.Add(FixedPoint(APosition.X + VHalfSize, APosition.Y - VCrossHalfWidth));
    VPolygon.Add(FixedPoint(APosition.X + VHalfSize, APosition.Y + VCrossHalfWidth));
    VPolygon.Add(FixedPoint(APosition.X + VCrossHalfWidth, APosition.Y + VCrossHalfWidth));
    VPolygon.Add(FixedPoint(APosition.X + VCrossHalfWidth, APosition.Y + VHalfSize));
    VPolygon.Add(FixedPoint(APosition.X - VCrossHalfWidth, APosition.Y + VHalfSize));
    VPolygon.Add(FixedPoint(APosition.X - VCrossHalfWidth, APosition.Y + VCrossHalfWidth));
    VPolygon.Add(FixedPoint(APosition.X - VHalfSize, APosition.Y + VCrossHalfWidth));
    VPolygon.Add(FixedPoint(APosition.X - VHalfSize, APosition.Y - VCrossHalfWidth));
    VPolygon.Add(FixedPoint(APosition.X - VCrossHalfWidth, APosition.Y - VCrossHalfWidth));

    VPolygon.DrawFill(ABitmap, Config.MarkerColor);
    VPolygon.DrawEdge(ABitmap, Config.BorderColor);
  finally
    VPolygon.Free;
  end;
end;

end.
