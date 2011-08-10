unit u_BitmapMarkerProviderSimpleArrow;

interface

uses
  Types,
  GR32,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig,
  u_BitmapMarkerProviderSimpleBase;

type
  TBitmapMarkerProviderSimpleArrow = class(TBitmapMarkerProviderSimpleBase)
  protected
    function CreateMarker: IBitmapMarker; override;
  end;

implementation

uses
  GR32_Polygons,
  t_GeoTypes,
  u_GeoFun,
  u_BitmapMarker;

{ TBitmapMarkerProviderSimpleArrow }

function TBitmapMarkerProviderSimpleArrow.CreateMarker: IBitmapMarker;
var
  VConfig: IBitmapMarkerProviderSimpleConfigStatic;
  VMarkerBitmap: TCustomBitmap32;
  VSize: TPoint;
  VPolygon: TPolygon32;
  VCenterPoint: TDoublePoint;
begin
  VMarkerBitmap := TCustomBitmap32.Create;
  try
    VConfig := Config.GetStatic;
    VSize := Point(VConfig.MarkerSize, VConfig.MarkerSize);

    VCenterPoint.X := VSize.X / 2;
    VCenterPoint.Y := VSize.Y / 2;

    VMarkerBitmap.SetSize(VSize.Y, VSize.Y);
    VMarkerBitmap.Clear(0);
    VPolygon := TPolygon32.Create;
    try
      VPolygon.Antialiased := true;
      VPolygon.AntialiasMode := am32times;
      VPolygon.Add(FixedPoint(VCenterPoint.X, 0));
      VPolygon.Add(FixedPoint(VCenterPoint.X - VConfig.MarkerSize / 3, VSize.Y - 1));
      VPolygon.Add(FixedPoint(VCenterPoint.X + VConfig.MarkerSize / 3, VSize.Y - 1));
      VPolygon.DrawFill(VMarkerBitmap, VConfig.MarkerColor);
      VPolygon.DrawEdge(VMarkerBitmap, VConfig.BorderColor);
    finally
      VPolygon.Free;
    end;
    Result :=
      TBitmapMarker.Create(
        VMarkerBitmap,
        DoublePoint(VCenterPoint.X, 0),
        True,
        0
      );
  finally
    VMarkerBitmap.Free;
  end;
end;

end.
