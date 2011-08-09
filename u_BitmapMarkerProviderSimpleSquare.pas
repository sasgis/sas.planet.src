unit u_BitmapMarkerProviderSimpleSquare;

interface

uses
  Types,
  GR32,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig,
  u_BitmapMarkerProviderSimpleBase;

type
  TBitmapMarkerProviderSimpleSquare = class(TBitmapMarkerProviderSimpleBase)
  protected
    function CreateMarker: IBitmapMarker; override;
  end;

implementation

uses
  GR32_Polygons,
  t_GeoTypes,
  u_GeoFun,
  u_BitmapMarker;

{ TBitmapMarkerProviderSimpleSquare }

function TBitmapMarkerProviderSimpleSquare.CreateMarker: IBitmapMarker;
var
  VConfig: IBitmapMarkerProviderSimpleConfigStatic;
  VMarkerBitmap: TCustomBitmap32;
  VSize: TPoint;
  VPolygon: TPolygon32;
  VCenterPoint: TDoublePoint;
  VPointHalfSize: Double;
  VMarkRect: TRect;
begin
  VMarkerBitmap := TCustomBitmap32.Create;
  try
    VConfig := Config.GetStatic;
    VSize := Point(VConfig.MarkerSize, VConfig.MarkerSize);

    VCenterPoint.X := VSize.X / 2;
    VCenterPoint.Y := VSize.Y / 2;

    VMarkerBitmap.SetSize(VSize.Y, VSize.Y);
    VMarkerBitmap.Clear(VConfig.MarkerColor);
    VPointHalfSize := VConfig.MarkerSize  / 2;

    VMarkRect := VMarkerBitmap.BoundsRect;
    VMarkerBitmap.FillRectS(VMarkRect, VConfig.BorderColor);

    Result :=
      TBitmapMarker.Create(
        VMarkerBitmap,
        VCenterPoint,
        False,
        0
      );
  finally
    VMarkerBitmap.Free;
  end;
end;

end.
