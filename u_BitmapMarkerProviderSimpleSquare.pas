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
    function CreateMarker(ASize: Integer): IBitmapMarker; override;
  public
    constructor Create(
      AConfig: IBitmapMarkerProviderSimpleConfig
    );
  end;

implementation

uses
  t_GeoTypes,
  u_BitmapMarker;

{ TBitmapMarkerProviderSimpleSquare }

constructor TBitmapMarkerProviderSimpleSquare.Create(
  AConfig: IBitmapMarkerProviderSimpleConfig);
begin
  inherited Create(False, 0, AConfig);
end;

function TBitmapMarkerProviderSimpleSquare.CreateMarker(ASize: Integer): IBitmapMarker;
var
  VConfig: IBitmapMarkerProviderSimpleConfigStatic;
  VMarkerBitmap: TCustomBitmap32;
  VSize: TPoint;
  VCenterPoint: TDoublePoint;
  VMarkRect: TRect;
begin
  VMarkerBitmap := TCustomBitmap32.Create;
  try
    VConfig := Config.GetStatic;
    VSize := Point(ASize, ASize);

    VCenterPoint.X := VSize.X / 2;
    VCenterPoint.Y := VSize.Y / 2;

    VMarkerBitmap.SetSize(VSize.Y, VSize.Y);
    VMarkerBitmap.Clear(VConfig.MarkerColor);

    VMarkRect := VMarkerBitmap.BoundsRect;
    VMarkerBitmap.FrameRectTS(VMarkRect, VConfig.BorderColor);

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
