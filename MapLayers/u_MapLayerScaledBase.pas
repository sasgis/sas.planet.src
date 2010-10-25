unit u_MapLayerScaledBase;

interface

uses
  Types,
  u_MapLayerBase;

type
  TMapLayerScaledBase = class(TMapLayerBase)
  protected
    function GetVisibleRectInMapPixels: TRect; virtual; abstract;
    procedure DoUpdatelLayerLocation; override;
  end;

implementation

uses
  GR32,
  t_GeoTypes,
  Ugeofun;

{ TMapLayerScaledBase }

procedure TMapLayerScaledBase.DoUpdatelLayerLocation;
var
  VMapRect: TRect;
  VVisibleRect: TExtendedRect;
begin
  inherited;
  VMapRect := GetVisibleRectInMapPixels;
  VVisibleRect := FViewPortState.MapRect2VisibleRect(ExtendedRect(VMapRect));
  FLayerPositioned.Location := FloatRect(
    VVisibleRect.Left,
    VVisibleRect.Top,
    VVisibleRect.Right,
    VVisibleRect.Bottom
  );
end;

end.
