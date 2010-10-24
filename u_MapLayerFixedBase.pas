unit u_MapLayerFixedBase;

interface

uses
  Types,
  t_GeoTypes,
  u_MapLayerBase;

type
  TMapLayerFixedBase = class(TMapLayerBase)
  protected
    function GetLayerVisibleSize: TPoint; virtual; abstract;
    function GetLonLat: TExtendedPoint; virtual; abstract;
    function GetVisibleShift: TPoint; virtual; abstract;
    procedure UpdatelLayerLocation; override;

  end;
implementation

{ TMapLayerFixedBase }

procedure TMapLayerFixedBase.UpdatelLayerLocation;
var
  VLonLat: TExtendedPoint;
  VVisiblePoint: TExtendedPoint;
begin
  inherited;
  VLonLat := GetLonLat;
  VVisiblePoint := FViewPortState.LonLat2VisiblePixel(VLonLat);
end;

end.
