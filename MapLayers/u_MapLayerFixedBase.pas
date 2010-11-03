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
    function GetLayerTopLeftVisibleShift: TPoint; virtual; abstract;
    procedure DoUpdatelLayerLocation; override;

  end;
implementation

uses
  GR32,
  Ugeofun;

{ TMapLayerFixedBase }

procedure TMapLayerFixedBase.DoUpdatelLayerLocation;
var
  VLonLat: TExtendedPoint;
  VVisiblePoint: TExtendedPoint;
  VVisibleLayerRect: TExtendedRect;
  VVisibleLayerSize: TPoint;
  VVisibleLayerShift: TPoint;
  VViewPortVisualRect: TExtendedRect;
  VTempRect: TExtendedRect;
begin
  inherited;
  VLonLat := GetLonLat;
  FViewPortState.LockRead;
  try
    VVisiblePoint := FViewPortState.LonLat2VisiblePixel(VLonLat);
    VViewPortVisualRect := ExtendedRect(FViewPortState.GetViewRectInVisualPixel);
  finally
    FViewPortState.UnLockRead;
  end;
  VVisibleLayerSize := GetLayerVisibleSize;
  VVisibleLayerShift := GetLayerTopLeftVisibleShift;

  VVisibleLayerRect.Left := VVisiblePoint.X + VVisibleLayerShift.X;
  VVisibleLayerRect.Top := VVisiblePoint.Y + VVisibleLayerShift.Y;
  VVisibleLayerRect.Right := VVisibleLayerRect.Left + VVisibleLayerSize.X;
  VVisibleLayerRect.Bottom := VVisibleLayerRect.Top + VVisibleLayerSize.Y;

  if IntersectExtendedRect(VTempRect, VVisibleLayerRect, VViewPortVisualRect) then begin
    FLayerPositioned.Location := FloatRect(
      VVisibleLayerRect.Left,
      VVisibleLayerRect.Top,
      VVisibleLayerRect.Right,
      VVisibleLayerRect.Bottom
    );
  end else begin
    FLayerPositioned.Location := FloatRect(-1, -1, -1, -1);
  end;
end;

end.
