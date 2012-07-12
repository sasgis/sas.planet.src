unit u_MarkerDrawableSimpleSquare;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  u_MarkerDrawableSimpleAbstract;

type
  TMarkerDrawableSimpleSquare = class(TMarkerDrawableSimpleAbstract)
  protected
    procedure DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ); override;
  end;

implementation

uses
  u_GeoFun;


{ TMarkerDrawableSimpleSquare }

procedure TMarkerDrawableSimpleSquare.DrawToBitmap(ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint);
var
  VHalfSize: Double;
  VDoubleRect: TDoubleRect;
  VRect: TRect;
begin
  VHalfSize := Config.MarkerSize / 2;
  VDoubleRect.Left := APosition.X - VHalfSize;
  VDoubleRect.Top := APosition.Y - VHalfSize;
  VDoubleRect.Right := APosition.X + VHalfSize;
  VDoubleRect.Bottom := APosition.Y + VHalfSize;
  VRect := RectFromDoubleRect(VDoubleRect, rrClosest);
  ABitmap.FillRectTS(VRect, Config.MarkerColor);
  ABitmap.FrameRectTS(VRect, Config.BorderColor);
end;

end.
