unit u_MarkerDrawableByBitmapMarker;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  i_BitmapMarker;

type
  TMarkerDrawableByBitmapMarker = class(TInterfacedObject, IMarkerDrawable)
  private
    FMarker: IBitmapMarker;
  private
    procedure DrawToBitmap(
      ABitmap: TCustomBitmap32;
      ACombineMode: TCombineMode;
      const APosition: TDoublePoint
    );
  public
    constructor Create(
      const AMarker: IBitmapMarker
    );
  end;

implementation

uses
  GR32_Resamplers,
  u_GeoFun;

{ TMarkerDrawableByBitmapMarker }

constructor TMarkerDrawableByBitmapMarker.Create(const AMarker: IBitmapMarker);
begin
  inherited Create;
  FMarker := AMarker;
end;

procedure TMarkerDrawableByBitmapMarker.DrawToBitmap(
  ABitmap: TCustomBitmap32;
  ACombineMode: TCombineMode;
  const APosition: TDoublePoint
);
var
  VTargetPoint: TPoint;
  VTargetPointFloat: TDoublePoint;
begin
  VTargetPointFloat :=
    DoublePoint(
      APosition.X - FMarker.AnchorPoint.X,
      APosition.Y - FMarker.AnchorPoint.Y
    );
  VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);
  if PtInRect(ABitmap.BoundsRect, VTargetPoint) then begin
    BlockTransfer(
      ABitmap,
      VTargetPoint.X, VTargetPoint.Y,
      ABitmap.ClipRect,
      FMarker.Bitmap,
      FMarker.Bitmap.BoundsRect,
      dmBlend,
      ACombineMode
    );
  end;
end;

end.
