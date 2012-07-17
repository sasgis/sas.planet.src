unit u_MarkerDrawableByBitmap32Static;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_Bitmap32Static,
  i_MarkerDrawable;

type
  TMarkerDrawableByBitmap32Static = class(TInterfacedObject, IMarkerDrawable)
  private
    FBitmap: IBitmap32Static;
    FAnchorPoint: TDoublePoint;
  private
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean;
  public
    constructor Create(
      const ABitmap: IBitmap32Static;
      const AAnchorPoint: TDoublePoint
    );
  end;

implementation

uses
  GR32_Resamplers,
  u_GeoFun;

{ TMarkerDrawableByBitmap32Static }

constructor TMarkerDrawableByBitmap32Static.Create(
  const ABitmap: IBitmap32Static;
  const AAnchorPoint: TDoublePoint
);
begin
  inherited Create;
  FBitmap := ABitmap;
  FAnchorPoint := AAnchorPoint;
end;

function TMarkerDrawableByBitmap32Static.DrawToBitmap(
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint
): Boolean;
var
  VTargetPoint: TPoint;
  VTargetPointFloat: TDoublePoint;
  VTargetRect: TRect;
  VSourceSize: TPoint;
begin
  VTargetPointFloat :=
    DoublePoint(
      APosition.X - FAnchorPoint.X,
      APosition.Y - FAnchorPoint.Y
    );
  VSourceSize := Point(FBitmap.Bitmap.Width, FBitmap.Bitmap.Height);
  VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);
  VTargetRect.TopLeft := VTargetPoint;
  VTargetRect.Right := VTargetRect.Left + VSourceSize.X;
  VTargetRect.Bottom := VTargetRect.Top + VSourceSize.Y;
  IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  BlockTransfer(
    ABitmap,
    VTargetPoint.X, VTargetPoint.Y,
    ABitmap.ClipRect,
    FBitmap.Bitmap,
    FBitmap.Bitmap.BoundsRect,
    dmBlend,
    ABitmap.CombineMode
  );
  Result := True;
end;

end.

