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
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect;
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean;
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

function TMarkerDrawableByBitmapMarker.DrawToBitmap(
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint
): Boolean;
var
  VTargetPoint: TPoint;
  VTargetRect: TRect;
begin
  VTargetRect := GetBoundsForPosition(APosition);
  VTargetPoint := VTargetRect.TopLeft;
  IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  BlockTransfer(
    ABitmap,
    VTargetPoint.X, VTargetPoint.Y,
    ABitmap.ClipRect,
    FMarker.Bitmap,
    FMarker.Bitmap.BoundsRect,
    dmBlend,
    ABitmap.CombineMode
  );
  Result := True;
end;

function TMarkerDrawableByBitmapMarker.GetBoundsForPosition(
  const APosition: TDoublePoint): TRect;
var
  VTargetPoint: TPoint;
  VTargetPointFloat: TDoublePoint;
  VSourceSize: TPoint;
begin
  VTargetPointFloat :=
    DoublePoint(
      APosition.X - FMarker.AnchorPoint.X,
      APosition.Y - FMarker.AnchorPoint.Y
    );
  VSourceSize := Point(FMarker.Bitmap.Width, FMarker.Bitmap.Height);
  VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);

  Result.TopLeft := VTargetPoint;
  Result.Right := Result.Left + VSourceSize.X;
  Result.Bottom := Result.Top + VSourceSize.Y;
end;

end.

