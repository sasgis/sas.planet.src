unit u_MarkerDrawableByBitmap32Static;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_Bitmap32Static,
  i_MarkerDrawable,
  u_BaseInterfacedObject;

type
  TMarkerDrawableByBitmap32Static = class(TBaseInterfacedObject, IMarkerDrawable)
  private
    FBitmap: IBitmap32Static;
    FAnchorPoint: TDoublePoint;
  private
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect;
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
  u_BitmapFunc,
  u_GeoFunc;

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
  VTargetRect: TRect;
begin
  VTargetRect := GetBoundsForPosition(APosition);
  VTargetPoint := VTargetRect.TopLeft;

  Types.IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if Types.IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  BlockTransferFull(
    ABitmap,
    VTargetPoint.X, VTargetPoint.Y,
    FBitmap,
    dmBlend,
    ABitmap.CombineMode
  );
  Result := True;
end;

function TMarkerDrawableByBitmap32Static.GetBoundsForPosition(
  const APosition: TDoublePoint): TRect;
var
  VTargetPoint: TPoint;
  VTargetPointFloat: TDoublePoint;
  VSourceSize: TPoint;
begin
  VTargetPointFloat :=
    DoublePoint(
      APosition.X - FAnchorPoint.X,
      APosition.Y - FAnchorPoint.Y
    );
  VSourceSize := FBitmap.Size;
  VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);

  Result.TopLeft := VTargetPoint;
  Result.Right := Result.Left + VSourceSize.X;
  Result.Bottom := Result.Top + VSourceSize.Y;
end;

end.

