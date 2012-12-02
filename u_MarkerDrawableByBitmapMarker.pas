unit u_MarkerDrawableByBitmapMarker;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  i_BitmapMarker,
  u_BaseInterfacedObject;

type
  TMarkerDrawableByBitmapMarker = class(TBaseInterfacedObject, IMarkerDrawable)
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
  u_BitmapFunc,
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
  Types.IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if Types.IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  BlockTransferFull(
    ABitmap,
    VTargetPoint.X, VTargetPoint.Y,
    FMarker,
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
  VSourceSize := FMarker.Size;
  VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);

  Result.TopLeft := VTargetPoint;
  Result.Right := Result.Left + VSourceSize.X;
  Result.Bottom := Result.Top + VSourceSize.Y;
end;

end.

