unit u_MarkerDrawableComplex;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  u_BaseInterfacedObject;

type
  TMarkerDrawableComplex = class(TBaseInterfacedObject, IMarkerDrawable)
  private
    FMarkerFirst: IMarkerDrawable;
    FMarkerSecond: IMarkerDrawable;
  private
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect;
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean;
  public
    constructor Create(
      const AMarkerFirst: IMarkerDrawable;
      const AMarkerSecond: IMarkerDrawable
    );
  end;

implementation

{ TMarkerDrawableComplex }

constructor TMarkerDrawableComplex.Create(
  const AMarkerFirst, AMarkerSecond: IMarkerDrawable
);
begin
  Assert(AMarkerFirst <> nil);
  Assert(AMarkerSecond <> nil);
  inherited Create;
  FMarkerFirst := AMarkerFirst;
  FMarkerSecond := AMarkerSecond;
end;

function TMarkerDrawableComplex.DrawToBitmap(ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint): Boolean;
begin
  Result := False;
  if FMarkerFirst.DrawToBitmap(ABitmap, APosition) then begin
    Result := True;
  end;
  if FMarkerSecond.DrawToBitmap(ABitmap, APosition) then begin
    Result := True;
  end;
end;

function TMarkerDrawableComplex.GetBoundsForPosition(
  const APosition: TDoublePoint): TRect;
begin
  UnionRect(
    Result,
    FMarkerFirst.GetBoundsForPosition(APosition),
    FMarkerSecond.GetBoundsForPosition(APosition)
  );
end;

end.
