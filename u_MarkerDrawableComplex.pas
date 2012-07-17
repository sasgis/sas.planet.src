unit u_MarkerDrawableComplex;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable;

type
  TMarkerDrawableComplex = class(TInterfacedObject, IMarkerDrawable)
  private
    FMarkerFirst: IMarkerDrawable;
    FMarkerSecond: IMarkerDrawable;
  private
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean;
  public
    constructor Create(
      AMarkerFirst: IMarkerDrawable;
      AMarkerSecond: IMarkerDrawable
    );
  end;

implementation

{ TMarkerDrawableComplex }

constructor TMarkerDrawableComplex.Create(
  AMarkerFirst, AMarkerSecond: IMarkerDrawable
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

end.
