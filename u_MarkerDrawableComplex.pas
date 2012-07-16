unit u_MarkerDrawableComplex;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_MarkerDrawable;

type
  TMarkerDrawableComplex = class(TInterfacedObject, IMarkerDrawable)
  private
    FMarkerFirst: IMarkerDrawable;
    FMarkerSecond: IMarkerDrawable;
  private
    procedure DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    );
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

procedure TMarkerDrawableComplex.DrawToBitmap(ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint);
begin
  FMarkerFirst.DrawToBitmap(ABitmap, APosition);
  FMarkerSecond.DrawToBitmap(ABitmap, APosition);
end;

end.
