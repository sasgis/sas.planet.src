unit u_LocalCoordConverter;

interface

uses
  Types,
  t_GeoTypes,
  i_ICoordConverter,
  i_ILocalCoordConverter;

type
  TLocalCoordConverter = class(TInterfacedObject, ILocalCoordConverter)
  private
    FGeoConverter: ICoordConverter;
    FMapScale: TDoublePoint;
    FLocalTopLeftAtMap: TDoublePoint;
  protected
    function GetGeoConverter: ICoordConverter;
    function LocalPixel2MapPixel(APoint: TPoint): TPoint;
    function LocalPixelFloat2MapPixelFloat(APoint: TDoublePoint): TDoublePoint;
    function MapPixel2LocalPixel(APoint: TPoint): TPoint;
    function MapPixelFloat2LocalPixelFloat(APoint: TDoublePoint): TDoublePoint;
  public
    constructor Create(
      AGeoConverter: ICoordConverter;
      AMapScale: TDoublePoint;
      ALocalTopLeftAtMap: TDoublePoint
    );
    destructor Destroy; override;
  end;

implementation

{ TLocalCoordConverter }

constructor TLocalCoordConverter.Create(AGeoConverter: ICoordConverter;
  AMapScale, ALocalTopLeftAtMap: TDoublePoint);
begin
  FGeoConverter := AGeoConverter;
  FMapScale := AMapScale;
  FLocalTopLeftAtMap := ALocalTopLeftAtMap;
end;

destructor TLocalCoordConverter.Destroy;
begin
  FGeoConverter := nil;
  inherited;
end;

function TLocalCoordConverter.GetGeoConverter: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TLocalCoordConverter.LocalPixel2MapPixel(APoint: TPoint): TPoint;
var
  VSourcePoint: TDoublePoint;
  VResultPoint: TDoublePoint;
begin
  VSourcePoint.X := APoint.X;
  VSourcePoint.Y := APoint.Y;
  VResultPoint := LocalPixelFloat2MapPixelFloat(VSourcePoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TLocalCoordConverter.LocalPixelFloat2MapPixelFloat(
  APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.X  / FMapScale.X + FLocalTopLeftAtMap.X;
  Result.Y := APoint.Y  / FMapScale.Y + FLocalTopLeftAtMap.Y;
end;

function TLocalCoordConverter.MapPixel2LocalPixel(APoint: TPoint): TPoint;
var
  VSourcePoint: TDoublePoint;
  VResultPoint: TDoublePoint;
begin
  VSourcePoint.X := APoint.X;
  VSourcePoint.Y := APoint.Y;
  VResultPoint := MapPixelFloat2LocalPixelFloat(VSourcePoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TLocalCoordConverter.MapPixelFloat2LocalPixelFloat(
  APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := (APoint.X - FLocalTopLeftAtMap.X) * FMapScale.X;
  Result.Y := (APoint.Y - FLocalTopLeftAtMap.Y) * FMapScale.Y;
end;

end.
