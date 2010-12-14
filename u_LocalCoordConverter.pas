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
    FZoom: Byte;
    FGeoConverter: ICoordConverter;
    FMapScale: TDoublePoint;
    FLocalTopLeftAtMap: TDoublePoint;
  protected
    function GetZoom: Byte;
    function GetGeoConverter: ICoordConverter;
    function LocalPixel2MapPixel(const APoint: TPoint): TPoint;
    function LocalPixel2MapPixelFloat(const APoint: TPoint): TDoublePoint;
    function LocalPixelFloat2MapPixelFloat(const APoint: TDoublePoint): TDoublePoint;
    function MapPixel2LocalPixel(const APoint: TPoint): TPoint;
    function MapPixel2LocalPixelFloat(const APoint: TPoint): TDoublePoint;
    function MapPixelFloat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint;

    function LocalRect2MapRect(const ARect: TRect): TRect;
    function LocalRect2MapRectFloat(const ARect: TRect): TDoubleRect;
    function LocalRectFloat2MapRectFloat(const ARect: TDoubleRect): TDoubleRect;
    function MapRect2LocalRect(const ARect: TRect): TRect;
    function MapRect2LocalRectFloat(const ARect: TRect): TDoubleRect;
    function MapRectFloat2LocalRectFloat(const ARect: TDoubleRect): TDoubleRect;

    function LonLat2LocalPixel(const APoint: TDoublePoint): TPoint;
    function LonLat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint;
    function LonLatRect2LocalRectFloat(const ARect: TDoubleRect): TDoubleRect;
  public
    constructor Create(
      AZoom: Byte;
      AGeoConverter: ICoordConverter;
      AMapScale: TDoublePoint;
      ALocalTopLeftAtMap: TDoublePoint
    );
    destructor Destroy; override;
  end;

implementation

{ TLocalCoordConverter }

constructor TLocalCoordConverter.Create(
  AZoom: Byte;
  AGeoConverter: ICoordConverter;
  AMapScale, ALocalTopLeftAtMap: TDoublePoint);
begin
  FZoom := AZoom;
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

function TLocalCoordConverter.GetZoom: Byte;
begin
  Result := FZoom;
end;

function TLocalCoordConverter.LocalPixel2MapPixel(const APoint: TPoint): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := LocalPixel2MapPixelFloat(APoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TLocalCoordConverter.LocalPixel2MapPixelFloat(
  const APoint: TPoint): TDoublePoint;
var
  VSourcePoint: TDoublePoint;
begin
  VSourcePoint.X := APoint.X;
  VSourcePoint.Y := APoint.Y;
  Result := LocalPixelFloat2MapPixelFloat(VSourcePoint);
end;

function TLocalCoordConverter.LocalPixelFloat2MapPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.X  / FMapScale.X + FLocalTopLeftAtMap.X;
  Result.Y := APoint.Y  / FMapScale.Y + FLocalTopLeftAtMap.Y;
end;

function TLocalCoordConverter.LocalRect2MapRect(const ARect: TRect): TRect;
begin
  Result.TopLeft := LocalPixel2MapPixel(ARect.TopLeft);
  Result.BottomRight := LocalPixel2MapPixel(ARect.BottomRight);
end;

function TLocalCoordConverter.LocalRect2MapRectFloat(
  const ARect: TRect): TDoubleRect;
begin
  Result.TopLeft := LocalPixel2MapPixelFloat(ARect.TopLeft);
  Result.BottomRight := LocalPixel2MapPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverter.LocalRectFloat2MapRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := LocalPixelFloat2MapPixelFloat(ARect.TopLeft);
  Result.BottomRight := LocalPixelFloat2MapPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverter.LonLat2LocalPixel(
  const APoint: TDoublePoint): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := LonLat2LocalPixelFloat(APoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TLocalCoordConverter.LonLat2LocalPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result :=
    MapPixelFloat2LocalPixelFloat(
      FGeoConverter.LonLat2PixelPosFloat(APoint, FZoom)
    );
end;

function TLocalCoordConverter.LonLatRect2LocalRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result :=
    MapRectFloat2LocalRectFloat(
      FGeoConverter.LonLatRect2PixelRectFloat(ARect, FZoom)
    );
end;

function TLocalCoordConverter.MapPixel2LocalPixel(const APoint: TPoint): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := MapPixel2LocalPixelFloat(APoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TLocalCoordConverter.MapPixel2LocalPixelFloat(
  const APoint: TPoint): TDoublePoint;
var
  VSourcePoint: TDoublePoint;
begin
  VSourcePoint.X := APoint.X;
  VSourcePoint.Y := APoint.Y;
  Result := MapPixelFloat2LocalPixelFloat(VSourcePoint);
end;

function TLocalCoordConverter.MapPixelFloat2LocalPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := (APoint.X - FLocalTopLeftAtMap.X) * FMapScale.X;
  Result.Y := (APoint.Y - FLocalTopLeftAtMap.Y) * FMapScale.Y;
end;

function TLocalCoordConverter.MapRect2LocalRect(const ARect: TRect): TRect;
begin
  Result.TopLeft := MapPixel2LocalPixel(ARect.TopLeft);
  Result.BottomRight := MapPixel2LocalPixel(ARect.BottomRight);
end;

function TLocalCoordConverter.MapRect2LocalRectFloat(
  const ARect: TRect): TDoubleRect;
begin
  Result.TopLeft := MapPixel2LocalPixelFloat(ARect.TopLeft);
  Result.BottomRight := MapPixel2LocalPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverter.MapRectFloat2LocalRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := MapPixelFloat2LocalPixelFloat(ARect.TopLeft);
  Result.BottomRight := MapPixelFloat2LocalPixelFloat(ARect.BottomRight);
end;

end.
