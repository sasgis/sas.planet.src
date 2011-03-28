unit u_TileIteratorAbstract;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_TileIterator;

type
  TTileIteratorAbstract = class(TInterfacedObject, ITileIterator)
  protected
    function GetTilesTotal: Int64; virtual; abstract;
    function GetTilesRect: TRect; virtual; abstract;
  public
    function Next(out ATile: TPoint): Boolean; virtual; abstract;
    procedure Reset; virtual; abstract;
    property TilesTotal: Int64 read GetTilesTotal;
    property TilesRect: TRect read GetTilesRect;
  end;

  TTileIteratorByPolygonAbstract = class(TTileIteratorAbstract)
  protected
    FPolygLL: TArrayOfDoublePoint;
    FZoom: byte;
    FGeoConvert: ICoordConverter;
    FCurrent: TPoint;
  public
    constructor Create(AZoom: byte; APolygLL: TArrayOfDoublePoint; AGeoConvert: ICoordConverter); virtual;
    destructor Destroy; override;
  end;

implementation



{ TTileIteratorByPolygonAbstract }

constructor TTileIteratorByPolygonAbstract.Create(AZoom: byte;
  APolygLL: TArrayOfDoublePoint; AGeoConvert: ICoordConverter);
begin
  FZoom := AZoom;
  FPolygLL := Copy(APolygLL);
  FGeoConvert := AGeoConvert;
end;

destructor TTileIteratorByPolygonAbstract.Destroy;
begin
  FPolygLL := nil;
  FGeoConvert := nil;
  inherited;
end;

end.


