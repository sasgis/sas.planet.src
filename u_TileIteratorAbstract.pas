unit u_TileIteratorAbstract;

interface

uses
  Types,
  t_GeoTypes,
  i_ICoordConverter;

type
  TTileIteratorAbstract = class
  protected
    FTilesTotal: Int64;
    FTilesRect: TRect;
  public
    function Next(out ATile: TPoint): Boolean; virtual; abstract;
    property TilesTotal: Int64 read FTilesTotal;
    property TilesRect: TRect read FTilesRect;
  end;

  TTileIteratorByPolygonAbstract = class(TTileIteratorAbstract)
  protected
    FPolygLL: TDoublePointArray;
    FZoom: byte;
    FGeoConvert: ICoordConverter;
    FCurrent: TPoint;
  public
    constructor Create(AZoom: byte; APolygLL: TDoublePointArray; AGeoConvert: ICoordConverter); overload; virtual;
    constructor Create(AZoom: byte; ARectLL: TDoubleRect; AGeoConvert: ICoordConverter); overload; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  Ugeofun;

{ TTileIteratorByPolygonAbstract }

constructor TTileIteratorByPolygonAbstract.Create(AZoom: byte;
  APolygLL: TDoublePointArray; AGeoConvert: ICoordConverter);
begin
  FZoom := AZoom;
  FPolygLL := Copy(APolygLL);
  FGeoConvert := AGeoConvert;
end;

constructor TTileIteratorByPolygonAbstract.Create(AZoom: byte;
  ARectLL: TDoubleRect; AGeoConvert: ICoordConverter);
begin
  Create(AZoom, PolygonFromRect(ARectLL), AGeoConvert);
end;

destructor TTileIteratorByPolygonAbstract.Destroy;
begin
  FPolygLL := nil;
  FGeoConvert := nil;
  inherited;
end;

end.


