unit u_TileIteratorAbstract;

interface

uses
  Types,
  t_GeoTypes,
  i_ICoordConverter;

type
  TTileIteratorAbstract = class
  protected
    FPolygLL: TExtendedPointArray;
    FZoom: byte;
    FGeoConvert: ICoordConverter;
    FCurrent: TPoint;
    FTilesTotal: Int64;
  public
    constructor Create(AZoom: byte; APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter);
    destructor Destroy; override;
    function Next: Boolean; virtual; abstract;
    property Current: TPoint  read FCurrent;
    property TilesTotal: Int64 read FTilesTotal;
  end;

implementation

{ TTileIteratorAbstract }

constructor TTileIteratorAbstract.Create(AZoom: byte;
  APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter);
begin
  FZoom := AZoom;
  FPolygLL := Copy(APolygLL);
  FGeoConvert := AGeoConvert;
end;


destructor TTileIteratorAbstract.Destroy;
begin
  FPolygLL := nil;
  FGeoConvert := nil;
  inherited;
end;

end.
