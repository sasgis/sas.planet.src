unit u_TileIteratorAbstract;

interface

uses
  Types,
  t_GeoTypes,
  i_ICoordConverter;

type
  TTileIteratorAbstract = class
  private
    FPolygLL: TExtendedPointArray;
    FZoom: byte;
    FGeoConvert: ICoordConverter;
    FCurrent: TPoint;
  public
    constructor Create(AZoom: byte; APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter);
    destructor Destroy; override;
    function Next: Boolean; virtual; abstract;
    property Current: TPoint  read FCurrent;
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
