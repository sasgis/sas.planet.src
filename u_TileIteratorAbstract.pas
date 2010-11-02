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
    FTilesRect: TRect;
  public
    constructor Create(AZoom: byte; APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter); overload; virtual;
    constructor Create(AZoom: byte; ARectLL: TExtendedRect; AGeoConvert: ICoordConverter); overload; virtual;
    destructor Destroy; override;
    function Next: Boolean; virtual; abstract;
    property Current: TPoint read FCurrent;
    property TilesTotal: Int64 read FTilesTotal;
    property TilesRect: TRect read FTilesRect;
  end;

implementation

uses
  Ugeofun;

{ TTileIteratorAbstract }

constructor TTileIteratorAbstract.Create(AZoom: byte;
  APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter);
begin
  FZoom := AZoom;
  FPolygLL := Copy(APolygLL);
  FGeoConvert := AGeoConvert;
end;

constructor TTileIteratorAbstract.Create(AZoom: byte;
  ARectLL: TExtendedRect; AGeoConvert: ICoordConverter);
begin
  Create(AZoom, PolygonFromRect(ARectLL), AGeoConvert);
end;

destructor TTileIteratorAbstract.Destroy;
begin
  FPolygLL := nil;
  FGeoConvert := nil;
  inherited;
end;

end.
