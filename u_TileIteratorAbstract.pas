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
    constructor Create(AZoom: byte; APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter); overload;
    constructor Create(AZoom: byte; ARectLL: TExtendedRect; AGeoConvert: ICoordConverter); overload;
    destructor Destroy; override;
    function Next: Boolean; virtual; abstract;
    property Current: TPoint read FCurrent;
    property TilesTotal: Int64 read FTilesTotal;
    property TilesRect: TRect read FTilesRect;
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

constructor TTileIteratorAbstract.Create(AZoom: byte;
  ARectLL: TExtendedRect; AGeoConvert: ICoordConverter);
begin
  FZoom := AZoom;

  SetLength(FPolygLL,5);
  FPolygLL[0]:=ARectLL.TopLeft;
  FPolygLL[1].Y:=ARectLL.Top;
  FPolygLL[1].X:=ARectLL.right;
  FPolygLL[2]:=ARectLL.BottomRight;
  FPolygLL[3].X:=ARectLL.Left;
  FPolygLL[3].Y:=ARectLL.Bottom;
  FPolygLL[4]:=ARectLL.TopLeft;

  FGeoConvert := AGeoConvert;
end;

destructor TTileIteratorAbstract.Destroy;
begin
  FPolygLL := nil;
  FGeoConvert := nil;
  inherited;
end;

end.
