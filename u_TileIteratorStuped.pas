unit u_TileIteratorStuped;

interface

uses
  Types,
  t_GeoTypes,
  i_ICoordConverter,
  u_TileIteratorAbstract;

type
  TTileIteratorStuped = class(TTileIteratorAbstract)
  private
    p_x, p_y: Integer;
    polyg: TPointArray;
    VPixelRect: TRect;
  public
    constructor Create(AZoom: byte; APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter);
    destructor Destroy; override;
    function Next: Boolean; override;
  end;

implementation

uses
  Ugeofun;

{ TTileIteratorStuped }

constructor TTileIteratorStuped.Create(AZoom: byte;
  APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter);
begin
  inherited;
  polyg := FGeoConvert.LonLatArray2PixelArray(FPolygLL, FZoom);
  FTilesTotal := GetDwnlNum(VPixelRect, Polyg, true);
  p_x := VPixelRect.Left;
end;

destructor TTileIteratorStuped.Destroy;
begin
  polyg := nil;
  inherited;
end;

function TTileIteratorStuped.Next: Boolean;
begin
  Result := False;
  while p_x < VPixelRect.Right do begin
    FCurrent.X := p_x shr 8;
    p_y := VPixelRect.Top;
    while p_y < VPixelRect.Bottom do begin
      FCurrent.Y := p_y shr 8;
      if (RgnAndRgn(Polyg, p_x, p_y, false)) then begin
        Result := True;
        Break;
      end;
      inc(p_y, 256);
    end;
    if Result then begin
      Break;
    end;
    inc(p_x, 256);
  end;
end;

end.
