unit u_TileIteratorStuped;

interface

uses
  Types,
  t_GeoTypes,
  i_ICoordConverter,
  u_TileIteratorAbstract;

type
  TTileIteratorStuped = class(TTileIteratorByPolygonAbstract)
  private
    p_x, p_y: Integer;
    FPolyg: TPointArray;
    FPixelRect: TRect;
  public
    constructor Create(AZoom: byte; APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter); override;
    destructor Destroy; override;
    function Next(out ATile: TPoint): Boolean; override;
  end;

implementation

uses
  Ugeofun;

{ TTileIteratorStuped }

constructor TTileIteratorStuped.Create(AZoom: byte;
  APolygLL: TExtendedPointArray; AGeoConvert: ICoordConverter);
begin
  inherited;
  FPolyg := FGeoConvert.LonLatArray2PixelArray(FPolygLL, FZoom);
  FTilesTotal := GetDwnlNum(FPixelRect, FPolyg, true);
  FTilesRect := FGeoConvert.PixelRect2TileRect(FPixelRect, FZoom);
  p_x := FPixelRect.Left;
  p_y := FPixelRect.Top;
end;

destructor TTileIteratorStuped.Destroy;
begin
  FPolyg := nil;
  inherited;
end;

function TTileIteratorStuped.Next(out ATile: TPoint): Boolean;
begin
  Result := False;
  while p_x < FPixelRect.Right do begin
    FCurrent.X := p_x shr 8;
    while p_y < FPixelRect.Bottom do begin
      FCurrent.Y := p_y shr 8;
      if (RgnAndRgn(FPolyg, p_x, p_y, false)) then begin
        Result := True;
      end;
      inc(p_y, 256);
      if Result then begin
        Break;
      end;
    end;
    if Result then begin
      Break;
    end;
    if p_y >= FPixelRect.Bottom then begin
      p_y := FPixelRect.Top;
      inc(p_x, 256);
    end;
  end;
  ATile := FCurrent;
end;

end.
