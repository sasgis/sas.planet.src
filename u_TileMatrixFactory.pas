unit u_TileMatrixFactory;

interface

uses
  Types,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_TileMatrix;

type
  TTileMatrixFactory = class(TInterfacedObject, ITileMatrixFactory)
  private
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    function BuildEmpty(
      ATileRect: TRect;
      ANewConverter: ILocalCoordConverter
    ): ITileMatrix;
    function BuildSameProjection(
      ASource: ITileMatrix;
      ATileRect: TRect;
      ANewConverter: ILocalCoordConverter
    ): ITileMatrix;
    function BuildZoomIn(ASource: ITileMatrix; ANewConverter: ILocalCoordConverter): ITileMatrix;
    function BuildZoomOut(ASource: ITileMatrix; ANewConverter: ILocalCoordConverter): ITileMatrix;
  private
    function BuildNewMatrix(ASource: ITileMatrix; ANewConverter: ILocalCoordConverter): ITileMatrix;
  public
    constructor Create(ALocalConverterFactory: ILocalCoordConverterFactorySimpe);
  end;


implementation

uses
  SysUtils,
  i_CoordConverter,
  u_TileMatrix;

{ TTileMatrixFactory }

constructor TTileMatrixFactory.Create(
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe
);
begin
  FLocalConverterFactory := ALocalConverterFactory;
end;

function TTileMatrixFactory.BuildNewMatrix(
  ASource: ITileMatrix;
  ANewConverter: ILocalCoordConverter
): ITileMatrix;
var
  VLocalConverter: ILocalCoordConverter;
  VTileRect: TRect;
  VZoom: Byte;
  VConverter: ICoordConverter;
  VMapPixelRect: TRect;
begin
  VMapPixelRect := ANewConverter.GetRectInMapPixel;
  VZoom := ANewConverter.Zoom;
  VConverter := ANewConverter.GeoConverter;
  VTileRect := VConverter.PixelRect2TileRect(VMapPixelRect, VZoom);
  if EqualRect(VMapPixelRect, VConverter.TileRect2PixelRect(VTileRect, VZoom)) then begin
    VLocalConverter := ANewConverter;
  end else begin
    VLocalConverter := FLocalConverterFactory.CreateBySourceWithStableTileRect(ANewConverter);
    VMapPixelRect := VLocalConverter.GetRectInMapPixel;
    VTileRect := VConverter.PixelRect2TileRect(VMapPixelRect, VZoom);
  end;

  if ASource = nil then begin
    Result := BuildEmpty(VTileRect, VLocalConverter);
  end else if VLocalConverter.GetIsSameConverter(ASource.LocalConverter) then begin
    Result := ASource;
  end else if not VLocalConverter.GeoConverter.IsSameConverter(ASource.LocalConverter.GeoConverter) then begin
    Result := BuildEmpty(VTileRect, VLocalConverter);
  end else if VLocalConverter.ProjectionInfo.GetIsSameProjectionInfo(ASource.LocalConverter.ProjectionInfo) then begin
    Result := BuildSameProjection(ASource, VTileRect, VLocalConverter);
  end else begin
    if VLocalConverter.Zoom + 1 = ASource.LocalConverter.Zoom then begin
      Result := BuildZoomOut(ASource, VLocalConverter);
    end else if VLocalConverter.Zoom = ASource.LocalConverter.Zoom + 1 then begin
      Result := BuildZoomIn(ASource, VLocalConverter);
    end else begin
      Result := BuildEmpty(VTileRect, VLocalConverter);
    end;
  end;
end;

function TTileMatrixFactory.BuildEmpty(
  ATileRect: TRect;
  ANewConverter: ILocalCoordConverter
): ITileMatrix;
begin
  Result :=
    TTileMatrix.Create(
      FLocalConverterFactory,
      ANewConverter,
      ATileRect,
      []
    );
end;

function TTileMatrixFactory.BuildSameProjection(
  ASource: ITileMatrix;
  ATileRect: TRect;
  ANewConverter: ILocalCoordConverter
): ITileMatrix;
var
  VIntersectRect: TRect;
  VTile: TPoint;
  VTileCount: TPoint;
  VElements: array of ITileMatrixElement;
  i: Integer;
  VIndex: Integer;
  VX, VY: Integer;
begin
  if IntersectRect(VIntersectRect, ATileRect, ASource.TileRect) then begin
    VTileCount := Point(ATileRect.Right - ATileRect.Left, ATileRect.Bottom - ATileRect.Top);
    SetLength(VElements, VTileCount.X * VTileCount.Y);
    try
      for VX := VIntersectRect.Left to VIntersectRect.Right - 1 do begin
        VTile.X := VX;
        for VY := VIntersectRect.Top to VIntersectRect.Bottom - 1 do begin
          VTile.Y := VY;
          VIndex := (VTile.Y - ATileRect.Top) * VTileCount.X + (VTile.X - ATileRect.Left);
          VElements[VIndex] := ASource.GetElementByTile(VTile);
        end;
      end;

      Result :=
        TTileMatrix.Create(
          FLocalConverterFactory,
          ANewConverter,
          ATileRect,
          VElements
        );
    finally
      for i := 0 to Length(VElements) - 1 do begin
        VElements[i] := nil;
      end;
    end;
  end else begin
    Result := BuildEmpty(ATileRect, ANewConverter);
  end;
end;

function TTileMatrixFactory.BuildZoomIn(
  ASource: ITileMatrix;
  ANewConverter: ILocalCoordConverter
): ITileMatrix;
begin

end;

function TTileMatrixFactory.BuildZoomOut(
  ASource: ITileMatrix;
  ANewConverter: ILocalCoordConverter
): ITileMatrix;
begin

end;

end.
