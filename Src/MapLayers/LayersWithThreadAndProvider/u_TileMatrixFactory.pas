{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_TileMatrixFactory;

interface

uses
  Types,
  GR32,
  i_TileRect,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_ImageResamplerFactoryChangeable,
  i_Bitmap32BufferFactory,
  i_TileMatrix,
  u_BaseInterfacedObject;

type
  TTileMatrixFactory = class(TBaseInterfacedObject, ITileMatrixFactory)
  private
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FBitmapFactory: IBitmap32StaticFactory;
    FImageResampler: IImageResamplerFactoryChangeable;
    function BuildEmpty(
      const ANewTileRect: ITileRect
    ): ITileMatrix;
    function BuildSameProjection(
      const ASource: ITileMatrix;
      const ANewTileRect: ITileRect
    ): ITileMatrix;
    function BuildZoomChange(
      const ASource: ITileMatrix;
      const ANewTileRect: ITileRect
    ): ITileMatrix;

    procedure PrepareCopyRects(
      const ASourceProjection: IProjectionInfo;
      const ASourceTile: TPoint;
      const ATargetProjection: IProjectionInfo;
      const ATargetTile: TPoint;
      out ASourceRect, ATargetRect: TRect
    );
    function PrepareElementFromSource(
      const ASource: ITileMatrix;
      const ATargetProjection: IProjectionInfo;
      const ATile: TPoint;
      var ABitmap: TCustomBitmap32;
      var AResampler: TCustomResampler
    ): ITileMatrixElement;
  private
    function BuildNewMatrix(
      const ASource: ITileMatrix;
      const ANewTileRect: ITileRect
    ): ITileMatrix;
  public
    constructor Create(
      const AImageResampler: IImageResamplerFactoryChangeable;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe
    );
  end;


implementation

uses
  t_GeoTypes,
  i_Bitmap32Static,
  i_CoordConverter,
  u_GeoFunc,
  u_BitmapFunc,
  u_TileMatrixElement,
  u_TileMatrix;

{ TTileMatrixFactory }

constructor TTileMatrixFactory.Create(
  const AImageResampler: IImageResamplerFactoryChangeable;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe
);
begin
  Assert(Assigned(AImageResampler));
  Assert(Assigned(ABitmapFactory));
  Assert(Assigned(ALocalConverterFactory));
  inherited Create;
  FImageResampler := AImageResampler;
  FLocalConverterFactory := ALocalConverterFactory;
  FBitmapFactory := ABitmapFactory;
end;

procedure TTileMatrixFactory.PrepareCopyRects(
  const ASourceProjection: IProjectionInfo;
  const ASourceTile: TPoint;
  const ATargetProjection: IProjectionInfo;
  const ATargetTile: TPoint;
  out ASourceRect, ATargetRect: TRect
);
var
  VConverter: ICoordConverter;
  VTargetMapPixelRect: TRect;
  VSourceMapPixelRect: TRect;
  VTargetAtSourceMapPixelRect: TRect;
  VSourceZoom: Byte;
  VTargetZoom: Byte;
  VResultSourceMapPixelRect: TRect;
  VResultTargetMapPixelRect: TRect;
  VRelativeRect: TDoubleRect;
begin
  VConverter := ASourceProjection.GeoConverter;
  Assert(VConverter.IsSameConverter(ATargetProjection.GeoConverter));
  VSourceZoom := ASourceProjection.Zoom;
  VTargetZoom := ATargetProjection.Zoom;
  VTargetMapPixelRect := VConverter.TilePos2PixelRect(ATargetTile, VTargetZoom);
  VRelativeRect := VConverter.TilePos2RelativeRect(ATargetTile, VTargetZoom);
  VTargetAtSourceMapPixelRect :=
    RectFromDoubleRect(
      VConverter.RelativeRect2PixelRectFloat(VRelativeRect, VSourceZoom),
      rrToTopLeft
    );
  VSourceMapPixelRect := VConverter.TilePos2PixelRect(ASourceTile, VSourceZoom);
  VResultSourceMapPixelRect.Left := VSourceMapPixelRect.Left;
  if VResultSourceMapPixelRect.Left < VTargetAtSourceMapPixelRect.Left then begin
    VResultSourceMapPixelRect.Left := VTargetAtSourceMapPixelRect.Left;
  end;

  VResultSourceMapPixelRect.Top := VSourceMapPixelRect.Top;
  if VResultSourceMapPixelRect.Top < VTargetAtSourceMapPixelRect.Top then begin
    VResultSourceMapPixelRect.Top := VTargetAtSourceMapPixelRect.Top;
  end;

  VResultSourceMapPixelRect.Right := VSourceMapPixelRect.Right;
  if VResultSourceMapPixelRect.Right > VTargetAtSourceMapPixelRect.Right then begin
    VResultSourceMapPixelRect.Right := VTargetAtSourceMapPixelRect.Right;
  end;

  VResultSourceMapPixelRect.Bottom := VSourceMapPixelRect.Bottom;
  if VResultSourceMapPixelRect.Bottom > VTargetAtSourceMapPixelRect.Bottom then begin
    VResultSourceMapPixelRect.Bottom := VTargetAtSourceMapPixelRect.Bottom;
  end;

  VRelativeRect := VConverter.PixelRect2RelativeRect(VResultSourceMapPixelRect, VSourceZoom);
  VResultTargetMapPixelRect :=
    RectFromDoubleRect(
      VConverter.RelativeRect2PixelRectFloat(VRelativeRect, VTargetZoom),
      rrToTopLeft
    );
  ASourceRect := VResultSourceMapPixelRect;
  ATargetRect := VResultTargetMapPixelRect;
  OffsetRect(ASourceRect, -VSourceMapPixelRect.Left, -VSourceMapPixelRect.Top);
  OffsetRect(ATargetRect, -VTargetMapPixelRect.Left, -VTargetMapPixelRect.Top);
end;

function TTileMatrixFactory.PrepareElementFromSource(
  const ASource: ITileMatrix;
  const ATargetProjection: IProjectionInfo;
  const ATile: TPoint;
  var ABitmap: TCustomBitmap32;
  var AResampler: TCustomResampler
): ITileMatrixElement;
var
  VSourceProjection: IProjectionInfo;
  VConverter: ICoordConverter;
  VRelativeRectTargetTile: TDoubleRect;
  VSourceZoom: Byte;
  VTileRectSource: TRect;
  VBitmapStatic: IBitmap32Static;
  VX, VY: Integer;
  VSourceTile: TPoint;
  VTargetTileCoordConverter: ILocalCoordConverter;
  VSourceElement: ITileMatrixElement;
  VSourceBitmap: IBitmap32Static;
  VTargetTileSize: TPoint;
  VSrcCopyRect: TRect;
  VDstCopyRect: TRect;
begin
  Result := nil;
  VSourceProjection := ASource.TileRect.ProjectionInfo;
  VSourceZoom := VSourceProjection.Zoom;
  VConverter := VSourceProjection.GeoConverter;
  VRelativeRectTargetTile := VConverter.TilePos2RelativeRect(ATile, ATargetProjection.Zoom);
  VTileRectSource := RectFromDoubleRect(VConverter.RelativeRect2TileRectFloat(VRelativeRectTargetTile, VSourceZoom), rrOutside);
  VBitmapStatic := nil;
  VTargetTileCoordConverter := nil;
  for VX := VTileRectSource.Left to VTileRectSource.Right - 1 do begin
    VSourceTile.X := VX;
    for VY := VTileRectSource.Top to VTileRectSource.Bottom - 1 do begin
      VSourceTile.Y := VY;
      VSourceElement := ASource.GetElementByTile(VSourceTile);
      if VSourceElement <> nil then begin
        VSourceBitmap := VSourceElement.GetBitmap;
        if VSourceBitmap <> nil then begin
          if VTargetTileCoordConverter = nil then begin
            if ABitmap = nil then begin
              ABitmap := TCustomBitmap32.Create;
            end;
            VTargetTileCoordConverter := FLocalConverterFactory.CreateForTile(ATile, ATargetProjection.Zoom, VConverter);
            VTargetTileSize := VTargetTileCoordConverter.GetLocalRectSize;
            ABitmap.SetSize(VTargetTileSize.X, VTargetTileSize.Y);
            ABitmap.Clear(0);
          end;
          PrepareCopyRects(
            VSourceProjection,
            VSourceTile,
            ATargetProjection,
            ATile,
            VSrcCopyRect,
            VDstCopyRect
          );
          if AResampler = nil then begin
            AResampler := FImageResampler.GetStatic.CreateResampler;
          end;
          Assert(AResampler <> nil);

          StretchTransfer(
            ABitmap,
            VDstCopyRect,
            VSourceBitmap,
            VSrcCopyRect,
            AResampler,
            dmOpaque
          );
        end;
      end;
    end;
  end;
  if VTargetTileCoordConverter <> nil then begin
    VBitmapStatic :=
      FBitmapFactory.Build(
        VTargetTileCoordConverter.GetLocalRectSize,
        ABitmap.Bits
      );
  end;
  if VBitmapStatic <> nil then begin
    Result := TTileMatrixElement.Create(ATile, VTargetTileCoordConverter, VBitmapStatic);
  end;
end;

function TTileMatrixFactory.BuildNewMatrix(
  const ASource: ITileMatrix;
  const ANewTileRect: ITileRect
): ITileMatrix;
begin
  Result := nil;
  if not Assigned(ANewTileRect) then begin
    Exit;
  end;

  if ASource = nil then begin
    Result := BuildEmpty(ANewTileRect);
  end else if ANewTileRect.IsEqual(ASource.TileRect) then begin
    Result := ASource;
  end else if not ANewTileRect.ProjectionInfo.GeoConverter.IsSameConverter(ASource.TileRect.ProjectionInfo.GeoConverter) then begin
    Result := BuildEmpty(ANewTileRect);
  end else if ANewTileRect.Zoom = ASource.TileRect.Zoom then begin
    Result := BuildSameProjection(ASource, ANewTileRect);
  end else if ANewTileRect.Zoom + 1 = ASource.TileRect.Zoom then begin
    Result := BuildZoomChange(ASource, ANewTileRect);
  end else if ANewTileRect.Zoom = ASource.TileRect.Zoom + 1 then begin
    Result := BuildZoomChange(ASource, ANewTileRect);
  end else begin
    Result := BuildEmpty(ANewTileRect);
  end;
end;

function TTileMatrixFactory.BuildEmpty(
  const ANewTileRect: ITileRect
): ITileMatrix;
begin
  Result :=
    TTileMatrix.Create(
      FLocalConverterFactory,
      ANewTileRect,
      []
    );
end;

function TTileMatrixFactory.BuildSameProjection(
  const ASource: ITileMatrix;
  const ANewTileRect: ITileRect
): ITileMatrix;
var
  VIntersectRect: TRect;
  VTile: TPoint;
  VTileCount: TPoint;
  VElements: array of ITileMatrixElement;
  i: Integer;
  VIndex: Integer;
  VX, VY: Integer;
  VRect: TRect;
begin
  if not Types.IntersectRect(VIntersectRect, ANewTileRect.Rect, ASource.TileRect.Rect) then begin
    Result := BuildEmpty(ANewTileRect);
  end else begin
    VRect := ANewTileRect.Rect;
    VTileCount := Types.Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
    SetLength(VElements, VTileCount.X * VTileCount.Y);
    try
      for VX := VIntersectRect.Left to VIntersectRect.Right - 1 do begin
        VTile.X := VX;
        for VY := VIntersectRect.Top to VIntersectRect.Bottom - 1 do begin
          VTile.Y := VY;
          VIndex := (VTile.Y - VRect.Top) * VTileCount.X + (VTile.X - VRect.Left);
          VElements[VIndex] := ASource.GetElementByTile(VTile);
        end;
      end;

      Result :=
        TTileMatrix.Create(
          FLocalConverterFactory,
          ANewTileRect,
          VElements
        );
    finally
      for i := 0 to Length(VElements) - 1 do begin
        VElements[i] := nil;
      end;
    end;
  end;
end;

function TTileMatrixFactory.BuildZoomChange(
  const ASource: ITileMatrix;
  const ANewTileRect: ITileRect
): ITileMatrix;
var
  VConverter: ICoordConverter;
  VZoom: Byte;
  VZoomSource: Byte;
  VRelativeRectSource: TDoubleRect;
  VTileRectSourceAtTarget: TRect;
  VIntersectRect: TRect;
  VTile: TPoint;
  VTileCount: TPoint;
  VElements: array of ITileMatrixElement;
  i: Integer;
  VIndex: Integer;
  VX, VY: Integer;
  VResampler: TCustomResampler;
  VBitmap: TCustomBitmap32;
  VNewTileRect: TRect;
  VTargetProjection: IProjectionInfo;
begin
  Assert(Assigned(ASource));
  VTargetProjection := ANewTileRect.ProjectionInfo;
  VConverter := VTargetProjection.GeoConverter;
  Assert(VConverter.IsSameConverter(ASource.TileRect.ProjectionInfo.GeoConverter));
  VZoom := ANewTileRect.Zoom;
  VZoomSource := ASource.TileRect.Zoom;
  VRelativeRectSource := VConverter.TileRect2RelativeRect(ASource.TileRect.Rect, VZoomSource);
  VTileRectSourceAtTarget :=
    RectFromDoubleRect(
      VConverter.RelativeRect2TileRectFloat(VRelativeRectSource, VZoom),
      rrToTopLeft
    );
  VNewTileRect := ANewTileRect.Rect;
  if not Types.IntersectRect(VIntersectRect, VNewTileRect, VTileRectSourceAtTarget) then begin
    Result := BuildEmpty(ANewTileRect);
  end else begin
    VTileCount := Types.Point(VNewTileRect.Right - VNewTileRect.Left, VNewTileRect.Bottom - VNewTileRect.Top);
    SetLength(VElements, VTileCount.X * VTileCount.Y);
    try
      VResampler := nil;
      VBitmap := nil;
      try
        for VX := VIntersectRect.Left to VIntersectRect.Right - 1 do begin
          VTile.X := VX;
          for VY := VIntersectRect.Top to VIntersectRect.Bottom - 1 do begin
            VTile.Y := VY;
            VIndex := (VTile.Y - VNewTileRect.Top) * VTileCount.X + (VTile.X - VNewTileRect.Left);

            VElements[VIndex] := PrepareElementFromSource(ASource, VTargetProjection, VTile, VBitmap, VResampler);
          end;
        end;
      finally
        VResampler.Free;
        VBitmap.Free;
      end;

      Result :=
        TTileMatrix.Create(
          FLocalConverterFactory,
          ANewTileRect,
          VElements
        );
    finally
      for i := 0 to Length(VElements) - 1 do begin
        VElements[i] := nil;
      end;
    end;
  end;
end;

end.
