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

unit u_VectorTileMatrixBuilder;

interface

uses
  Types,
  i_TileRect,
  i_VectorItemSubset,
  i_VectorTileMatrix,
  i_VectorTileMatrixBuilder,
  i_InterfaceListSimple,
  i_HashFunction,
  i_VectorItemSubsetBuilder,
  u_BaseInterfacedObject;

type
  TVectorTileMatrixBuilder = class(TBaseInterfacedObject, IVectorTileMatrixBuilder)
  private
    FVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FHashFunction: IHashFunction;
    FTileRect: ITileRect;
    FItems: IInterfaceListSimple;
  private
    function GetTileRect: ITileRect;

    function GetTile(const ATile: TPoint): IVectorItemSubset;
    procedure SetTile(const ATile: TPoint; const AValue: IVectorItemSubset);

    procedure SetRectWithReset(const ATileRect: ITileRect);
    procedure SetRect(const ATileRect: ITileRect);

    function MakeStatic: IVectorTileMatrix;
  public
    constructor Create(
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  t_Hash,
  t_GeoTypes,
  i_TileIterator,
  i_CoordConverter,
  i_InterfaceListStatic,
  u_TileIteratorByRect,
  u_InterfaceListSimple,
  u_GeoFunc,
  u_VectorTileMatrix;

function IndexByPos(const ARect: TRect; const APos: TPoint): Integer; inline;
begin
  Result := APos.X - ARect.Left + (APos.Y - ARect.Top) * (ARect.Right - ARect.Left);
end;

{ TVectorTileMatrixBuilder }

constructor TVectorTileMatrixBuilder.Create(
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AHashFunction: IHashFunction
);
begin
  Assert(Assigned(AVectorSubsetBuilderFactory));
  Assert(Assigned(AHashFunction));
  inherited Create;
  FVectorSubsetBuilderFactory := AVectorSubsetBuilderFactory;
  FHashFunction := AHashFunction;
  FTileRect := nil;
  FItems := TInterfaceListSimple.Create;
end;

function TVectorTileMatrixBuilder.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

function TVectorTileMatrixBuilder.GetTile(
  const ATile: TPoint
): IVectorItemSubset;
var
  VIndex: Integer;
begin
  Result := nil;
  if Assigned(FTileRect) then begin
    if PtInRect(FTileRect.Rect, ATile) then begin
      VIndex := IndexByPos(FTileRect.Rect, ATile);
      Result := IVectorItemSubset(FItems.Items[VIndex]);
    end;
  end;
end;

function TVectorTileMatrixBuilder.MakeStatic: IVectorTileMatrix;
var
  VHash: THashValue;
  i: Integer;
  VStaticList: IInterfaceListStatic;
  VTile: IVectorItemSubset;
begin
  Result := nil;
  if not Assigned(FTileRect) then begin
    Exit;
  end;
  VStaticList := FItems.MakeStaticCopy;

  VHash := $5f9ef5b5f150c35a;

  for i := 0 to VStaticList.Count - 1 do begin
    VTile := IVectorItemSubset(VStaticList.Items[i]);
    if Assigned(VTile) then begin
      FHashFunction.UpdateHashByHash(VHash, VTile.Hash);
    end;
  end;

  Result :=
    TVectorTileMatrix.Create(
      VHash,
      FTileRect,
      VStaticList
    );
end;

procedure TVectorTileMatrixBuilder.SetTile(
  const ATile: TPoint;
  const AValue: IVectorItemSubset
);
var
  VIndex: Integer;
begin
  if Assigned(FTileRect) then begin
    if PtInRect(FTileRect.Rect, ATile) then begin
      VIndex := IndexByPos(FTileRect.Rect, ATile);
      FItems.Items[VIndex] := AValue;
    end;
  end;
end;

procedure TVectorTileMatrixBuilder.SetRectWithReset(
  const ATileRect: ITileRect
);
var
  VSize: Integer;
begin
  FItems.Clear;
  FTileRect := ATileRect;
  if Assigned(FTileRect) then begin
    VSize := (FTileRect.Right - FTileRect.Left) * (FTileRect.Bottom - FTileRect.Top);
    FItems.Count := VSize;
  end;
end;

procedure TVectorTileMatrixBuilder.SetRect(
  const ATileRect: ITileRect
);
var
  VZoom: Byte;
  VTileRect: TRect;
  VIntersectRect: TRect;
  VOldItems: IInterfaceListStatic;
  VOldRect: TRect;
  VIterator: ITileIterator;
  VTile: TPoint;
  VRelativeRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VZoomSource: Byte;
  VConverter: ICoordConverter;
  VConverterSource: ICoordConverter;
  VSourceAtTarget: TRect;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  Assert(Assigned(ATileRect));
  VZoom := ATileRect.Zoom;
  VTileRect := ATileRect.Rect;
  Assert(ATileRect.ProjectionInfo.GeoConverter.CheckTileRect(VTileRect, VZoom));
  if Assigned(FTileRect) then begin
    if not FTileRect.ProjectionInfo.GetIsSameProjectionInfo(ATileRect.ProjectionInfo) then begin
      VConverter := ATileRect.ProjectionInfo.GeoConverter;
      VConverterSource := FTileRect.ProjectionInfo.GeoConverter;
      if VConverter.IsSameConverter(VConverterSource) then begin
        VZoomSource := FTileRect.Zoom;
        VOldRect := FTileRect.Rect;
        VRelativeRect := VConverter.TileRect2RelativeRect(VOldRect, VZoomSource);
        VSourceAtTarget :=
          RectFromDoubleRect(
            VConverter.RelativeRect2TileRectFloat(VRelativeRect, VZoom),
            rrOutside
          );
        if not IntersectRect(VIntersectRect, ATileRect.Rect, VSourceAtTarget) then begin
          SetRectWithReset(ATileRect);
        end else begin
          VSubsetBuilder := FVectorSubsetBuilderFactory.Build;

          VIterator := TTileIteratorByRect.Create(VIntersectRect);
          while VIterator.Next(VTile) do begin
//            FItems.Items[IndexByPos(VTileRect, VTile)] := VTileProvider.GetTile(0, nil, VTile);
          end;
        end;
      end else begin
        VZoomSource := FTileRect.Zoom;
        VOldRect := FTileRect.Rect;
        VLonLatRect := VConverterSource.TileRect2LonLatRect(VOldRect, VZoomSource);
        VConverter.ValidateLonLatRect(VLonLatRect);
        VSourceAtTarget :=
          RectFromDoubleRect(
            VConverter.LonLatRect2TileRectFloat(VLonLatRect, VZoom),
            rrOutside
          );
        if not IntersectRect(VIntersectRect, ATileRect.Rect, VSourceAtTarget) then begin
          SetRectWithReset(ATileRect);
        end else begin
          VSubsetBuilder := FVectorSubsetBuilderFactory.Build;
        end;
      end;
    end else begin
      if not FTileRect.IsEqual(ATileRect) then begin
        if not IntersectRect(VIntersectRect, ATileRect.Rect, FTileRect.Rect) then begin
          SetRectWithReset(ATileRect);
        end else begin
          VOldItems := FItems.MakeStaticAndClear;
          VOldRect := FTileRect.Rect;
          SetRectWithReset(ATileRect);
          VIterator := TTileIteratorByRect.Create(VIntersectRect);
          while VIterator.Next(VTile) do begin
            FItems.Items[IndexByPos(VTileRect, VTile)] :=
              VOldItems.Items[IndexByPos(VOldRect, VTile)];
          end;
        end;
      end;
    end;
  end else begin
    SetRectWithReset(ATileRect);
  end;
end;

end.
