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

unit u_BitmapTileMatrixBuilder;

interface

uses
  Types,
  i_TileRect,
  i_Bitmap32Static,
  i_BitmapTileMatrix,
  i_BitmapTileMatrixBuilder,
  i_InterfaceListSimple,
  i_ImageResamplerFactoryChangeable,
  i_Bitmap32BufferFactory,
  i_HashFunction,
  u_BaseInterfacedObject;

type
  TBitmapTileMatrixBuilder = class(TBaseInterfacedObject, IBitmapTileMatrixBuilder)
  private
    FHashFunction: IHashFunction;
    FImageResampler: IImageResamplerFactoryChangeable;
    FBitmapFactory: IBitmap32StaticFactory;
    FTileRect: ITileRect;
    FItems: IInterfaceListSimple;
  private
    function GetTileRect: ITileRect;

    function GetTile(const ATile: TPoint): IBitmap32Static;
    procedure SetTile(const ATile: TPoint; const AValue: IBitmap32Static);

    procedure SetRectWithReset(const ATileRect: ITileRect);
    procedure SetRect(const ATileRect: ITileRect);

    function MakeStatic: IBitmapTileMatrix;
  public
    constructor Create(
      const AImageResampler: IImageResamplerFactoryChangeable;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  Math,
  t_Hash,
  t_GeoTypes,
  i_BitmapTileProvider,
  i_CoordConverter,
  i_ProjectionInfo,
  i_InterfaceListStatic,
  u_TileIteratorByRect,
  u_InterfaceListSimple,
  u_GeoFunc,
  u_BitmapTileProviderByMatrix,
  u_BitmapTileProviderByOtherProjection,
  u_BitmapTileMatrix;

function IndexByPos(const ARect: TRect; const APos: TPoint): Integer; inline;
begin
  Result := APos.X - ARect.Left + (APos.Y - ARect.Top) * (ARect.Right - ARect.Left);
end;

{ TBitmapTileMatrixBuilder }

constructor TBitmapTileMatrixBuilder.Create(
  const AImageResampler: IImageResamplerFactoryChangeable;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AHashFunction: IHashFunction
);
begin
  Assert(Assigned(AImageResampler));
  Assert(Assigned(ABitmapFactory));
  Assert(Assigned(AHashFunction));
  inherited Create;
  FImageResampler := AImageResampler;
  FBitmapFactory := ABitmapFactory;
  FHashFunction := AHashFunction;
  FTileRect := nil;
  FItems := TInterfaceListSimple.Create;
end;

function TBitmapTileMatrixBuilder.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

function TBitmapTileMatrixBuilder.GetTile(
  const ATile: TPoint
): IBitmap32Static;
var
  VIndex: Integer;
begin
  Result := nil;
  if Assigned(FTileRect) then begin
    if PtInRect(FTileRect.Rect, ATile) then begin
      VIndex := IndexByPos(FTileRect.Rect, ATile);
      Result := IBitmap32Static(FItems.Items[VIndex]);
    end;
  end;
end;

function TBitmapTileMatrixBuilder.MakeStatic: IBitmapTileMatrix;
var
  VHash: THashValue;
  i: Integer;
  VStaticList: IInterfaceListStatic;
  VBitmap: IBitmap32Static;
  VIsEmpty: Boolean;
begin
  Result := nil;
  if not Assigned(FTileRect) then begin
    Exit;
  end;
  VStaticList := FItems.MakeStaticCopy;

  VHash := $5f9ef5b5f150c35a;
  VIsEmpty := True;

  for i := 0 to VStaticList.Count - 1 do begin
    VBitmap := IBitmap32Static(VStaticList.Items[i]);
    if Assigned(VBitmap) then begin
      FHashFunction.UpdateHashByHash(VHash, VBitmap.Hash);
      VIsEmpty := False;
    end;
  end;
  if not VIsEmpty then begin
    Result :=
      TBitmapTileMatrix.Create(
        VHash,
        FTileRect,
        VStaticList
      );
  end;
end;

procedure TBitmapTileMatrixBuilder.SetTile(
  const ATile: TPoint;
  const AValue: IBitmap32Static
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

procedure TBitmapTileMatrixBuilder.SetRectWithReset(
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

procedure TBitmapTileMatrixBuilder.SetRect(
  const ATileRect: ITileRect
);
var
  VProjectionNew: IProjectionInfo;
  VProjectionOld: IProjectionInfo;
  VTileRect: TRect;
  VIntersectRect: TRect;
  VOldItems: IInterfaceListStatic;
  VOldRect: TRect;
  VIterator: TTileIteratorByRectRecord;
  VTile: TPoint;
  VRelativeRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VSourceAtTarget: TRect;
  VTileProvider: IBitmapTileProvider;
  VSourceTileMatrix: IBitmapTileMatrix;
begin
  Assert(Assigned(ATileRect));
  VProjectionNew := ATileRect.ProjectionInfo;
  VTileRect := ATileRect.Rect;
  Assert(VProjectionNew.CheckTileRect(VTileRect));
  if Assigned(FTileRect) then begin
    VProjectionOld := FTileRect.ProjectionInfo;
    if not VProjectionOld.GetIsSameProjectionInfo(VProjectionNew) then begin
      if VProjectionNew.ProjectionType.IsSame(VProjectionOld.ProjectionType) then begin
        VOldRect := FTileRect.Rect;
        VRelativeRect := VProjectionOld.TileRect2RelativeRect(VOldRect);
        VSourceAtTarget :=
          RectFromDoubleRect(
            VProjectionNew.RelativeRect2TileRectFloat(VRelativeRect),
            rrOutside
          );
        if not IntersectRect(VIntersectRect, ATileRect.Rect, VSourceAtTarget) then begin
          SetRectWithReset(ATileRect);
        end else begin
          VSourceTileMatrix := MakeStatic;
          SetRectWithReset(ATileRect);
          if Assigned(VSourceTileMatrix) then begin
            VTileProvider :=
              TBitmapTileProviderBySameProjection.Create(
                FBitmapFactory,
                FImageResampler.GetStatic,
                TBitmapTileProviderByMatrix.Create(VSourceTileMatrix),
                VProjectionNew
              );
            VIterator.Init(VIntersectRect);
            while VIterator.Next(VTile) do begin
              FItems.Items[IndexByPos(VTileRect, VTile)] := VTileProvider.GetTile(0, nil, VTile);
            end;
          end;
        end;
      end else begin
        VOldRect := FTileRect.Rect;
        VLonLatRect := VProjectionOld.TileRect2LonLatRect(VOldRect);
        VProjectionNew.ProjectionType.ValidateLonLatRect(VLonLatRect);
        VSourceAtTarget :=
          RectFromDoubleRect(
            VProjectionNew.LonLatRect2TileRectFloat(VLonLatRect),
            rrOutside
          );
        if not IntersectRect(VIntersectRect, ATileRect.Rect, VSourceAtTarget) then begin
          SetRectWithReset(ATileRect);
        end else begin
          VSourceTileMatrix := MakeStatic;
          SetRectWithReset(ATileRect);
          if Assigned(VSourceTileMatrix) then begin
            VTileProvider :=
              TBitmapTileProviderByOtherProjection.Create(
                FBitmapFactory,
                FImageResampler.GetStatic,
                TBitmapTileProviderByMatrix.Create(VSourceTileMatrix),
                VProjectionNew
              );
            VIterator.Init(VIntersectRect);
            while VIterator.Next(VTile) do begin
              FItems.Items[IndexByPos(VTileRect, VTile)] := VTileProvider.GetTile(0, nil, VTile);
            end;
          end;
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
          VIterator.Init(VIntersectRect);
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
