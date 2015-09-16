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

unit u_HashTileMatrixBuilder;

interface

uses
  Types,
  t_Hash,
  i_TileRect,
  i_HashTileMatrix,
  i_HashTileMatrixBuilder,
  i_HashFunction,
  u_BaseInterfacedObject;

type
  THashTileMatrixBuilder = class(TBaseInterfacedObject, IHashTileMatrixBuilder)
  private type
    THashValueArray = array of THashValue;
  private
    FHashFunction: IHashFunction;
    FTileRect: ITileRect;
    FItems: THashValueArray;
  private
    function GetTileRect: ITileRect;

    function GetTile(const ATile: TPoint): THashValue;
    procedure SetTile(const ATile: TPoint; const AValue: THashValue);

    procedure Reset(const AValue: THashValue);
    procedure SetRectWithReset(const ATileRect: ITileRect; const AValue: THashValue);
    procedure SetRect(const ATileRect: ITileRect; const AValue: THashValue);

    function MakeStatic: IHashTileMatrix;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  u_TileIteratorByRect,
  u_HashTileMatrix;

function IndexByPos(const ARect: TRect; const APos: TPoint): Integer; inline;
begin
  Result := APos.X - ARect.Left + (APos.Y - ARect.Top) * (ARect.Right - ARect.Left);
end;

{ THashTileMatrixBuilder }

constructor THashTileMatrixBuilder.Create(
  const AHashFunction: IHashFunction
);
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
  FTileRect := nil;
  FItems := nil;
end;

function THashTileMatrixBuilder.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

function THashTileMatrixBuilder.GetTile(
  const ATile: TPoint
): THashValue;
var
  VIndex: Integer;
begin
  Result := 0;
  if Assigned(FTileRect) then begin
    if PtInRect(FTileRect.Rect, ATile) then begin
      VIndex := IndexByPos(FTileRect.Rect, ATile);
      Result := THashValue(FItems[VIndex]);
    end;
  end;
end;

function THashTileMatrixBuilder.MakeStatic: IHashTileMatrix;
var
  VHash: THashValue;
  i: Integer;
begin
  Result := nil;
  if not Assigned(FTileRect) then begin
    Exit;
  end;

  VHash := $5f9ef5b5f150c35a;

  for i := 0 to High(FItems) do begin
    FHashFunction.UpdateHashByHash(VHash, FItems[i]);
  end;

  Result :=
    THashTileMatrix.Create(
      VHash,
      FTileRect,
      FItems
    );
end;

procedure THashTileMatrixBuilder.Reset(const AValue: THashValue);
var
  i: Integer;
begin
  if Assigned(FTileRect) then begin
    for i := 0 to Length(FItems) - 1 do begin
      FItems[i] := AValue;
    end;
  end;
end;

procedure THashTileMatrixBuilder.SetTile(
  const ATile: TPoint;
  const AValue: THashValue
);
var
  VIndex: Integer;
begin
  if Assigned(FTileRect) then begin
    if PtInRect(FTileRect.Rect, ATile) then begin
      VIndex := IndexByPos(FTileRect.Rect, ATile);
      FItems[VIndex] := AValue;
    end;
  end;
end;

procedure THashTileMatrixBuilder.SetRectWithReset(
  const ATileRect: ITileRect;
  const AValue: THashValue
);
var
  VSize: Integer;
begin
  FTileRect := ATileRect;
  if Assigned(FTileRect) then begin
    VSize := (FTileRect.Right - FTileRect.Left) * (FTileRect.Bottom - FTileRect.Top);
    if VSize <> Length(FItems) then begin
      SetLength(FItems, VSize);
    end;
    Reset(AValue);
  end;
end;

procedure THashTileMatrixBuilder.SetRect(
  const ATileRect: ITileRect;
  const AValue: THashValue
);
var
  VTileRect: TRect;
  VIntersectRect: TRect;
  VOldItems: THashValueArray;
  VOldRect: TRect;
  VIterator: TTileIteratorByRectRecord;
  VTile: TPoint;
begin
  if not Assigned(ATileRect) then begin
    SetRectWithReset(ATileRect, AValue);
  end else begin
    if not Assigned(FTileRect) then begin
      SetRectWithReset(ATileRect, AValue);
    end else begin
      if not FTileRect.Projection.GetIsSameProjectionInfo(ATileRect.Projection) then begin
        SetRectWithReset(ATileRect, AValue);
      end else begin
        if not FTileRect.IsEqual(ATileRect) then begin
          if not IntersectRect(VIntersectRect, ATileRect.Rect, FTileRect.Rect) then begin
            SetRectWithReset(ATileRect, AValue);
          end else begin
            VOldItems := FItems;
            FItems := nil;
            VTileRect := ATileRect.Rect;
            VOldRect := FTileRect.Rect;
            SetRectWithReset(ATileRect, AValue);
            VIterator.Init(VIntersectRect);
            while VIterator.Next(VTile) do begin
              FItems[IndexByPos(VTileRect, VTile)] :=
                VOldItems[IndexByPos(VOldRect, VTile)];
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
