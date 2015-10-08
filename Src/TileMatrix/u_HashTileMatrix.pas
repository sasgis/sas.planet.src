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

unit u_HashTileMatrix;

interface

uses
  Types,
  t_Hash,
  i_TileRect,
  i_HashTileMatrix,
  u_BaseInterfacedObject;

type
  THashTileMatrix = class(TBaseInterfacedObject, IHashTileMatrix)
  private
    FHash: THashValue;
    FTileRect: ITileRect;
    FTileCount: TPoint;
    FItems: array of THashValue;
  private
    function GetHash: THashValue;
    function GetTileRect: ITileRect;
    function GetElementByTile(const ATile: TPoint): THashValue;
    function GetItem(AX, AY: Integer): THashValue;
  public
    constructor Create(
      const AHash: THashValue;
      const ATileRect: ITileRect;
      const AItems: array of THashValue
    );
  end;

implementation

{ THashTileMatrix }

constructor THashTileMatrix.Create(
  const AHash: THashValue;
  const ATileRect: ITileRect;
  const AItems: array of THashValue
);
var
  VItemsCount: Integer;
  VTileRect: TRect;
begin
  Assert(Assigned(ATileRect));
  inherited Create;
  FHash := AHash;
  FTileRect := ATileRect;
  VTileRect := FTileRect.Rect;
  FTileCount := Point(VTileRect.Right - VTileRect.Left, VTileRect.Bottom - VTileRect.Top);
  Assert(FTileCount.X > 0);
  Assert(FTileCount.Y > 0);
  if FTileCount.X < 0 then begin
    FTileCount.X := 0;
  end;
  if FTileCount.Y < 0 then begin
    FTileCount.Y := 0;
  end;
  VItemsCount := FTileCount.X * FTileCount.Y;
  Assert(High(AItems) = VItemsCount - 1);
  SetLength(FItems, VItemsCount);
  Move(AItems[0], FItems[0], SizeOf(THashValue) * VItemsCount);
end;

function THashTileMatrix.GetElementByTile(
  const ATile: TPoint
): THashValue;
begin
  Result := GetItem(ATile.X - FTileRect.Left, ATile.Y - FTileRect.Top);
end;

function THashTileMatrix.GetHash: THashValue;
begin
  Result := FHash;
end;

function THashTileMatrix.GetItem(AX, AY: Integer): THashValue;
var
  VIndex: Integer;
  VX, VY: Integer;
begin
  Result := 0;
  VX := AX;
  if VX >= FTileCount.X then begin
    VX := -1;
  end;

  VY := AY;
  if VY >= FTileCount.Y then begin
    VY := -1;
  end;

  if (VX >= 0) and (VY >= 0) then begin
    VIndex := VY * FTileCount.X + VX;
    Result := THashValue(FItems[VIndex]);
  end;
end;

function THashTileMatrix.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

end.
