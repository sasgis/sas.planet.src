{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_VectorTileMatrix;

interface

uses
  Types,
  t_Hash,
  i_TileRect,
  i_VectorItemSubset,
  i_VectorTileMatrix,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TVectorTileMatrix = class(TBaseInterfacedObject, IVectorTileMatrix)
  private
    FHash: THashValue;
    FTileRect: ITileRect;
    FTileCount: TPoint;
    FItems: IInterfaceListStatic;
  private
    function GetHash: THashValue;
    function GetTileRect: ITileRect;
    function GetElementByTile(const ATile: TPoint): IVectorItemSubset;
    function GetItem(AX, AY: Integer): IVectorItemSubset;
  public
    constructor Create(
      const AHash: THashValue;
      const ATileRect: ITileRect;
      const AItems: IInterfaceListStatic
    );
  end;

implementation

{ TVectorTileMatrix }

constructor TVectorTileMatrix.Create(
  const AHash: THashValue;
  const ATileRect: ITileRect;
  const AItems: IInterfaceListStatic
);
var
  VItemsCount: Integer;
begin
  Assert(Assigned(AItems));
  Assert(Assigned(ATileRect));
  inherited Create;
  FHash := AHash;
  FTileRect := ATileRect;
  FItems := AItems;
  FTileCount := Point(FTileRect.Right - FTileRect.Left, FTileRect.Bottom - FTileRect.Top);
  Assert(FTileCount.X > 0);
  Assert(FTileCount.Y > 0);
  if FTileCount.X < 0 then begin
    FTileCount.X := 0;
  end;
  if FTileCount.Y < 0 then begin
    FTileCount.Y := 0;
  end;
  VItemsCount := FTileCount.X * FTileCount.Y;
  Assert(VItemsCount = AItems.Count);
end;

function TVectorTileMatrix.GetElementByTile(
  const ATile: TPoint
): IVectorItemSubset;
begin
  Result := GetItem(ATile.X - FTileRect.Left, ATile.Y - FTileRect.Top);
end;

function TVectorTileMatrix.GetHash: THashValue;
begin
  Result := FHash;
end;

function TVectorTileMatrix.GetItem(AX, AY: Integer): IVectorItemSubset;
var
  VIndex: Integer;
  VX, VY: Integer;
begin
  Result := nil;
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
    Result := IVectorItemSubset(FItems[VIndex]);
  end;
end;

function TVectorTileMatrix.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

end.
