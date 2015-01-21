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

unit u_TileMatrix;

interface

uses
  Types,
  SysUtils,
  i_TileRect,
  i_TileMatrix,
  u_BaseInterfacedObject;

type
  TTileMatrix = class(TBaseInterfacedObject, ITileMatrix)
  private
    FTileRect: ITileRect;
    FTileCount: TPoint;
    FItems: array of ITileMatrixElement;
  private
    function GetTileRect: ITileRect;
    function GetElementByTile(const ATile: TPoint): ITileMatrixElement;
    function GetItem(AX, AY: Integer): ITileMatrixElement;
  public
    constructor Create(
      const ASync: IReadWriteSync;
      const ATileRect: ITileRect;
      const AItems: array of ITileMatrixElement
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_CoordConverter,
  u_TileMatrixElement;

{ TTileMatrix }

constructor TTileMatrix.Create(
  const ASync: IReadWriteSync;
  const ATileRect: ITileRect;
  const AItems: array of ITileMatrixElement
);
var
  VItemsCount: Integer;
  VSourceItems: Integer;
  i: Integer;
  VTile: TPoint;
begin
  inherited Create;
  FTileRect := ATileRect;
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
  SetLength(FItems, VItemsCount);
  VSourceItems := Length(AItems);
  if VSourceItems > VItemsCount then begin
    VSourceItems := VItemsCount;
  end;

  for i := 0 to VSourceItems - 1 do begin
    FItems[i] := AItems[i];
  end;

  for i := 0 to VItemsCount - 1 do begin
    if FItems[i] = nil then begin
      VTile.Y := i div FTileCount.X;
      VTile.X := i - FTileCount.X * VTile.Y;
      Inc(VTile.X, FTileRect.Left);
      Inc(VTile.Y, FTileRect.Top);

      FItems[i] := TTileMatrixElement.Create(ASync, nil);
    end;
  end;
end;

destructor TTileMatrix.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTileCount.X * FTileCount.Y - 1 do begin
    FItems[i] := nil;
  end;

  inherited;
end;

function TTileMatrix.GetElementByTile(const ATile: TPoint): ITileMatrixElement;
begin
  Result := GetItem(ATile.X - FTileRect.Left, ATile.Y - FTileRect.Top);
end;

function TTileMatrix.GetItem(AX, AY: Integer): ITileMatrixElement;
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
    Result := FItems[VIndex];
  end;
end;

function TTileMatrix.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

end.
