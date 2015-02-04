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

unit u_BitmapTileMatrix;

interface

uses
  Types,
  t_Hash,
  i_TileRect,
  i_Bitmap32Static,
  i_BitmapTileMatrix,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TBitmapTileMatrix = class(TBaseInterfacedObject, IBitmapTileMatrix)
  private
    FHash: THashValue;
    FTileRect: ITileRect;
    FTileCount: TPoint;
    FItems: IInterfaceListStatic;
  private
    function GetHash: THashValue;
    function GetTileRect: ITileRect;
    function GetElementByTile(const ATile: TPoint): IBitmap32Static;
    function GetItem(AX, AY: Integer): IBitmap32Static;
  public
    constructor Create(
      const AHash: THashValue;
      const ATileRect: ITileRect;
      const AItems: IInterfaceListStatic
    );
  end;

implementation

{ TBitmapTileMatrix }

constructor TBitmapTileMatrix.Create(
  const AHash: THashValue;
  const ATileRect: ITileRect;
  const AItems: IInterfaceListStatic
);
var
  VItemsCount: Integer;
  VTileRect: TRect;
begin
  Assert(Assigned(AItems));
  Assert(Assigned(ATileRect));
  inherited Create;
  FHash := AHash;
  FTileRect := ATileRect;
  FItems := AItems;
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
  Assert(VItemsCount = AItems.Count);
end;

function TBitmapTileMatrix.GetElementByTile(
  const ATile: TPoint
): IBitmap32Static;
begin
  Result := GetItem(ATile.X - FTileRect.Left, ATile.Y - FTileRect.Top);
end;

function TBitmapTileMatrix.GetHash: THashValue;
begin
  Result := FHash;
end;

function TBitmapTileMatrix.GetItem(AX, AY: Integer): IBitmap32Static;
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
    Result := IBitmap32Static(FItems[VIndex]);
  end;
end;

function TBitmapTileMatrix.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

end.

