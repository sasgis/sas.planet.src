{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

    function CalcItemIndex(const X, Y: Integer): Integer; inline;
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

function TBitmapTileMatrix.CalcItemIndex(const X, Y: Integer): Integer;
begin
  if (X >= 0) and (X < FTileCount.X) and (Y >= 0) and (Y < FTileCount.Y) then begin
    Result := Y * FTileCount.X + X;
  end else begin
    Result := -1;
  end;
end;

function TBitmapTileMatrix.GetItem(AX, AY: Integer): IBitmap32Static;
var
  VIndex: Integer;
begin
  VIndex := CalcItemIndex(AX, AY);
  if VIndex >= 0 then begin
    Result := IBitmap32Static(FItems[VIndex]);
  end else begin
    Result := nil;
  end;
end;

function TBitmapTileMatrix.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

end.
