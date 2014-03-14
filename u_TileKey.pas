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

unit u_TileKey;

interface

uses
  Types,
  i_TileKey,
  u_BaseInterfacedObject;

type
  TTileKey = class(TBaseInterfacedObject, ITileKey)
  private
    FTile: TPoint;
    FZoom: Byte;
  private
    function GetTile: TPoint;
    function GetZoom: Byte;

    function IsSame(const AValue: ITileKey): Boolean;
  public
    constructor Create(
      const ATile: TPoint;
      const AZoom: Byte
    );
  end;

implementation

{ TTileKey }

constructor TTileKey.Create(
  const ATile: TPoint;
  const AZoom: Byte
);
begin
  inherited Create;
  FTile := ATile;
  FZoom := AZoom;
end;

function TTileKey.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileKey.GetZoom: Byte;
begin
  Result := FZoom;
end;

function TTileKey.IsSame(const AValue: ITileKey): Boolean;
var
  VTile: TPoint;
begin
  if AValue = nil then begin
    Result := False;
  end else begin
    VTile := AValue.Tile;
    if (FTile.X <> VTile.X) or (FTile.Y <> VTile.Y) or (FZoom <> AValue.Zoom) then begin
      Result := False;
    end else begin
      Result := True;
    end;
  end;
end;

end.
