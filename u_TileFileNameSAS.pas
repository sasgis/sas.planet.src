{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_TileFileNameSAS;

interface

uses
  Types,
  i_TileFileNameGenerator;

type
  TTileFileNameSAS = class(TInterfacedObject, ITileFileNameGenerator)
  public
    function GetTileFileName(
      AXY: TPoint;
      Azoom: byte
    ): string;
  end;

implementation

uses
  SysUtils;

{ TTileFileNameSAS }

function TTileFileNameSAS.GetTileFileName(
  AXY: TPoint;
  Azoom: byte
): string;
begin
  result := format('z%d' + PathDelim + '%d' + PathDelim + 'x%d' + PathDelim + '%d' + PathDelim + 'y%d', [
    Azoom + 1,
    AXY.x shr 10,
    AXY.x,
    AXY.y shr 10,
    AXY.y
    ]);
end;

end.
 