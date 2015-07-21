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

unit u_TileFileNameBase;

interface

uses
  Types,
  i_TileFileNameParser,
  i_TileFileNameGenerator,
  u_BaseInterfacedObject;

type
  TTileFileNameBase = class(
    TBaseInterfacedObject,
    ITileFileNameParser,
    ITileFileNameGenerator
  )
  protected
    function GetTileFileName(
      AXY: TPoint;
      AZoom: Byte
    ): string; virtual; abstract;

    function GetTilePoint(
      const ATileFileName: string;
      out ATileXY: TPoint;
      out ATileZoom: Byte
    ): Boolean; virtual; abstract;

    function AddExt(const AFileName, AExt: String): String; virtual;
  end;

implementation

uses
  SysUtils;

{ TTileFileNameBase }

function TTileFileNameBase.AddExt(const AFileName, AExt: String): String;
begin
  Result := ChangeFileExt(AFileName, AExt);
end;

end.
