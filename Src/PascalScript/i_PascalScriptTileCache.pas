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

unit i_PascalScriptTileCache;

interface

type
  TPascalScriptTileInfo = packed record // Used from the script - must be packed!
    IsExists    : Boolean;
    IsExistsTne : Boolean;
    LoadDate    : Int64; // unix timestamp
    Size        : Cardinal;
    Version     : string;
    ContentType : AnsiString;
    Data        : AnsiString;
  end;

  IPascalScriptTileCache = interface
    ['{075FA877-89C4-4DB2-A7B8-3CB385389024}']
    function Read(
      const X: Integer;
      const Y: Integer;
      const AZoom: Byte;
      const AVersion: string;
      const AWithData: Boolean
    ): TPascalScriptTileInfo;

    function Write(
      const X: Integer;
      const Y: Integer;
      const AZoom: Byte;
      const AVersion: string;
      const AContentType: AnsiString;
      const AData: AnsiString;
      const AIsOverwrite: Boolean
    ): Boolean;

    function WriteTne(
      const X: Integer;
      const Y: Integer;
      const AZoom: Byte;
      const AVersion: string
    ): Boolean;

    function Delete(
      const X: Integer;
      const Y: Integer;
      const AZoom: Byte;
      const AVersion: string
    ): Boolean;
  end;

implementation

end.
