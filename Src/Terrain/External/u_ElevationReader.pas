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

unit u_ElevationReader;

interface

uses
  Windows,
  u_ElevationValue;

type
  TElevationReaderParams = record
    FileName: string;
    FileHandle: THandle;
    RowsCount: Integer;
    ColsCount: Integer;
  end;

  TElevationReader = class
  protected
    FParams: TElevationReaderParams;
  public
    function Open(
      const AParams: TElevationReaderParams
    ): Boolean; virtual; abstract;

    function ReadElevationValue(
      const ARow: Integer;
      const ACol: Integer;
      out AValue: TElevationValue
    ): Boolean; virtual; abstract;
  end;

implementation

end.
