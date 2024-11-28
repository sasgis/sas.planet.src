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

unit u_ElevationReaderRAW;

interface

uses
  u_ElevationValue,
  u_ElevationReader;

type
  TElevationReaderRAW = class(TElevationReader)
  public
    function Open(
      const AParams: TElevationReaderParams
    ): Boolean; override;

    function ReadElevationValue(
      const ARow: Integer;
      const ACol: Integer;
      out AValue: TElevationValue
    ): Boolean; override;
  end;

implementation

uses
  NTFiles;

{ TElevationReaderRAW }

function TElevationReaderRAW.Open(
  const AParams: TElevationReaderParams
): Boolean;
begin
  FParams := AParams;
  Result := True;
end;

function TElevationReaderRAW.ReadElevationValue(
  const ARow, ACol: Integer;
  out AValue: TElevationValue
): Boolean;
var
  VOffset: Int64;
begin
  AValue.TypeId := evtSmallInt;

  // The data are stored in row major order
  // (all the data for row 1, followed by all the data for row 2, etc.).

  VOffset := (ARow * FParams.ColsCount + ACol) * SizeOf(AValue.ValueSmall);

  Result := NtReadFromFile(
    FParams.FileHandle,
    @AValue.ValueSmall,
    SizeOf(AValue.ValueSmall),
    VOffset
  );
end;

end.
