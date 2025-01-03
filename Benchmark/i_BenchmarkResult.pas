{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit i_BenchmarkResult;

interface

uses
  i_BenchmarkItem;

type
  IBenchmarkResult = interface
    ['{C1EFC3B6-6650-40AF-B728-8F0DBA45F083}']
    function GetBenchmarkItem: IBenchmarkItem;
    property BenchmarkItem: IBenchmarkItem read GetBenchmarkItem;

    function GetWarmUpTimePerStep: Double;
    property WarmUpTimePerStep: Double read GetWarmUpTimePerStep;

    function GetRunCount: Integer;
    property RunCount: Integer read GetRunCount;

    function GetRunResultTimePerStep(const AIndex: Integer): Double;
    property RunResultTimePerStep[const AIndex: Integer]: Double read GetRunResultTimePerStep;
  end;

implementation

end.
