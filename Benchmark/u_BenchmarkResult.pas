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

unit u_BenchmarkResult;

interface

uses
  i_BenchmarkItem,
  i_BenchmarkResult;

type
  TBenchmarkResult = class(TInterfacedObject, IBenchmarkResult)
  private
    FBenchmarkItem: IBenchmarkItem;
    FWarmUpRunRps: Double;
    FCount: Integer;
    FResults: array of Double;
  private
    function GetBenchmarkItem: IBenchmarkItem;
    function GetWarmUpRunRps: Double;
    function GetRunCount: Integer;
    function GetRunResultRps(const AIndex: Integer): Double;
  public
    constructor Create(
      const ABenchmarkItem: IBenchmarkItem;
      const AWarmUpRunRps: Double;
      const AResults: array of Double
    );
  end;

implementation

{ TBenchmarkResult }

constructor TBenchmarkResult.Create(
  const ABenchmarkItem: IBenchmarkItem;
  const AWarmUpRunRps: Double;
  const AResults: array of Double
);
var
  i: Integer;
begin
  Assert(Assigned(ABenchmarkItem));
  Assert(High(AResults) > 0);
  inherited Create;
  FBenchmarkItem := ABenchmarkItem;
  FWarmUpRunRps := AWarmUpRunRps;
  FCount := High(AResults);
  SetLength(FResults, FCount);
  for i := 0 to FCount - 1 do begin
    FResults[i] := AResults[i];
  end;
end;

function TBenchmarkResult.GetBenchmarkItem: IBenchmarkItem;
begin
  Result := FBenchmarkItem;
end;

function TBenchmarkResult.GetRunCount: Integer;
begin
  Result := FCount;
end;

function TBenchmarkResult.GetRunResultRps(const AIndex: Integer): Double;
begin
  Result := FResults[AIndex];
end;

function TBenchmarkResult.GetWarmUpRunRps: Double;
begin
  Result := FWarmUpRunRps;
end;

end.
