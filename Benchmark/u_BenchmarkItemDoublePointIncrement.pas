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

unit u_BenchmarkItemDoublePointIncrement;

interface

uses
  t_GeoTypes,
  u_BenchmarkItemDoublePointBaseTest;

type
  TBenchmarkItemDoublePointIncrement = class(TBenchmarkItemDoublePointBaseTest)
  protected
    function RunOneStep: Integer; override;
  public
    constructor Create;
  end;

  TBenchmarkItemDoublePointIncrementInplace = class(TBenchmarkItemDoublePointBaseTest)
  protected
    function RunOneStep: Integer; override;
  public
    constructor Create;
  end;

implementation

uses
  u_GeoFunc;

const CPointsCount = 1000;

{ TBenchmarkItemDoublePointIncrement }

constructor TBenchmarkItemDoublePointIncrement.Create;
begin
  inherited Create(
    True,
    'DoublePointIncrement',
    CPointsCount,
    DoubleRect(-170, -75, 170, 75)
  );
end;

function DoublePointIncrement(const ASrc: TDoublePoint): TDoublePoint; inline;
begin
  Result.X := ASrc.X + 1;
  Result.Y := ASrc.Y + 1;
end;

function TBenchmarkItemDoublePointIncrement.RunOneStep: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FCount - 1 do begin
    FDst[i] := DoublePointIncrement(FPoints[i]);
    Inc(Result);
  end;
end;

{ TBenchmarkItemDoublePointIncrementInplace }

constructor TBenchmarkItemDoublePointIncrementInplace.Create;
begin
  inherited Create(
    True,
    'DoublePointIncrementInplace',
    CPointsCount,
    DoubleRect(-170, -75, 170, 75)
  );
end;

procedure DoublePointIncrementInplace(var ASrc: TDoublePoint); inline;
begin
  ASrc.X := ASrc.X + 1;
  ASrc.Y := ASrc.Y + 1;
end;

function TBenchmarkItemDoublePointIncrementInplace.RunOneStep: Integer;
var
  i: Integer;
begin
  Result := 0;
  Move(FPoints[0], FDst[0], FCount * SizeOf(TDoublePoint));
  for i := 0 to FCount - 1 do begin
    DoublePointIncrementInplace(FDst[i]);
    Inc(Result);
  end;
end;

end.
