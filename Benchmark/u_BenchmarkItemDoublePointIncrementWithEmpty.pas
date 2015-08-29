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

unit u_BenchmarkItemDoublePointIncrementWithEmpty;

interface

uses
  t_GeoTypes,
  u_BenchmarkItemDoublePointBaseTest;

type
  TBenchmarkItemDoublePointIncrementWithEmpty = class(TBenchmarkItemDoublePointBaseTest)
  private
    FEmptyStep: Integer;
  protected
    procedure SetUp; override;
    function RunOneStep: Integer; override;
  public
    constructor Create(
      const AEmptyStep: Integer
    );
  end;

implementation

uses
  SysUtils,
  Math,
  u_GeoFunc;

const CPointsCount = 1000;

{ TBenchmarkItemDoublePointIncrementWithEmpty }

function DoublePointIncrement(const ASrc: TDoublePoint): TDoublePoint; inline;
begin
  Result.X := ASrc.X + 1;
  Result.Y := ASrc.Y + 1;
end;

constructor TBenchmarkItemDoublePointIncrementWithEmpty.Create(
  const AEmptyStep: Integer
);
begin
  Assert(AEmptyStep > 0);
  inherited Create(
    True,
    Format('DoublePointIncrementWithEmpty step %d', [AEmptyStep]),
    CPointsCount,
    DoubleRect(-170, -75, 170, 75)
  );
  FEmptyStep := AEmptyStep;
end;

procedure TBenchmarkItemDoublePointIncrementWithEmpty.SetUp;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FCount - 1 do begin
    if Random(FEmptyStep) = 0 then begin
      FPoints[i] := CEmptyDoublePoint;
    end;
  end;
end;

function TBenchmarkItemDoublePointIncrementWithEmpty.RunOneStep: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FCount - 1 do begin
    if not PointIsEmpty(FPoints[i]) then begin
      FDst[i] := DoublePointIncrement(FPoints[i]);
    end else begin
      FDst[i] := CEmptyDoublePoint;
    end;
    Inc(Result);
  end;
end;

end.
