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

unit u_BenchmarkItemCoordConverter;

interface

uses
  i_ProjectionType,
  u_BenchmarkItemDoublePointBaseTest;

type
  TBenchmarkItemProjectionTypeForvard = class(TBenchmarkItemDoublePointBaseTest)
  private
    FProjectionType: IProjectionType;
  protected
    function RunOneStep: Integer; override;
  public
    constructor Create(
      const AProjectionTypeName: string;
      const AProjectionType: IProjectionType
    );
  end;

  TBenchmarkItemProjectionTypeBackvard = class(TBenchmarkItemDoublePointBaseTest)
  private
    FProjectionType: IProjectionType;
  protected
    function RunOneStep: Integer; override;
  public
    constructor Create(
      const AProjectionTypeName: string;
      const AProjectionType: IProjectionType
    );
  end;

implementation

uses
  t_GeoTypes,
  u_GeoFunc;

const CPointsCount = 1000;

{ TBenchmarkItemProjectionTypeForvard }

constructor TBenchmarkItemProjectionTypeForvard.Create(
  const AProjectionTypeName: string;
  const AProjectionType: IProjectionType
);
begin
  inherited Create(
    Assigned(AProjectionType),
    'CoordConverter LlToRel ' + AProjectionTypeName,
    CPointsCount,
    DoubleRect(-170, -75, 170, 75)
  );
  FProjectionType := AProjectionType;
end;

function TBenchmarkItemProjectionTypeForvard.RunOneStep: Integer;
var
  i: Integer;
  VResult: TDoublePoint;
begin
  Result := 0;
  for i := 0 to FCount - 1 do begin
    VResult := FProjectionType.LonLat2Relative(FPoints[i]);
    Inc(Result);
  end;
end;

{ TBenchmarkItemProjectionTypeBackvard }

constructor TBenchmarkItemProjectionTypeBackvard.Create(
  const AProjectionTypeName: string;
  const AProjectionType: IProjectionType
);
begin
  inherited Create(
    Assigned(AProjectionType),
    'CoordConverter RelToLl ' + AProjectionTypeName,
    CPointsCount,
    DoubleRect(0, 0, 1, 1)
  );
  FProjectionType := AProjectionType;
end;

function TBenchmarkItemProjectionTypeBackvard.RunOneStep: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FCount - 1 do begin
    FDst[i] := FProjectionType.Relative2LonLat(FPoints[i]);
    Inc(Result);
  end;
end;

end.
