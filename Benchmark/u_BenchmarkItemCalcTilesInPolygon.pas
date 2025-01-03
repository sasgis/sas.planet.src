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

unit u_BenchmarkItemCalcTilesInPolygon;

interface

uses
  i_Projection,
  i_GeometryProjected,
  u_BenchmarkItemBase;

type
  TBenchmarkItemCalcTilesInPolygon = class(TBenchmarkItemBase)
  private
    FProjection: IProjection;
    FPolygon: IGeometryProjectedPolygon;
  protected
    function RunOneStep: Integer; override;
  public
    constructor Create(
      const AName: string;
      const AProjection: IProjection;
      const APolygon: IGeometryProjectedPolygon
    );
  end;

implementation

uses
  u_GeometryFunc;

{ TBenchmarkItemCalcTilesInPolygon }

constructor TBenchmarkItemCalcTilesInPolygon.Create(
  const AName: string;
  const AProjection: IProjection;
  const APolygon: IGeometryProjectedPolygon
);
begin
  Assert(Assigned(AProjection));
  Assert(Assigned(APolygon));
  inherited Create(
    True,
    AName,
    1
  );
  FProjection := AProjection;
  FPolygon := APolygon;
end;

function TBenchmarkItemCalcTilesInPolygon.RunOneStep: Integer;
begin
  Result := CalcTileCountInProjectedPolygon(FProjection, FPolygon);
end;

end.
