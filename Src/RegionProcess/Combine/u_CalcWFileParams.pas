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

unit u_CalcWFileParams;

interface

uses
  t_GeoTypes,
  t_ECW,
  i_ProjectionType;

  function GetUnitsByProjectionEPSG(const AEPSG: Integer): TCellSizeUnits;
  procedure CalculateWFileParams(
    const LL1,LL2:TDoublePoint;
    ImageWidth,ImageHeight:integer;
    const AProjectionType: IProjectionType;
    var CellIncrementX,CellIncrementY,OriginX,OriginY:Double
  );

implementation

uses
  c_CoordConverter;

function GetUnitsByProjectionEPSG(const AEPSG: Integer): TCellSizeUnits;
begin
  case AEPSG of
    CGoogleProjectionEPSG: Result := CELL_UNITS_METERS;
    53004: Result := CELL_UNITS_METERS;
    CYandexProjectionEPSG: Result := CELL_UNITS_METERS;
    CGELonLatProjectionEPSG: Result := CELL_UNITS_DEGREES;
  else
    Result := CELL_UNITS_UNKNOWN;
  end;
end;

procedure CalculateWFileParams(
  const LL1, LL2: TDoublePoint;
  ImageWidth, ImageHeight: integer;
  const AProjectionType: IProjectionType;
  var CellIncrementX, CellIncrementY, OriginX, OriginY: Double
);
var
  VM1: TDoublePoint;
  VM2: TDoublePoint;
begin
  case GetUnitsByProjectionEPSG(AProjectionType.ProjectionEPSG) of
    CELL_UNITS_METERS: begin
      VM1 := AProjectionType.LonLat2Metr(LL1);
      VM2 := AProjectionType.LonLat2Metr(LL2);

      OriginX := VM1.X;
      OriginY := VM1.Y;

      CellIncrementX := (VM2.X-VM1.X)/ImageWidth;
      CellIncrementY := (VM2.Y-VM1.Y)/ImageHeight;
    end;
    CELL_UNITS_DEGREES: begin
      OriginX:=LL1.x;
      OriginY:=LL1.y;
      CellIncrementX:=(LL2.x-LL1.x)/ImageWidth;
      CellIncrementY:=-CellIncrementX;
    end;
  end;
end;

end.
