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

unit u_MergePolygonsResult;

interface

uses
  i_MergePolygonsResult,
  i_GeometryLonLat,
  u_ChangeableBase;

type
  TMergePolygonsResult = class(TChangeableWithSimpleLockBase, IMergePolygonsResult)
  private
    FPolygon: IGeometryLonLatPolygon;
  private
    function GetPolygon: IGeometryLonLatPolygon;
    procedure SetPolygon(const ALonLatPolygon: IGeometryLonLatPolygon);
  public
    constructor Create;
  end;

implementation

{ TMergePolygonsResult }

constructor TMergePolygonsResult.Create;
begin
  inherited Create;
  FPolygon := nil;
end;

function TMergePolygonsResult.GetPolygon: IGeometryLonLatPolygon;
begin
  CS.BeginRead;
  try
    Result := FPolygon;
  finally
    CS.EndRead;
  end;
end;

procedure TMergePolygonsResult.SetPolygon(const ALonLatPolygon: IGeometryLonLatPolygon);
begin
  CS.BeginWrite;
  try
    FPolygon := ALonLatPolygon;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
