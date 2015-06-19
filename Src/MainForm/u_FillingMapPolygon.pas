{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_FillingMapPolygon;

interface

uses
  i_GeometryLonLat,
  i_FillingMapPolygon,
  u_ChangeableBase;

type
  TFillingMapPolygon = class(TChangeableWithSimpleLockBase, IFillingMapPolygon)
  private
    FPolygon: IGeometryLonLatPolygon;
  private
    { IFillingMapPolygon }
    function GetPolygon: IGeometryLonLatPolygon;
    procedure SetPolygon(const ALonLatPolygon: IGeometryLonLatPolygon);
  public
    constructor Create;
  end;

implementation

{ TFillingMapPolygon }

constructor TFillingMapPolygon.Create;
begin
  inherited Create;
  FPolygon := nil;
end;

function TFillingMapPolygon.GetPolygon: IGeometryLonLatPolygon;
begin
  CS.BeginRead;
  try
    Result := FPolygon;
  finally
    CS.EndRead;
  end;
end;

procedure TFillingMapPolygon.SetPolygon(const ALonLatPolygon: IGeometryLonLatPolygon);
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
