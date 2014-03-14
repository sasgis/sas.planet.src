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

unit u_LastSelectionInfo;

interface

uses
  i_LastSelectionInfo,
  i_GeometryLonLat,
  u_ConfigDataElementBase;

type
  TLastSelectionInfo = class(TConfigDataElementBaseEmptySaveLoad, ILastSelectionInfo)
  private
    // Полигон последнего выделения при операциях с областью.
    FPolygon: IGeometryLonLatMultiPolygon;
    // Масштаб, на котором было последнее выделение
    FZoom: Byte;
  private
    function GetZoom: Byte;
    function GetPolygon: IGeometryLonLatMultiPolygon;
    procedure SetPolygon(
      const ALonLatPolygon: IGeometryLonLatMultiPolygon;
      AZoom: Byte
    );
  public
    constructor Create;
  end;

implementation

{ TLastSelectionInfo }

constructor TLastSelectionInfo.Create;
begin
  inherited Create;
  FPolygon := nil;
  FZoom := 0;
end;

function TLastSelectionInfo.GetPolygon: IGeometryLonLatMultiPolygon;
begin
  LockRead;
  try
    Result := FPolygon;
  finally
    UnlockRead;
  end;
end;

function TLastSelectionInfo.GetZoom: Byte;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

procedure TLastSelectionInfo.SetPolygon(
  const ALonLatPolygon: IGeometryLonLatMultiPolygon;
  AZoom: Byte
);
begin
  LockWrite;
  try
    FPolygon := ALonLatPolygon;
    FZoom := AZoom;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
