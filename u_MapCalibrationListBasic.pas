{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapCalibrationListBasic;

interface

uses
  Classes,
  i_MapCalibration;

type
  TMapCalibrationListBasic = class(TInterfacedObject, IMapCalibrationList)
  private
    FList: IInterfaceList;
    procedure Add(AItem: IMapCalibration);
  protected
    function GetCount: Integer;
    function Get(AIndex: Integer): IMapCalibration;
  public
    constructor Create();
  end;

implementation

uses
  u_MapCalibrationOzi,
  u_MapCalibrationDat,
  u_MapCalibrationKml,
  u_MapCalibrationTab,
  u_MapCalibrationWorldFiles;

{ TMapCalibrationListBasic }

constructor TMapCalibrationListBasic.Create;
begin
  inherited;
  FList := TInterfaceList.Create;
  Add(TMapCalibrationOzi.Create);
  Add(TMapCalibrationDat.Create);
  Add(TMapCalibrationKml.Create);
  Add(TMapCalibrationTab.Create);
  Add(TMapCalibrationWorldFiles.Create);
end;

procedure TMapCalibrationListBasic.Add(AItem: IMapCalibration);
begin
  FList.Add(AItem);
end;

function TMapCalibrationListBasic.Get(AIndex: Integer): IMapCalibration;
begin
  Result := IMapCalibration(FList.Items[AIndex]);
end;

function TMapCalibrationListBasic.GetCount: Integer;
begin
  Result := FList.Count;
end;

end.
