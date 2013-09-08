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
  i_InterfaceListStatic,
  i_MapCalibration,
  u_BaseInterfacedObject;

type
  TMapCalibrationListByInterfaceList = class(TBaseInterfacedObject, IMapCalibrationList)
  private
    FList: IInterfaceListStatic;
  private
    function GetCount: Integer;
    function Get(AIndex: Integer): IMapCalibration;
  public
    constructor Create(const AList: IInterfaceListStatic);
  end;

  TMapCalibrationListBasic = class(TMapCalibrationListByInterfaceList)
  public
    constructor Create;
  end;

implementation

uses
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_MapCalibrationOzi,
  u_MapCalibrationDat,
  u_MapCalibrationKml,
  u_MapCalibrationTab,
  u_MapCalibrationWorldFiles;

{ TMapCalibrationListByInterfaceList }

constructor TMapCalibrationListByInterfaceList.Create(const AList: IInterfaceListStatic);
begin
  inherited Create;
  FList := AList;
end;

function TMapCalibrationListByInterfaceList.Get(
  AIndex: Integer
): IMapCalibration;
begin
  Result := IMapCalibration(FList.Items[AIndex]);
end;

function TMapCalibrationListByInterfaceList.GetCount: Integer;
begin
  Result := FList.Count;
end;

{ TMapCalibrationListBasic }

constructor TMapCalibrationListBasic.Create;
var
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;
  VList.Add(IMapCalibration(TMapCalibrationOzi.Create));
  VList.Add(IMapCalibration(TMapCalibrationDat.Create));
  VList.Add(IMapCalibration(TMapCalibrationKml.Create));
  VList.Add(IMapCalibration(TMapCalibrationTab.Create));
  VList.Add(IMapCalibration(TMapCalibrationWorldFiles.Create));
  inherited Create(VList.MakeStaticAndClear);
end;

end.
