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

unit u_MapCoverageInfo;

interface

uses
  t_GeoTypes,
  i_MapCoverageInfo,
  u_BaseInterfacedObject;

type
  TMapCoverageInfo = class(TBaseInterfacedObject, IMapCoverageInfo)
  private
    FCenterPos: TDoublePoint;
    FCenterZoom: Integer;
    function ParseCenterValue(const AValue: string): Boolean;
  private
    { IMapCoverageInfo }
    function GetCenterPos: TDoublePoint;
    function GetCenterZoom: Byte;
  public
    constructor Create(const ACenter: string);
  end;

implementation

uses
  Types,
  Math,
  SysUtils,
  StrUtils,
  u_GeoFunc,
  u_GeoToStrFunc;

{ TMapCoverageInfo }

constructor TMapCoverageInfo.Create(const ACenter: string);
begin
  inherited Create;

  if not ParseCenterValue(ACenter) then begin
    FCenterPos := CEmptyDoublePoint;
    FCenterZoom := 0;
  end;
end;

function TMapCoverageInfo.ParseCenterValue(const AValue: string): Boolean;
var
  VItems: TStringDynArray;
begin
  Result := False;
  if AValue <> '' then begin
    VItems := SplitString(AValue, ',');
    Result :=
      (Length(VItems) = 3) and
      TryStrPointToFloat(Trim(VItems[0]), FCenterPos.X) and
      TryStrPointToFloat(Trim(VItems[1]), FCenterPos.Y) and
      TryStrToInt(Trim(VItems[2]), FCenterZoom) and
      not (SameValue(FCenterPos.X, 0) or SameValue(FCenterPos.Y, 0));
  end;
end;

function TMapCoverageInfo.GetCenterPos: TDoublePoint;
begin
  Result := FCenterPos;
end;

function TMapCoverageInfo.GetCenterZoom: Byte;
begin
  Result := FCenterZoom;
end;

end.
