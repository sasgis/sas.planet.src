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

unit u_MapTypeBasic;

interface

uses
  u_MapType,
  i_MapTypes,
  u_BaseInterfacedObject;

type
  TMapTypeBasic = class(TBaseInterfacedObject, IMapType)
  private
    FMapType: TMapType;
  private
    function GetMapType: TMapType;
    function GetGUID: TGUID;
  public
    constructor Create(AMapType: TMapType);
  end;

implementation

uses
  c_ZeroGUID;

{ TMapTypeBasic }

constructor TMapTypeBasic.Create(AMapType: TMapType);
begin
  inherited Create;
  FMapType := AMapType;
end;

function TMapTypeBasic.GetGUID: TGUID;
begin
  if FMapType <> nil then begin
    Result := FMapType.Zmp.GUID;
  end else begin
    Result := CGUID_Zero;
  end;
end;

function TMapTypeBasic.GetMapType: TMapType;
begin
  Result := FMapType;
end;

end.
 