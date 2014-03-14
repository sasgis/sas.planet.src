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

unit u_MapAbilitiesConfigStatic;

interface

uses
  i_MapAbilitiesConfig,
  u_BaseInterfacedObject;

type
  TMapAbilitiesConfigStatic = class(TBaseInterfacedObject, IMapAbilitiesConfigStatic)
  private
    FIsShowOnSmMap: Boolean;
    FUseDownload: Boolean;
  private
    function GetIsShowOnSmMap: Boolean;
    function GetUseDownload: Boolean;
  public
    constructor Create(
      AIsShowOnSmMap: Boolean;
      AUseDownload: Boolean
    );
  end;

implementation

{ TMapAbilitiesConfigStatic }

constructor TMapAbilitiesConfigStatic.Create(
  AIsShowOnSmMap,
  AUseDownload: boolean
);
begin
  inherited Create;
  FIsShowOnSmMap := AIsShowOnSmMap;
  FUseDownload := AUseDownload;
end;

function TMapAbilitiesConfigStatic.GetIsShowOnSmMap: Boolean;
begin
  Result := FIsShowOnSmMap;
end;

function TMapAbilitiesConfigStatic.GetUseDownload: Boolean;
begin
  Result := FUseDownload;
end;

end.
