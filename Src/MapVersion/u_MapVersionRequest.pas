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

unit u_MapVersionRequest;

interface

uses
  i_MapVersionInfo,
  i_MapVersionRequest,
  u_BaseInterfacedObject;

type
  TMapVersionRequest = class(TBaseInterfacedObject, IMapVersionRequest)
  private
    FBaseVersion: IMapVersionInfo;
    FShowOtherVersions: Boolean;
  private
    function GetBaseVersion: IMapVersionInfo;
    function GetShowOtherVersions: Boolean;
    function GetIsValidVersion(const AVersion: IMapVersionInfo): Boolean;
  public
    constructor Create(
      const ABaseVersion: IMapVersionInfo;
      const AShowOtherVersions: Boolean
    );
  end;

implementation

{ TMapVersionRequest }

constructor TMapVersionRequest.Create(
  const ABaseVersion: IMapVersionInfo;
  const AShowOtherVersions: Boolean
);
begin
  inherited Create;
  FBaseVersion := ABaseVersion;
  FShowOtherVersions := AShowOtherVersions;
end;

function TMapVersionRequest.GetBaseVersion: IMapVersionInfo;
begin
  Result := FBaseVersion;
end;

function TMapVersionRequest.GetIsValidVersion(
  const AVersion: IMapVersionInfo
): Boolean;
begin
  Result := FShowOtherVersions or FBaseVersion.IsSame(AVersion);
end;

function TMapVersionRequest.GetShowOtherVersions: Boolean;
begin
  Result := FShowOtherVersions;
end;

end.
