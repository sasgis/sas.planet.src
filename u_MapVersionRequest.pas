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
    FShowPrevVersion: Boolean;
  private
    function GetBaseVersion: IMapVersionInfo;
    function GetShowPrevVersion: Boolean;
    function GetIsValidVersion(const AVersion: IMapVersionInfo): Boolean;
  public
    constructor Create(
      const ABaseVersion: IMapVersionInfo;
      const AShowPrevVersion: Boolean
    );
  end;

implementation

{ TMapVersionRequest }

constructor TMapVersionRequest.Create(
  const ABaseVersion: IMapVersionInfo;
  const AShowPrevVersion: Boolean
);
begin
  inherited Create;
  FBaseVersion := ABaseVersion;
  FShowPrevVersion := AShowPrevVersion;
end;

function TMapVersionRequest.GetBaseVersion: IMapVersionInfo;
begin
  Result := FBaseVersion;
end;

function TMapVersionRequest.GetIsValidVersion(
  const AVersion: IMapVersionInfo
): Boolean;
begin
  Result := FShowPrevVersion or FBaseVersion.IsSame(AVersion);
end;

function TMapVersionRequest.GetShowPrevVersion: Boolean;
begin
  Result := FShowPrevVersion;
end;

end.
