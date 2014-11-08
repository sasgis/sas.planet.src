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

unit u_TerrainProviderListElement;

interface

uses
  i_TerrainProvider,
  i_TerrainProviderListElement,
  u_BaseInterfacedObject;

type
  TTerrainProviderListElement = class(TBaseInterfacedObject, ITerrainProviderListElement)
  private
    FGUID: TGUID;
    FCaption: WideString;
    FProvider: ITerrainProvider;
  private
    function GetGUID: TGUID;
    function GetCaption: WideString;
    function GetProvider: ITerrainProvider;
  public
    constructor Create(
      const AGUID: TGUID;
      const ACaption: WideString;
      const AProvider: ITerrainProvider
    );
  end;

implementation

{ TTerrainProviderListElement }

constructor TTerrainProviderListElement.Create(
  const AGUID: TGUID;
  const ACaption: WideString;
  const AProvider: ITerrainProvider
);
begin
  inherited Create;
  FGUID := AGUID;
  FCaption := ACaption;
  FProvider := AProvider;
end;

function TTerrainProviderListElement.GetCaption: WideString;
begin
  Result := FCaption;
end;

function TTerrainProviderListElement.GetProvider: ITerrainProvider;
begin
  Result := FProvider;
end;

function TTerrainProviderListElement.GetGUID: TGUID;
begin
  Result := FGUID;
end;

end.
