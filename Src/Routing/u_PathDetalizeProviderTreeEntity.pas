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

unit u_PathDetalizeProviderTreeEntity;

interface

uses
  i_PathDetalizeProvider,
  i_PathDetalizeProviderTreeEntity,
  u_BaseInterfacedObject;

type
  TPathDetalizeProviderTreeEntity = class(TBaseInterfacedObject, IPathDetalizeProviderTreeEntity)
  private
    FGUID: TGUID;
    FDescription: string;
    FMenuItemName: string;
    FProvider: IPathDetalizeProvider;
  private
    function GetGUID: TGUID;
    function GetDescription: string;
    function GetMenuItemName: string;

    function GetProvider: IPathDetalizeProvider;
  public
    constructor Create(
      const AGUID: TGUID;
      const ADescription: string;
      const AMenuItemName: string;
      const AProvider: IPathDetalizeProvider
    );
  end;

implementation

{ TPathDetalizeProviderListEntity }

constructor TPathDetalizeProviderTreeEntity.Create(
  const AGUID: TGUID;
  const ADescription: string;
  const AMenuItemName: string;
  const AProvider: IPathDetalizeProvider
);
begin
  inherited Create;
  FGUID := AGUID;
  FDescription := ADescription;
  FMenuItemName := AMenuItemName;
  FProvider := AProvider;
end;

function TPathDetalizeProviderTreeEntity.GetDescription: string;
begin
  Result := FDescription;
end;

function TPathDetalizeProviderTreeEntity.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TPathDetalizeProviderTreeEntity.GetMenuItemName: string;
begin
  Result := FMenuItemName;
end;

function TPathDetalizeProviderTreeEntity.GetProvider: IPathDetalizeProvider;
begin
  Result := FProvider;
end;

end.
