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

unit u_StringProviderForMapTileItem;

interface

uses
  Types,
  i_StringProvider,
  u_BaseInterfacedObject;

type
  TStringProviderForMapTileItem = class(TBaseInterfacedObject, IStringProvider)
  private
    FURLPrefix: string;
    FTile: TPoint;
    FZoom: Byte;
  private
    function GetValue: string;
  public
    constructor Create(
      const AURLPrefix: string;
      const ATile: TPoint;
      const AZoom: Byte
    );
  end;

implementation

uses
  SysUtils;

{ TUrlForMapTileItem }

constructor TStringProviderForMapTileItem.Create(
  const AURLPrefix: string;
  const ATile: TPoint;
  const AZoom: Byte
);
begin
  inherited Create;
  FURLPrefix := AURLPrefix;
  FTile := ATile;
  FZoom := AZoom;
end;

function TStringProviderForMapTileItem.GetValue: string;
begin
  Result :=
    FURLPrefix +
    IntToStr(FZoom) + '/' +
    IntToStr(FTile.X) + '/' +
    IntToStr(FTile.Y) + '/';
end;

end.
