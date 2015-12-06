{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_FavoriteMapSetItemStatic;

interface

uses
  Classes,
  i_GUIDListStatic,
  i_FavoriteMapSetItemStatic,
  u_BaseInterfacedObject;

type
  TFavoriteMapSetItemStatic = class(TBaseInterfacedObject, IFavoriteMapSetItemStatic)
  private
    FID: TGUID;
    FBaseMap: TGUID;
    FLayers: IGUIDSetStatic;
    FZoom: Integer;
    FName: string;
    FHotKey: TShortCut;
    FSortIndex: Integer;
    FMergeLayers: Boolean;
  private
    { IFavoriteMapSetItemStatic }
    function GetID: TGUID;
    function GetBaseMap: TGUID;
    function GetLayers: IGUIDSetStatic;
    function GetMergeLayers: Boolean;
    function GetZoom: Integer;
    function GetName: string;
    function GetHotKey: TShortCut;
    function GetSortIndex: Integer;
  public
    constructor Create(
      const AID: TGUID;
      const ABaseMap: TGUID;
      const ALayers: IGUIDSetStatic;
      const AMergeLayers: Boolean;
      const AZoom: Integer;
      const AName: string;
      const AHotKey: TShortCut;
      const ASortIndex: Integer
    );
  end;

implementation

{ TFavoriteMapSetItemStatic }

constructor TFavoriteMapSetItemStatic.Create(
  const AID: TGUID;
  const ABaseMap: TGUID;
  const ALayers: IGUIDSetStatic;
  const AMergeLayers: Boolean;
  const AZoom: Integer;
  const AName: string;
  const AHotKey: TShortCut;
  const ASortIndex: Integer
);
begin
  inherited Create;
  FID := AID;
  FBaseMap := ABaseMap;
  FLayers := ALayers;
  FMergeLayers := AMergeLayers;
  FZoom := AZoom;
  FName := AName;
  FHotKey := AHotKey;
  FSortIndex := ASortIndex;
end;

function TFavoriteMapSetItemStatic.GetID: TGUID;
begin
  Result := FID;
end;

function TFavoriteMapSetItemStatic.GetBaseMap: TGUID;
begin
  Result := FBaseMap;
end;

function TFavoriteMapSetItemStatic.GetLayers: IGUIDSetStatic;
begin
  Result := FLayers;
end;

function TFavoriteMapSetItemStatic.GetMergeLayers: Boolean;
begin
  Result := FMergeLayers;
end;

function TFavoriteMapSetItemStatic.GetZoom: Integer;
begin
  Result := FZoom;
end;

function TFavoriteMapSetItemStatic.GetName: string;
begin
  Result := FName;
end;

function TFavoriteMapSetItemStatic.GetHotKey: TShortCut;
begin
  Result := FHotKey;
end;

function TFavoriteMapSetItemStatic.GetSortIndex: Integer;
begin
  Result := FSortIndex;
end;

end.
