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

unit u_MapTypeGUIConfigStatic;

interface

uses
  Classes,
  i_MapTypeGUIConfig,
  u_BaseInterfacedObject;

type
  TMapTypeGUIConfigStatic = class(TBaseInterfacedObject, IMapTypeGUIConfigStatic)
  private
    FName: string;
    FSortIndex: Integer;
    FHotKey: TShortCut;
    FSeparator: Boolean;
    FParentSubMenu: string;
    FEnabled: Boolean;
    FInfoUrl: string;
  private
    function GetName: string;
    function GetSortIndex: Integer;
    function GetHotKey: TShortCut;
    function GetSeparator: Boolean;
    function GetParentSubMenu: string;
    function GetEnabled: Boolean;
    function GetInfoUrl: string;
  public
    constructor Create(
      const AName: string;
      ASortIndex: Integer;
      AHotKey: TShortCut;
      ASeparator: Boolean;
      const AParentSubMenu: string;
      AEnabled: Boolean;
      const AInfoUrl: string
    );
  end;

implementation

{ TMapTypeGUIConfigStatic }

constructor TMapTypeGUIConfigStatic.Create(
  const AName: string;
  ASortIndex: Integer;
  AHotKey: TShortCut;
  ASeparator: Boolean;
  const AParentSubMenu: string;
  AEnabled: Boolean;
  const AInfoUrl: string
);
begin
  inherited Create;
  FName := AName;
  FSortIndex := ASortIndex;
  FHotKey := AHotKey;
  FSeparator := ASeparator;
  FParentSubMenu := AParentSubMenu;
  FEnabled := AEnabled;
  FInfoUrl := AInfoUrl;
end;

function TMapTypeGUIConfigStatic.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMapTypeGUIConfigStatic.GetHotKey: TShortCut;
begin
  Result := FHotKey;
end;

function TMapTypeGUIConfigStatic.GetInfoUrl: string;
begin
  Result := FInfoUrl;
end;

function TMapTypeGUIConfigStatic.GetName: string;
begin
  Result := FName;
end;

function TMapTypeGUIConfigStatic.GetParentSubMenu: string;
begin
  Result := FParentSubMenu;
end;

function TMapTypeGUIConfigStatic.GetSeparator: Boolean;
begin
  Result := FSeparator;
end;

function TMapTypeGUIConfigStatic.GetSortIndex: Integer;
begin
  Result := FSortIndex;
end;

end.
