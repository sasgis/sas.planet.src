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

unit u_TreeByMapActiveMapsSet;

interface

uses
  i_JclNotify,
  i_StaticTreeItem,
  i_TreeChangeable,
  i_ActiveMapsConfig;

type
  TTreeByMapActiveMapsSet = class(TInterfacedObject, ITreeChangeable)
  private
    FMapsSet: IActiveMapsSet;
    FStaticTree: IStaticTreeItem;
    FChangeNotifier: IJclNotifier;
  protected
    function CreateStatic: IStaticTreeItem;
  protected
    function GetStatic: IStaticTreeItem;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(const AMapsSet: IActiveMapsSet);
  end;

implementation

uses
  u_JclNotify;

{ TTreeByMapActiveMapsSet }

constructor TTreeByMapActiveMapsSet.Create(const AMapsSet: IActiveMapsSet);
begin
  FMapsSet := AMapsSet;
  FChangeNotifier := TJclBaseNotifier.Create;
  FStaticTree := CreateStatic;
end;

function TTreeByMapActiveMapsSet.CreateStatic: IStaticTreeItem;
begin
  Result := nil;
end;

function TTreeByMapActiveMapsSet.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TTreeByMapActiveMapsSet.GetStatic: IStaticTreeItem;
begin
  Result := FStaticTree;
end;

end.
