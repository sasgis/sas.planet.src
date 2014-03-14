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

unit u_TreeByMapActiveMapsSet;

interface

uses
  i_Notifier,
  i_StaticTreeItem,
  i_MapTypeSet,
  i_TreeChangeable,
  u_BaseInterfacedObject;

type
  TTreeByMapActiveMapsSet = class(TBaseInterfacedObject, ITreeChangeable)
  private
    FMapsSet: IMapTypeSet;
    FStaticTree: IStaticTreeItem;
    FChangeNotifier: INotifier;
  protected
    function CreateStatic: IStaticTreeItem;
  protected
    function GetStatic: IStaticTreeItem;
    function GetChangeNotifier: INotifier;
  public
    constructor Create(const AMapsSet: IMapTypeSet);
  end;

implementation

uses
  u_Notifier;

{ TTreeByMapActiveMapsSet }

constructor TTreeByMapActiveMapsSet.Create(const AMapsSet: IMapTypeSet);
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FChangeNotifier := TNotifierBase.Create;
  FStaticTree := CreateStatic;
end;

function TTreeByMapActiveMapsSet.CreateStatic: IStaticTreeItem;
begin
  Result := nil;
end;

function TTreeByMapActiveMapsSet.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TTreeByMapActiveMapsSet.GetStatic: IStaticTreeItem;
begin
  Result := FStaticTree;
end;

end.
