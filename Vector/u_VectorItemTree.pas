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

unit u_VectorItemTree;

interface

uses
  i_InterfaceListStatic,
  i_VectorItemSubset,
  i_VectorItemTree,
  u_BaseInterfacedObject;

type
  TVectorItemTree = class(TBaseInterfacedObject, IVectorItemTree)
  private
    FName: string;
    FItems: IVectorItemSubset;
    FSubTreeItemList: IInterfaceListStatic;
  private
    function GetName: string;
    function GetSubTreeItemCount: Integer;
    function GetSubTreeItem(const AIndex: Integer): IVectorItemTree;
    function GetItems: IVectorItemSubset;
  public
    constructor Create(
      const AName: string;
      const AItems: IVectorItemSubset;
      const ASubTreeItemList: IInterfaceListStatic
    );
  end;

implementation

{ TVectorItemTree }

constructor TVectorItemTree.Create(
  const AName: string;
  const AItems: IVectorItemSubset;
  const ASubTreeItemList: IInterfaceListStatic
);
begin
  inherited Create;
  FName := AName;
  FItems := AItems;
  FSubTreeItemList := ASubTreeItemList;
end;

function TVectorItemTree.GetName: string;
begin
  Result := FName;
end;

function TVectorItemTree.GetItems: IVectorItemSubset;
begin
  Result := FItems;
end;

function TVectorItemTree.GetSubTreeItem(const AIndex: Integer): IVectorItemTree;
begin
  Result := nil;
  if Assigned(FSubTreeItemList) then begin
    if (AIndex >= 0) and (AIndex < FSubTreeItemList.Count) then begin
      Result := IVectorItemTree(FSubTreeItemList[AIndex]);
    end;
  end;
end;

function TVectorItemTree.GetSubTreeItemCount: Integer;
begin
  if Assigned(FSubTreeItemList) then begin
    Result := FSubTreeItemList.Count;
  end else begin
    Result := 0;
  end;

end;

end.
