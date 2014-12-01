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

unit u_MarkCategoryTree;

interface

uses
  i_MarkCategory,
  i_MarkCategoryTree,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TMarkCategoryTree = class(TBaseInterfacedObject, IMarkCategoryTree)
  private
    FMarkCategory: IMarkCategory;
    FName: string;
    FSubItems: IInterfaceListStatic;
  private
    function GetMarkCategory: IMarkCategory;
    function GetName: string;
    function GetSubItemCount: Integer;
    function GetSubItem(AIndex: Integer): IMarkCategoryTree;
  public
    constructor Create(
      const AMarkCategory: IMarkCategory;
      const AName: string;
      const ASubItems: IInterfaceListStatic
    );
  end;

implementation

{ TMarkCategoryTree }

constructor TMarkCategoryTree.Create(
  const AMarkCategory: IMarkCategory;
  const AName: string;
  const ASubItems: IInterfaceListStatic
);
begin
  inherited Create;
  FMarkCategory := AMarkCategory;
  FName := AName;
  FSubItems := ASubItems;
end;

function TMarkCategoryTree.GetMarkCategory: IMarkCategory;
begin
  Result := FMarkCategory;
end;

function TMarkCategoryTree.GetName: string;
begin
  Result := FName;
end;

function TMarkCategoryTree.GetSubItem(AIndex: Integer): IMarkCategoryTree;
begin
  Result := nil;
  if FSubItems <> nil then begin
    if (AIndex >= 0) and (AIndex < FSubItems.Count) then begin
      Result := IMarkCategoryTree(FSubItems.Items[AIndex]);
    end;
  end;
end;

function TMarkCategoryTree.GetSubItemCount: Integer;
begin
  if FSubItems <> nil then begin
    Result := FSubItems.Count;
  end else begin
    Result := 0;
  end;
end;

end.
