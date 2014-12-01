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

unit u_MarkCategoryList;

interface

uses
  i_MarkCategory,
  i_MarkCategoryList,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TMarkCategoryList = class(TBaseInterfacedObject, IMarkCategoryList)
  private
    FList: IInterfaceListStatic;
  private
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): IMarkCategory;
  public
    constructor Create(const AList: IInterfaceListStatic);
    class function Build(const AList: IInterfaceListStatic): IMarkCategoryList;
  end;

implementation

{ TMarkCategoryList }

class function TMarkCategoryList.Build(
  const AList: IInterfaceListStatic
): IMarkCategoryList;
begin
  Result := nil;
  if Assigned(AList) and (AList.Count > 0) then begin
    Result := Self.Create(AList);
  end;
end;

constructor TMarkCategoryList.Create(const AList: IInterfaceListStatic);
begin
  Assert(Assigned(AList));
  Assert(AList.Count > 0);
  inherited Create;
  FList := AList;
end;

function TMarkCategoryList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMarkCategoryList.GetItem(const AIndex: Integer): IMarkCategory;
begin
  Result := IMarkCategory(FList.Items[AIndex]);
end;

end.
