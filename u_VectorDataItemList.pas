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

unit u_VectorDataItemList;

interface

uses
  Classes,
  i_VectorDataItemSimple;

type
  TVectorDataItemList = class(TInterfacedObject, IVectorDataItemList)
  private
    FList: IInterfaceList;
  protected
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IVectorDataItemSimple;
  public
    constructor Create(AList: IInterfaceList);
  end;

implementation

{ TVectorDataItemList }

constructor TVectorDataItemList.Create(AList: IInterfaceList);
begin
  FList := AList;
end;

function TVectorDataItemList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TVectorDataItemList.GetItem(AIndex: Integer): IVectorDataItemSimple;
begin
  Result := IVectorDataItemSimple(FList.Items[AIndex]);
end;

end.
