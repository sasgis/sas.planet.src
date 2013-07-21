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

unit u_VectorDataItemSubset;

interface

uses
  Classes,
  ActiveX,
  t_GeoTypes,
  i_VectorDataItemSimple,
  i_Category,
  i_InterfaceListStatic,
  i_VectorItemSubset,
  u_BaseInterfacedObject;

type
  TVectorItemSubset = class(TBaseInterfacedObject, IVectorItemSubset)
  private
    FList: IInterfaceListStatic;
  private
    function GetSubsetByLonLatRect(const ARect: TDoubleRect): IVectorItemSubset;
    function GetSubsetByCategory(const ACategory: ICategory): IVectorItemSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): IVectorDataItemSimple;
  public
    constructor Create(AList: IInterfaceListStatic);
  end;

implementation

uses
  SysUtils,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_EnumUnknown;

{ TVectorItemSubset }

constructor TVectorItemSubset.Create(AList: IInterfaceListStatic);
begin
  inherited Create;
  FList := AList;
end;

function TVectorItemSubset.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TVectorItemSubset.GetEnum: IEnumUnknown;
begin
  Result := TEnumUnknownByStatic.Create(FList);
end;

function TVectorItemSubset.GetItem(AIndex: Integer): IVectorDataItemSimple;
begin
  Result := IVectorDataItemSimple(FList.Items[AIndex]);
end;

function TVectorItemSubset.GetSubsetByCategory(
  const ACategory: ICategory): IVectorItemSubset;
var
  VNewList: IInterfaceListSimple;
  i: Integer;
  VCategory: ICategory;
  VItem: IVectorDataItemSimple;
  VItemWithCategory: IVectorDataItemWithCategory;
begin
  VNewList := TInterfaceListSimple.Create;
    for i := 0 to FList.Count - 1 do begin
      VItem := IVectorDataItemSimple(FList.Items[i]);
      VCategory := nil;
      if Supports(VItem, IVectorDataItemWithCategory, VItemWithCategory) then begin
        VCategory := VItemWithCategory.Category;
      end;
      if (ACategory <> nil) and (VCategory <> nil) then begin
        if VCategory.IsSame(ACategory) then begin
          VNewList.Add(VItem);
        end;
      end else if (ACategory = nil) and (VCategory = nil) then begin
        VNewList.Add(VItem);
      end;
    end;
  Result := TVectorItemSubset.Create(VNewList.MakeStaticAndClear);
end;

function TVectorItemSubset.GetSubsetByLonLatRect(
  const ARect: TDoubleRect): IVectorItemSubset;
var
  VNewList: IInterfaceListSimple;
  i: Integer;
  VItem: IVectorDataItemSimple;
begin
  VNewList := TInterfaceListSimple.Create;
    for i := 0 to FList.Count - 1 do begin
      VItem := IVectorDataItemSimple(FList.Items[i]);
      if VItem.LLRect.IsIntersecWithRect(ARect) then begin
        VNewList.Add(VItem);
      end;
    end;
  Result := TVectorItemSubset.Create(VNewList.MakeStaticAndClear);
end;

function TVectorItemSubset.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

end.
