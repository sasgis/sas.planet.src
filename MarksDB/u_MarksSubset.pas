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

unit u_MarksSubset;

interface

uses
  Classes,
  ActiveX,
  t_GeoTypes,
  i_MarkCategory,
  i_MarksSimple,
  u_BaseInterfacedObject;

type
  TMarksSubset = class(TBaseInterfacedObject, IMarksSubset)
  private
    FList: IInterfaceList;
  private
    function GetSubsetByLonLatRect(const ARect: TDoubleRect): IMarksSubset;
    function GetSubsetByCategory(const ACategory: ICategory): IMarksSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;
  public
    constructor Create(const AList: IInterfaceList);
  end;

implementation

uses
  u_EnumUnknown;

{ TMarksSubset }

constructor TMarksSubset.Create(const AList: IInterfaceList);
begin
  inherited Create;
  FList := AList;
end;

function TMarksSubset.GetEnum: IEnumUnknown;
begin
  Result := TEnumUnknown.Create(FList);
end;

function TMarksSubset.GetSubsetByCategory(const ACategory: ICategory): IMarksSubset;
var
  VNewList: IInterfaceList;
  i: Integer;
  VCategory: ICategory;
  VMark: IMark;
begin
  VNewList := TInterfaceList.Create;
  VNewList.Lock;
  try
    for i := 0 to FList.Count - 1 do begin
      VMark := IMark(FList.Items[i]);
      VCategory := VMark.Category;
      if (ACategory <> nil) and (VCategory <> nil) then begin
        if VCategory.IsSame(ACategory) then begin
          VNewList.Add(VMark);
        end;
      end else if (ACategory = nil) and (VCategory = nil) then begin
        VNewList.Add(VMark);
      end;
    end;
  finally
    VNewList.Unlock;
  end;
  Result := TMarksSubset.Create(VNewList);
end;

function TMarksSubset.GetSubsetByLonLatRect(const ARect: TDoubleRect): IMarksSubset;
var
  VNewList: IInterfaceList;
  i: Integer;
  VMark: IMark;
begin
  VNewList := TInterfaceList.Create;
  VNewList.Lock;
  try
    for i := 0 to FList.Count - 1 do begin
      VMark := IMark(FList.Items[i]);
      if VMark.LLRect.IsIntersecWithRect(ARect) then begin
        VNewList.Add(VMark);
      end;
    end;
  finally
    VNewList.Unlock;
  end;
  Result := TMarksSubset.Create(VNewList);
end;

function TMarksSubset.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

end.
