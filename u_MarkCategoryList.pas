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

unit u_MarkCategoryList;

interface

uses
  Classes,
  i_MarkCategory,
  i_MarkCategoryFactoryDbInternal,
  i_ConfigDataElement,
  u_IDInterfaceList;

type
  TMarkCategoryList = class(TIDInterfaceList)
  private
    FActual: Boolean;
    FCategoryNotifier: IExtConfigDataElement;
  public
    constructor Create;

    procedure Clear; override;

    procedure CopyToList(const AList: IInterfaceList);

    function GetCategoryByID(const AIdCategory: Integer): IMarkCategory; inline;

    function GetCategoryByName(const ACategoryName: String): IMarkCategory;
    function GetCategoryIdByName(const ACategoryName: String): Integer;

    function AddCategory(const AIdCategory: Integer; const ANewCategory: IMarkCategory): IMarkCategory; inline;
    procedure ReplaceCategory(const AIdCategory: Integer; const ANewCategory: IMarkCategory); inline;
    procedure DeleteCategory(const AIdCategory: Integer; const ACategoryName: String);

    procedure SetAllCategoriesVisible(
      const ACategoryFactoryDbInternal: IMarkCategoryFactoryDbInternal;
      const ANewVisible: Boolean
    );

    property Actual: Boolean read FActual write FActual;
    property CategoryNotifier: IExtConfigDataElement read FCategoryNotifier;
  end;

implementation

uses
  SysUtils,
  u_ConfigDataElementBase;

type
  TCategoryListNotifier = class(TConfigDataElementBaseEmptySaveLoad, IExtConfigDataElement)
  end;

{ TMarkCategoryList }

function TMarkCategoryList.AddCategory(const AIdCategory: Integer; const ANewCategory: IMarkCategory): IMarkCategory;
begin
  Result := IMarkCategory(Self.Add(AIdCategory, ANewCategory));
  FCategoryNotifier.SetChanged;
end;

procedure TMarkCategoryList.Clear;
begin
  FActual := FALSE;
  inherited;
  FCategoryNotifier.SetChanged;
end;

procedure TMarkCategoryList.CopyToList(const AList: IInterfaceList);
var
  i: Integer;
begin
  if (FCount>0) then begin
    AList.Lock;
    try
      AList.Capacity := FCount;
      for i := 0 to FCount-1 do begin
        AList.Add(IMarkCategory(FList^[i].Obj));
      end;
    finally
      AList.Unlock;
    end;
  end;
end;

constructor TMarkCategoryList.Create;
begin
  inherited Create;
  FActual := FALSE;
  FCategoryNotifier := TCategoryListNotifier.Create;
end;

procedure TMarkCategoryList.DeleteCategory(const AIdCategory: Integer; const ACategoryName: String);
var
  VIndex: Integer;
begin
  if (AIdCategory<>CNotExistCategoryID) and Find(AIdCategory, VIndex) then begin
    // есть идентификатор категории - и категории в кэше нашлась
    if not SameText(IMarkCategory(FList^[VIndex].Obj).Name, ACategoryName) then
      Actual := FALSE; // однако имя не то - подозрительненько
    Delete(VIndex);
    FCategoryNotifier.SetChanged;
  end else if (FCount>0) then begin
    // нет идентификатора категории или категория не нашлась в кэше - удалим по имени
    for VIndex := FCount-1 downto 0 do
    if SameText(IMarkCategory(FList^[VIndex].Obj).Name, ACategoryName) then begin
      Delete(VIndex);
      FCategoryNotifier.SetChanged;
      break;
    end;
    Actual := FALSE;
  end;
end;

function TMarkCategoryList.GetCategoryByID(const AIdCategory: Integer): IMarkCategory;
begin
  Result := IMarkCategory(GetByID(AIdCategory));
end;

function TMarkCategoryList.GetCategoryByName(const ACategoryName: String): IMarkCategory;
var i: Integer;
begin
  if (FCount>0) then
  for i := FCount-1 downto 0 do
  if SameText(IMarkCategory(FList^[i].Obj).Name, ACategoryName) then begin
    Result := IMarkCategory(FList^[i].Obj);
    Exit;
  end;
  Result := nil;
end;

function TMarkCategoryList.GetCategoryIdByName(const ACategoryName: String): Integer;
var i: Integer;
begin
  if (FCount>0) then
  for i := FCount-1 downto 0 do
  if SameText(IMarkCategory(FList^[i].Obj).Name, ACategoryName) then begin
    Result := FList^[i].Id;
    Exit;
  end;
  Result := CNotExistCategoryID;
end;

procedure TMarkCategoryList.ReplaceCategory(const AIdCategory: Integer; const ANewCategory: IMarkCategory);
begin
  Replace(AIdCategory, ANewCategory);
  FCategoryNotifier.SetChanged;
end;

procedure TMarkCategoryList.SetAllCategoriesVisible(
  const ACategoryFactoryDbInternal: IMarkCategoryFactoryDbInternal;
  const ANewVisible: Boolean
);
var
  i: Integer;
  VExisting: IMarkCategory;
begin
  if (0=FCount) then
    Exit;
  
  CategoryNotifier.LockWrite;
  try
    if (FCount>0) then
    for i := FCount-1 downto 0 do
    if IMarkCategory(FList^[i].Obj).Visible<>ANewVisible then begin
      VExisting := IMarkCategory(FList^[i].Obj);
      FList^[i].Obj := ACategoryFactoryDbInternal.CreateCategory(
        FList^[i].Id,
        VExisting.Name,
        ANewVisible,
        VExisting.AfterScale,
        VExisting.BeforeScale
      );
      CategoryNotifier.SetChanged;
    end;
  finally
    CategoryNotifier.LockWrite;
  end;
end;

end.
