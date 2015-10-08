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

unit fr_MarkCategorySelectOrAdd;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  i_LanguageManager,
  i_Category,
  i_MarkCategoryList,
  i_MarkCategoryDB,
  u_CommonFormAndFrameParents;

type
  TfrMarkCategorySelectOrAdd = class(TFrame)
    CBKateg: TComboBox;
    lblCategory: TLabel;
  private
    FCategoryDB: IMarkCategoryDB;
    FCategoryList: IMarkCategoryList;
    FLastUsedCategoryName: string;
    procedure CategoryListToStrings(
      const AList: IMarkCategoryList;
      AStrings: TStrings
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACategoryDb: IMarkCategoryDB
    ); reintroduce;
    destructor Destroy; override;
    procedure Init(const ACategory: ICategory);
    function GetCategory: ICategory;
    procedure Clear;
  end;

implementation

uses
  SysUtils,
  i_MarkCategory;

{$R *.dfm}

{ TfrMarkCategorySelectOrAdd }

constructor TfrMarkCategorySelectOrAdd.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDb: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);
  FCategoryDB := ACategoryDb;
  FCategoryList := nil;
  FLastUsedCategoryName := '';
end;

destructor TfrMarkCategorySelectOrAdd.Destroy;
begin
  Clear;
  FCategoryDB := nil;
  inherited;
end;

procedure TfrMarkCategorySelectOrAdd.CategoryListToStrings(
  const AList: IMarkCategoryList;
  AStrings: TStrings
);
var
  i: Integer;
  VCategory: ICategory;
begin
  AStrings.Clear;
  if Assigned(AList) then begin
    for i := 0 to AList.Count - 1 do begin
      VCategory := ICategory(AList[i]);
      AStrings.AddObject(VCategory.Name, Pointer(VCategory));
    end;
  end;
end;

procedure TfrMarkCategorySelectOrAdd.Clear;
begin
  FCategoryList := nil;
  if Assigned(CBKateg) then begin
    CBKateg.Items.Clear;
  end;
end;

function TfrMarkCategorySelectOrAdd.GetCategory: ICategory;
var
  VIndex: Integer;
  VCategoryText: string;
  VCategory: IMarkCategory;
begin
  Result := nil;
  VCategoryText := Trim(CBKateg.Text);
  VIndex := CBKateg.ItemIndex;
  if VIndex < 0 then begin
    VIndex := CBKateg.Items.IndexOf(VCategoryText);
  end;
  if VIndex >= 0 then begin
    Result := ICategory(Pointer(CBKateg.Items.Objects[VIndex]));
  end;
  if Result = nil then begin
    VCategory := FCategoryDB.Factory.CreateNew(VCategoryText);
    Result := FCategoryDB.GetCategoryByName(VCategory.Name);
    if Result = nil then begin
      Result := FCategoryDB.UpdateCategory(nil, VCategory);
    end;
  end;
  if Result <> nil then begin
    FLastUsedCategoryName := Result.Name;
  end;
end;

procedure TfrMarkCategorySelectOrAdd.Init(const ACategory: ICategory);
var
  i: Integer;
  VCategory: ICategory;
begin
  FCategoryList := FCategoryDB.GetCategoriesList;
  CategoryListToStrings(FCategoryList, CBKateg.Items);
  CBKateg.Sorted := True;
  if ACategory <> nil then begin
    for i := 0 to CBKateg.Items.Count - 1 do begin
      VCategory := ICategory(Pointer(CBKateg.Items.Objects[i]));
      if VCategory <> nil then begin
        if VCategory.Name = ACategory.Name then begin
          CBKateg.ItemIndex := i;
          Break;
        end;
      end;
    end;
  end else begin
    VCategory := FCategoryDB.Factory.CreateNew(FLastUsedCategoryName);
    CBKateg.Text := VCategory.Name;
    CBKateg.ItemIndex := -1;
  end;
end;

end.
