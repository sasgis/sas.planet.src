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

unit u_MarkSystemHelpers;

interface

uses
  i_MarkDbImpl,
  i_MarkFactory,
  i_MarkCategoryDBImpl,
  i_MarkCategoryFactory,
  i_StaticTreeItem,
  i_VectorItemTree,
  i_InterfaceListSimple,
  i_ImportConfig;

procedure PrepareFromTreeForImport(
  const AMarkList: IInterfaceListSimple;
  const ADataItemTree: IVectorItemTree;
  const AImportConfig: IImportConfig;
  const AMarkDB: IMarkDBImpl;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDBImpl;
  const ACategoryFactory: IMarkCategoryFactory;
  const AParentCategoryName: string = ''
);

function CategoryTreeToMarkTreeHelper(
  const AMarkDB: IMarkDBImpl;
  const ACategoryTree: IStaticTreeItem;
  const AIncludeHiddenMarks: Boolean
): IVectorItemTree;

implementation

uses
  SysUtils,
  i_Category,
  i_MarkCategory,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_InterfaceListStatic,
  i_GeometryLonLat,
  u_VectorItemTree,
  u_InterfaceListSimple;

procedure PrepareFromTreeForImport( // ToDo: придумать более внятное название
  const AMarkList: IInterfaceListSimple;
  const ADataItemTree: IVectorItemTree;
  const AImportConfig: IImportConfig;
  const AMarkDB: IMarkDBImpl;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDBImpl;
  const ACategoryFactory: IMarkCategoryFactory;
  const AParentCategoryName: string
);
var
  I: Integer;
  VItem: IVectorDataItem;
  VMark: IVectorDataItem;
  VName: string;
  VCategoryName: string;
  VCategory: ICategory;
  VMarkCategory: IMarkCategory;
  VParams: IImportMarkParams;
begin
  {ToDo: http://sasgis.org/mantis/view.php?id=2143}

  VCategory := AImportConfig.RootCategory;

  VCategoryName := AParentCategoryName;
  if VCategoryName = '' then begin
    VCategoryName := VCategory.Name;
  end;

  if Assigned(ADataItemTree) and (ADataItemTree.Name <> '') then begin
    VCategoryName := VCategoryName + '\' + ADataItemTree.Name;
    VMarkCategory := ACategoryDB.GetCategoryByName(VCategoryName);
    if not Assigned(VMarkCategory) then begin
      VMarkCategory := ACategoryFactory.CreateNew(VCategoryName);
      VMarkCategory := ACategoryDB.UpdateCategory(nil, VMarkCategory);
    end;
    VCategory := VMarkCategory;
  end;

  if Assigned(ADataItemTree.Items) then begin
    for I := 0 to ADataItemTree.Items.Count - 1 do begin
      VMark := nil;
      VItem := ADataItemTree.Items.Items[I];
      VName := VItem.Name;
      if (VName = '') and (ADataItemTree.Name <> '') then begin
        if ADataItemTree.Items.Count > 1 then begin
          VName := ADataItemTree.Name + '-' + IntToStr(I + 1);
        end else begin
          VName := ADataItemTree.Name;
        end;
      end else begin
        if AImportConfig.CategoryParams.IsIgnoreMarkIfExistsWithSameNameInCategory then begin
          if AMarkDB.GetMarkByName(VName, VCategory) <> nil then begin
            Continue;
          end;
        end;
      end;
      VParams := nil;
      if Supports(VItem.Geometry, IGeometryLonLatPoint) then begin
        VParams := AImportConfig.PointParams;
      end else if Supports(VItem.Geometry, IGeometryLonLatLine) then begin
        VParams := AImportConfig.LineParams;
      end else if Supports(VItem.Geometry, IGeometryLonLatPolygon) then begin
        VParams := AImportConfig.PolyParams;
      end;
      VMark :=
        AMarkFactory.PrepareMark(
          VItem,
          VName,
          VParams,
          VCategory
        );
      if VMark <> nil then begin
        AMarkList.Add(VMark);
      end;
    end;
  end;
  for I := 0 to ADataItemTree.SubTreeItemCount - 1 do begin
    PrepareFromTreeForImport(
      AMarkList,
      ADataItemTree.GetSubTreeItem(I),
      AImportConfig,
      AMarkDB,
      AMarkFactory,
      ACategoryDB,
      ACategoryFactory,
      VCategoryName
    );
  end;
end;

function CategoryTreeToMarkTreeHelper(
  const AMarkDB: IMarkDBImpl;
  const ACategoryTree: IStaticTreeItem;
  const AIncludeHiddenMarks: Boolean
): IVectorItemTree;
var
  VCategory: IMarkCategory;
  VMarkSubset: IVectorItemSubset;
  VSubItems: IInterfaceListStatic;
  i: Integer;
  VTemp: IInterfaceListSimple;
begin
  Assert(Assigned(AMarkDB));
  Assert(Assigned(ACategoryTree));

  Result := nil;
  VMarkSubset := nil;

  if Assigned(ACategoryTree) then begin
    if Supports(ACategoryTree.Data, IMarkCategory, VCategory) then begin
      VMarkSubset := AMarkDb.GetMarkSubsetByCategory(VCategory, AIncludeHiddenMarks);
    end;

    VSubItems := nil;
    if ACategoryTree.SubItemCount > 0 then begin
      VTemp := TInterfaceListSimple.Create;
      for i := 0 to ACategoryTree.SubItemCount - 1 do begin
        VTemp.Add(
          CategoryTreeToMarkTreeHelper(
            AMarkDB,
            ACategoryTree.SubItem[i],
            AIncludeHiddenMarks
          )
        );
      end;
      VSubItems := VTemp.MakeStaticAndClear;
    end;

    Result :=
      TVectorItemTree.Create(
        ACategoryTree.Name,
        VMarkSubset,
        VSubItems
      );
  end;
end;

end.
