{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_VectorItemTreeMarksDb;

interface

uses
  i_NotifierOperation,
  i_VectorItemTree,
  i_VectorItemTreeExporter,
  i_VectorItemTreeImporter,
  i_AppearanceOfMarkFactory,
  i_ReadWriteState,
  i_MarkFactory,
  i_MarkSystemImpl,
  i_ImportConfig,
  i_MarkCategoryFactory,
  i_MarkSystemImplFactory,
  u_BaseInterfacedObject;

type
  TVectorItemTreeMarksDb = class(
    TBaseInterfacedObject,
    IVectorItemTreeExporter,
    IVectorItemTreeImporter
  )
  private
    FDatabase: TGUID;
    FMarkFactory: IMarkFactory;
    FCategoryFactory: IMarkCategoryFactory;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic;
  private
    function _GetImpl(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string
    ): IMarkSystemImpl;
    function _GetConfigForExporter: IImportConfig;
  private
    { IVectorItemTreeExporter }
    procedure ProcessExport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const ATree: IVectorItemTree
    );
    { IVectorItemTreeImporter }
    function ProcessImport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      var AConfig: IInterface
    ): IVectorItemTree;
  public
    constructor Create(
      const ADatabase: TGUID;
      const AMarkFactory: IMarkFactory;
      const ACategoryFactory: IMarkCategoryFactory;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic
    );
  end;

implementation

uses
  SysUtils,
  c_MarkSystem,
  t_CommonTypes,
  i_MarkCategoryList,
  i_MarkSystemImplConfig,
  u_MarkSystemImplConfigSML,
  u_MarkSystemImplConfigORM,
  u_Category,
  u_ImportConfig,
  u_MarkSystemHelpers;

{ TVectorItemTreeExporterMarksDb }

constructor TVectorItemTreeMarksDb.Create(
  const ADatabase: TGUID;
  const AMarkFactory: IMarkFactory;
  const ACategoryFactory: IMarkCategoryFactory;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic
);
begin
  inherited Create;
  FDatabase := ADatabase;
  FMarkFactory := AMarkFactory;
  FCategoryFactory := ACategoryFactory;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FMarkSystemImplFactoryListStatic := AMarkSystemImplFactoryListStatic;
end;

function TVectorItemTreeMarksDb._GetConfigForExporter: IImportConfig;
begin
  Result := TImportConfig.Create(

    TCategory.Create(''),

    TImportCategoryParams.Create(True, False, False, False), // ToDo

    TImportPointParams.Create(
      FAppearanceOfMarkFactory.CreatePointAppearance(0, 0, 0, '', nil, 0),
      False, False, False, False, False
    ),

    TImportLineParams.Create(
      FAppearanceOfMarkFactory.CreateLineAppearance(0, 0),
      False, False
    ),

    TImportPolyParams.Create(
      FAppearanceOfMarkFactory.CreatePolygonAppearance(0, 0, 0),
      False, False, False
    )
  );
end;

function TVectorItemTreeMarksDb._GetImpl(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string
): IMarkSystemImpl;
var
  VConfig: IMarkSystemImplConfigStatic;
  VElement: IMarkSystemImplFactoryListElement;
begin
  Result := nil;

  if IsEqualGUID(FDatabase, cSMLMarksDbGUID) then begin
    VConfig := TMarkSystemImplConfigSML.Create(AFileName, False);
  end else if IsEqualGUID(FDatabase, cORMSQLiteMarksDbGUID) then begin
    VConfig := TMarkSystemImplConfigORM.Create(AFileName, False, '', '', '');
  end else begin
    Assert(False);
    Exit;
  end;

  VElement := FMarkSystemImplFactoryListStatic.Get(FDatabase);
  if not Assigned(VElement) then begin
    Assert(False);
    Exit;
  end;

  Result :=
    VElement.Factory.Build(
      AOperationID,
      ACancelNotifier,
      ExtractFilePath(AFileName),
      VConfig
    );
end;

procedure TVectorItemTreeMarksDb.ProcessExport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const ATree: IVectorItemTree
);
var
  VImpl: IMarkSystemImpl;
begin
  VImpl := _GetImpl(AOperationID, ACancelNotifier, AFileName);
  if Assigned(VImpl) and (VImpl.State.GetStatic.WriteAccess = asEnabled) then begin
    ImportItemsTree(
      ATree,
      _GetConfigForExporter,
      VImpl.MarkDb,
      FMarkFactory,
      VImpl.CategoryDB,
      FCategoryFactory
    );
  end;
end;

function TVectorItemTreeMarksDb.ProcessImport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  var AConfig: IInterface
): IVectorItemTree;
var
  VImpl: IMarkSystemImpl;
  VCategoiesList: IMarkCategoryList;
begin
  Result := nil;
  VImpl := _GetImpl(AOperationID, ACancelNotifier, AFileName);
  if Assigned(VImpl) and (VImpl.State.GetStatic.ReadAccess = asEnabled) then begin
    VCategoiesList := VImpl.CategoryDB.GetCategoriesList;
    if Assigned(VCategoiesList) then begin
      Result := CategoryTreeToMarkTreeHelper(
        VImpl.MarkDb,
        CategoryListToCategoryTree(VCategoiesList),
        True {IncludeHiddenMarks}
      );
    end;
  end;
end;

end.
