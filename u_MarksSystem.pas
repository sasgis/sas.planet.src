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

unit u_MarksSystem;

interface

uses
  Windows,
  Classes,
  i_PathConfig,
  i_LanguageManager,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_VectorItemsFactory,
  i_InternalPerformanceCounter,
  i_ReadWriteState,
  i_Listener,
  i_MarksSimple,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  i_MarksFactoryConfig,
  i_MarkCategory,
  i_MarkCategoryFactoryConfig,
  i_MarkCategoryDB,
  i_MarkCategoryDBSmlInternal,
  i_MarksDb,
  i_MarksSystem,
  i_MarksDbSmlInternal,
  i_StaticTreeItem,
  i_StaticTreeBuilder,
  u_BaseInterfacedObject;

type
  TMarksSystem = class(TBaseInterfacedObject, IMarksSystem)
  private
    FBasePath: IPathConfig;
    FMarksFactoryConfig: IMarksFactoryConfig;
    FState: IReadWriteStateChangeble;
    FCategoryFactoryConfig: IMarkCategoryFactoryConfig;
    FFactoryConfigListener: IListener;
    FLanguageManager: ILanguageManager;
    FMarkPictureList: IMarkPictureList;
    FVectorItemsFactory: IVectorItemsFactory;
    FPerfCounterList: IInternalPerformanceCounterList;
    FHintConverter: IHtmlToHintTextConverter;
    FDBFilename: String;
    FMarksDb: IMarksDb;
    FMarksDbInternal: IMarksDbSmlInternal;
    FCategoryDB: IMarkCategoryDB;
    FCategoryDBInternal: IMarkCategoryDBSmlInternal;
    FCategoryTreeBuilder: IStaticTreeBuilder;
    FMarksSubsetTreeBuilder: IStaticTreeBuilder;
  private
    procedure InternalCloseMarksDBs;
    procedure InternalOpenMarksDBs(const ADBFileName: String);
    procedure OnFactoryConfigChange;
  private
    function GetState: IReadWriteStateChangeble;
    function GetMarksDb: IMarksDb;
    function GetCategoryDB: IMarkCategoryDB;
    function GetMarksFactoryConfig: IMarksFactoryConfig;

    function GetMarkByStringId(const AId: string): IMark;

    function GetVisibleCategories(AZoom: Byte): IInterfaceList;
    function GetVisibleCategoriesIgnoreZoom: IInterfaceList;
    procedure DeleteCategoryWithMarks(const ACategory: IMarkCategory);

    function MarksSubsetToStaticTree(const ASubset: IMarksSubset): IStaticTreeItem;
    function CategoryListToStaticTree(const AList: IInterfaceList): IStaticTreeItem;

    procedure ReadConfig(const AConfigData: IConfigDataProvider);
    procedure WriteConfig(const AConfigData: IConfigDataWriteProvider);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ABasePath: IPathConfig;
      const AMarkPictureList: IMarkPictureList;
      const AVectorItemsFactory: IVectorItemsFactory;
      const APerfCounterList: IInternalPerformanceCounterList;
      const AHintConverter: IHtmlToHintTextConverter;
      const ACategoryFactoryConfig: IMarkCategoryFactoryConfig
    );
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  ActiveX,
  SQLite3Handler,
  u_StaticTreeBuilderBase,
  u_ReadWriteStateInternal,
  u_ListenerByEvent,
  u_MarksSQLDb,
  u_MarksDb,
  u_MarkCategoryDB,
  u_MarksFactoryConfig;

type
  TStaticTreeByCategoryListBuilder = class(TStaticTreeBuilderBaseBySlash)
  protected
    procedure ProcessItems(
      const ASource: IInterface;
      AList: TStringList
    ); override;
    function GetNameFromItem(
      const ASource: IInterface;
      const AItem: IInterface
    ): string; override;
  end;

{ TStaticTreeByCategoryListBuilder }

function TStaticTreeByCategoryListBuilder.GetNameFromItem(
  const ASource: IInterface;
  const AItem: IInterface
): string;
begin
  Result := (AItem as ICategory).Name;
end;

procedure TStaticTreeByCategoryListBuilder.ProcessItems(
  const ASource: IInterface;
  AList: TStringList
);
var
  VList: IInterfaceList;
  i: Integer;
begin
  inherited;
  VList := ASource as IInterfaceList;
  for i := 0 to VList.Count - 1 do begin
    ProcessItem(ASource, VList.Items[i], AList);
  end;
end;

type
  TStaticTreeByMarksSubsetBuilder = class(TStaticTreeBuilderBaseBySlash)
  protected
    procedure ProcessItems(
      const ASource: IInterface;
      AList: TStringList
    ); override;
    function GetNameFromItem(
      const ASource: IInterface;
      const AItem: IInterface
    ): string; override;
  end;

{ TStaticTreeByMarksSubsetBuilder }

function TStaticTreeByMarksSubsetBuilder.GetNameFromItem(
  const ASource: IInterface;
  const AItem: IInterface
): string;
var
  VMark: IMark;
begin
  VMark := AItem as IMark;
  if VMark.Category <> nil then begin
    Result := VMark.Category.Name + LevelsSeparator + VMark.Name;
  end else begin
    Result := LevelsSeparator + VMark.Name;
  end;
end;

procedure TStaticTreeByMarksSubsetBuilder.ProcessItems(
  const ASource: IInterface;
  AList: TStringList
);
var
  VSubset: IMarksSubset;
  VEnum: IEnumUnknown;
  VMark: IMark;
  i: Cardinal;
begin
  inherited;
  VSubset := ASource as IMarksSubset;
  VEnum := VSubset.GetEnum;
  while (VEnum.Next(1, VMark, @i) = S_OK) do begin
    ProcessItem(ASource, VMark, AList);
  end;
end;

{ TMarksSystem }

constructor TMarksSystem.Create(
  const ALanguageManager: ILanguageManager;
  const ABasePath: IPathConfig;
  const AMarkPictureList: IMarkPictureList;
  const AVectorItemsFactory: IVectorItemsFactory;
  const APerfCounterList: IInternalPerformanceCounterList;
  const AHintConverter: IHtmlToHintTextConverter;
  const ACategoryFactoryConfig: IMarkCategoryFactoryConfig
);
begin
  inherited Create;
  FBasePath := ABasePath;
  FDBFilename := '';
  
  FCategoryFactoryConfig := ACategoryFactoryConfig;
  FFactoryConfigListener := TNotifyNoMmgEventListener.Create(Self.OnFactoryConfigChange);
  FCategoryFactoryConfig.ChangeNotifier.Add(FFactoryConfigListener);

  FLanguageManager := ALanguageManager;
  FMarkPictureList := AMarkPictureList;
  FVectorItemsFactory := AVectorItemsFactory;
  FPerfCounterList := APerfCounterList;
  FHintConverter := AHintConverter;
  
  InternalOpenMarksDBs('');

  FCategoryTreeBuilder := TStaticTreeByCategoryListBuilder.Create('\', '');
  FMarksSubsetTreeBuilder := TStaticTreeByMarksSubsetBuilder.Create('\', '');
end;

destructor TMarksSystem.Destroy;
begin
  InternalCloseMarksDBs;

  FHintConverter := nil;
  FPerfCounterList := nil;
  FVectorItemsFactory := nil;
  FMarkPictureList := nil;
  FLanguageManager := nil;

  if (FFactoryConfigListener <> nil) then begin
    FCategoryFactoryConfig.ChangeNotifier.Remove(FFactoryConfigListener);
    FFactoryConfigListener := nil;
  end;

  FCategoryFactoryConfig := nil;

  inherited;
end;

function TMarksSystem.CategoryListToStaticTree(
  const AList: IInterfaceList
): IStaticTreeItem;
begin
  Result := FCategoryTreeBuilder.BuildStatic(AList);
end;

procedure TMarksSystem.DeleteCategoryWithMarks(const ACategory: IMarkCategory);
var
  VMarkIdList: IInterfaceList;
  VCategoryDB: IMarkCategoryDB;
begin
  // если БД категорий и БД меток - это один объект, то зовём его сразу,
  // а внутри он сам знает, удаляются каскадно метки в категории, или нет
  if Supports(FMarksDb, IMarkCategoryDB, VCategoryDB) and (VCategoryDB=FCategoryDB) then begin
    FCategoryDB.UpdateCategory(ACategory, nil);
  end else begin
    // БД категорий и меток разные - нет выхода
    VMarkIdList := FMarksDb.GetMarksIdListByCategory(ACategory);
    FMarksDb.UpdateMarksList(VMarkIdList, nil);
    FCategoryDB.UpdateCategory(ACategory, nil);
  end;
end;

function TMarksSystem.GetCategoryDB: IMarkCategoryDB;
begin
  Result := FCategoryDB;
end;

function TMarksSystem.GetMarkByStringId(const AId: string): IMark;
var
  VId: Integer;
begin
  Result := nil;
  if AId <> '' then begin
    if TryStrToInt(AId, VId) then begin
      if not Supports(FMarksDbInternal.GetById(VId), IMark, Result) then begin
        Result := nil;
      end;
    end;
  end;
end;

function TMarksSystem.GetMarksDb: IMarksDb;
begin
  Result := FMarksDb;
end;

function TMarksSystem.GetMarksFactoryConfig: IMarksFactoryConfig;
begin
  Result := FMarksFactoryConfig;
end;

function TMarksSystem.GetState: IReadWriteStateChangeble;
begin
  Result := FState;
end;

function TMarksSystem.GetVisibleCategories(AZoom: Byte): IInterfaceList;
var
  VList: IInterfaceList;
  VCategory: IMarkCategory;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  VList := FCategoryDB.GetCategoriesList;
  for i := 0 to VList.Count - 1 do begin
    VCategory := IMarkCategory(VList[i]);
    if (VCategory.Visible) and
      (VCategory.AfterScale <= AZoom + 1) and
      (VCategory.BeforeScale >= AZoom + 1) then begin
      Result.Add(VCategory);
    end;
  end;
end;

function TMarksSystem.GetVisibleCategoriesIgnoreZoom: IInterfaceList;
var
  VList: IInterfaceList;
  VCategory: IMarkCategory;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  VList := FCategoryDB.GetCategoriesList;
  for i := 0 to VList.Count - 1 do begin
    VCategory := IMarkCategory(VList[i]);
    if VCategory.Visible then begin
      Result.Add(VCategory);
    end;
  end;
end;

procedure TMarksSystem.InternalCloseMarksDBs;
begin
  FMarksDb := nil;
  FMarksDbInternal := nil;
  FCategoryDB := nil;
  FCategoryDBInternal := nil;
  FMarksFactoryConfig := nil;
  FState := nil;
end;

procedure TMarksSystem.InternalOpenMarksDBs(const ADBFileName: String);
var
  VCategoryDb: TMarkCategoryDB;
  VMarksDb: TMarksDb;
  VState: TReadWriteStateInternal;
  VMarksSQLDb: TMarksSQLDb;
begin
  VState := TReadWriteStateInternal.Create;
  FState := VState;

  // имя файла БД
  FDBFilename := ADBFileName;
  if SameText(ExtractFileExt(FDBFilename), c_SQLite_Ext) then begin
    // метки в БД SQLite3
    VMarksSQLDb := TMarksSQLDb.Create(
      VState,
      FBasePath,
      FDBFilename,
      FLanguageManager,
      FMarkPictureList,
      FPerfCounterList.CreateAndAddNewSubList('MarksSQLite'),
      FVectorItemsFactory,
      FHintConverter,
      FCategoryFactoryConfig
    );

    FCategoryDB := VMarksSQLDb;
    FCategoryDBInternal := VMarksSQLDb;

    FMarksFactoryConfig := VMarksSQLDb.MarksFactoryConfig;

    FMarksDb := VMarksSQLDb;
    FMarksDbInternal := VMarksSQLDb;
  end else begin
    // метки по умолчанию в паре файлов SML
    VCategoryDb := TMarkCategoryDB.Create(VState, FBasePath, FCategoryFactoryConfig);
    FCategoryDB := VCategoryDb;
    FCategoryDBInternal := VCategoryDb;

    FMarksFactoryConfig :=
      TMarksFactoryConfig.Create(
        FLanguageManager,
        FCategoryDBInternal,
        FMarkPictureList
      );

    VMarksDb :=
      TMarksDb.Create(
        VState,
        FBasePath,
        FCategoryDBInternal,
        FPerfCounterList.CreateAndAddNewSubList('MarksDb'),
        FVectorItemsFactory,
        FHintConverter,
        FMarksFactoryConfig
      );
    FMarksDb := VMarksDb;
    FMarksDbInternal := VMarksDb;
  end;
end;

function TMarksSystem.MarksSubsetToStaticTree(
  const ASubset: IMarksSubset
): IStaticTreeItem;
begin
  Result := FMarksSubsetTreeBuilder.BuildStatic(ASubset);
end;

procedure TMarksSystem.OnFactoryConfigChange;
var
  VDBFileName: String;
  VCopyMarks: Boolean;
  // ссылки, чтобы реализовать жизнь после смерти
  VMarksDb: IMarksDb;
  VMarksDbInternal: IMarksDbSmlInternal;
  VCategoryDB: IMarkCategoryDB;
  VCategoryDBInternal: IMarkCategoryDBSmlInternal;
  VMarksFactoryConfig: IMarksFactoryConfig;
  VState: IReadWriteStateChangeble;
  // данные
  VOldCategoryList: IInterfaceList;
  VOldMarksList: IInterfaceList;
begin
  // настройка имени файла БД
  VDBFileName := FCategoryFactoryConfig.DBFileName;
  // тупенько, просто и без лишних полей в ini-шке
  // импорт из старой БД запускаем, если в начале символ потокового копирования
  VCopyMarks := (0<Length(VDBFileName)) and (VDBFileName[1]='>');
  if VCopyMarks then begin
    System.Delete(VDBFileName,1,1);
  end;

  if (nil=FCategoryDB) then begin
    // если по какой-то причине БД меток не открыта - всегда её (пере)открываем
    // обычно сюда не должно попадать - так что подчищаем все хвосты
    InternalCloseMarksDBs;
    InternalOpenMarksDBs(VDBFileName);
    Exit;
  end;

  // БД меток уже открыта

  // если то же самое имя файла - ничего не делаем совсем
  if SameText(FDBFilename,VDBFileName) then
    Exit;

  // тут будем, если требуется закрыть старую БД и открыть новую другую БД
  if (not VCopyMarks) then begin
    // однако импорт старья не разрешён
    InternalCloseMarksDBs;
    InternalOpenMarksDBs(VDBFileName);
    Exit;
  end;

  // а тут окажемся, если надо импортировать данные из старой БД

  // сохраним все ссылки локально
  VMarksDb := FMarksDb;
  VMarksDbInternal := FMarksDbInternal;
  VCategoryDB := FCategoryDB;
  VCategoryDBInternal := FCategoryDBInternal;
  VMarksFactoryConfig := FMarksFactoryConfig;
  VState := FState;

  // читаем все категории и метки
  VCategoryDBInternal.LoadCategoriesFromFile;
  VOldCategoryList := VCategoryDB.GetCategoriesList;
  VMarksDbInternal.LoadMarksFromFile;
  VOldMarksList := VMarksDb.GetAllMarksIdList;

  // переключаем БД меток на новое хранилище
  InternalCloseMarksDBs;
  InternalOpenMarksDBs(VDBFileName);

  // импортируем все категории в новое хранилище
  FCategoryDB.ImportCategoriesList(VOldCategoryList);

  // импортируем все метки в новое хранилище
  FMarksDb.ImportMarksList(VOldMarksList, []);

  // успешно импортировались - умирают локальные интерфейсы
  // заодно попробуем предотвратить ошибку повторного импорта
  FCategoryFactoryConfig.DBFileName := VDBFileName;
end;

procedure TMarksSystem.ReadConfig(const AConfigData: IConfigDataProvider);
begin
  FMarksFactoryConfig.ReadConfig(AConfigData);
  FCategoryDBInternal.LoadCategoriesFromFile;
  FMarksDbInternal.LoadMarksFromFile;
end;

procedure TMarksSystem.WriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  FMarksFactoryConfig.WriteConfig(AConfigData);
  FCategoryDBInternal.SaveCategory2File;
  FMarksDbInternal.SaveMarks2File;
end;

end.
