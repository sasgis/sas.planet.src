unit u_MarksDb;

interface

uses
  Windows,
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  dm_MarksDb,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarkCategory,
  i_MarkCategoryFactoryConfig,
  i_MarkCategoryDB,
  i_MarkCategoryDBSmlInternal,
  u_MarksOnlyDb,
  u_MarkCategoryDB;

type

  TMarksDB = class
  private
    FBasePath: string;
    FDMMarksDb: TDMMarksDb;
    FMarksFactoryConfig: IMarksFactoryConfig;
    FMarksDb: TMarksOnlyDb;
    FCategoryDB: IMarkCategoryDB;
    FCategoryDBInternal: IMarkCategoryDBSmlInternal;
  public
    constructor Create(
      ABasePath: string;
      AMarkPictureList: IMarkPictureList;
      ACategoryFactoryConfig: IMarkCategoryFactoryConfig
    );
    destructor Destroy; override;

    procedure ReadConfig(AConfigData: IConfigDataProvider);
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider);

    property MarksDb: TMarksOnlyDb read FMarksDb;
    property CategoryDB: IMarkCategoryDB read FCategoryDB;
    property MarksFactoryConfig: IMarksFactoryConfig read FMarksFactoryConfig;

    function GetVisibleCategories(AZoom: Byte): IInterfaceList;
    procedure DeleteCategoryWithMarks(ACategory: IMarkCategory);
  end;


implementation

uses
  SysUtils,
  u_MarksFactoryConfig;

{ TMarksDB }

constructor TMarksDB.Create(
  ABasePath: string;
  AMarkPictureList: IMarkPictureList;
  ACategoryFactoryConfig: IMarkCategoryFactoryConfig
);
var
  VCategoryDb: TMarkCategoryDB;
begin
  FBasePath := ABasePath;
  FDMMarksDb := TDMMarksDb.Create(nil);
  VCategoryDB := TMarkCategoryDB.Create(ABasePath, FDMMarksDb, ACategoryFactoryConfig);
  FCategoryDB := VCategoryDb;
  FCategoryDBInternal := VCategoryDb;
  FMarksFactoryConfig := TMarksFactoryConfig.Create(FCategoryDBInternal, AMarkPictureList);
  FMarksDb := TMarksOnlyDb.Create(ABasePath, FDMMarksDb, FMarksFactoryConfig);
end;

destructor TMarksDB.Destroy;
begin
  FreeAndNil(FMarksDb);
  FCategoryDB := nil;
  FCategoryDBInternal := nil;
  FMarksFactoryConfig := nil;
  FreeAndNil(FDMMarksDb);
  inherited;
end;

procedure TMarksDB.DeleteCategoryWithMarks(ACategory: IMarkCategory);
begin
  FMarksDb.DeleteMarksByCategoryID(ACategory);
  FCategoryDB.DeleteCategory(ACategory);
end;

function TMarksDB.GetVisibleCategories(AZoom: Byte): IInterfaceList;
var
  VList: IInterfaceList;
  VCategory: IMarkCategory;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  VList := FCategoryDB.GetCategoriesList;
  for i := 0 to VList.Count - 1 do begin
    VCategory := IMarkCategory(VList[i]);
    if
      (VCategory.visible) and
      (VCategory.AfterScale <= AZoom + 1) and
      (VCategory.BeforeScale >= AZoom + 1)
    then begin
      Result.Add(VCategory);
    end;
  end;
end;

procedure TMarksDB.ReadConfig(AConfigData: IConfigDataProvider);
begin
  FMarksDb.LoadMarksFromFile;
  FCategoryDBInternal.LoadCategoriesFromFile;
  FMarksFactoryConfig.ReadConfig(AConfigData);
end;

procedure TMarksDB.WriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  FMarksFactoryConfig.WriteConfig(AConfigData);
  FCategoryDBInternal.SaveCategory2File;
  FMarksDb.SaveMarks2File;
end;

end.



