unit u_MarksReadWriteSimple;

interface

uses
  Windows,
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  dm_MarksDb,
  i_IMarksFactoryConfig,
  i_IMarkCategory,
  i_IMarkCategoryFactoryConfig,
  u_MarksOnlyDb,
  u_MarkCategoryDB,
  u_MarksSimple;

type

  TMarksDB = class
  private
    FBasePath: string;
    FDMMarksDb: TDMMarksDb;
    FMarksDb: TMarksOnlyDb;
    FCategoryDB: TMarkCategoryDB;
  public
    constructor Create(
      ABasePath: string;
      AMarkFactoryConfig: IMarksFactoryConfig;
      ACategoryFactoryConfig: IMarkCategoryFactoryConfig
    );
    destructor Destroy; override;

    procedure ReadConfig(AConfigData: IConfigDataProvider);
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider);

    property MarksDb: TMarksOnlyDb read FMarksDb;
    property CategoryDB: TMarkCategoryDB read FCategoryDB;
    function GetVisibleCategories(AZoom: Byte): IInterfaceList;
    procedure DeleteCategoryWithMarks(ACategory: IMarkCategory);
  end;


implementation

uses
  SysUtils;

{ TMarksDB }

constructor TMarksDB.Create(
  ABasePath: string;
  AMarkFactoryConfig: IMarksFactoryConfig;
  ACategoryFactoryConfig: IMarkCategoryFactoryConfig
);
begin
  FBasePath := ABasePath;
  FDMMarksDb := TDMMarksDb.Create(nil);
  FMarksDb := TMarksOnlyDb.Create(ABasePath, FDMMarksDb, AMarkFactoryConfig);
  FCategoryDB := TMarkCategoryDB.Create(ABasePath, FDMMarksDb, ACategoryFactoryConfig);
end;

procedure TMarksDB.DeleteCategoryWithMarks(ACategory: IMarkCategory);
begin
  FMarksDb.DeleteMarksByCategoryID(ACategory);
  FCategoryDB.DeleteCategory(ACategory);
end;

destructor TMarksDB.Destroy;
begin
  FreeAndNil(FMarksDb);
  FreeAndNil(FCategoryDB);
  FreeAndNil(FDMMarksDb);
  inherited;
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
  FCategoryDB.LoadCategoriesFromFile;
end;

procedure TMarksDB.WriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  FCategoryDB.SaveCategory2File;
  FMarksDb.SaveMarks2File;
end;

end.



