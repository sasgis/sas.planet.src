unit u_MarksSystem;

interface

uses
  Windows,
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  i_MarksFactoryConfig,
  i_MarkCategory,
  i_MarkCategoryFactoryConfig,
  i_MarkCategoryDB,
  i_MarkCategoryDBSmlInternal,
  u_MarksDb,
  u_MarkCategoryDB;

type
  TMarksSystem = class
  private
    FBasePath: string;
    FMarksFactoryConfig: IMarksFactoryConfig;
    FMarksDb: TMarksDb;
    FCategoryDB: IMarkCategoryDB;
    FCategoryDBInternal: IMarkCategoryDBSmlInternal;
  public
    constructor Create(
      ABasePath: string;
      AMarkPictureList: IMarkPictureList;
      AHintConverter: IHtmlToHintTextConverter;
      ACategoryFactoryConfig: IMarkCategoryFactoryConfig
    );
    destructor Destroy; override;

    procedure ReadConfig(AConfigData: IConfigDataProvider);
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider);

    property MarksDb: TMarksDb read FMarksDb;
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

constructor TMarksSystem.Create(
  ABasePath: string;
  AMarkPictureList: IMarkPictureList;
  AHintConverter: IHtmlToHintTextConverter;
  ACategoryFactoryConfig: IMarkCategoryFactoryConfig
);
var
  VCategoryDb: TMarkCategoryDB;
begin
  FBasePath := ABasePath;
  VCategoryDB := TMarkCategoryDB.Create(ABasePath, ACategoryFactoryConfig);
  FCategoryDB := VCategoryDb;
  FCategoryDBInternal := VCategoryDb;
  FMarksFactoryConfig :=
    TMarksFactoryConfig.Create(
      FCategoryDBInternal,
      AMarkPictureList
    );
  FMarksDb :=
    TMarksDb.Create(
      ABasePath,
      FCategoryDBInternal,
      AHintConverter,
      FMarksFactoryConfig
    );
end;

destructor TMarksSystem.Destroy;
begin
  FreeAndNil(FMarksDb);
  FCategoryDB := nil;
  FCategoryDBInternal := nil;
  FMarksFactoryConfig := nil;
  inherited;
end;

procedure TMarksSystem.DeleteCategoryWithMarks(ACategory: IMarkCategory);
begin
  FMarksDb.DeleteMarksByCategoryID(ACategory);
  FCategoryDB.DeleteCategory(ACategory);
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
    if
      (VCategory.visible) and
      (VCategory.AfterScale <= AZoom + 1) and
      (VCategory.BeforeScale >= AZoom + 1)
    then begin
      Result.Add(VCategory);
    end;
  end;
end;

procedure TMarksSystem.ReadConfig(AConfigData: IConfigDataProvider);
begin
  FMarksDb.LoadMarksFromFile;
  FCategoryDBInternal.LoadCategoriesFromFile;
  FMarksFactoryConfig.ReadConfig(AConfigData);
end;

procedure TMarksSystem.WriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  FMarksFactoryConfig.WriteConfig(AConfigData);
  FCategoryDBInternal.SaveCategory2File;
  FMarksDb.SaveMarks2File;
end;

end.



