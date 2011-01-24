unit u_MarksReadWriteSimple;

interface

uses
  Windows,
  Classes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  dm_MarksDb,
  i_IMarkPicture,
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
    constructor Create(ABasePath: string; AMarkPictureList: IMarkPictureList);
    destructor Destroy; override;

    procedure ReadConfig(AConfigData: IConfigDataProvider);
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider);

    property MarksDb: TMarksOnlyDb read FMarksDb;
    property CategoryDB: TMarkCategoryDB read FCategoryDB;
    function GetVisibleCateroriesIDList(AZoom: Byte): TList;
  end;


implementation

uses
  SysUtils;

{ TMarksDB }

constructor TMarksDB.Create(ABasePath: string; AMarkPictureList: IMarkPictureList);
begin
  FBasePath := ABasePath;
  FDMMarksDb := TDMMarksDb.Create(nil);
  FMarksDb := TMarksOnlyDb.Create(ABasePath, AMarkPictureList, FDMMarksDb);
  FCategoryDB := TMarkCategoryDB.Create(ABasePath, FMarksDb, FDMMarksDb);
end;

destructor TMarksDB.Destroy;
begin
  FreeAndNil(FMarksDb);
  FreeAndNil(FCategoryDB);
  FreeAndNil(FDMMarksDb);
  inherited;
end;

function TMarksDB.GetVisibleCateroriesIDList(AZoom: Byte): TList;
var
  VList: TList;
  VCategory: TCategoryId;
  i: Integer;
begin
  Result := TList.Create;
  VList := FCategoryDB.GetCategoriesList;
  try
    for i := 0 to VList.Count - 1 do begin
      VCategory := TCategoryId(VList[i]);
      if
        (VCategory.visible) and
        (VCategory.AfterScale <= AZoom + 1) and
        (VCategory.BeforeScale >= AZoom + 1)
      then begin
        Result.Add(Pointer(VCategory.id));
      end;
    end;
  finally
    VList.Free;
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



