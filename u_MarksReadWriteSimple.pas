unit u_MarksReadWriteSimple;

interface

uses
  Windows,
  Classes,
  t_GeoTypes,
  t_CommonTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  dm_MarksDb,
  i_IMarkPicture,
  i_MarksSimple,
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
  end;


implementation

uses
  DB,
  SysUtils,
  Contnrs,
  GR32,
  u_MarksSimpleNew;

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



