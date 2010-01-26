unit u_MarksDb;

interface

uses
  SysUtils,
  Classes,
  ActiveX,
  DB,
  DBClient,
  t_GeoTypes,
  i_Marks;

type
  TDMMarksDb = class(TDataModule, IMarksDb)
    CDSKategory: TClientDataSet;
    CDSKategoryid: TAutoIncField;
    CDSKategoryname: TStringField;
    CDSKategoryvisible: TBooleanField;
    CDSKategoryAfterScale: TSmallintField;
    CDSKategoryBeforeScale: TSmallintField;
    CDSmarks: TClientDataSet;
    CDSmarksid: TAutoIncField;
    CDSmarksname: TStringField;
    CDSmarksdescr: TMemoField;
    CDSmarksscale1: TIntegerField;
    CDSmarksscale2: TIntegerField;
    CDSmarkslonlatarr: TBlobField;
    CDSmarkslonL: TFloatField;
    CDSmarkslatT: TFloatField;
    CDSmarksLonR: TFloatField;
    CDSmarksLatB: TFloatField;
    CDSmarkscolor1: TIntegerField;
    CDSmarkscolor2: TIntegerField;
    CDSmarksvisible: TBooleanField;
    CDSmarkspicname: TStringField;
    CDSmarkscategoryid: TIntegerField;
  private
    function GetMarkIconsPath: string;
    function GetMarksBackUpFileName: string;
    function GetMarksCategoryBackUpFileName: string;
    function GetMarksCategoryFileName: string;
    function GetMarksFileName: string;

  public
    function AddMark(AMark: IMarkBasic): integer;
    function GetMark(IdMark: integer): IMarkBasic;
    procedure DeleteMark(IdMark: integer);
    procedure ReplaceMark(IdMark: integer; AMark: IMarkBasic);

    function AddCategory(ACategory: IMarkCategory): integer;
    function GetCategory(IdCategory: integer): IMarkCategory;
    procedure DeleteCategory(IdCategory: integer);
    procedure ReplaceCategory(IdCategory: integer; ACategory: IMarkCategory);

    function GetAllCategories: IEnumUnknown;
    function GetVisibleCategories: IEnumUnknown;
    function GetAllMarksOfCategory(ACategoryId: integer): IEnumUnknown;
    function GetMarksVisibleInRect(ALonLat: TExtendedRect): IEnumUnknown;

    // Путь к иконкам меток
    property MarkIconsPath: string read GetMarkIconsPath;
    // Имя файла с метками
    property MarksFileName: string read GetMarksFileName;
    // Име резервной копии файла с метками
    property MarksBackUpFileName: string read GetMarksBackUpFileName;

    // Имя файла с категориями меток
    property MarksCategoryFileName: string read GetMarksCategoryFileName;
    // Име резервной копии файла с категориями меток
    property MarksCategoryBackUpFileName: string read GetMarksCategoryBackUpFileName;
  end;

var
  DMMarksDb: TDMMarksDb;

implementation

uses
  u_GlobalState;
{$R *.dfm}

{ TDataModule3 }

function TDMMarksDb.AddCategory(ACategory: IMarkCategory): integer;
begin
  CDSKategory.Insert;
  CDSKategoryname.AsString := ACategory.Name;
  CDSKategoryvisible.AsBoolean := ACategory.Visible;
  CDSKategoryAfterScale.AsInteger := ACategory.AfterScale;
  CDSKategoryBeforeScale.AsInteger := ACategory.BeforeScale;
  CDSKategory.Post;
  Result := CDSKategoryid.Value;
  CDSKategory.SaveToFile(MarksCategoryFileName,dfXMLUTF8);
end;

function TDMMarksDb.AddMark(AMark: IMarkBasic): integer;
begin

end;

procedure TDMMarksDb.DeleteCategory(IdCategory: integer);
begin

end;

procedure TDMMarksDb.DeleteMark(IdMark: integer);
begin

end;

function TDMMarksDb.GetAllCategories: IEnumUnknown;
begin

end;

function TDMMarksDb.GetAllMarksOfCategory(
  ACategoryId: integer): IEnumUnknown;
begin

end;

function TDMMarksDb.GetCategory(IdCategory: integer): IMarkCategory;
begin

end;

function TDMMarksDb.GetMark(IdMark: integer): IMarkBasic;
begin

end;

function TDMMarksDb.GetMarkIconsPath: string;
begin
  Result := GState.MarkIconsPath;
end;

function TDMMarksDb.GetMarksBackUpFileName: string;
begin
  Result := GState.MarksBackUpFileName;
end;

function TDMMarksDb.GetMarksCategoryBackUpFileName: string;
begin
  Result := GState.MarksCategoryBackUpFileName;
end;

function TDMMarksDb.GetMarksCategoryFileName: string;
begin
  Result := GState.MarksCategoryFileName;
end;

function TDMMarksDb.GetMarksFileName: string;
begin
  Result := GState.MarksFileName;
end;

function TDMMarksDb.GetMarksVisibleInRect(
  ALonLat: TExtendedRect): IEnumUnknown;
begin

end;

function TDMMarksDb.GetVisibleCategories: IEnumUnknown;
begin

end;

procedure TDMMarksDb.ReplaceCategory(IdCategory: integer;
  ACategory: IMarkCategory);
begin

end;

procedure TDMMarksDb.ReplaceMark(IdMark: integer; AMark: IMarkBasic);
begin

end;

end.
