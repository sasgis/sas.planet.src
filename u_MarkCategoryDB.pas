unit u_MarkCategoryDB;

interface

uses
  Windows,
  Classes,
  i_IMarkCategory,
  dm_MarksDb;

type
  TMarkCategoryDB = class
  private
    FBasePath: string;
    FDMMarksDb: TDMMarksDb;
    function ReadCurrentCategory: IMarkCategory;
    procedure WriteCurrentCategory(ACategory: IMarkCategory);
    function GetMarksCategoryBackUpFileName: string;
    function GetMarksCategoryFileName: string;
  public
    function SaveCategory2File: boolean;
    procedure LoadCategoriesFromFile;
  public
    constructor Create(ABasePath: string; ADMMarksDb: TDMMarksDb);

    function GetCategoryByName(AName: string): IMarkCategory;
    function GetCategoryByID(id: integer): IMarkCategory;
    function WriteCategory(ACategory: IMarkCategory): IMarkCategory;
    procedure DeleteCategory(ACategory: IMarkCategory);

    function GetCategoriesList: IInterfaceList;
    procedure SetAllCategoriesVisible(ANewVisible: Boolean);
  end;


implementation

uses
  DB,
  SysUtils,
  u_MarkCategory;

function TMarkCategoryDB.ReadCurrentCategory: IMarkCategory;
var
  VId: Integer;
  VName: string;
  VVisible: Boolean;
  VAfterScale: Integer;
  VBeforeScale: Integer;
begin
  VName := FDMMarksDb.CDSKategory.fieldbyname('name').AsString;
  VId := FDMMarksDb.CDSKategory.fieldbyname('id').AsInteger;
  VVisible := FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean;
  VAfterScale := FDMMarksDb.CDSKategory.fieldbyname('AfterScale').AsInteger;
  VBeforeScale := FDMMarksDb.CDSKategory.fieldbyname('BeforeScale').AsInteger;
  Result := TMarkCategory.Create(VId, VName, VVisible, VAfterScale, VBeforeScale);
end;

procedure TMarkCategoryDB.WriteCurrentCategory(ACategory: IMarkCategory);
begin
  FDMMarksDb.CDSKategory.fieldbyname('name').AsString := ACategory.name;
  FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean := ACategory.visible;
  FDMMarksDb.CDSKategory.fieldbyname('AfterScale').AsInteger := ACategory.AfterScale;
  FDMMarksDb.CDSKategory.fieldbyname('BeforeScale').AsInteger := ACategory.BeforeScale;
end;

function TMarkCategoryDB.WriteCategory(ACategory: IMarkCategory): IMarkCategory;
begin
  if ACategory.id < 0 then begin
    FDMMarksDb.CDSKategory.Insert;
  end else begin
    FDMMarksDb.CDSKategory.Locate('id', ACategory.id, []);
    FDMMarksDb.CDSKategory.Edit;
  end;
  WriteCurrentCategory(ACategory);
  FDMMarksDb.CDSKategory.post;
  Result := TMarkCategory.Create(FDMMarksDb.CDSKategory.fieldbyname('id').AsInteger, ACategory);
  SaveCategory2File;
end;

constructor TMarkCategoryDB.Create(ABasePath: string;
  ADMMarksDb: TDMMarksDb);
begin
  FBasePath := ABasePath;
  FDMMarksDb := ADMMarksDb;
end;

procedure TMarkCategoryDB.DeleteCategory(ACategory: IMarkCategory);
begin
  if FDMMarksDb.CDSKategory.Locate('id', ACategory.id, []) then begin
    FDMMarksDb.CDSKategory.DisableControls;
    try
      if FDMMarksDb.CDSKategory.Locate('id', ACategory.id, []) then begin
        FDMMarksDb.CDSKategory.Delete;
      end;
    finally
      FDMMarksDb.CDSKategory.EnableControls;
    end;
    SaveCategory2File;
  end;
end;

function TMarkCategoryDB.GetCategoryByID(id: integer): IMarkCategory;
begin
  Result := nil;
  if FDMMarksDb.CDSKategory.Locate('id', id, []) then begin
    Result := ReadCurrentCategory;
  end;
end;

function TMarkCategoryDB.GetCategoryByName(AName: string): IMarkCategory;
begin
  Result := nil;
  if FDMMarksDb.CDSKategory.Locate('name', AName, []) then begin
    Result := ReadCurrentCategory;
  end;
end;

procedure TMarkCategoryDB.SetAllCategoriesVisible(ANewVisible: Boolean);
begin
  FDMMarksDb.CDSKategory.DisableControls;
  try
    FDMMarksDb.CDSKategory.Filtered := false;
    FDMMarksDb.CDSKategory.First;
      while not (FDMMarksDb.CDSKategory.Eof) do begin
        if FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean <> ANewVisible then begin
          FDMMarksDb.CDSKategory.Edit;
          FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean := ANewVisible;
          FDMMarksDb.CDSKategory.post;
        end;
        FDMMarksDb.CDSKategory.Next;
      end;
  finally
    FDMMarksDb.CDSKategory.EnableControls;
  end;
end;

function TMarkCategoryDB.GetCategoriesList: IInterfaceList;
var
  VKategory: IMarkCategory;
begin
  Result := TInterfaceList.Create;
  FDMMarksDb.CDSKategory.DisableControls;
  try
    FDMMarksDb.CDSKategory.Filtered := false;
    FDMMarksDb.CDSKategory.First;
    while not (FDMMarksDb.CDSKategory.Eof) do begin
      VKategory := ReadCurrentCategory;
      Result.Add(VKategory);
      FDMMarksDb.CDSKategory.Next;
    end;
  finally
    FDMMarksDb.CDSKategory.EnableControls;
  end;
end;


function TMarkCategoryDB.GetMarksCategoryBackUpFileName: string;
begin
  Result := FBasePath + 'Categorymarks.~sml';
end;

function TMarkCategoryDB.GetMarksCategoryFileName: string;
begin
  Result := FBasePath + 'Categorymarks.sml';
end;

procedure TMarkCategoryDB.LoadCategoriesFromFile;
var
  VFileName: string;
begin
  VFileName := GetMarksCategoryFileName;
  if FileExists(VFileName) then begin
    FDMMarksDb.CDSKategory.LoadFromFile(VFileName);
    if FDMMarksDb.CDSKategory.RecordCount > 0 then begin
      CopyFile(PChar(VFileName), PChar(GetMarksCategoryBackUpFileName), false);
    end;
  end;
end;

function TMarkCategoryDB.SaveCategory2File: boolean;
var
  VStream: TFileStream;
  XML: string;
begin
  result := true;
  VStream := TFileStream.Create(GetMarksCategoryFileName, fmCreate);;
  try
    try
      FDMMarksDb.CDSKategory.MergeChangeLog;
      XML := FDMMarksDb.CDSKategory.XMLData;
      VStream.Write(XML[1], length(XML));
    except
      result := false;
    end;
  finally
    VStream.Free;
  end;
end;


end.
