unit u_MarkCategoryDB;

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
  u_MarksSimple;

type
  TMarkCategoryDB = class
  private
    FBasePath: string;
    FDMMarksDb: TDMMarksDb;
    FMarksDb: TMarksOnlyDb;
    procedure ReadCurrentCategory(ACategory: TCategoryId);
    procedure WriteCurrentCategory(ACategory: TCategoryId);
    function GetMarksCategoryBackUpFileName: string;
    function GetMarksCategoryFileName: string;
  public
    function SaveCategory2File: boolean;
    procedure LoadCategoriesFromFile;
  public
    constructor Create(ABasePath: string; AMarksDb: TMarksOnlyDb; ADMMarksDb: TDMMarksDb);

    function GetCategoryByID(id: integer): TCategoryId;
    procedure WriteCategory(ACategory: TCategoryId);
    procedure DeleteCategoryWithMarks(ACategory: TCategoryId);

    function GetCategoriesList: TList;
    procedure SetAllCategoriesVisible(ANewVisible: Boolean);
  end;


implementation

uses
  DB,
  SysUtils,
  Contnrs,
  GR32,
  u_MarksSimpleNew;

procedure TMarkCategoryDB.ReadCurrentCategory(ACategory: TCategoryId);
begin
  ACategory.name := FDMMarksDb.CDSKategory.fieldbyname('name').AsString;
  ACategory.id := FDMMarksDb.CDSKategory.fieldbyname('id').AsInteger;
  ACategory.visible := FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean;
  ACategory.AfterScale := FDMMarksDb.CDSKategory.fieldbyname('AfterScale').AsInteger;
  ACategory.BeforeScale := FDMMarksDb.CDSKategory.fieldbyname('BeforeScale').AsInteger;
end;

procedure TMarkCategoryDB.WriteCurrentCategory(ACategory: TCategoryId);
begin
  FDMMarksDb.CDSKategory.fieldbyname('name').AsString := ACategory.name;
  FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean := ACategory.visible;
  FDMMarksDb.CDSKategory.fieldbyname('AfterScale').AsInteger := ACategory.AfterScale;
  FDMMarksDb.CDSKategory.fieldbyname('BeforeScale').AsInteger := ACategory.BeforeScale;
end;

procedure TMarkCategoryDB.WriteCategory(ACategory: TCategoryId);
begin
  if ACategory.id < 0 then begin
    FDMMarksDb.CDSKategory.Insert;
  end else begin
    FDMMarksDb.CDSKategory.Locate('id', ACategory.id, []);
    FDMMarksDb.CDSKategory.Edit;
  end;
  WriteCurrentCategory(ACategory);
  FDMMarksDb.CDSKategory.post;
  ACategory.id := FDMMarksDb.CDSKategory.fieldbyname('id').AsInteger;
end;

constructor TMarkCategoryDB.Create(ABasePath: string; AMarksDb: TMarksOnlyDb;
  ADMMarksDb: TDMMarksDb);
begin
  FBasePath := ABasePath;
  FMarksDb := AMarksDb;
  FDMMarksDb := ADMMarksDb;
end;

procedure TMarkCategoryDB.DeleteCategoryWithMarks(ACategory: TCategoryId);
begin
  FDMMarksDb.CDSmarks.DisableControls;
  try
    if FDMMarksDb.CDSKategory.Locate('id', ACategory.id, []) then begin
      FDMMarksDb.CDSmarks.Filtered := false;
      FDMMarksDb.CDSmarks.Filter := 'categoryid = ' + inttostr(ACategory.id);
      FDMMarksDb.CDSmarks.Filtered := true;
      FDMMarksDb.CDSmarks.First;
      while not (FDMMarksDb.CDSmarks.Eof) do begin
        FDMMarksDb.CDSmarks.Delete;
      end;
      FMarksDb.SaveMarks2File;
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
  finally
    FDMMarksDb.CDSmarks.EnableControls;
  end;
end;

function TMarkCategoryDB.GetCategoryByID(id: integer): TCategoryId;
begin
  Result := nil;
  if FDMMarksDb.CDSKategory.Locate('id', id, []) then begin
    Result := TCategoryId.Create;
    ReadCurrentCategory(Result);
  end;
end;

procedure TMarkCategoryDB.SetAllCategoriesVisible(ANewVisible: Boolean);
var
  VKategoryId: TCategoryId;
begin
  FDMMarksDb.CDSKategory.DisableControls;
  try
    FDMMarksDb.CDSKategory.Filtered := false;
    FDMMarksDb.CDSKategory.First;
    VKategoryId := TCategoryId.Create;
    try
      while not (FDMMarksDb.CDSKategory.Eof) do begin
        ReadCurrentCategory(VKategoryId);
        if VKategoryId.visible <> ANewVisible then begin
          VKategoryId.visible := ANewVisible;
          FDMMarksDb.CDSKategory.Edit;
          WriteCurrentCategory(VKategoryId);
          FDMMarksDb.CDSKategory.post;
        end;
        FDMMarksDb.CDSKategory.Next;
      end;
    finally
      VKategoryId.Free;
    end;
  finally
    FDMMarksDb.CDSKategory.EnableControls;
  end;
end;

function TMarkCategoryDB.GetCategoriesList: TList;
var
  VKategory: TCategoryId;
begin
  Result := TObjectList.Create(True);
  FDMMarksDb.CDSKategory.DisableControls;
  try
    FDMMarksDb.CDSKategory.Filtered := false;
    FDMMarksDb.CDSKategory.First;
    while not (FDMMarksDb.CDSKategory.Eof) do begin
      VKategory := TCategoryId.Create;
      ReadCurrentCategory(VKategory);
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
  ms: TMemoryStream;
  XML: string;
begin
  result := true;
  ms := TMemoryStream.Create;
  try
    try
      FDMMarksDb.CDSKategory.MergeChangeLog;
      XML := FDMMarksDb.CDSKategory.XMLData;
      ms.Write(XML[1], length(XML));
      ms.SaveToFile(GetMarksCategoryFileName);
    except
      result := false;
    end;
  finally
    ms.Free;
  end;
end;


end.
