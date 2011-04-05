unit u_MarkCategoryDB;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_IDList,
  i_MarkCategory,
  i_MarkCategoryFactory,
  i_MarkCategoryFactoryDbInternal,
  i_MarkCategoryFactoryConfig,
  i_MarkCategoryDB,
  i_MarkCategoryDBSmlInternal,
  dm_MarksDb;

type
  TMarkCategoryDB = class(TInterfacedObject, IMarkCategoryDB, IMarkCategoryDBSmlInternal)
  private
    FSync: IReadWriteSync;
    FBasePath: string;
    FDMMarksDb: TDMMarksDb;
    FList: IIDInterfaceList;
    FFactoryDbInternal: IMarkCategoryFactoryDbInternal;
    FFactory: IMarkCategoryFactory;
    function ReadCurrentCategory(out AId: Integer): IMarkCategory;
    procedure WriteCurrentCategory(ACategory: IMarkCategory);
    function GetMarksCategoryBackUpFileName: string;
    function GetMarksCategoryFileName: string;
  protected
    procedure LockRead; virtual;
    procedure LockWrite; virtual;
    procedure UnlockRead; virtual;
    procedure UnlockWrite; virtual;
  protected
    function GetCategoryByName(AName: string): IMarkCategory;
    function WriteCategory(ACategory: IMarkCategory): IMarkCategory;
    procedure DeleteCategory(ACategory: IMarkCategory);

    function GetCategoriesList: IInterfaceList;
    procedure SetAllCategoriesVisible(ANewVisible: Boolean);

    function GetFactory: IMarkCategoryFactory;
  protected
    function SaveCategory2File: boolean;
    procedure LoadCategoriesFromFile;
    function GetCategoryByID(id: integer): IMarkCategory;
  public
    constructor Create(
      ABasePath: string;
      ADMMarksDb: TDMMarksDb;
      AFactoryConfig: IMarkCategoryFactoryConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  DB,
  i_EnumID,
  u_IDInterfaceList,
  i_MarksDbSmlInternal,
  u_MarkCategoryFactory;

constructor TMarkCategoryDB.Create(
  ABasePath: string;
  ADMMarksDb: TDMMarksDb;
  AFactoryConfig: IMarkCategoryFactoryConfig
);
var
  VFactory: TMarkCategoryFactory;
begin
  FBasePath := ABasePath;
  FDMMarksDb := ADMMarksDb;
  FSync := TSimpleRWSync.Create;
  FList := TIDInterfaceList.Create;
  VFactory := TMarkCategoryFactory.Create(AFactoryConfig);
  FFactoryDbInternal := VFactory;
  FFactory := VFactory;
end;

destructor TMarkCategoryDB.Destroy;
begin
  FSync := nil;
  FList := nil;
  FFactory := nil;
  FFactoryDbInternal := nil;
  inherited;
end;

function TMarkCategoryDB.ReadCurrentCategory(out AId: Integer): IMarkCategory;
var
  VName: string;
  VVisible: Boolean;
  VAfterScale: Integer;
  VBeforeScale: Integer;
begin
  AId := FDMMarksDb.CDSKategory.fieldbyname('id').AsInteger;
  VName := FDMMarksDb.CDSKategory.fieldbyname('name').AsString;
  VVisible := FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean;
  VAfterScale := FDMMarksDb.CDSKategory.fieldbyname('AfterScale').AsInteger;
  VBeforeScale := FDMMarksDb.CDSKategory.fieldbyname('BeforeScale').AsInteger;
  Result := FFactoryDbInternal.CreateCategory(AId, VName, VVisible, VAfterScale, VBeforeScale);
end;

procedure TMarkCategoryDB.WriteCurrentCategory(ACategory: IMarkCategory);
begin
  FDMMarksDb.CDSKategory.fieldbyname('name').AsString := ACategory.name;
  FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean := ACategory.visible;
  FDMMarksDb.CDSKategory.fieldbyname('AfterScale').AsInteger := ACategory.AfterScale;
  FDMMarksDb.CDSKategory.fieldbyname('BeforeScale').AsInteger := ACategory.BeforeScale;
end;

function TMarkCategoryDB.WriteCategory(ACategory: IMarkCategory): IMarkCategory;
var
  VId: Integer;
  VExists: Boolean;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  VId := -1;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    VId := VCategoryInternal.Id;
  end;
  LockRead;
  try
    if VId < 0 then begin
      VExists := False;
    end else begin
      VExists := FDMMarksDb.CDSKategory.Locate('id', VId, []);
    end;
    if VExists then begin
      FDMMarksDb.CDSKategory.Edit;
    end else begin
      FDMMarksDb.CDSKategory.Insert;
    end;
    WriteCurrentCategory(ACategory);
    FDMMarksDb.CDSKategory.post;
    if not VExists then begin
      VId := FDMMarksDb.CDSKategory.fieldbyname('id').AsInteger;
      Result := FFactoryDbInternal.CreateCategory(
        VId,
        ACategory.Name,
        ACategory.Visible,
        ACategory.AfterScale,
        ACategory.BeforeScale
      );
    end else begin
      Result := ACategory;
    end;
    SaveCategory2File;
    FList.Replace(VId, Result);
  finally
    UnlockWrite;
  end;
end;

procedure TMarkCategoryDB.DeleteCategory(ACategory: IMarkCategory);
var
  VId: Integer;
  VExist: Boolean;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  VId := -1;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    VId := VCategoryInternal.Id;
  end;
  LockWrite;
  try
    VExist := False;
    if VId >= 0 then begin
      FDMMarksDb.CDSKategory.DisableControls;
      try
        if FDMMarksDb.CDSKategory.Locate('id', VId, []) then begin
          FDMMarksDb.CDSKategory.Delete;
          VExist := True;
        end;
      finally
        FDMMarksDb.CDSKategory.EnableControls;
      end;
    end;
    if VExist then begin
      SaveCategory2File;
      FList.Remove(VId);
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDB.GetCategoryByID(id: integer): IMarkCategory;
begin
  Result := nil;
  LockRead;
  try
    Result := IMarkCategory(FList.GetByID(id));
  finally
    UnlockRead;
  end;
end;

function TMarkCategoryDB.GetCategoryByName(AName: string): IMarkCategory;
var
  VEnum: IEnumID;
  i: Cardinal;
  VId: Integer;
  VCategory:  IMarkCategory;
begin
  Result := nil;
  LockRead;
  try
    VEnum := FList.GetIDEnum;
    while VEnum.Next(1, VId, i) = S_OK do begin
      VCategory := IMarkCategory(FList.GetByID(VId));
      if SameStr(VCategory.Name, AName) then begin
        Result := VCategory;
        Break;
      end;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarkCategoryDB.GetFactory: IMarkCategoryFactory;
begin
  Result := FFactory;
end;

procedure TMarkCategoryDB.SetAllCategoriesVisible(ANewVisible: Boolean);
var
  VCategory: IMarkCategory;
  VId: Integer;
begin
  LockWrite;
  try
    FDMMarksDb.CDSKategory.DisableControls;
    try
      FDMMarksDb.CDSKategory.Filtered := false;
      FDMMarksDb.CDSKategory.First;
        while not (FDMMarksDb.CDSKategory.Eof) do begin
          if FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean <> ANewVisible then begin
            FDMMarksDb.CDSKategory.Edit;
            FDMMarksDb.CDSKategory.FieldByName('visible').AsBoolean := ANewVisible;
            FDMMarksDb.CDSKategory.post;
            VCategory := ReadCurrentCategory(VId);
            FList.Replace(VId, VCategory);
          end;
          FDMMarksDb.CDSKategory.Next;
        end;
    finally
      FDMMarksDb.CDSKategory.EnableControls;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkCategoryDB.UnlockRead;
begin
  FSync.EndRead;
end;

procedure TMarkCategoryDB.UnlockWrite;
begin
  FSync.EndWrite;
end;

function TMarkCategoryDB.GetCategoriesList: IInterfaceList;
var
  VEnum: IEnumID;
  i: Cardinal;
  VId: Integer;
  VCategory: IMarkCategory;
begin
  Result := TInterfaceList.Create;
  LockRead;
  try
    VEnum := FList.GetIDEnum;
    while VEnum.Next(1, VId, i) = S_OK do begin
      VCategory := IMarkCategory(FList.GetByID(VId));
      Result.Add(VCategory);
    end;
  finally
    UnlockRead;
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
  VCategory: IMarkCategory;
  VId: Integer;
begin
  VFileName := GetMarksCategoryFileName;
  if FileExists(VFileName) then begin
    FDMMarksDb.CDSKategory.LoadFromFile(VFileName);
    if FDMMarksDb.CDSKategory.RecordCount > 0 then begin
      CopyFile(PChar(VFileName), PChar(GetMarksCategoryBackUpFileName), false);
    end;

    FDMMarksDb.CDSKategory.DisableControls;
    try
      FDMMarksDb.CDSKategory.Filtered := false;
      FDMMarksDb.CDSKategory.First;
      while not (FDMMarksDb.CDSKategory.Eof) do begin
        VCategory := ReadCurrentCategory(VId);
        FList.Add(VId, VCategory);
        FDMMarksDb.CDSKategory.Next;
      end;
    finally
      FDMMarksDb.CDSKategory.EnableControls;
    end;
  end;
end;

procedure TMarkCategoryDB.LockRead;
begin
  FSync.BeginRead;
end;

procedure TMarkCategoryDB.LockWrite;
begin
  FSync.BeginWrite;
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
