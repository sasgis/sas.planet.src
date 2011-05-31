unit u_MarkCategoryDB;

interface

uses
  Windows,
  Classes,
  SysUtils,
  DBClient,
  i_IDList,
  i_MarkCategory,
  i_MarkCategoryFactory,
  i_MarkCategoryFactoryDbInternal,
  i_MarkCategoryFactoryConfig,
  i_MarkCategoryDB,
  i_MarkCategoryDBSmlInternal;

type
  TMarkCategoryDB = class(TInterfacedObject, IMarkCategoryDB, IMarkCategoryDBSmlInternal)
  private
    FSync: IReadWriteSync;
    FBasePath: string;
    CDSKategory: TClientDataSet;
    FList: IIDInterfaceList;
    FFactoryDbInternal: IMarkCategoryFactoryDbInternal;
    FFactory: IMarkCategoryFactory;
    function ReadCurrentCategory: IMarkCategory;
    procedure WriteCurrentCategory(ACategory: IMarkCategory);
    function GetMarksCategoryBackUpFileName: string;
    function GetMarksCategoryFileName: string;
    procedure InitEmptyDS;
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
      AFactoryConfig: IMarkCategoryFactoryConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  DB,
  i_EnumID,
  u_IDInterfaceList,
  u_MarkCategoryFactory;

constructor TMarkCategoryDB.Create(
  ABasePath: string;
  AFactoryConfig: IMarkCategoryFactoryConfig
);
var
  VFactory: TMarkCategoryFactory;
begin
  FBasePath := ABasePath;
  FSync := TSimpleRWSync.Create;
  FList := TIDInterfaceList.Create;
  VFactory := TMarkCategoryFactory.Create(AFactoryConfig);
  FFactoryDbInternal := VFactory;
  FFactory := VFactory;
  CDSKategory := TClientDataSet.Create(nil);
  CDSKategory.Name := 'CDSKategory';
  InitEmptyDS;
end;

destructor TMarkCategoryDB.Destroy;
begin
  FreeAndNil(CDSKategory);
  FSync := nil;
  FList := nil;
  FFactory := nil;
  FFactoryDbInternal := nil;
  inherited;
end;

function TMarkCategoryDB.ReadCurrentCategory: IMarkCategory;
var
  VId: Integer;
  VName: string;
  VVisible: Boolean;
  VAfterScale: Integer;
  VBeforeScale: Integer;
begin
  VName := CDSKategory.fieldbyname('name').AsString;
  VId := CDSKategory.fieldbyname('id').AsInteger;
  VVisible := CDSKategory.FieldByName('visible').AsBoolean;
  VAfterScale := CDSKategory.fieldbyname('AfterScale').AsInteger;
  VBeforeScale := CDSKategory.fieldbyname('BeforeScale').AsInteger;
  Result := FFactoryDbInternal.CreateCategory(VId, VName, VVisible, VAfterScale, VBeforeScale);
end;

procedure TMarkCategoryDB.WriteCurrentCategory(ACategory: IMarkCategory);
begin
  CDSKategory.fieldbyname('name').AsString := ACategory.name;
  CDSKategory.FieldByName('visible').AsBoolean := ACategory.visible;
  CDSKategory.fieldbyname('AfterScale').AsInteger := ACategory.AfterScale;
  CDSKategory.fieldbyname('BeforeScale').AsInteger := ACategory.BeforeScale;
end;

function TMarkCategoryDB.WriteCategory(ACategory: IMarkCategory): IMarkCategory;
var
  VId: Integer;
  VExists: Boolean;
begin
  VId := ACategory.id;
  LockRead;
  try
    if VId < 0 then begin
      VExists := False;
    end else begin
      VExists := CDSKategory.Locate('id', VId, []);
    end;
    if VExists then begin
      CDSKategory.Edit;
    end else begin
      CDSKategory.Insert;
    end;
    WriteCurrentCategory(ACategory);
    CDSKategory.post;
    if not VExists then begin
      VId := CDSKategory.fieldbyname('id').AsInteger;
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

procedure TMarkCategoryDB.InitEmptyDS;
begin
  CDSKategory.Close;
  CDSKategory.XMLData :=
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'+
    '<DATAPACKET Version="2.0">'+
    '<METADATA>'+
    '<FIELDS>'+
    '<FIELD attrname="id" fieldtype="i4" readonly="true" SUBTYPE="Autoinc"/>'+
    '<FIELD attrname="name" fieldtype="string" WIDTH="256"/>'+
    '<FIELD attrname="visible" fieldtype="boolean"/>'+
    '<FIELD attrname="AfterScale" fieldtype="i2"/>'+
    '<FIELD attrname="BeforeScale" fieldtype="i2"/>'+
    '</FIELDS>'+
    '<PARAMS AUTOINCVALUE="1"/>'+
    '</METADATA>'+
    '<ROWDATA></ROWDATA>'+
    '</DATAPACKET>';
  CDSKategory.Open;
end;

procedure TMarkCategoryDB.DeleteCategory(ACategory: IMarkCategory);
var
  VId: Integer;
  VExist: Boolean;
begin
  VId := ACategory.id;
  LockWrite;
  try
    VExist := False;
    if VId >= 0 then begin
      CDSKategory.DisableControls;
      try
        if CDSKategory.Locate('id', VId, []) then begin
          CDSKategory.Delete;
          VExist := True;
        end;
      finally
        CDSKategory.EnableControls;
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
begin
  LockWrite;
  try
    CDSKategory.DisableControls;
    try
      CDSKategory.Filtered := false;
      CDSKategory.First;
        while not (CDSKategory.Eof) do begin
          if CDSKategory.FieldByName('visible').AsBoolean <> ANewVisible then begin
            CDSKategory.Edit;
            CDSKategory.FieldByName('visible').AsBoolean := ANewVisible;
            CDSKategory.post;
            VCategory := ReadCurrentCategory;
            FList.Replace(VCategory.Id, VCategory);
          end;
          CDSKategory.Next;
        end;
    finally
      CDSKategory.EnableControls;
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
begin
  VFileName := GetMarksCategoryFileName;
  if FileExists(VFileName) then begin
    try
      CDSKategory.LoadFromFile(VFileName);
    except
      InitEmptyDS;
    end;
    if CDSKategory.RecordCount > 0 then begin
      CopyFile(PChar(VFileName), PChar(GetMarksCategoryBackUpFileName), false);
    end;

    CDSKategory.DisableControls;
    try
      CDSKategory.Filtered := false;
      CDSKategory.First;
      while not (CDSKategory.Eof) do begin
        VCategory := ReadCurrentCategory;
        FList.Add(VCategory.Id, VCategory);
        CDSKategory.Next;
      end;
    finally
      CDSKategory.EnableControls;
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
      CDSKategory.MergeChangeLog;
      XML := CDSKategory.XMLData;
      VStream.Write(XML[1], length(XML));
    except
      result := false;
    end;
  finally
    VStream.Free;
  end;
end;


end.
