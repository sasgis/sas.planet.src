{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MarkCategoryDB;

interface

uses
  Windows,
  Classes,
  SysUtils,
  DBClient,
  i_IDList,
  i_PathConfig,
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
    FDbCode: Integer;
    FBasePath: IPathConfig;
    FCdsKategory: TClientDataSet;
    FList: IIDInterfaceList;
    FFactoryDbInternal: IMarkCategoryFactoryDbInternal;
    FFactory: IMarkCategoryFactory;
    function ReadCurrentCategory(out AId: Integer): IMarkCategory;
    procedure WriteCurrentCategory(const ACategory: IMarkCategory);
    function GetMarksCategoryBackUpFileName: string;
    function GetMarksCategoryFileName: string;
    function GetDbCode: Integer;
    procedure InitEmptyDS;
  protected
    procedure LockRead; virtual;
    procedure LockWrite; virtual;
    procedure UnlockRead; virtual;
    procedure UnlockWrite; virtual;
  protected
    function GetCategoryByName(const AName: string): IMarkCategory;
    function WriteCategory(const ACategory: IMarkCategory): IMarkCategory;
    procedure DeleteCategory(const ACategory: IMarkCategory);

    function GetCategoriesList: IInterfaceList;
    procedure SetAllCategoriesVisible(ANewVisible: Boolean);

    function GetFactory: IMarkCategoryFactory;
  protected
    function SaveCategory2File: boolean;
    procedure LoadCategoriesFromFile;
    function GetCategoryByID(id: integer): IMarkCategory;
  public
    constructor Create(
      const ABasePath: IPathConfig;
      const AFactoryConfig: IMarkCategoryFactoryConfig
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
  const ABasePath: IPathConfig;
  const AFactoryConfig: IMarkCategoryFactoryConfig
);
var
  VFactory: TMarkCategoryFactory;
begin
  FDbCode := Integer(Self);
  FBasePath := ABasePath;
  FSync := TSimpleRWSync.Create;
  FList := TIDInterfaceList.Create;
  VFactory := TMarkCategoryFactory.Create(GetDbCode, AFactoryConfig);
  FFactoryDbInternal := VFactory;
  FFactory := VFactory;
  FCdsKategory := TClientDataSet.Create(nil);
  FCdsKategory.Name := 'CDSKategory';
  InitEmptyDS;
end;

destructor TMarkCategoryDB.Destroy;
begin
  FreeAndNil(FCdsKategory);
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
  AId := FCdsKategory.fieldbyname('id').AsInteger;
  VName := FCdsKategory.fieldbyname('name').AsString;
  AId := FCdsKategory.fieldbyname('id').AsInteger;
  VVisible := FCdsKategory.FieldByName('visible').AsBoolean;
  VAfterScale := FCdsKategory.fieldbyname('AfterScale').AsInteger;
  VBeforeScale := FCdsKategory.fieldbyname('BeforeScale').AsInteger;
  Result := FFactoryDbInternal.CreateCategory(AId, VName, VVisible, VAfterScale, VBeforeScale);
end;

procedure TMarkCategoryDB.WriteCurrentCategory(const ACategory: IMarkCategory);
begin
  FCdsKategory.fieldbyname('name').AsString := ACategory.name;
  FCdsKategory.FieldByName('visible').AsBoolean := ACategory.visible;
  FCdsKategory.fieldbyname('AfterScale').AsInteger := ACategory.AfterScale;
  FCdsKategory.fieldbyname('BeforeScale').AsInteger := ACategory.BeforeScale;
end;

function TMarkCategoryDB.WriteCategory(const ACategory: IMarkCategory): IMarkCategory;
var
  VId: Integer;
  VExists: Boolean;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  VId := -1;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    if GetDbCode = VCategoryInternal.DbCode then begin
      VId := VCategoryInternal.Id;
    end;
  end;
  LockRead;
  try
    if VId < 0 then begin
      VExists := False;
    end else begin
      VExists := FCdsKategory.Locate('id', VId, []);
    end;
    if VExists then begin
      FCdsKategory.Edit;
    end else begin
      FCdsKategory.Insert;
    end;
    WriteCurrentCategory(ACategory);
    FCdsKategory.post;
    if not VExists then begin
      VId := FCdsKategory.fieldbyname('id').AsInteger;
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
  FCdsKategory.Close;
  FCdsKategory.XMLData :=
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
  FCdsKategory.Open;
end;

procedure TMarkCategoryDB.DeleteCategory(const ACategory: IMarkCategory);
var
  VId: Integer;
  VExist: Boolean;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  VId := -1;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    if GetDbCode = VCategoryInternal.DbCode then begin
      VId := VCategoryInternal.Id;
    end;
  end;
  LockWrite;
  try
    VExist := False;
    if VId >= 0 then begin
      FCdsKategory.DisableControls;
      try
        if FCdsKategory.Locate('id', VId, []) then begin
          FCdsKategory.Delete;
          VExist := True;
        end;
      finally
        FCdsKategory.EnableControls;
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

function TMarkCategoryDB.GetCategoryByName(const AName: string): IMarkCategory;
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

function TMarkCategoryDB.GetDbCode: Integer;
begin
  Result := FDbCode;
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
    FCdsKategory.DisableControls;
    try
      FCdsKategory.Filtered := false;
      FCdsKategory.First;
        while not (FCdsKategory.Eof) do begin
          if FCdsKategory.FieldByName('visible').AsBoolean <> ANewVisible then begin
            FCdsKategory.Edit;
            FCdsKategory.FieldByName('visible').AsBoolean := ANewVisible;
            FCdsKategory.post;
            VCategory := ReadCurrentCategory(VId);
            FList.Replace(VId, VCategory);
          end;
          FCdsKategory.Next;
        end;
    finally
      FCdsKategory.EnableControls;
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
  Result := IncludeTrailingPathDelimiter(FBasePath.FullPath) + 'Categorymarks.~sml';
end;

function TMarkCategoryDB.GetMarksCategoryFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(FBasePath.FullPath) + 'Categorymarks.sml';
end;

procedure TMarkCategoryDB.LoadCategoriesFromFile;
var
  VFileName: string;
  VCategory: IMarkCategory;
  VId: Integer;
begin
  VFileName := GetMarksCategoryFileName;
  if FileExists(VFileName) then begin
    try
      FCdsKategory.LoadFromFile(VFileName);
    except
      InitEmptyDS;
    end;
    if FCdsKategory.RecordCount > 0 then begin
      CopyFile(PChar(VFileName), PChar(GetMarksCategoryBackUpFileName), false);
    end;

    FCdsKategory.DisableControls;
    try
      FCdsKategory.Filtered := false;
      FCdsKategory.First;
      while not (FCdsKategory.Eof) do begin
        VCategory := ReadCurrentCategory(VId);
        FList.Add(VId, VCategory);
        FCdsKategory.Next;
      end;
    finally
      FCdsKategory.EnableControls;
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
  try
    VStream := TFileStream.Create(GetMarksCategoryFileName, fmCreate);;
    try
      LockRead;
      try
        FCdsKategory.MergeChangeLog;
        XML := FCdsKategory.XMLData;
        VStream.Write(XML[1], length(XML));
      finally
        UnlockRead;
      end;
    finally
      VStream.Free;
    end;
  except
    result := false;
  end;
end;


end.
