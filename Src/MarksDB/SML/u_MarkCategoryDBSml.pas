{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MarkCategoryDBSml;

interface

uses
  Windows,
  Classes,
  SysUtils,
  DBClient,
  i_IDList,
  i_SimpleFlag,
  i_NotifierOperation,
  i_Category,
  i_MarkCategory,
  i_MarkCategoryList,
  i_MarkCategoryFactory,
  i_MarkCategoryFactoryDbInternal,
  i_MarkCategoryDBSmlInternal,
  i_ReadWriteStateInternal,
  i_MarkCategoryDBImpl,
  u_ConfigDataElementBase;

type
  TMarkCategoryDBSml = class(TConfigDataElementBaseEmptySaveLoad, IMarkCategoryDBSmlInternal, IMarkCategoryDBImpl)
  private
    FDbId: Integer;
    FStateInternal: IReadWriteStateInternal;
    FStream: TStream;

    FUseUnicodeSchema: Boolean;
    FStoreInBinaryFormat: Boolean;
    FUseDataSetIndex: Boolean;

    FCdsCategory: TClientDataSet;
    FList: IIDInterfaceList;
    FFactoryDbInternal: IMarkCategoryFactoryDbInternal;
    FFactory: IMarkCategoryFactory;
    FNeedSaveFlag: ISimpleFlag;

    function ReadCurrentCategory(out AId: Integer): IMarkCategory;
    procedure WriteCurrentCategory(const ACategory: IMarkCategory);
    procedure InitEmptyDS;
    function _UpdateCategory(
      const AOldCategory: IInterface;
      const ANewCategory: IInterface
    ): IMarkCategory;
    function _LocateByID(const AId: Integer): Boolean; inline;
    function Save: boolean;
  private
    { IMarkCategoryDBSmlInternal }
    procedure Initialize(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    function IsCategoryFromThisDb(const ACategory: ICategory): Boolean;
    function GetCategoryByID(id: integer): IMarkCategory;
    function GetCategoryByName(const AName: string): IMarkCategory;
  private
    { IMarkCategoryDBImpl }
    function UpdateCategory(
      const AOldCategory: IMarkCategory;
      const ANewCategory: IMarkCategory
    ): IMarkCategory;
    function UpdateCategoryList(
      const AOldCategoryList: IMarkCategoryList;
      const ANewCategoryList: IMarkCategoryList
    ): IMarkCategoryList;

    function GetCategoriesList: IMarkCategoryList;
    procedure SetAllCategoriesVisible(ANewVisible: Boolean);
  public
    constructor Create(
      const ADbId: Integer;
      const AStateInternal: IReadWriteStateInternal;
      const ADataStream: TStream;
      const AUseUnicodeSchema: Boolean;
      const AStoreInBinaryFormat: Boolean;
      const AUseDataSetIndex: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  DB,
  StrUtils,
  t_CommonTypes,
  i_EnumID,
  i_InterfaceListSimple,
  u_IDInterfaceList,
  u_InterfaceListSimple,
  u_SimpleFlagWithInterlock,
  i_MarkDbSmlInternal,
  u_MarkCategoryList,
  u_MarkCategoryFactorySmlDbInternal;

constructor TMarkCategoryDBSml.Create(
  const ADbId: Integer;
  const AStateInternal: IReadWriteStateInternal;
  const ADataStream: TStream;
  const AUseUnicodeSchema: Boolean;
  const AStoreInBinaryFormat: Boolean;
  const AUseDataSetIndex: Boolean
);
begin
  inherited Create;
  FDbId := ADbId;
  FStream := ADataStream;
  FUseUnicodeSchema := AUseUnicodeSchema;
  FStoreInBinaryFormat := AStoreInBinaryFormat;
  FUseDataSetIndex := AUseDataSetIndex;
  FStateInternal := AStateInternal;
  FList := TIDInterfaceList.Create;
  FNeedSaveFlag := TSimpleFlagWithInterlock.Create;
  FFactoryDbInternal := TMarkCategoryFactorySmlDbInternal.Create(FDbId);
  FCdsCategory := TClientDataSet.Create(nil);
  FCdsCategory.Name := 'MarkCategoryDB';
  FCdsCategory.DisableControls;
end;

destructor TMarkCategoryDBSml.Destroy;
begin
  if Assigned(FCdsCategory) then begin
    Save;
  end;
  FreeAndNil(FStream);
  FreeAndNil(FCdsCategory);
  FList := nil;
  FFactory := nil;
  FFactoryDbInternal := nil;
  inherited;
end;

function TMarkCategoryDBSml.ReadCurrentCategory(out AId: Integer): IMarkCategory;
var
  VName: string;
  VVisible: Boolean;
  VAfterScale: Integer;
  VBeforeScale: Integer;
begin
  AId := FCdsCategory.fieldbyname('id').AsInteger;
  VName := FCdsCategory.fieldbyname('name').AsString;
  VVisible := FCdsCategory.FieldByName('visible').AsBoolean;
  VAfterScale := FCdsCategory.fieldbyname('AfterScale').AsInteger;
  VBeforeScale := FCdsCategory.fieldbyname('BeforeScale').AsInteger;
  Result := FFactoryDbInternal.CreateCategory(AId, VName, VVisible, VAfterScale, VBeforeScale);
end;

procedure TMarkCategoryDBSml.WriteCurrentCategory(const ACategory: IMarkCategory);
begin
  FCdsCategory.fieldbyname('name').AsString := ACategory.Name;
  FCdsCategory.FieldByName('visible').AsBoolean := ACategory.Visible;
  FCdsCategory.fieldbyname('AfterScale').AsInteger := ACategory.AfterScale;
  FCdsCategory.fieldbyname('BeforeScale').AsInteger := ACategory.BeforeScale;
end;

function TMarkCategoryDBSml._LocateByID(const AId: Integer): Boolean;
begin
  if FUseDataSetIndex then begin
    Result := FCdsCategory.FindKey([AId]);
  end else begin
    Result := FCdsCategory.Locate('id', AId, []);
  end;
end;

function TMarkCategoryDBSml._UpdateCategory(
  const AOldCategory, ANewCategory: IInterface
): IMarkCategory;
var
  VIdOld: Integer;
  VIdNew: Integer;
  VIdSub: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
  VCategory: IMarkCategory;
  VOldCategory: IMarkCategory;
  VLocated: Boolean;
  VName, VNewName, VOldName: string;
begin
  Result := nil;
  VIdOld := CNotExistCategoryID;
  VOldName := '';
  if Supports(AOldCategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    if VCategoryInternal.DbId = FDbId then begin
      VIdOld := VCategoryInternal.Id;
    end;
  end;
  VIdNew := CNotExistCategoryID;
  VLocated := False;
  if VIdOld >= 0 then begin
    VOldCategory := IMarkCategory(FList.GetByID(VIdOld));
    if VOldCategory <> nil then begin
      VOldName := VOldCategory.Name + '\';
      if Supports(ANewCategory, IMarkCategory, VCategory) then begin
        if VOldCategory.IsEqual(VCategory) then begin
          Result := VOldCategory;
          Exit;
        end;
      end;
    end;
    VLocated := _LocateByID(VIdOld);
  end;
  if VLocated then begin
    if Supports(ANewCategory, IMarkCategory, VCategory) then begin
      FCdsCategory.Edit;
      WriteCurrentCategory(VCategory);
      FCdsCategory.post;
      Result := ReadCurrentCategory(VIdNew);
      VNewName := Result.Name + '\';

      Assert(Result <> nil);
      Assert(VIdNew >= 0);
      Assert(VIdNew = VIdOld);

      Assert(VNewName <> '');
      Assert(VOldName <> '');

      // update sub categories
      if (VNewName <> VOldName) then begin
        FCdsCategory.Filtered := False;
        FCdsCategory.First;
        while not (FCdsCategory.Eof) do begin
          VName := FCdsCategory.FieldByName('name').AsString;
          if StartsText(VOldName, VName) then begin
            FCdsCategory.Edit;
            FCdsCategory.FieldByName('name').AsString :=
              StringReplace(VName, VOldName, VNewName, [rfIgnoreCase]);
            FCdsCategory.Post;
            VCategory := ReadCurrentCategory(VIdSub);
            FList.Replace(VIdSub, VCategory);
          end;
          FCdsCategory.Next;
        end;
      end;

      SetChanged;
      FNeedSaveFlag.SetFlag;
    end else begin
      FCdsCategory.Delete;
      SetChanged;
      FNeedSaveFlag.SetFlag;
    end;
  end else begin
    if Supports(ANewCategory, IMarkCategory, VCategory) then begin
      FCdsCategory.Insert;
      WriteCurrentCategory(VCategory);
      FCdsCategory.post;
      Result := ReadCurrentCategory(VIdNew);
      SetChanged;
      FNeedSaveFlag.SetFlag;
      Assert(Result <> nil);
      Assert(VIdNew >= 0);
    end;
  end;
  if VIdOld >= 0 then begin
    if VIdNew >= 0 then begin
      if VIdNew <> VIdOld then begin
        FList.Remove(VIdOld);
        FList.Add(VIdNew, Result);
      end else begin
        FList.Replace(VIdOld, Result);
      end;
    end else begin
      FList.Remove(VIdOld);
    end;
  end else begin
    if VIdNew >= 0 then begin
      FList.Add(VIdNew, Result);
    end;
  end;
end;

function TMarkCategoryDBSml.UpdateCategory(
  const AOldCategory: IMarkCategory;
  const ANewCategory: IMarkCategory
): IMarkCategory;
begin
  Assert((AOldCategory <> nil) or (ANewCategory <> nil));
  LockWrite;
  try
    Result := _UpdateCategory(AOldCategory, ANewCategory);
  finally
    UnlockWrite;
  end;
  Save;
end;

function TMarkCategoryDBSml.UpdateCategoryList(
  const AOldCategoryList, ANewCategoryList: IMarkCategoryList
): IMarkCategoryList;
var
  i: Integer;
  VNew: IInterface;
  VOld: IInterface;
  VResult: ICategory;
  VMinCount: Integer;
  VMaxCount: Integer;
  VTemp: IInterfaceListSimple;
begin
  Result := nil;
  if ANewCategoryList <> nil then begin
    VTemp := TInterfaceListSimple.Create;
    VTemp.Capacity := ANewCategoryList.Count;

    LockWrite;
    try
      if (AOldCategoryList <> nil) then begin
        if AOldCategoryList.Count < ANewCategoryList.Count then begin
          VMinCount := AOldCategoryList.Count;
          VMaxCount := ANewCategoryList.Count;
        end else begin
          VMinCount := ANewCategoryList.Count;
          VMaxCount := AOldCategoryList.Count;
        end;
      end else begin
        VMinCount := 0;
        VMaxCount := ANewCategoryList.Count;
      end;
      for i := 0 to VMinCount - 1 do begin
        VOld := AOldCategoryList[i];
        VNew := ANewCategoryList[i];
        VResult := _UpdateCategory(VOld, VNew);
        VTemp.Add(VResult);
      end;
      for i := VMinCount to VMaxCount - 1 do begin
        VOld := nil;
        if (AOldCategoryList <> nil) and (i < AOldCategoryList.Count) then begin
          VOld := AOldCategoryList[i];
        end;
        VNew := nil;
        if (i < ANewCategoryList.Count) then begin
          VNew := ANewCategoryList[i];
        end;
        VResult := _UpdateCategory(VOld, VNew);
        if i < VTemp.Capacity then begin
          VTemp.Add(VResult);
        end;
      end;
    finally
      UnlockWrite;
    end;
    Save;
    Result := TMarkCategoryList.Build(VTemp.MakeStaticAndClear);
  end else begin
    LockWrite;
    try
      for i := 0 to AOldCategoryList.Count - 1 do begin
        _UpdateCategory(AOldCategoryList[i], nil);
      end;
    finally
      UnlockWrite;
    end;
    Save;
  end;
end;

procedure TMarkCategoryDBSml.InitEmptyDS;
const
  cIdxName = 'CategoryIDIdx';
var
  I: Integer;
  VIdxExists: Boolean;
  VStringType: string;
  VStringWidth: string;
begin
  if FUseUnicodeSchema then begin
    VStringType := 'string.uni';
    VStringWidth := '512';
  end else begin
    VStringType := 'string';
    VStringWidth := '255';
  end;
  FCdsCategory.Close;
  FCdsCategory.XMLData :=
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
    '<DATAPACKET Version="2.0">' +
    '   <METADATA>' +
    '     <FIELDS>' +
    '       <FIELD attrname="id" fieldtype="i4" readonly="true" SUBTYPE="Autoinc"/>' +
    '       <FIELD attrname="name" fieldtype="' + VStringType + '" WIDTH="' + VStringWidth + '"/>' +
    '       <FIELD attrname="visible" fieldtype="boolean"/>' +
    '       <FIELD attrname="AfterScale" fieldtype="i2"/>' +
    '       <FIELD attrname="BeforeScale" fieldtype="i2"/>' +
    '     </FIELDS>' +
    '     <PARAMS AUTOINCVALUE="1"/>' +
    '   </METADATA>' +
    '   <ROWDATA></ROWDATA>' +
    '</DATAPACKET>';

  if FUseDataSetIndex then begin
    VIdxExists := False;

    for I := 0 to FCdsCategory.IndexDefs.Count - 1 do begin
      if FCdsCategory.IndexDefs.Items[I].Name = cIdxName then begin
        VIdxExists := True;
        Break;
      end;
    end;

    if not VIdxExists then begin
      with FCdsCategory.IndexDefs.AddIndexDef do begin
        Name := cIdxName;
        Fields := 'id';
        Options := [ixPrimary, ixUnique, ixCaseInsensitive];
      end;
    end;

    FCdsCategory.IndexName := cIdxName;
  end;

  FCdsCategory.Open;

  FCdsCategory.LogChanges := False;
end;

function TMarkCategoryDBSml.IsCategoryFromThisDb(
  const ACategory: ICategory
): Boolean;
var
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  Assert(Assigned(ACategory));
  Result := False;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    Result := VCategoryInternal.DbId = FDbId;
  end;
end;

function TMarkCategoryDBSml.GetCategoryByID(id: integer): IMarkCategory;
begin
  Result := nil;
  LockRead;
  try
    Result := IMarkCategory(FList.GetByID(id));
  finally
    UnlockRead;
  end;
end;

function TMarkCategoryDBSml.GetCategoryByName(const AName: string): IMarkCategory;
var
  VEnum: IEnumID;
  i: Cardinal;
  VId: Integer;
  VCategory: IMarkCategory;
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

procedure TMarkCategoryDBSml.SetAllCategoriesVisible(ANewVisible: Boolean);
var
  VCategory: IMarkCategory;
  VId: Integer;
  VChanged: Boolean;
begin
  LockWrite;
  try
    VChanged := False;
    FCdsCategory.Filtered := false;
    FCdsCategory.First;
    while not (FCdsCategory.Eof) do begin
      if FCdsCategory.FieldByName('visible').AsBoolean <> ANewVisible then begin
        FCdsCategory.Edit;
        FCdsCategory.FieldByName('visible').AsBoolean := ANewVisible;
        FCdsCategory.post;
        VCategory := ReadCurrentCategory(VId);
        FList.Replace(VId, VCategory);
        VChanged := True;
      end;
      FCdsCategory.Next;
    end;
    if VChanged then begin
      SetChanged;
      FNeedSaveFlag.SetFlag;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDBSml.GetCategoriesList: IMarkCategoryList;
var
  VEnum: IEnumID;
  i: Cardinal;
  VId: Integer;
  VCategory: IMarkCategory;
  VTemp: IInterfaceListSimple;
begin
  Result := nil;
  VTemp := TInterfaceListSimple.Create;
  LockRead;
  try
    VEnum := FList.GetIDEnum;
    while VEnum.Next(1, VId, i) = S_OK do begin
      VCategory := IMarkCategory(FList.GetByID(VId));
      VTemp.Add(VCategory);
    end;
  finally
    UnlockRead;
  end;
  Result := TMarkCategoryList.Build(VTemp.MakeStaticAndClear);
end;

procedure TMarkCategoryDBSml.Initialize(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VCategory: IMarkCategory;
  VId: Integer;
begin
  LockWrite;
  try
    InitEmptyDS;
    FList.Clear;
    if FStateInternal.ReadAccess <> asDisabled then begin
      if FStream <> nil then begin
        try
          FCdsCategory.LoadFromStream(FStream);
          FCdsCategory.MergeChangeLog;
          FCdsCategory.LogChanges := False;
        except
          FStateInternal.WriteAccess := asDisabled;
          InitEmptyDS;
        end;
      end;

      FCdsCategory.Filtered := false;
      FCdsCategory.First;
      while not (FCdsCategory.Eof) do begin
        VCategory := ReadCurrentCategory(VId);
        FList.Add(VId, VCategory);
        FCdsCategory.Next;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDBSml.Save: boolean;
var
  VFormat: TDataPacketFormat;
begin
  result := true;
  if FNeedSaveFlag.CheckFlagAndReset then begin
    try
      if FStateInternal.WriteAccess = asEnabled then begin
        LockWrite;
        try
          if FStream <> nil then begin
            FStream.Size := 0;
            FStream.Position := 0;
            if FStoreInBinaryFormat then begin
              VFormat := dfBinary;
            end else begin
              VFormat := dfXMLUTF8;
            end;
            FCdsCategory.SaveToStream(FStream, VFormat);
          end else begin
            FNeedSaveFlag.SetFlag;
          end;
        finally
          UnlockWrite;
        end;
      end else begin
        FNeedSaveFlag.SetFlag;
      end;
    except
      result := false;
      FNeedSaveFlag.SetFlag;
    end;
  end;
end;

end.
