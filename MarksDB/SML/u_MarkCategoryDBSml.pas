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

unit u_MarkCategoryDBSml;

interface

uses
  Windows,
  Classes,
  SysUtils,
  DBClient,
  i_IDList,
  i_SimpleFlag,
  i_MarkCategory,
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
    FFileName: string;
    FStateInternal: IReadWriteStateInternal;

    FStream: TStream;
    FCdsKategory: TClientDataSet;
    FList: IIDInterfaceList;
    FFactoryDbInternal: IMarkCategoryFactoryDbInternal;
    FFactory: IMarkCategoryFactory;
    FNeedSaveFlag: ISimpleFlag;

    function ReadCurrentCategory(out AId: Integer): IMarkCategory;
    procedure WriteCurrentCategory(const ACategory: IMarkCategory);
    function GetMarksCategoryBackUpFileName: string;
    procedure InitEmptyDS;
    function _UpdateCategory(
      const AOldCategory: IInterface;
      const ANewCategory: IInterface
    ): IMarkCategory;
    function SaveCategory2File: boolean;
    procedure LoadCategoriesFromFile;
  private
    function GetCategoryByName(const AName: string): IMarkCategory;
    function UpdateCategory(
      const AOldCategory: IMarkCategory;
      const ANewCategory: IMarkCategory
    ): IMarkCategory;
    function UpdateCategoryList(
      const AOldCategoryList: IInterfaceList;
      const ANewCategoryList: IInterfaceList
    ): IInterfaceList;

    function GetCategoriesList: IInterfaceList;
    procedure SetAllCategoriesVisible(ANewVisible: Boolean);
  private
    function GetCategoryByID(id: integer): IMarkCategory;
  public
    constructor Create(
      const ADbId: Integer;
      const AStateInternal: IReadWriteStateInternal;
      const AFileName: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  DB,
  t_CommonTypes,
  i_EnumID,
  i_Category,
  u_IDInterfaceList,
  u_SimpleFlagWithInterlock,
  i_MarkDbSmlInternal,
  u_MarkCategoryFactorySmlDbInternal;

constructor TMarkCategoryDBSml.Create(
  const ADbId: Integer;
  const AStateInternal: IReadWriteStateInternal;
  const AFileName: string
);
begin
  inherited Create;
  FDbId := ADbId;
  FFileName := AFileName;
  FStateInternal := AStateInternal;
  FList := TIDInterfaceList.Create;
  FFactoryDbInternal := TMarkCategoryFactorySmlDbInternal.Create(FDbId);
  FNeedSaveFlag := TSimpleFlagWithInterlock.Create;
  FCdsKategory := TClientDataSet.Create(nil);
  FCdsKategory.Name := 'CDSKategory';
  FCdsKategory.DisableControls;
  InitEmptyDS;
  LoadCategoriesFromFile;
end;

destructor TMarkCategoryDBSml.Destroy;
begin
  if Assigned(FCdsKategory) then begin
    SaveCategory2File;
  end;
  FreeAndNil(FStream);
  FreeAndNil(FCdsKategory);
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
  AId := FCdsKategory.fieldbyname('id').AsInteger;
  VName := FCdsKategory.fieldbyname('name').AsString;
  VVisible := FCdsKategory.FieldByName('visible').AsBoolean;
  VAfterScale := FCdsKategory.fieldbyname('AfterScale').AsInteger;
  VBeforeScale := FCdsKategory.fieldbyname('BeforeScale').AsInteger;
  Result := FFactoryDbInternal.CreateCategory(AId, VName, VVisible, VAfterScale, VBeforeScale);
end;

procedure TMarkCategoryDBSml.WriteCurrentCategory(const ACategory: IMarkCategory);
begin
  FCdsKategory.fieldbyname('name').AsString := ACategory.Name;
  FCdsKategory.FieldByName('visible').AsBoolean := ACategory.Visible;
  FCdsKategory.fieldbyname('AfterScale').AsInteger := ACategory.AfterScale;
  FCdsKategory.fieldbyname('BeforeScale').AsInteger := ACategory.BeforeScale;
end;

function TMarkCategoryDBSml._UpdateCategory(
  const AOldCategory, ANewCategory: IInterface
): IMarkCategory;
var
  VIdOld: Integer;
  VIdNew: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
  VCategory: IMarkCategory;
  VOldCategory: IMarkCategory;
  VLocated: Boolean;
begin
  Result := nil;
  VIdOld := CNotExistCategoryID;
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
      if Supports(ANewCategory, IMarkCategory, VCategory) then begin
        if VOldCategory.IsEqual(VCategory) then begin
          Result := VOldCategory;
          Exit;
        end;
      end;
    end;
    VLocated := FCdsKategory.Locate('id', VIdOld, []);
  end;
  if VLocated then begin
    if Supports(ANewCategory, IMarkCategory, VCategory) then begin
      FCdsKategory.Edit;
      WriteCurrentCategory(VCategory);
      FCdsKategory.post;
      SetChanged;
      FNeedSaveFlag.SetFlag;
      Result := ReadCurrentCategory(VIdNew);
      Assert(Result <> nil);
      Assert(VIdNew >= 0);
      Assert(VIdNew = VIdOld);
    end else begin
      FCdsKategory.Delete;
      SetChanged;
      FNeedSaveFlag.SetFlag;
    end;
  end else begin
    if Supports(ANewCategory, IMarkCategory, VCategory) then begin
      FCdsKategory.Insert;
      WriteCurrentCategory(VCategory);
      FCdsKategory.post;
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
    SaveCategory2File;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDBSml.UpdateCategoryList(const AOldCategoryList,
  ANewCategoryList: IInterfaceList): IInterfaceList;
var
  i: Integer;
  VNew: IInterface;
  VOld: IInterface;
  VResult: ICategory;
  VMinCount: Integer;
  VMaxCount: Integer;
begin
  Result := nil;
  if ANewCategoryList <> nil then begin
    Result := TInterfaceList.Create;
    Result.Capacity := ANewCategoryList.Count;

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
        Result.Add(VResult);
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
        if i < Result.Capacity then begin
          Result.Add(VResult);
        end;
      end;
    finally
      UnlockWrite;
    end;
    SaveCategory2File;
  end else begin
    LockWrite;
    try
      for i := 0 to AOldCategoryList.Count - 1 do begin
        _UpdateCategory(AOldCategoryList[i], nil);
      end;
    finally
      UnlockWrite;
    end;
    SaveCategory2File;
  end;
end;

procedure TMarkCategoryDBSml.InitEmptyDS;
begin
  FCdsKategory.Close;
  FCdsKategory.XMLData :=
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
    '<DATAPACKET Version="2.0">' +
    '<METADATA>' +
    '<FIELDS>' +
    '<FIELD attrname="id" fieldtype="i4" readonly="true" SUBTYPE="Autoinc"/>' +
    '<FIELD attrname="name" fieldtype="string" WIDTH="256"/>' +
    '<FIELD attrname="visible" fieldtype="boolean"/>' +
    '<FIELD attrname="AfterScale" fieldtype="i2"/>' +
    '<FIELD attrname="BeforeScale" fieldtype="i2"/>' +
    '</FIELDS>' +
    '<PARAMS AUTOINCVALUE="1"/>' +
    '</METADATA>' +
    '<ROWDATA></ROWDATA>' +
    '</DATAPACKET>';
  FCdsKategory.Open;
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
    FCdsKategory.Filtered := false;
    FCdsKategory.First;
    while not (FCdsKategory.Eof) do begin
      if FCdsKategory.FieldByName('visible').AsBoolean <> ANewVisible then begin
        FCdsKategory.Edit;
        FCdsKategory.FieldByName('visible').AsBoolean := ANewVisible;
        FCdsKategory.post;
        VCategory := ReadCurrentCategory(VId);
        FList.Replace(VId, VCategory);
        VChanged := True;
      end;
      FCdsKategory.Next;
    end;
    if VChanged then begin
      SetChanged;
      FNeedSaveFlag.SetFlag;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDBSml.GetCategoriesList: IInterfaceList;
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

function TMarkCategoryDBSml.GetMarksCategoryBackUpFileName: string;
begin
  Result := ChangeFileExt(FFileName, '.~sml');
end;

procedure TMarkCategoryDBSml.LoadCategoriesFromFile;
var
  VFileName: string;
  VCategory: IMarkCategory;
  VId: Integer;
  XML: AnsiString;
  VStream: TFileStream;
begin
  VFileName := FFileName;
  FStateInternal.LockWrite;
  try
    LockWrite;
    try
      InitEmptyDS;
      FList.Clear;
      if FStateInternal.ReadAccess <> asDisabled then begin
        VStream := nil;
        try
          if FileExists(VFileName) then begin
            if FStateInternal.WriteAccess <> asDisabled then begin
              try
                VStream := TFileStream.Create(VFileName, fmOpenReadWrite + fmShareDenyWrite);
                FStateInternal.WriteAccess := asEnabled;
              except
                VStream := nil;
                FStateInternal.WriteAccess := asDisabled;
              end;
            end;
            if VStream = nil then begin
              try
                VStream := TFileStream.Create(VFileName, fmOpenRead + fmShareDenyNone);
                FStateInternal.ReadAccess := asEnabled;
              except
                FStateInternal.ReadAccess := asDisabled;
                VStream := nil;
              end;
            end;
            if VStream <> nil then begin
              try
                SetLength(XML, VStream.Size);
                VStream.ReadBuffer(XML[1], length(XML));
              except
                FStateInternal.ReadAccess := asDisabled;
                VStream.Free;
                VStream := nil;
              end;
            end;

            if length(XML) > 0 then begin
              try
                FCdsKategory.XMLData := XML;
              except
                InitEmptyDS;
              end;
            end;

            if FCdsKategory.RecordCount > 0 then begin
              if FStateInternal.WriteAccess = asEnabled then begin
                CopyFile(PChar(VFileName), PChar(GetMarksCategoryBackUpFileName), false);
              end;
            end;

            FCdsKategory.Filtered := false;
            FCdsKategory.First;
            while not (FCdsKategory.Eof) do begin
              VCategory := ReadCurrentCategory(VId);
              FList.Add(VId, VCategory);
              FCdsKategory.Next;
            end;
          end else begin
            if FStateInternal.WriteAccess <> asDisabled then begin
              try
                VStream := TFileStream.Create(VFileName, fmCreate);
                VStream.Free;
                VStream := nil;
              except
                FStateInternal.WriteAccess := asDisabled;
                VStream := nil;
              end;
              if FStateInternal.WriteAccess <> asDisabled then begin
                try
                  VStream := TFileStream.Create(VFileName, fmOpenReadWrite + fmShareDenyWrite);
                  FStateInternal.WriteAccess := asEnabled;
                except
                  VStream := nil;
                  FStateInternal.WriteAccess := asDisabled;
                end;
              end;
            end;
          end;
          if FStream <> nil then begin
            FreeAndNil(FStream);
          end;
          if FStateInternal.WriteAccess = asEnabled then begin
            FStream := VStream;
            VStream := nil;
          end;
        finally
          VStream.Free;
        end;
      end;
    finally
      UnlockWrite;
    end;
  finally
    FStateInternal.UnlockWrite;
  end;
end;

function TMarkCategoryDBSml.SaveCategory2File: boolean;
var
  XML: AnsiString;
begin
  result := true;
  if FNeedSaveFlag.CheckFlagAndReset then begin
    try
      FStateInternal.LockRead;
      try
        if FStateInternal.WriteAccess = asEnabled then begin
          LockRead;
          try
            if FStream <> nil then begin
              FCdsKategory.MergeChangeLog;
              XML := FCdsKategory.XMLData;
              FStream.Size := length(XML);
              FStream.Position := 0;
              FStream.WriteBuffer(XML[1], length(XML));
            end else begin
              FNeedSaveFlag.SetFlag;
            end;
          finally
            UnlockRead;
          end;
        end else begin
          FNeedSaveFlag.SetFlag;
        end;
      finally
        FStateInternal.UnlockRead;
      end;
    except
      result := false;
      FNeedSaveFlag.SetFlag;
    end;
  end;
end;


end.
