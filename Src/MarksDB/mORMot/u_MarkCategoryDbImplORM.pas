{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_MarkCategoryDbImplORM;

interface

uses
  Windows,
  Classes,
  SysUtils,
  mORMot,
  SynCommons,
  i_IDList,
  i_SimpleFlag,
  i_NotifierOperation,
  i_Category,
  i_MarkCategory,
  i_MarkCategoryList,
  i_MarkCategoryFactory,
  i_MarkCategoryFactoryDbInternalORM,
  i_MarkCategoryDbInternalORM,
  i_ReadWriteStateInternal,
  i_MarkCategoryDBImpl,
  u_ConfigDataElementBase;

type
  TMarkCategoryDbImplORM = class(
    TConfigDataElementBaseEmptySaveLoad,
    IMarkCategoryDbInternalORM,
    IMarkCategoryDBImpl
  )
  private
    FDbId: Integer;
    FUserID: TID;
    FClient: TSQLRestClient;
    FFactoryDbInternal: IMarkCategoryFactoryDbInternalORM;
  private
    function _UpdateCategory(
      const AOldCategory: IInterface;
      const ANewCategory: IInterface;
      out AIsChanged: Boolean
    ): IMarkCategory;
    function _GetCategory(const ID: TID; const AName: string): IMarkCategory;
  private
    { IMarkCategoryInternalORM }
    function IsCategoryFromThisDb(const ACategory: ICategory): Boolean;
    function GetCategoryByID(const ID: TID): IMarkCategory;
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
      const AUserID: TID;
      const AClient: TSQLRestClient
    );
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  t_MarkSystemORM,
  t_MarkSystemModelORM,
  t_CommonTypes,
  i_EnumID,
  i_InterfaceListSimple,
  u_IDInterfaceList,
  u_InterfaceListSimple,
  u_SimpleFlagWithInterlock,
  i_MarkCategoryInternalORM,
  u_MarkCategoryList,
  u_MarkCategoryFactoryDbInternalORM;

type
  TCategoryUpdateOperType = (uoUnk, uoUpdate, uoInsert, uoDelete);

constructor TMarkCategoryDbImplORM.Create(
  const ADbId: Integer;
  const AUserID: TID;
  const AClient: TSQLRestClient
);
begin
  Assert(ADbId <> 0);
  Assert(AUserID > 0);
  Assert(Assigned(AClient));

  inherited Create;

  FDbId := ADbId;
  FUserID := AUserID;
  FClient := AClient;
  FFactoryDbInternal := TMarkCategoryFactoryDbInternalORM.Create(FDbId);
end;

destructor TMarkCategoryDbImplORM.Destroy;
begin
  FFactoryDbInternal := nil;
  inherited;
end;

function TMarkCategoryDbImplORM._UpdateCategory(
  const AOldCategory: IInterface;
  const ANewCategory: IInterface;
  out AIsChanged: Boolean
): IMarkCategory;
var
  VOperation: TCategoryUpdateOperType;
  VCategoryID: TID;
  VCategoryInternal: IMarkCategoryInternalORM;
  VCategory: IMarkCategory;
  VOldCategory: IMarkCategory;
  VName, VNewName, VOldCategoryName: string;
  VSQLWhere: RawUTF8;
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
  VSessionID: Cardinal;
begin
  Result := nil;
  AIsChanged := False;

  VOldCategory := nil;
  VCategoryID := 0;
  VOldCategoryName := '';

  // DELETE: if AOldCategory <> nil and ANewCategory = nil  --> Result = nil
  // INSERT: if AOldCategory = nil and ANewCategory <> nil  --> Result <> nil
  // UPDATE: if AOldCategory <> nil and ANewCategory <> nil --> Result <> nil

  if Assigned(AOldCategory) and Assigned(ANewCategory) then begin
    VOperation := uoUpdate;
  end else if not Assigned(AOldCategory) and Assigned(ANewCategory) then begin
    VOperation := uoInsert;
  end else if Assigned(AOldCategory) and not Assigned(ANewCategory) then begin
    VOperation := uoDelete;
  end else begin
    Exit;
  end;

  if Supports(AOldCategory, IMarkCategoryInternalORM, VCategoryInternal) then begin
    if VCategoryInternal.DbId = FDbId then begin
      VCategoryID := VCategoryInternal.ID;
    end;
  end;

  // DELETE
  if VOperation = uoDelete then begin
    if (VCategoryID > 0) then begin
      // first delete view
      VSQLWhere := FormatUTF8('Category=?', [], [VCategoryID]);
      FClient.Delete(TSQLCategoryView, VSQLWhere);
      // then delete category
      AIsChanged := FClient.Delete(TSQLCategory, VCategoryID);
      Assert(AIsChanged);
    end else begin
      Assert(False);
    end;
    Exit;
  end;

  // INSERT
  if VOperation = uoInsert then begin
    if Supports(ANewCategory, IMarkCategory, VCategory) then begin
      VSQLCategory := TSQLCategory.Create;
      try
        // insert category
        VSQLCategory.Name := StringToUTF8(VCategory.Name);
        FClient.Add(VSQLCategory, True);
        // insert view
        if VSQLCategory.ID > 0 then begin
          VSQLCategoryView := TSQLCategoryView.Create;
          try
            VSQLCategoryView.User := Pointer(FUserID);
            VSQLCategoryView.Category := Pointer(VSQLCategory.ID);
            VSQLCategoryView.Visible := VCategory.Visible;
            VSQLCategoryView.MinZoom := VCategory.AfterScale;
            VSQLCategoryView.MaxZoom := VCategory.BeforeScale;
            if FClient.Add(VSQLCategoryView, True) = 0 then begin
              Assert(False);
            end;
          finally
            VSQLCategoryView.Free;
          end;
          Result :=
            FFactoryDbInternal.CreateCategory(
              VSQLCategory.ID,
              VCategory.Name,
              VCategory.Visible,
              VCategory.AfterScale,
              VCategory.BeforeScale
            );
          AIsChanged := True;
        end else begin
          Assert(False);
        end;
      finally
        VSQLCategory.Free;
      end;
    end;
    Exit;
  end;

  // UPDATE

  if VCategoryID > 0 then begin
    VOldCategory := GetCategoryByID(VCategoryID);
    if Assigned(VOldCategory) then begin
      VOldCategoryName := VOldCategory.Name + '\';
      if Supports(ANewCategory, IMarkCategory, VCategory) then begin
        if VOldCategory.IsEqual(VCategory) then begin
          Result := VOldCategory;
          Exit;
        end;
      end;
    end else begin
      VCategoryID := 0;
    end;
  end;

  if Assigned(VOldCategory) then begin
    if Supports(ANewCategory, IMarkCategory, VCategory) then begin
      // update
      VSQLCategory := TSQLCategory.Create(FClient, VCategoryID);
      try
        if VSQLCategory.ID > 0 then begin
          // update view
          VSQLCategoryView := TSQLCategoryView.Create(
            FClient, '(Category=? AND User=?)', [VSQLCategory.ID, FUserID]
          );
          try
            if VSQLCategoryView.ID > 0 then begin
              if (VSQLCategoryView.Visible <> VCategory.Visible) or
                 (VSQLCategoryView.MinZoom <> VCategory.AfterScale) or
                 (VSQLCategoryView.MaxZoom <> VCategory.BeforeScale) then
              begin
                VSQLCategoryView.Visible := VCategory.Visible;
                VSQLCategoryView.MinZoom := VCategory.AfterScale;
                VSQLCategoryView.MaxZoom := VCategory.BeforeScale;
                FClient.Update(VSQLCategoryView);
              end;
            end else begin
              VSQLCategoryView.User := Pointer(FUserID);
              VSQLCategoryView.Category := Pointer(VSQLCategory.ID);
              VSQLCategoryView.Visible := VCategory.Visible;
              VSQLCategoryView.MinZoom := VCategory.AfterScale;
              VSQLCategoryView.MaxZoom := VCategory.BeforeScale;
              FClient.Add(VSQLCategoryView, True);
            end;
          finally
            VSQLCategoryView.Free;
          end;
          // update name
          if UTF8ToString(VSQLCategory.Name) <> VCategory.Name then begin
            VSQLCategory.Name := StringToUTF8(VCategory.Name);
            FClient.Update(VSQLCategory);
            // update sub categories names
            VSessionID := GetTickCount;
            FClient.TransactionBegin(TSQLCategory, VSessionID);
            try
              VNewName := VCategory.Name + '\';
              VSQLCategory.FillPrepare(FClient, '');
              while VSQLCategory.FillOne do begin
                VName := UTF8ToString(VSQLCategory.Name);
                if StartsText(VOldCategoryName, VName) then begin
                  VSQLCategory.Name := StringToUTF8(
                    StringReplace(VName, VOldCategoryName, VNewName, [rfIgnoreCase])
                  );
                  FClient.Update(VSQLCategory);
                end;
              end;
            finally
              FClient.Commit(VSessionID, False);
            end;
          end;
          Result :=
            FFactoryDbInternal.CreateCategory(
              VSQLCategory.ID,
              VCategory.Name,
              VCategory.Visible,
              VCategory.AfterScale,
              VCategory.BeforeScale
            );
          AIsChanged := True;
        end else begin
          Assert(False);
        end;
      finally
        VSQLCategory.Free;
      end;
    end;
  end;
end;

function TMarkCategoryDbImplORM.UpdateCategory(
  const AOldCategory: IMarkCategory;
  const ANewCategory: IMarkCategory
): IMarkCategory;
var
  VIsChanged: Boolean;
begin
  Assert( (AOldCategory <> nil) or (ANewCategory <> nil) );
  LockWrite;
  try
    Result := _UpdateCategory(AOldCategory, ANewCategory, VIsChanged);
    if VIsChanged then begin
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDbImplORM.UpdateCategoryList(
  const AOldCategoryList: IMarkCategoryList;
  const ANewCategoryList: IMarkCategoryList
): IMarkCategoryList;
var
  I: Integer;
  VNew: IInterface;
  VOld: IInterface;
  VResult: ICategory;
  VMinCount: Integer;
  VMaxCount: Integer;
  VIsChanged: Boolean;
  VDoNotify: Boolean;
  VSessionID: Cardinal;
  VTemp:  IInterfaceListSimple;
begin
  Result := nil;
  LockWrite;
  try
    VDoNotify := False;
    VSessionID := GetTickCount;
    FClient.TransactionBegin(TSQLCategory, VSessionID);
    try
      if ANewCategoryList <> nil then begin
        VTemp := TInterfaceListSimple.Create;
        VTemp.Capacity := ANewCategoryList.Count;

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

        for I := 0 to VMinCount - 1 do begin
          VOld := AOldCategoryList[I];
          VNew := ANewCategoryList[I];
          VResult := _UpdateCategory(VOld, VNew, VIsChanged);
          VDoNotify := VDoNotify or VIsChanged;
          VTemp.Add(VResult);
        end;

        for I := VMinCount to VMaxCount - 1 do begin
          VOld := nil;
          if (AOldCategoryList <> nil) and (I < AOldCategoryList.Count) then begin
            VOld := AOldCategoryList[I];
          end;
          VNew := nil;
          if (I < ANewCategoryList.Count) then begin
            VNew := ANewCategoryList[I];
          end;
          VResult := _UpdateCategory(VOld, VNew, VIsChanged);
          VDoNotify := VDoNotify or VIsChanged;
          if I < VTemp.Capacity then begin
            VTemp.Add(VResult);
          end;
        end;

        Result := TMarkCategoryList.Build(VTemp.MakeStaticAndClear);
      end else begin
        for I := 0 to AOldCategoryList.Count - 1 do begin
          _UpdateCategory(AOldCategoryList[I], nil, VIsChanged);
          VDoNotify := VDoNotify or VIsChanged;
        end;
      end;
    finally
      FClient.Commit(VSessionID, False);
    end;
    if VDoNotify then begin
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDbImplORM.IsCategoryFromThisDb(
  const ACategory: ICategory
): Boolean;
var
  VCategoryInternal: IMarkCategoryInternalORM;
begin
  Assert(Assigned(ACategory));
  Result := False;
  LockRead;
  try
    if Supports(ACategory, IMarkCategoryInternalORM, VCategoryInternal) then begin
      Result := (VCategoryInternal.DbId = FDbId);
    end;
  finally
    UnlockRead;
  end;
end;

function TMarkCategoryDbImplORM._GetCategory(const ID: TID; const AName: string): IMarkCategory;
var
  VCategory: TSQLCategory;
  VCategoryView: TSQLCategoryView;
begin
  Assert( (ID > 0) or (AName <> '') );

  Result := nil;

  if ID > 0 then begin
    VCategory := TSQLCategory.Create(FClient, ID);
  end else if AName <> '' then begin
    VCategory := TSQLCategory.Create(FClient, 'Name=?', [AName]);
  end else begin
    Exit;
  end;

  try
    if VCategory.ID > 0 then begin
      VCategoryView := TSQLCategoryView.Create(
        FClient, 'User=? AND Category=?', [FUserID, VCategory.ID]
      );
      try
        if VCategoryView.ID > 0 then begin
          Result :=
            FFactoryDbInternal.CreateCategory(
              VCategory.ID,
              UTF8ToString(VCategory.Name),
              VCategoryView.Visible,
              VCategoryView.MinZoom,
              VCategoryView.MaxZoom
            );
        end else begin
          Result :=
            FFactoryDbInternal.CreateCategory(
              VCategory.ID,
              UTF8ToString(VCategory.Name)
            );
        end;
      finally
        VCategoryView.Free;
      end;
    end;
  finally
    VCategory.Free;
  end;
end;

function TMarkCategoryDbImplORM.GetCategoryByID(const ID: TID): IMarkCategory;
begin
  Assert(ID > 0);
  LockRead;
  try
    Result := _GetCategory(ID, '');
  finally
    UnlockRead;
  end;
end;

function TMarkCategoryDbImplORM.GetCategoryByName(const AName: string): IMarkCategory;
begin
  Assert(AName <> '');
  LockRead;
  try
    Result := _GetCategory(0, AName);
  finally
    UnlockRead;
  end;
end;

procedure TMarkCategoryDbImplORM.SetAllCategoriesVisible(ANewVisible: Boolean);
var
  VSQLWhere: RawUTF8;
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
  VDoNotify: Boolean;
  VSessionID: Cardinal;
begin
  LockWrite;
  try
    VDoNotify := False;
    VSessionID := GetTickCount;
    FClient.TransactionBegin(TSQLCategoryView, VSessionID);
    try
      if ANewVisible then begin
        VSQLCategoryView := TSQLCategoryView.CreateAndFillPrepare(
          FClient, 'User=?', [FUserID]
        );
        try
          while VSQLCategoryView.FillOne do begin
            if VSQLCategoryView.Visible <> ANewVisible then begin
              VDoNotify := VDoNotify or FClient.UpdateField(
                TSQLCategoryView, VSQLCategoryView.ID, 'Visible', [ANewVisible]
              );
              Assert(VDoNotify);
            end;
          end;
        finally
          VSQLCategoryView.Free;
        end;
      end else begin
        VSQLCategoryView := TSQLCategoryView.Create;
        try
          VSQLCategory := TSQLCategory.CreateAndFillPrepare(FClient, '');
          try
            while VSQLCategory.FillOne do begin
              VSQLWhere := FormatUTF8('(Category=? AND User=?)', [], [VSQLCategory.ID, FUserID]);
              if FClient.Retrieve(VSQLWhere, VSQLCategoryView) then begin
                if VSQLCategoryView.Visible <> ANewVisible then begin
                  VDoNotify := VDoNotify or FClient.UpdateField(
                    TSQLCategoryView, VSQLCategoryView.ID, 'Visible', [ANewVisible]
                  );
                  Assert(VDoNotify);
                end;
              end else begin
                VSQLCategoryView.User := Pointer(FUserID);
                VSQLCategoryView.Category := Pointer(VSQLCategory.ID);
                VSQLCategoryView.Visible := ANewVisible;
                VSQLCategoryView.MinZoom := 3;
                VSQLCategoryView.MaxZoom := 23;
                if FClient.Add(VSQLCategoryView, True) > 0 then begin
                  VDoNotify := True;
                end else begin
                  Assert(False);
                end;
              end;
            end;
          finally
            VSQLCategory.Free;
          end;
        finally
          VSQLCategoryView.Free;
        end;
      end;
    finally
      FClient.Commit(VSessionID, False);
    end;
    if VDoNotify then begin
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDbImplORM.GetCategoriesList: IMarkCategoryList;
var
  VMarkCategory: IMarkCategory;
  VTemp: IInterfaceListSimple;
  VSQLWhere: RawUTF8;
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
begin
  VTemp := TInterfaceListSimple.Create;

  LockRead;
  try
    VSQLCategoryView := TSQLCategoryView.Create;
    try
      VSQLCategory := TSQLCategory.CreateAndFillPrepare(FClient, '');
      try
        while VSQLCategory.FillOne do begin
          VSQLWhere := FormatUTF8('(Category=? AND User=?)', [], [VSQLCategory.ID, FUserID]);
          if FClient.Retrieve(VSQLWhere, VSQLCategoryView) then begin
            VMarkCategory :=
              FFactoryDbInternal.CreateCategory(
                VSQLCategory.ID,
                UTF8ToString(VSQLCategory.Name),
                VSQLCategoryView.Visible,
                VSQLCategoryView.MinZoom,
                VSQLCategoryView.MaxZoom
              );
          end else begin
            VMarkCategory :=
              FFactoryDbInternal.CreateCategory(
                VSQLCategory.ID,
                UTF8ToString(VSQLCategory.Name)
              );
          end;
          VTemp.Add(VMarkCategory);
        end;
      finally
        VSQLCategory.Free;
      end;
    finally
      VSQLCategoryView.Free;
    end;
  finally
    UnlockRead;
  end;

  Result := TMarkCategoryList.Build(VTemp.MakeStaticAndClear);
end;

end.
