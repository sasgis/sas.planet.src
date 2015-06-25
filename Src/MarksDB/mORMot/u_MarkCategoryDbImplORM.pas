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
      out AIsChanged: Boolean;
      const AUseTransactions: Boolean
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
  i_MarkCategoryInternalORM,
  u_IDInterfaceList,
  u_InterfaceListSimple,
  u_SimpleFlagWithInterlock,
  u_MarkCategoryList,
  u_MarkSystemORMTools,
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

procedure _SQLCategoryRecFromCategory(
  const ACategoryID: TID;
  const ACategory: IMarkCategory;
  out ACategoryRec: TSQLCategoryRec
);
begin
  ACategoryRec.FCategoryId := ACategoryID;
  ACategoryRec.FName := ACategory.Name;
  ACategoryRec.FVisible := ACategory.Visible;
  ACategoryRec.FMinZoom := ACategory.AfterScale;
  ACategoryRec.FMaxZoom := ACategory.BeforeScale;
end;

procedure _DeleteCategorySQL(const ACategoryID: TID; const AClient: TSQLRestClient);
var
  VSQLWhere: RawUTF8;
begin
  CheckID(ACategoryID);
  // first delete view for all users
  VSQLWhere := FormatUTF8('Category=?', [], [ACategoryID]);
  CheckDeleteResult( AClient.Delete(TSQLCategoryView, VSQLWhere) );
  // then delete category
  CheckDeleteResult( AClient.Delete(TSQLCategory, ACategoryID) );
end;

procedure _InsertCategorySQL(
  var ACategoryRec: TSQLCategoryRec;
  const AUserID: TID;
  const AClient: TSQLRestClient
);
var
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
begin
  // insert category
  VSQLCategory := TSQLCategory.Create;
  try
    VSQLCategory.Name := StringToUTF8(ACategoryRec.FName);
    CheckID( AClient.Add(VSQLCategory, True) );
    ACategoryRec.FCategoryId := VSQLCategory.ID;
  finally
    VSQLCategory.Free;
  end;
  // insert view
  VSQLCategoryView := TSQLCategoryView.Create;
  try
    VSQLCategoryView.User := Pointer(AUserID);
    VSQLCategoryView.Category := Pointer(ACategoryRec.FCategoryId);
    VSQLCategoryView.Visible := ACategoryRec.FVisible;
    VSQLCategoryView.MinZoom := ACategoryRec.FMinZoom;
    VSQLCategoryView.MaxZoom := ACategoryRec.FMaxZoom;
    CheckID( AClient.Add(VSQLCategoryView, True) );
  finally
    VSQLCategoryView.Free;
  end;
end;

procedure _UpdateCategorySQL(
  const ACategoryRecOld: TSQLCategoryRec;
  var ACategoryRecNew: TSQLCategoryRec;
  const AUserID: TID;
  const AClient: TSQLRestClient
);
var
  VName: RawUTF8;
  VUpdateName: string;
  VSearchName: string;
  VOldName: string;
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
  VUpdateView: Boolean;
begin
  CheckID(ACategoryRecOld.FCategoryId);

  ACategoryRecNew.FCategoryId := ACategoryRecOld.FCategoryId;

  VUpdateView :=
    (ACategoryRecNew.FVisible <> ACategoryRecOld.FVisible) or
    (ACategoryRecNew.FMinZoom <> ACategoryRecOld.FMinZoom) or
    (ACategoryRecNew.FMaxZoom <> ACategoryRecOld.FMaxZoom);

  if VUpdateView then begin
    // update view
    VSQLCategoryView := TSQLCategoryView.Create(
      AClient, 'Category=? AND User=?', [ACategoryRecNew.FCategoryId, AUserID]
    );
    try
      if VSQLCategoryView.ID > 0 then begin
        VSQLCategoryView.Visible := ACategoryRecNew.FVisible;
        VSQLCategoryView.MinZoom := ACategoryRecNew.FMinZoom;
        VSQLCategoryView.MaxZoom := ACategoryRecNew.FMaxZoom;
        AClient.Update(VSQLCategoryView);
      end else begin
        VSQLCategoryView.User := Pointer(AUserID);
        VSQLCategoryView.Category := Pointer(ACategoryRecNew.FCategoryId);
        VSQLCategoryView.Visible := ACategoryRecNew.FVisible;
        VSQLCategoryView.MinZoom := ACategoryRecNew.FMinZoom;
        VSQLCategoryView.MaxZoom := ACategoryRecNew.FMaxZoom;
        CheckID( AClient.Add(VSQLCategoryView, True) );
      end;
    finally
      VSQLCategoryView.Free;
    end;
  end;

  if ACategoryRecNew.FName <> ACategoryRecOld.FName then begin
    VName := StringToUTF8(ACategoryRecNew.FName);
    // update name
    CheckUpdateResult(
      AClient.UpdateField(
        TSQLCategory, ACategoryRecNew.FCategoryId, 'Name', [VName]
      )
    );
    // update sub categories names
    VSearchName := ACategoryRecOld.FName + '\';
    VUpdateName := ACategoryRecNew.FName + '\';
    VSQLCategory := TSQLCategory.CreateAndFillPrepare(AClient, '');
    try
      while VSQLCategory.FillOne do begin
        VOldName := UTF8ToString(VSQLCategory.Name);
        if StartsText(VSearchName, VOldName) then begin
          VName := StringToUTF8(
            StringReplace(VOldName, VSearchName, VUpdateName, [rfIgnoreCase])
          );
          CheckUpdateResult(
            AClient.UpdateField(
              TSQLCategory, VSQLCategory.ID, 'Name', [VName]
            )
          );
        end;
      end;
    finally
      VSQLCategory.Free;
    end;
  end;
end;

function TMarkCategoryDbImplORM._UpdateCategory(
  const AOldCategory: IInterface;
  const ANewCategory: IInterface;
  out AIsChanged: Boolean;
  const AUseTransactions: Boolean
): IMarkCategory;
var
  VOperation: TCategoryUpdateOperType;
  VCategoryID: TID;
  VNewCategory: IMarkCategory;
  VOldCategory: IMarkCategory;
  VCategoryInternal: IMarkCategoryInternalORM;
  VSQLCategoryRecNew: TSQLCategoryRec;
  VSQLCategoryRecOld: TSQLCategoryRec;
  VTransaction: TTransactionRec;
begin
  Result := nil;
  AIsChanged := False;

  VOldCategory := nil;
  VCategoryID := 0;

  if not Assigned(AOldCategory) and not Assigned(ANewCategory) then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Old and New is nil!');
  end;

  if Supports(AOldCategory, IMarkCategoryInternalORM, VCategoryInternal) then begin
    if VCategoryInternal.DbId = FDbId then begin
      VCategoryID := VCategoryInternal.ID;
    end;
  end;

  if AUseTransactions then begin
    StartTransaction(FClient, VTransaction, TSQLCategory);
  end;
  try
    if Assigned(AOldCategory) and not Assigned(ANewCategory) then
    begin // DELETE
      _DeleteCategorySQL(VCategoryID, FClient);
      AIsChanged := True;
    end
    else
    if not Assigned(AOldCategory) and Assigned(ANewCategory) then
    begin // INSERT
      if Supports(ANewCategory, IMarkCategory, VNewCategory) then begin
        _SQLCategoryRecFromCategory(0, VNewCategory, VSQLCategoryRecNew);
        _InsertCategorySQL(VSQLCategoryRecNew, FUserID, FClient);
        Result :=
          FFactoryDbInternal.CreateCategory(
            VSQLCategoryRecNew.FCategoryId,
            VSQLCategoryRecNew.FName,
            VSQLCategoryRecNew.FVisible,
            VSQLCategoryRecNew.FMinZoom,
            VSQLCategoryRecNew.FMaxZoom
          );
        AIsChanged := True;
      end else begin
        raise EMarkSystemORMError.Create('MarkSystemORM: Unknown category interface!');
      end;
    end
    else if Assigned(AOldCategory) and Assigned(ANewCategory) then         
    begin // UPDATE
      if VCategoryID > 0 then begin
        VOldCategory := GetCategoryByID(VCategoryID);
        if Assigned(VOldCategory) then begin
          if Supports(ANewCategory, IMarkCategory, VNewCategory) then begin
            if VOldCategory.IsEqual(VNewCategory) then begin
              Result := VOldCategory;
              VOldCategory := nil; // force abort update
            end;
          end;
        end else begin
          VCategoryID := 0;
        end;
      end;
      if Assigned(VOldCategory) then begin
        if Supports(ANewCategory, IMarkCategory, VNewCategory) then begin
          _SQLCategoryRecFromCategory(0, VNewCategory, VSQLCategoryRecNew);
          _SQLCategoryRecFromCategory(VCategoryID, VOldCategory, VSQLCategoryRecOld);
          _UpdateCategorySQL(VSQLCategoryRecOld, VSQLCategoryRecNew, FUserID, FClient);
          Result :=
            FFactoryDbInternal.CreateCategory(
              VSQLCategoryRecNew.FCategoryId,
              VSQLCategoryRecNew.FName,
              VSQLCategoryRecNew.FVisible,
              VSQLCategoryRecNew.FMinZoom,
              VSQLCategoryRecNew.FMaxZoom
            );
          AIsChanged := True;
        end;
      end;
    end;
    if AUseTransactions then begin
      CommitTransaction(FClient, VTransaction);
    end;
  except
    if AUseTransactions then begin
      RollBackTransaction(FClient, VTransaction);
    end;
    raise;
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
    Result := _UpdateCategory(AOldCategory, ANewCategory, VIsChanged, True);
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
  VTransaction: TTransactionRec;
  VTemp:  IInterfaceListSimple;
begin
  Result := nil;
  LockWrite;
  try
    VDoNotify := False;

    StartTransaction(FClient, VTransaction, TSQLCategory);
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
          VResult := _UpdateCategory(VOld, VNew, VIsChanged, False);
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
          VResult := _UpdateCategory(VOld, VNew, VIsChanged, False);
          VDoNotify := VDoNotify or VIsChanged;
          if I < VTemp.Capacity then begin
            VTemp.Add(VResult);
          end;
        end;

        Result := TMarkCategoryList.Build(VTemp.MakeStaticAndClear);
      end else begin
        for I := 0 to AOldCategoryList.Count - 1 do begin
          _UpdateCategory(AOldCategoryList[I], nil, VIsChanged, False);
          VDoNotify := VDoNotify or VIsChanged;
        end;
      end;

      CommitTransaction(FClient, VTransaction);
    except
      RollBackTransaction(FClient, VTransaction);
      raise;
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
  VName: RawUTF8;
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
begin
  Assert( (ID > 0) or (AName <> '') );

  Result := nil;

  if ID > 0 then begin
    VSQLCategory := TSQLCategory.Create(FClient, ID);
  end else if AName <> '' then begin
    VName := StringToUTF8(AName);
    VSQLCategory := TSQLCategory.Create(FClient, 'Name=?', [VName]);
  end else begin
    Exit;
  end;

  try
    if VSQLCategory.ID > 0 then begin
      VSQLCategoryView := TSQLCategoryView.Create(
        FClient, 'Category=? AND User=?', [VSQLCategory.ID, FUserID]
      );
      try
        if VSQLCategoryView.ID > 0 then begin
          Result :=
            FFactoryDbInternal.CreateCategory(
              VSQLCategory.ID,
              UTF8ToString(VSQLCategory.Name),
              VSQLCategoryView.Visible,
              VSQLCategoryView.MinZoom,
              VSQLCategoryView.MaxZoom
            );
        end else begin
          Result :=
            FFactoryDbInternal.CreateCategory(
              VSQLCategory.ID,
              UTF8ToString(VSQLCategory.Name)
            );
        end;
      finally
        VSQLCategoryView.Free;
      end;
    end;
  finally
    VSQLCategory.Free;
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
  VTransaction: TTransactionRec;
begin
  LockWrite;
  try
    VDoNotify := False;

    StartTransaction(FClient, VTransaction, TSQLCategoryView);
    try
      if ANewVisible then begin
        VSQLCategoryView := TSQLCategoryView.CreateAndFillPrepare(
          FClient, 'User=?', [FUserID]
        );
        try
          while VSQLCategoryView.FillOne do begin
            if VSQLCategoryView.Visible <> ANewVisible then begin
              CheckUpdateResult(
                FClient.UpdateField(
                  TSQLCategoryView, VSQLCategoryView.ID, 'Visible', [ANewVisible]
                )
              );
              VDoNotify := True;
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
              VSQLWhere := FormatUTF8('Category=? AND User=?', [], [VSQLCategory.ID, FUserID]);
              if FClient.Retrieve(VSQLWhere, VSQLCategoryView) then begin
                if VSQLCategoryView.Visible <> ANewVisible then begin
                  CheckUpdateResult(
                    FClient.UpdateField(
                      TSQLCategoryView, VSQLCategoryView.ID, 'Visible', [ANewVisible]
                    )
                  );
                  VDoNotify := True;
                end;
              end else begin
                VSQLCategoryView.User := Pointer(FUserID);
                VSQLCategoryView.Category := Pointer(VSQLCategory.ID);
                VSQLCategoryView.Visible := ANewVisible;
                VSQLCategoryView.MinZoom := 3;
                VSQLCategoryView.MaxZoom := 23;
                CheckID( FClient.Add(VSQLCategoryView, True) );
                VDoNotify := True;
              end;
            end;
          finally
            VSQLCategory.Free;
          end;
        finally
          VSQLCategoryView.Free;
        end;
      end;

      CommitTransaction(FClient, VTransaction);
    except
      RollBackTransaction(FClient, VTransaction);
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
          VSQLWhere := FormatUTF8('Category=? AND User=?', [], [VSQLCategory.ID, FUserID]);
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
