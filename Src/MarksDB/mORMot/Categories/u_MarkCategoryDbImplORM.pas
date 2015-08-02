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
  SysUtils,
  mORMot,
  SynCommons,
  i_Category,
  i_MarkCategory,
  i_MarkCategoryList,
  i_MarkCategoryFactoryDbInternalORM,
  i_MarkCategoryDbInternalORM,
  i_MarkCategoryDBImpl,
  i_MarkSystemImplORMClientProvider,
  u_MarkCategoryDbImplORMCache,
  u_MarkCategoryDbImplORMHelper,
  u_ConfigDataElementBase;

type
  TMarkCategoryDbImplORM = class(
    TConfigDataElementBaseEmptySaveLoad,
    IMarkCategoryDbInternalORM,
    IMarkCategoryDBImpl
  )
  private
    FDbId: Integer;
    FClient: TSQLRestClient;
    FFactoryDbInternal: IMarkCategoryFactoryDbInternalORM;
    FHelper: TMarkCategoryDbImplORMHelper;
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
      const AClientProvider: IMarkSystemImplORMClientProvider
    );
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  t_MarkSystemORM,
  i_InterfaceListSimple,
  i_MarkCategoryInternalORM,
  u_InterfaceListSimple,
  u_MarkCategoryList,
  u_MarkSystemORMTools,
  u_MarkSystemORMModel,
  u_MarkCategoryFactoryDbInternalORM;

constructor TMarkCategoryDbImplORM.Create(
  const ADbId: Integer;
  const AClientProvider: IMarkSystemImplORMClientProvider
);
begin
  Assert(ADbId <> 0);
  Assert(Assigned(AClientProvider));

  inherited Create;

  FDbId := ADbId;
  FClient := AClientProvider.RestClient;
  FHelper := TMarkCategoryDbImplORMHelper.Create(AClientProvider);
  FFactoryDbInternal := TMarkCategoryFactoryDbInternalORM.Create(FDbId);
end;

destructor TMarkCategoryDbImplORM.Destroy;
begin
  FreeAndNil(FHelper);
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
  ACategoryRec.FViewId := 0;
  ACategoryRec.FVisible := ACategory.Visible;
  ACategoryRec.FMinZoom := ACategory.AfterScale;
  ACategoryRec.FMaxZoom := ACategory.BeforeScale;
end;

function TMarkCategoryDbImplORM._UpdateCategory(
  const AOldCategory: IInterface;
  const ANewCategory: IInterface;
  out AIsChanged: Boolean;
  const AUseTransactions: Boolean
): IMarkCategory;
var
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
      if Assigned(VCategoryInternal) and (VCategoryID > 0) then begin
        FHelper.DeleteCategorySQL(VCategoryID);
        AIsChanged := True;
      end;
    end
    else
    if not Assigned(AOldCategory) and Assigned(ANewCategory) then
    begin // INSERT
      if Supports(ANewCategory, IMarkCategory, VNewCategory) then begin
        _SQLCategoryRecFromCategory(0, VNewCategory, VSQLCategoryRecNew);
        FHelper.InsertCategorySQL(VSQLCategoryRecNew);
        Result := FFactoryDbInternal.CreateCategory(VSQLCategoryRecNew);
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
          FHelper.UpdateCategorySQL(VSQLCategoryRecOld, VSQLCategoryRecNew);
          Result := FFactoryDbInternal.CreateCategory(VSQLCategoryRecNew);
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
  VRec: TSQLCategoryRec;
begin
  if FHelper.ReadCategorySQL(VRec, ID, AName) then begin
    Result := FFactoryDbInternal.CreateCategory(VRec);
  end else begin
    Result := nil;
  end;
end;

function TMarkCategoryDbImplORM.GetCategoryByID(const ID: TID): IMarkCategory;
begin
  Assert(ID > 0);
  LockWrite;
  try
    Result := _GetCategory(ID, '');
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDbImplORM.GetCategoryByName(const AName: string): IMarkCategory;
begin
  Assert(AName <> '');
  LockWrite;
  try
    Result := _GetCategory(0, AName);
  finally
    UnlockWrite;
  end;
end;

procedure TMarkCategoryDbImplORM.SetAllCategoriesVisible(ANewVisible: Boolean);
var
  VTransaction: TTransactionRec;
begin
  LockWrite;
  try
    StartTransaction(FClient, VTransaction, TSQLCategoryView);
    try
      FHelper.SetAllCategoriesVisibleSQL(ANewVisible);
      CommitTransaction(FClient, VTransaction);
    except
      RollBackTransaction(FClient, VTransaction);
      raise;
    end;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

function TMarkCategoryDbImplORM.GetCategoriesList: IMarkCategoryList;
var
  I: Integer;
  VCount: Integer;
  VTemp: IInterfaceListSimple;
  VCategory: IMarkCategory;
  VCategoryRecArray: TSQLCategoryRecDynArray;
begin
  LockWrite;
  try
    VCount := FHelper.GetCategoriesRecArray(VCategoryRecArray);
  finally
    UnlockWrite;
  end;
  if VCount > 0 then begin
    Assert(Length(VCategoryRecArray) >= VCount);
    VTemp := TInterfaceListSimple.Create;
    LockRead;
    try
      VTemp.Capacity := VCount;
      for I := 0 to VCount - 1 do begin
        VCategory := FFactoryDbInternal.CreateCategory(VCategoryRecArray[I]);
        VTemp.Add(VCategory);
      end;
    finally
      UnlockRead;
    end;
    Result := TMarkCategoryList.Build(VTemp.MakeStaticAndClear);
  end;
end;

end.
