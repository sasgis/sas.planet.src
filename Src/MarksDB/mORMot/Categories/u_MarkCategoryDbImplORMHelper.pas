{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MarkCategoryDbImplORMHelper;

interface

uses
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.os,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.client,
  mormot.rest.sqlite3,
  t_MarkSystemORM,
  i_MarkSystemImplORMClientProvider,
  u_MarkSystemORMModel,
  u_MarkSystemORMTools,
  u_MarkCategoryDbImplORMCache;

type
  TMarkCategoryDbImplORMHelper = class
  private
    FUserID: TID;
    FRestClient: TRestClientDB;
    FClient: TRestOrm;
    FCache: TOrmCategoryDbCache;
    FIsReadOnly: Boolean;
    FClientProvider: IMarkSystemImplORMClientProvider;
  private
    function _FillPrepareCategoryCache: Integer;
    function _FillPrepareCategoryViewCache: Integer;
    procedure SetReadOnly(const AValue: Boolean);
  public
    function DeleteCategorySQL(
      const ACategoryID: TID
    ): Boolean;
    function InsertCategorySQL(
      var ACategoryRec: TOrmCategoryRec
    ): Boolean;
    function UpdateCategorySQL(
      const ACategoryRecOld: TOrmCategoryRec;
      var ACategoryRecNew: TOrmCategoryRec
    ): Boolean;
    function ReadCategorySQL(
      out ACategoryRec: TOrmCategoryRec;
      const ID: TID;
      const AName: string
    ): Boolean;
    function CountCategorySQL(
      const AName: string
    ): Integer;
    procedure SetAllCategoriesVisibleSQL(
      const AVisible: Boolean
    );
    function GetCategoriesRecArray(
      out ACategoryRecArray: TOrmCategoryRecDynArray
    ): Integer;
  public
    constructor Create(
      const AIsReadOnly: Boolean;
      const AClientProvider: IMarkSystemImplORMClientProvider
    );
    destructor Destroy; override;
  public
    property IsReadOnly: Boolean read FIsReadOnly write SetReadOnly;
  end;

implementation

uses
  SysUtils,
  StrUtils;

{ TMarkCategoryDbImplORMHelper }

constructor TMarkCategoryDbImplORMHelper.Create(
  const AIsReadOnly: Boolean;
  const AClientProvider: IMarkSystemImplORMClientProvider
);
begin
  inherited Create;
  FIsReadOnly := AIsReadOnly;
  FClientProvider := AClientProvider;
  FUserID := FClientProvider.UserID;
  FRestClient := FClientProvider.RestClient;
  FClient := FRestClient.OrmInstance;
  FCache.Init;
end;

destructor TMarkCategoryDbImplORMHelper.Destroy;
begin
  FCache.Done;
  inherited Destroy;
end;

function TMarkCategoryDbImplORMHelper.DeleteCategorySQL(
  const ACategoryID: TID
): Boolean;
begin
  Result := False;

  if FIsReadOnly then begin
    Exit;
  end;

  CheckID(ACategoryID);

  // first delete view for all users (if exists)
  FClient.Delete(TOrmCategoryView, FormatSql('cvCategory=?', [], [ACategoryID]));

  // then delete category
  CheckDeleteResult( FClient.Delete(TOrmCategory, ACategoryID) );

  // delete from cache
  FCache.FCategoryCache.Delete(ACategoryID);
  FCache.FCategoryViewCache.Delete(ACategoryID);

  Result := True;
end;

function TMarkCategoryDbImplORMHelper.InsertCategorySQL(
  var ACategoryRec: TOrmCategoryRec
): Boolean;
var
  VOrmCategory: TOrmCategory;
  VOrmCategoryView: TOrmCategoryView;
begin
  Result := False;

  if FIsReadOnly then begin
    Exit;
  end;

  // insert category
  VOrmCategory := TOrmCategory.Create;
  try
    VOrmCategory.FName := StringToUtf8(ACategoryRec.FName);
    // add to db
    CheckID( FClient.Add(VOrmCategory, True) );
    ACategoryRec.FCategoryId := VOrmCategory.ID;
    // add to cache
    FCache.FCategoryCache.AddOrUpdate(ACategoryRec);
  finally
    VOrmCategory.Free;
  end;

  // insert view
  if not ACategoryRec.FVisible then begin
    VOrmCategoryView := TOrmCategoryView.Create;
    try
      VOrmCategoryView.FUser := FUserID;
      VOrmCategoryView.FCategory := ACategoryRec.FCategoryId;
      VOrmCategoryView.FVisible := ACategoryRec.FVisible;
      VOrmCategoryView.FMinZoom := ACategoryRec.FMinZoom;
      VOrmCategoryView.FMaxZoom := ACategoryRec.FMaxZoom;
      // add to db
      CheckID( FClient.Add(VOrmCategoryView, True) );
      ACategoryRec.FViewId := VOrmCategoryView.ID;
    finally
      VOrmCategoryView.Free;
    end;
  end;
  // add to cache
  FCache.FCategoryViewCache.AddOrUpdate(ACategoryRec);

  Result := True;
end;

function TMarkCategoryDbImplORMHelper.UpdateCategorySQL(
  const ACategoryRecOld: TOrmCategoryRec;
  var ACategoryRecNew: TOrmCategoryRec
): Boolean;

var
  VFieldsCount: Integer;
  VFieldsNames: array of RawUtf8;

  procedure _ClearFields;
  begin
    VFieldsCount := 0;
    SetLength(VFieldsNames, VFieldsCount);
  end;

  procedure _AddField(const AName: string);
  begin
    SetLength(VFieldsNames, VFieldsCount + 1);
    VFieldsNames[VFieldsCount] := StringToUtf8(AName);
    Inc(VFieldsCount);
  end;

  function _FieldsNamesStr: RawUtf8;
  var
    I: Integer;
    VSep: RawUtf8;
  begin
    VSep := '';
    Result := '';
    for I := 0 to Length(VFieldsNames) - 1 do begin
      if I = 1 then begin
        VSep := RawUtf8(',');
      end;
      Result := Result + VSep + VFieldsNames[I];
    end;
  end;

var
  I: Integer;
  VUpdateName: string;
  VSearchName: string;
  VOldName: string;
  VItem: POrmCategoryViewRow;
  VCategories: TOrmCategoryRowDynArray;
  VOrmCategory: TOrmCategory;
  VOrmCategoryView: TOrmCategoryView;
  VUpdateView: Boolean;
  VSQLWhere: RawUtf8;
  VFound: Boolean;
begin
  Result := False;

  CheckID(ACategoryRecOld.FCategoryId);

  ACategoryRecNew.FCategoryId := ACategoryRecOld.FCategoryId;

  VUpdateView :=
    (ACategoryRecNew.FVisible <> ACategoryRecOld.FVisible) or
    (ACategoryRecNew.FMinZoom <> ACategoryRecOld.FMinZoom) or
    (ACategoryRecNew.FMaxZoom <> ACategoryRecOld.FMaxZoom);

  if VUpdateView then begin
    // update view
    VOrmCategoryView := TOrmCategoryView.Create;
    try
      VOrmCategoryView.IDValue := 0;
      VFound := FCache.FCategoryViewCache.Find(ACategoryRecNew.FCategoryId, VItem);
      if VFound then begin
        // init from cache
        VOrmCategoryView.IDValue := VItem.ViewId;
        VOrmCategoryView.FCategory := VItem.CategoryId;
        VOrmCategoryView.FVisible := VItem.Visible;
        VOrmCategoryView.FMinZoom := VItem.MinZoom;
        VOrmCategoryView.FMaxZoom := VItem.MaxZoom;
      end else if not FCache.FCategoryViewCache.IsPrepared then begin
        // init from db
        VSQLWhere := FormatSql('cvCategory=? AND cvUser=?', [], [ACategoryRecNew.FCategoryId, FUserID]);
        VFound := FClient.Retrieve(VSQLWhere, VOrmCategoryView);
      end;
      if not FIsReadOnly then begin
        if VFound and (VOrmCategoryView.ID > 0) then begin
          _ClearFields;
          // update existing
          if ACategoryRecNew.FVisible <> ACategoryRecOld.FVisible then begin
            VOrmCategoryView.FVisible := ACategoryRecNew.FVisible;
            _AddField('cvVisible');
          end;
          if ACategoryRecNew.FMinZoom <> ACategoryRecOld.FMinZoom then begin
            VOrmCategoryView.FMinZoom := ACategoryRecNew.FMinZoom;
            _AddField('cvMinZoom');
          end;
          if ACategoryRecNew.FMaxZoom <> ACategoryRecOld.FMaxZoom then begin
            VOrmCategoryView.FMaxZoom := ACategoryRecNew.FMaxZoom;
            _AddField('cvMaxZoom');
          end;
          // update db
          CheckUpdateResult( FClient.Update(VOrmCategoryView, _FieldsNamesStr) );
        end else begin
          // add new
          VOrmCategoryView.FUser := FUserID;
          VOrmCategoryView.FCategory := ACategoryRecNew.FCategoryId;
          VOrmCategoryView.FVisible := ACategoryRecNew.FVisible;
          VOrmCategoryView.FMinZoom := ACategoryRecNew.FMinZoom;
          VOrmCategoryView.FMaxZoom := ACategoryRecNew.FMaxZoom;
          // add to db
          CheckID( FClient.Add(VOrmCategoryView, True) );
        end;
      end;
      // update cache
      ACategoryRecNew.FViewId := VOrmCategoryView.ID; // can be zero
      FCache.FCategoryViewCache.AddOrUpdate(ACategoryRecNew);
      Result := True;
    finally
      VOrmCategoryView.Free;
    end;
  end;

  if FIsReadOnly then begin
    Exit;
  end;

  if ACategoryRecNew.FName <> ACategoryRecOld.FName then begin
    VOrmCategory := TOrmCategory.Create;
    try
      VOrmCategory.IDValue := ACategoryRecNew.FCategoryId;
      VOrmCategory.FName := StringToUtf8(ACategoryRecNew.FName);
      // update name
      CheckUpdateResult( FClient.Update(VOrmCategory, 'cName') );
      // update cache
      FCache.FCategoryCache.AddOrUpdate(ACategoryRecNew);

      // update sub categories names
      VSearchName := ACategoryRecOld.FName + '\';
      VUpdateName := ACategoryRecNew.FName + '\';

      if _FillPrepareCategoryCache > 0 then begin
        VCategories := FCache.FCategoryCache.Rows;
        for I := 0 to FCache.FCategoryCache.Count - 1 do begin
          VOldName := VCategories[I].Name;
          if StartsText(VSearchName, VOldName) then begin
            // update cache
            VCategories[I].Name := StringReplace(VOldName, VSearchName, VUpdateName, [rfIgnoreCase]);
            VOrmCategory.IDValue := VCategories[I].CategoryId;
            VOrmCategory.FName := StringToUtf8(VCategories[I].Name);
            // update db
            CheckUpdateResult( FClient.Update(VOrmCategory, 'cName') );
          end;
        end;
      end;
      Result := True;
    finally
      VOrmCategory.Free;
    end;
  end;
end;

function TMarkCategoryDbImplORMHelper.ReadCategorySQL(
  out ACategoryRec: TOrmCategoryRec;
  const ID: TID;
  const AName: string
): Boolean;
var
  VName: RawUtf8;
  VCategoryItem: POrmCategoryRow;
  VViewItem: POrmCategoryViewRow;
  VOrmCategory: TOrmCategory;
  VOrmCategoryView: TOrmCategoryView;
  VFound: Boolean;
begin
  Assert( (ID > 0) or (AName <> '') );

  Result := False;

  ACategoryRec := cEmptyOrmCategoryRec;

  VOrmCategory := TOrmCategory.Create;
  try
    // try get category from cache
    if ID > 0 then begin
      Result := FCache.FCategoryCache.Find(ID, VCategoryItem);
    end else if AName <> '' then begin
      Result := FCache.FCategoryCache.Find(AName, VCategoryItem);
    end;
    if Result then begin
      // found in cache
      ACategoryRec.FCategoryId := VCategoryItem.CategoryId;
      ACategoryRec.FName := VCategoryItem.Name;
    end else if not FCache.FCategoryCache.IsPrepared then begin
      // not found in cache -> get from db
      if ID > 0 then begin
        Result := FClient.Retrieve(ID, VOrmCategory);
      end else if AName <> '' then begin
        VName := StringToUtf8(AName);
        Result := FClient.Retrieve('cName=?', [], [VName], VOrmCategory);
      end;
      if Result then begin
        CheckID(VOrmCategory.ID);
        ACategoryRec.FCategoryId := VOrmCategory.ID;
        ACategoryRec.FName := UTF8ToString(VOrmCategory.FName);
        // add to cache
        FCache.FCategoryCache.AddOrUpdate(ACategoryRec);
      end;
    end;
  finally
    VOrmCategory.Free;
  end;

  if not Result then begin
    Exit;
  end;

  VOrmCategoryView := TOrmCategoryView.Create;
  try
    // try get view from cache
    VFound := FCache.FCategoryViewCache.Find(ACategoryRec.FCategoryId, VViewItem);
    if VFound then begin
      // view found in cache
      ACategoryRec.FViewId := VViewItem.ViewId;
      ACategoryRec.FVisible := VViewItem.Visible;
      ACategoryRec.FMinZoom := VViewItem.MinZoom;
      ACategoryRec.FMaxZoom := VViewItem.MaxZoom;
      Exit;
    end else if not FCache.FCategoryViewCache.IsPrepared then begin
      // view not in cache -> get it from db
      VFound := FClient.Retrieve(
        'cvCategory=? AND cvUser=?', [], [ACategoryRec.FCategoryId, FUserID],
        VOrmCategoryView
      );
      if VFound then begin
        CheckID(VOrmCategoryView.ID);
        ACategoryRec.FViewId := VOrmCategoryView.ID;
        ACategoryRec.FVisible := VOrmCategoryView.FVisible;
        ACategoryRec.FMinZoom := VOrmCategoryView.FMinZoom;
        ACategoryRec.FMaxZoom := VOrmCategoryView.FMaxZoom;
      end;
    end;
    // add view to cache
    FCache.FCategoryViewCache.AddOrUpdate(ACategoryRec);
  finally
    VOrmCategoryView.Free;
  end;
end;

function TMarkCategoryDbImplORMHelper.CountCategorySQL(
  const AName: string
): Integer;
var
  VName: RawUtf8;
  VList: TOrmTable;
begin
  Assert(AName <> '');
  VName := StringToUtf8(AName);

  Result := 0;

  VList := FClient.ExecuteList(
    [TOrmCategory],
    FormatSql('SELECT RowID FROM Category WHERE cName=?', [], [VName])
  );

  if VList <> nil then
  try
    Result := VList.RowCount;
  finally
    VList.Free;
  end;
end;

function TMarkCategoryDbImplORMHelper._FillPrepareCategoryCache: Integer;
var
  I, J: Integer;
  VCount: Integer;
  VList: TOrmTable;
  VArray: TOrmCategoryRowDynArray;
begin
  if not FCache.FCategoryCache.IsPrepared then begin
    VList := FClient.ExecuteList(
      [TOrmCategory],
      'SELECT RowID,cName FROM Category'
    );
    if Assigned(VList) then
    try
      VCount := VList.RowCount;
      SetLength(VArray, VCount);
      for I := 0 to VCount - 1 do begin
        J := I + 1;
        VArray[I].CategoryId := VList.GetAsInt64(J, 0);
        VArray[I].Name := VList.GetString(J, 1);
      end;
      FCache.FCategoryCache.AddPrepared(VArray);
    finally
      VList.Free;
    end;
  end;
  Result := FCache.FCategoryCache.Count;
end;

function TMarkCategoryDbImplORMHelper._FillPrepareCategoryViewCache: Integer;
var
  I, J: Integer;
  VCount: Integer;
  VList: TOrmTable;
  VArray: TOrmCategoryViewRowDynArray;
begin
  if not FCache.FCategoryViewCache.IsPrepared then begin
    VList := FClient.ExecuteList(
      [TOrmCategoryView],
      FormatSql(
        'SELECT RowID,cvCategory,cvVisible,cvMinZoom,cvMaxZoom FROM CategoryView WHERE cvUser=?',
        [], [FUserID]
      )
    );
    if Assigned(VList) then
    try
      VCount := VList.RowCount;
      SetLength(VArray, VCount);
      for I := 0 to VCount - 1 do begin
        J := I + 1;
        VArray[I].ViewId := VList.GetAsInt64(J, 0);
        VArray[I].CategoryId := VList.GetAsInt64(J, 1);
        VArray[I].Visible := VList.GetB(J, 2); // as boolean
        VArray[I].MinZoom := VList.GetAsInteger(J, 3);
        VArray[I].MaxZoom := VList.GetAsInteger(J, 4);
      end;
      FCache.FCategoryViewCache.AddPrepared(VArray);
    finally
      VList.Free;
    end;
  end;
  Result := FCache.FCategoryViewCache.Count;
end;

procedure TMarkCategoryDbImplORMHelper.SetAllCategoriesVisibleSQL(
  const AVisible: Boolean
);
var
  I: Integer;
  VViewCount: Integer;
  VCategoryCount: Integer;
  VIsFound: Boolean;
  VRec: TOrmCategoryRec;
  VItem: POrmCategoryViewRow;
  VViews: TOrmCategoryViewRowDynArray;
  VCategories: TOrmCategoryRowDynArray;
  VCategoryID: TID;
  VOrmCategoryView: TOrmCategoryView;
  VViewCache: TOrmCategoryViewCache;
begin
  if not FIsReadOnly then begin
    // update db
    CheckUpdateResult(
      FClient.UpdateField(TOrmCategoryView, 'cvUser', [FUserID], 'cvVisible', [AVisible])
    );
  end;
  VViewCount := _FillPrepareCategoryViewCache;
  VViewCache := FCache.FCategoryViewCache;

  // update cache
  VViews := VViewCache.Rows;
  for I := 0 to VViewCount - 1 do begin
    VViews[I].Visible := AVisible;
  end;

  if not AVisible then begin
    VCategoryCount := _FillPrepareCategoryCache;
    if VCategoryCount > 0 then begin
      VCategories := FCache.FCategoryCache.Rows;
      VOrmCategoryView := TOrmCategoryView.Create;
      try
        VRec := cEmptyOrmCategoryRec;
        for I := 0 to VCategoryCount - 1 do begin
          VCategoryID := VCategories[I].CategoryId;
          VIsFound := (VViewCount > 0) and VViewCache.Find(VCategoryID, VItem);
          if not VIsFound or ( VIsFound and (VItem.ViewId = 0) ) then begin

            VRec.FCategoryId := VCategoryID;
            VRec.FVisible := AVisible;

            VOrmCategoryView.FUser := FUserID;
            VOrmCategoryView.FCategory := VRec.FCategoryId;
            VOrmCategoryView.FVisible := VRec.FVisible;
            VOrmCategoryView.FMinZoom := VRec.FMinZoom;
            VOrmCategoryView.FMaxZoom := VRec.FMaxZoom;

            // add to db
            if not FIsReadOnly then begin
              CheckID( FClient.Add(VOrmCategoryView, True) );
              VRec.FViewId := VOrmCategoryView.ID;
            end else begin
              VRec.FViewId := 0; // fake id
            end;
            // add to cache
            VViewCache.AddOrUpdate(VRec);
          end;
        end;
      finally
        VOrmCategoryView.Free;
      end;
    end;
  end;
end;

function TMarkCategoryDbImplORMHelper.GetCategoriesRecArray(
  out ACategoryRecArray: TOrmCategoryRecDynArray
): Integer;
var
  I: Integer;
  VRec: POrmCategoryRec;
  VCount: Integer;
  VViewCount: Integer;
  VItem: POrmCategoryViewRow;
  VViewCache: TOrmCategoryViewCache;
  VRows: TOrmCategoryRowDynArray;
begin
  VCount := _FillPrepareCategoryCache;
  if VCount > 0 then begin
    VViewCount := _FillPrepareCategoryViewCache;
  end else begin
    VViewCount := 0;
  end;

  SetLength(ACategoryRecArray, VCount);
  if VCount > 0 then begin
    VRows := FCache.FCategoryCache.Rows;
    VViewCache := FCache.FCategoryViewCache;
    for I := 0 to FCache.FCategoryCache.Count - 1 do begin
      ACategoryRecArray[I] := cEmptyOrmCategoryRec;
      VRec := @ACategoryRecArray[I];
      VRec.FCategoryId := VRows[I].CategoryId;
      VRec.FName := VRows[I].Name;
      if VViewCount > 0 then begin
        if VViewCache.Find(VRec.FCategoryId, VItem) then begin
          VRec.FViewId := VItem.ViewId;
          VRec.FVisible := VItem.Visible;
          VRec.FMinZoom := VItem.MinZoom;
          VRec.FMaxZoom := VItem.MaxZoom;
        end;
      end;
    end;
  end;
  Result := VCount;
end;

procedure TMarkCategoryDbImplORMHelper.SetReadOnly(const AValue: Boolean);
begin
  if FIsReadOnly <> AValue then begin
    if FIsReadOnly then begin
      FCache.FCategoryViewCache.Reset; // remove possible fake id's
    end;
    FIsReadOnly := AValue;
  end;
end;

end.
