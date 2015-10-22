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

unit u_MarkCategoryDbImplORMHelper;

interface

uses
  Windows,
  mORMot,
  t_MarkSystemORM,
  i_MarkSystemImplORMClientProvider,
  u_MarkSystemORMModel,
  u_MarkSystemORMTools,
  u_MarkCategoryDbImplORMCache;

type
  TMarkCategoryDbImplORMHelper = class
  private
    FUserID: TID;
    FClient: TSQLRestClient;
    FCache: TSQLCategoryDbCache;
    FIsReadOnly: Boolean;
    FClientProvider: IMarkSystemImplORMClientProvider;
  private
    function _FillPrepareCategoryCache: Integer;
    function _FillPrepareCategoryViewCache: Integer;
  public
    function DeleteCategorySQL(
      const ACategoryID: TID
    ): Boolean;
    function InsertCategorySQL(
      var ACategoryRec: TSQLCategoryRec
    ): Boolean;
    function UpdateCategorySQL(
      const ACategoryRecOld: TSQLCategoryRec;
      var ACategoryRecNew: TSQLCategoryRec
    ): Boolean;
    function ReadCategorySQL(
      out ACategoryRec: TSQLCategoryRec;
      const ID: TID;
      const AName: string
    ): Boolean;
    procedure SetAllCategoriesVisibleSQL(
      const AVisible: Boolean
    );
    function GetCategoriesRecArray(
      out ACategoryRecArray: TSQLCategoryRecDynArray
    ): Integer;
  public
    constructor Create(
      const AIsReadOnly: Boolean;
      const AClientProvider: IMarkSystemImplORMClientProvider
    );
    destructor Destroy; override;
  public
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  SynCommons;

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
  FClient := FClientProvider.RestClient;
  FCache.Init;
end;

destructor TMarkCategoryDbImplORMHelper.Destroy;
begin
  FCache.Done;
  FClient := nil;
  FClientProvider := nil;
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
  FClient.Delete(TSQLCategoryView, FormatUTF8('cvCategory=?', [], [ACategoryID]));

  // then delete category
  CheckDeleteResult( FClient.Delete(TSQLCategory, ACategoryID) );

  // delete from cache
  FCache.FCategoryCache.Delete(ACategoryID);
  FCache.FCategoryViewCache.Delete(ACategoryID);

  Result := True;
end;

function TMarkCategoryDbImplORMHelper.InsertCategorySQL(
  var ACategoryRec: TSQLCategoryRec
): Boolean;
var
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
begin
  Result := False;

  if FIsReadOnly then begin
    Exit;
  end;

  // insert category
  VSQLCategory := TSQLCategory.Create;
  try
    VSQLCategory.FName := StringToUTF8(ACategoryRec.FName);
    // add to db
    CheckID( FClient.Add(VSQLCategory, True) );
    ACategoryRec.FCategoryId := VSQLCategory.ID;
    // add to cache
    FCache.FCategoryCache.AddOrUpdate(ACategoryRec);
  finally
    VSQLCategory.Free;
  end;

  // insert view
  if not ACategoryRec.FVisible then begin
    VSQLCategoryView := TSQLCategoryView.Create;
    try
      VSQLCategoryView.FUser := FUserID;
      VSQLCategoryView.FCategory := ACategoryRec.FCategoryId;
      VSQLCategoryView.FVisible := ACategoryRec.FVisible;
      VSQLCategoryView.FMinZoom := ACategoryRec.FMinZoom;
      VSQLCategoryView.FMaxZoom := ACategoryRec.FMaxZoom;
      // add to db
      CheckID( FClient.Add(VSQLCategoryView, True) );
      ACategoryRec.FViewId := VSQLCategoryView.ID;
    finally
      VSQLCategoryView.Free;
    end;
  end;
  // add to cache
  FCache.FCategoryViewCache.AddOrUpdate(ACategoryRec);

  Result := True;
end;

function TMarkCategoryDbImplORMHelper.UpdateCategorySQL(
  const ACategoryRecOld: TSQLCategoryRec;
  var ACategoryRecNew: TSQLCategoryRec
): Boolean;

var
  VFieldsCount: Integer;
  VFieldsNames: array of RawUTF8;

  procedure _ClearFields;
  begin
    VFieldsCount := 0;
    SetLength(VFieldsNames, VFieldsCount);
  end;

  procedure _AddField(const AName: string);
  begin
    SetLength(VFieldsNames, VFieldsCount + 1);
    VFieldsNames[VFieldsCount] := StringToUTF8(AName);
    Inc(VFieldsCount);
  end;

  function _FieldsNamesStr: RawUTF8;
  var
    I: Integer;
    VSep: RawUTF8;
  begin
    VSep := '';
    Result := '';
    for I := 0 to Length(VFieldsNames) - 1 do begin
      if I = 1 then begin
        VSep := RawUTF8(',');
      end;
      Result := Result + VSep + VFieldsNames[I];
    end;
  end;

var
  I: Integer;
  VUpdateName: string;
  VSearchName: string;
  VOldName: string;
  VItem: PSQLCategoryViewRow;
  VCategories: TSQLCategoryRowDynArray;
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
  VUpdateView: Boolean;
  VSQLWhere: RawUTF8;
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
    VSQLCategoryView := TSQLCategoryView.Create;
    try
      VFound := FCache.FCategoryViewCache.Find(ACategoryRecNew.FCategoryId, VItem);
      if VFound then begin
        // init from cache
        VSQLCategoryView.IDValue := VItem.ViewId;
        VSQLCategoryView.FCategory := VItem.CategoryId;
        VSQLCategoryView.FVisible := VItem.Visible;
        VSQLCategoryView.FMinZoom := VItem.MinZoom;
        VSQLCategoryView.FMaxZoom := VItem.MaxZoom;
      end else if not FCache.FCategoryViewCache.IsPrepared then begin
        // init from db
        VSQLWhere := FormatUTF8('cvCategory=? AND cvUser=?', [], [ACategoryRecNew.FCategoryId, FUserID]);
        VFound := FClient.Retrieve(VSQLWhere, VSQLCategoryView);
      end;
      if not FIsReadOnly then begin
        if VFound and (VSQLCategoryView.ID > 0) then begin
          _ClearFields;
          // update existing
          if ACategoryRecNew.FVisible <> ACategoryRecOld.FVisible then begin
            VSQLCategoryView.FVisible := ACategoryRecNew.FVisible;
            _AddField('cvVisible');
          end;
          if ACategoryRecNew.FMinZoom <> ACategoryRecOld.FMinZoom then begin
            VSQLCategoryView.FMinZoom := ACategoryRecNew.FMinZoom;
            _AddField('cvMinZoom');
          end;
          if ACategoryRecNew.FMaxZoom <> ACategoryRecOld.FMaxZoom then begin
            VSQLCategoryView.FMaxZoom := ACategoryRecNew.FMaxZoom;
            _AddField('cvMaxZoom');
          end;
          // update db
          CheckUpdateResult( FClient.Update(VSQLCategoryView, _FieldsNamesStr) );
        end else begin
          // add new
          VSQLCategoryView.FUser := FUserID;
          VSQLCategoryView.FCategory := ACategoryRecNew.FCategoryId;
          VSQLCategoryView.FVisible := ACategoryRecNew.FVisible;
          VSQLCategoryView.FMinZoom := ACategoryRecNew.FMinZoom;
          VSQLCategoryView.FMaxZoom := ACategoryRecNew.FMaxZoom;
          // add to db
          CheckID( FClient.Add(VSQLCategoryView, True) );
        end;
      end;
      // update cache
      ACategoryRecNew.FViewId := VSQLCategoryView.ID;
      FCache.FCategoryViewCache.AddOrUpdate(ACategoryRecNew);
      Result := True;
    finally
      VSQLCategoryView.Free;
    end;
  end;

  if FIsReadOnly then begin
    Exit;
  end;

  if ACategoryRecNew.FName <> ACategoryRecOld.FName then begin
    VSQLCategory := TSQLCategory.Create;
    try
      VSQLCategory.IDValue := ACategoryRecNew.FCategoryId;
      VSQLCategory.FName := StringToUTF8(ACategoryRecNew.FName);
      // update name
      CheckUpdateResult( FClient.Update(VSQLCategory, 'cName') );
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
            VSQLCategory.IDValue := VCategories[I].CategoryId;
            VSQLCategory.FName := StringToUTF8(VCategories[I].Name);
            // update db
            CheckUpdateResult( FClient.Update(VSQLCategory, 'cName') );
          end;
        end;
      end;
      Result := True;
    finally
      VSQLCategory.Free;
    end;
  end;
end;

function TMarkCategoryDbImplORMHelper.ReadCategorySQL(
  out ACategoryRec: TSQLCategoryRec;
  const ID: TID;
  const AName: string
): Boolean;
var
  VName: RawUTF8;
  VCategoryItem: PSQLCategoryRow;
  VViewItem: PSQLCategoryViewRow;
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
  VFound: Boolean;
begin
  Assert( (ID > 0) or (AName <> '') );

  Result := False;

  ACategoryRec := cEmptySQLCategoryRec;

  VSQLCategory := TSQLCategory.Create;
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
        Result := FClient.Retrieve(ID, VSQLCategory);
      end else if AName <> '' then begin
        VName := StringToUTF8(AName);
        Result := FClient.Retrieve('cName=?', [], [VName], VSQLCategory);
      end;
      if Result then begin
        CheckID(VSQLCategory.ID);
        ACategoryRec.FCategoryId := VSQLCategory.ID;
        ACategoryRec.FName := UTF8ToString(VSQLCategory.FName);
        // add to cache
        FCache.FCategoryCache.AddOrUpdate(ACategoryRec);
      end;
    end;
  finally
    VSQLCategory.Free;
  end;

  if not Result then begin
    Exit;
  end;

  VSQLCategoryView := TSQLCategoryView.Create;
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
        VSQLCategoryView
      );
      if VFound then begin
        CheckID(VSQLCategoryView.ID);
        ACategoryRec.FViewId := VSQLCategoryView.ID;
        ACategoryRec.FVisible := VSQLCategoryView.FVisible;
        ACategoryRec.FMinZoom := VSQLCategoryView.FMinZoom;
        ACategoryRec.FMaxZoom := VSQLCategoryView.FMaxZoom;
      end;
    end;
    // add view to cache
    FCache.FCategoryViewCache.AddOrUpdate(ACategoryRec);
  finally
    VSQLCategoryView.Free;
  end;
end;

function TMarkCategoryDbImplORMHelper._FillPrepareCategoryCache: Integer;
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VArray: TSQLCategoryRowDynArray;
begin
  if not FCache.FCategoryCache.IsPrepared then begin
    VList := FClient.ExecuteList(
      [TSQLCategory],
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
  VList: TSQLTableJSON;
  VArray: TSQLCategoryViewRowDynArray;
begin
  if not FCache.FCategoryViewCache.IsPrepared then begin
    VList := FClient.ExecuteList(
      [TSQLCategoryView],
      FormatUTF8(
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
        VArray[I].Visible := VList.GetAsInteger(J, 2) <> 0;
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
  VRec: TSQLCategoryRec;
  VItem: PSQLCategoryViewRow;
  VViews: TSQLCategoryViewRowDynArray;
  VCategories: TSQLCategoryRowDynArray;
  VCategoryID: TID;
  VSQLCategoryView: TSQLCategoryView;
  VViewCache: TSQLCategoryViewCache;
begin
  if not FIsReadOnly then begin
    // update db
    CheckUpdateResult(
      FClient.UpdateField(TSQLCategoryView,'cvUser',[FUserID],'cvVisible',[AVisible])
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
      VSQLCategoryView := TSQLCategoryView.Create;
      try
        VRec := cEmptySQLCategoryRec;
        for I := 0 to VCategoryCount - 1 do begin
          VCategoryID := VCategories[I].CategoryId;
          VIsFound := (VViewCount > 0) and VViewCache.Find(VCategoryID, VItem);
          if not VIsFound or ( VIsFound and (VItem.ViewId = 0) ) then begin

            VRec.FCategoryId := VCategoryID;
            VRec.FVisible := AVisible;

            VSQLCategoryView.FUser := FUserID;
            VSQLCategoryView.FCategory := VRec.FCategoryId;
            VSQLCategoryView.FVisible := VRec.FVisible;
            VSQLCategoryView.FMinZoom := VRec.FMinZoom;
            VSQLCategoryView.FMaxZoom := VRec.FMaxZoom;

            // add to db
            if not FIsReadOnly then begin
              CheckID( FClient.Add(VSQLCategoryView, True) );
              VRec.FViewId := VSQLCategoryView.ID;
            end else begin
              VRec.FViewId := 0;
            end;
            // add to cache
            VViewCache.AddOrUpdate(VRec);
          end;
        end;
      finally
        VSQLCategoryView.Free;
      end;
    end;
  end;
end;

function TMarkCategoryDbImplORMHelper.GetCategoriesRecArray(
  out ACategoryRecArray: TSQLCategoryRecDynArray
): Integer;
var
  I: Integer;
  VRec: PSQLCategoryRec;
  VCount: Integer;
  VViewCount: Integer;
  VItem: PSQLCategoryViewRow;
  VViewCache: TSQLCategoryViewCache;
  VRows: TSQLCategoryRowDynArray;
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
      ACategoryRecArray[I] := cEmptySQLCategoryRec;
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

end.
