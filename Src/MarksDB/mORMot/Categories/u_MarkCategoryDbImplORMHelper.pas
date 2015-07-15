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
  mORMot,
  t_MarkSystemORM,
  u_MarkSystemORMModel,
  u_MarkSystemORMTools,
  u_MarkCategoryDbImplORMCache;

procedure DeleteCategorySQL(
  const ACategoryID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
);

procedure InsertCategorySQL(
  var ACategoryRec: TSQLCategoryRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
);

procedure UpdateCategorySQL(
  const ACategoryRecOld: TSQLCategoryRec;
  var ACategoryRecNew: TSQLCategoryRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
);

function ReadCategorySQL(
  out ACategoryRec: TSQLCategoryRec;
  const ID: TID;
  const AName: string;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
): Boolean;

function FillPrepareCategoryCache(
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
): Integer;

function FillPrepareCategoryViewCache(
  const AClient: TSQLRestClient;
  const AUserID: TID;
  const ACache: TSQLCategoryDbCache
): Integer;

implementation

uses
  SysUtils,
  StrUtils,
  SynCommons;

procedure DeleteCategorySQL(
  const ACategoryID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
);
begin
  CheckID(ACategoryID);

  // first delete view for all users (if exists)
  AClient.Delete(TSQLCategoryView, FormatUTF8('Category=?', [], [ACategoryID]));

  // then delete category
  CheckDeleteResult( AClient.Delete(TSQLCategory, ACategoryID) );

  // delete from cache
  ACache.FCategoryCache.Delete(ACategoryID);
  ACache.FCategoryViewCache.Delete(ACategoryID);
end;

procedure InsertCategorySQL(
  var ACategoryRec: TSQLCategoryRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
);
var
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
begin
  // insert category
  VSQLCategory := TSQLCategory.Create;
  try
    VSQLCategory.Name := StringToUTF8(ACategoryRec.FName);
    // add to db
    CheckID( AClient.Add(VSQLCategory, True) );
    ACategoryRec.FCategoryId := VSQLCategory.ID;
    // add to cache
    ACache.FCategoryCache.AddOrUpdate(ACategoryRec);
  finally
    VSQLCategory.Free;
  end;
  if not ACategoryRec.FVisible then begin
    // insert view
    VSQLCategoryView := TSQLCategoryView.Create;
    try
      VSQLCategoryView.User := Pointer(AUserID);
      VSQLCategoryView.Category := Pointer(ACategoryRec.FCategoryId);
      VSQLCategoryView.Visible := ACategoryRec.FVisible;
      VSQLCategoryView.MinZoom := ACategoryRec.FMinZoom;
      VSQLCategoryView.MaxZoom := ACategoryRec.FMaxZoom;
      // add to db
      CheckID( AClient.Add(VSQLCategoryView, True) );
      ACategoryRec.FViewId := VSQLCategoryView.ID;
      // add to cache
      ACache.FCategoryViewCache.AddOrUpdate(ACategoryRec);
    finally
      VSQLCategoryView.Free;
    end;
  end;
end;

procedure UpdateCategorySQL(
  const ACategoryRecOld: TSQLCategoryRec;
  var ACategoryRecNew: TSQLCategoryRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
);

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
  VName: RawUTF8;
  VUpdateName: string;
  VSearchName: string;
  VOldName: string;
  VItem: PSQLCategoryViewRow;
  VCategories: TSQLCategoryRowDynArray;
  VSQLCategoryView: TSQLCategoryView;
  VUpdateView: Boolean;
  VSQLWhere: RawUTF8;
begin
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
      if ACache.FCategoryViewCache.IsPrepared then begin
        // init from cache
        if ACache.FCategoryViewCache.Find(ACategoryRecNew.FCategoryId, VItem) then begin
          VSQLCategoryView.IDValue := VItem.ViewId;
          VSQLCategoryView.Category := Pointer(VItem.CategoryId);
          VSQLCategoryView.Visible := VItem.Visible;
          VSQLCategoryView.MinZoom := VItem.MinZoom;
          VSQLCategoryView.MaxZoom := VItem.MaxZoom;
        end;
      end else begin
        // init from db
        VSQLWhere := FormatUTF8('Category=? AND User=?', [], [ACategoryRecNew.FCategoryId, AUserID]);
        AClient.Retrieve(VSQLWhere, VSQLCategoryView);
      end;
      if VSQLCategoryView.ID > 0 then begin
        _ClearFields;
        // update existing
        if ACategoryRecNew.FVisible <> ACategoryRecOld.FVisible then begin
          VSQLCategoryView.Visible := ACategoryRecNew.FVisible;
          _AddField('Visible');
        end;
        if ACategoryRecNew.FMinZoom <> ACategoryRecOld.FMinZoom then begin
          VSQLCategoryView.MinZoom := ACategoryRecNew.FMinZoom;
          _AddField('MinZoom');
        end;
        if ACategoryRecNew.FMaxZoom <> ACategoryRecOld.FMaxZoom then begin
          VSQLCategoryView.MaxZoom := ACategoryRecNew.FMaxZoom;
          _AddField('MaxZoom');
        end;
        // update db
        CheckUpdateResult( AClient.Update(VSQLCategoryView, _FieldsNamesStr) );
        ACategoryRecNew.FViewId := VSQLCategoryView.ID;
        // update cache
        ACache.FCategoryViewCache.AddOrUpdate(ACategoryRecNew);
      end else begin
        // add new
        VSQLCategoryView.User := Pointer(AUserID);
        VSQLCategoryView.Category := Pointer(ACategoryRecNew.FCategoryId);
        VSQLCategoryView.Visible := ACategoryRecNew.FVisible;
        VSQLCategoryView.MinZoom := ACategoryRecNew.FMinZoom;
        VSQLCategoryView.MaxZoom := ACategoryRecNew.FMaxZoom;
        // add to db
        CheckID( AClient.Add(VSQLCategoryView, True) );
        ACategoryRecNew.FViewId := VSQLCategoryView.ID;
        // add to cache
        ACache.FCategoryViewCache.AddOrUpdate(ACategoryRecNew);
      end;
    finally
      VSQLCategoryView.Free;
    end;
  end;

  if ACategoryRecNew.FName <> ACategoryRecOld.FName then begin
    VName := StringToUTF8(ACategoryRecNew.FName);
    // update name
    CheckUpdateResult(
      AClient.UpdateField(TSQLCategory, ACategoryRecNew.FCategoryId, 'Name', [VName])
    );
    // update cache
    ACache.FCategoryCache.AddOrUpdate(ACategoryRecNew);

    // update sub categories names
    VSearchName := ACategoryRecOld.FName + '\';
    VUpdateName := ACategoryRecNew.FName + '\';

    if FillPrepareCategoryCache(AClient, ACache) > 0 then begin
      VCategories := ACache.FCategoryCache.Rows;
      for I := 0 to ACache.FCategoryCache.Count - 1 do begin
        VOldName := VCategories[I].Name;
        if StartsText(VSearchName, VOldName) then begin
          // update cache
          VCategories[I].Name := StringReplace(VOldName, VSearchName, VUpdateName, [rfIgnoreCase]);
          VName := StringToUTF8(VCategories[I].Name);
          // update db
          CheckUpdateResult(
            AClient.UpdateField(TSQLCategory, VCategories[I].CategoryId, 'Name', [VName])
          );
        end;
      end;
    end;
  end;
end;

function ReadCategorySQL(
  out ACategoryRec: TSQLCategoryRec;
  const ID: TID;
  const AName: string;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
): Boolean;
var
  VName: RawUTF8;
  VFound: Boolean;
  VCategoryItem: PSQLCategoryRow;
  VViewItem: PSQLCategoryViewRow;
  VSQLCategory: TSQLCategory;
  VSQLCategoryView: TSQLCategoryView;
begin
  Assert( (ID > 0) or (AName <> '') );

  Result := False;

  ACategoryRec := cEmptySQLCategoryRec;

  VFound := False;
  VSQLCategory := TSQLCategory.Create;
  try
    // get category
    if ACache.FCategoryCache.IsPrepared then begin
      // from cache
      if ID > 0 then begin
        VFound := ACache.FCategoryCache.Find(ID, VCategoryItem);
      end else if AName <> '' then begin
        VFound := ACache.FCategoryCache.Find(AName, VCategoryItem);
      end;
      if VFound then begin
        VSQLCategory.IDValue := VCategoryItem.CategoryId;
        VSQLCategory.Name := StringToUTF8(VCategoryItem.Name);
      end;
    end else begin
      // from db
      if ID > 0 then begin
        VFound := AClient.Retrieve(ID, VSQLCategory);
      end else if AName <> '' then begin
        VName := StringToUTF8(AName);
        VFound := AClient.Retrieve('Name=?', [], [VName], VSQLCategory);
      end;
    end;

    if VFound and (VSQLCategory.ID > 0) then begin
      Result := True;
      ACategoryRec.FCategoryId := VSQLCategory.ID;
      ACategoryRec.FName := UTF8ToString(VSQLCategory.Name);

      VSQLCategoryView := TSQLCategoryView.Create;
      try
        // get view
        if ACache.FCategoryViewCache.IsPrepared then begin
          // from cache
          VFound := ACache.FCategoryViewCache.Find(VSQLCategory.ID, VViewItem);
          if VFound then begin
            VSQLCategoryView.IDValue := VViewItem.ViewId;
            VSQLCategoryView.Category := Pointer(VViewItem.CategoryId);
            VSQLCategoryView.Visible := VViewItem.Visible;
            VSQLCategoryView.MinZoom := VViewItem.MinZoom;
            VSQLCategoryView.MaxZoom := VViewItem.MaxZoom;
          end;
        end else begin
          // from db
          VFound := AClient.Retrieve(
            'Category=? AND User=?',
            [], [VSQLCategory.ID, AUserID],
            VSQLCategoryView
          );
        end;
        if VFound and (VSQLCategoryView.ID > 0) then begin
          ACategoryRec.FViewId := VSQLCategoryView.ID;
          ACategoryRec.FVisible := VSQLCategoryView.Visible;
          ACategoryRec.FMinZoom := VSQLCategoryView.MinZoom;
          ACategoryRec.FMaxZoom := VSQLCategoryView.MaxZoom;
        end;
      finally
        VSQLCategoryView.Free;
      end;
    end;
  finally
    VSQLCategory.Free;
  end;
end;

function FillPrepareCategoryCache(
  const AClient: TSQLRestClient;
  const ACache: TSQLCategoryDbCache
): Integer;
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VArray: TSQLCategoryRowDynArray;
begin
  if not ACache.FCategoryCache.IsPrepared then begin
    VList := AClient.ExecuteList(
      [TSQLCategory],
      'SELECT ID,Name FROM Category'
    );
    if Assigned(VList) then
    try
      VCount := VList.RowCount;
      if VCount > 0 then begin
        SetLength(VArray, VCount);
        for I := 0 to VCount - 1 do begin
          J := I + 1;
          VArray[I].CategoryId := VList.GetAsInt64(J, 0);
          VArray[I].Name := VList.GetString(J, 1);
        end;
        ACache.FCategoryCache.AddPrepared(VArray);
      end;
    finally
      VList.Free;
    end;
  end;
  Result := ACache.FCategoryCache.Count;
end;

function FillPrepareCategoryViewCache(
  const AClient: TSQLRestClient;
  const AUserID: TID;
  const ACache: TSQLCategoryDbCache
): Integer;
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VArray: TSQLCategoryViewRowDynArray;
begin
  if not ACache.FCategoryViewCache.IsPrepared then begin
    VList := AClient.ExecuteList(
      [TSQLCategoryView],
      FormatUTF8(
        'SELECT ID,Category,Visible,MinZoom,MaxZoom FROM CategoryView WHERE User=?',
        [], [AUserID]
      )
    );
    if Assigned(VList) then
    try
      VCount := VList.RowCount;
      if VCount > 0 then begin
        SetLength(VArray, VCount);
        for I := 0 to VCount - 1 do begin
          J := I + 1;
          VArray[I].ViewId := VList.GetAsInt64(J, 0);
          VArray[I].CategoryId := VList.GetAsInt64(J, 1);
          VArray[I].Visible := VList.GetAsInteger(J, 2) <> 0;
          VArray[I].MinZoom := VList.GetAsInteger(J, 3);
          VArray[I].MaxZoom := VList.GetAsInteger(J, 4);
        end;
        ACache.FCategoryViewCache.AddPrepared(VArray);
      end;
    finally
      VList.Free;
    end;
  end;
  Result := ACache.FCategoryViewCache.Count;
end;

end.
