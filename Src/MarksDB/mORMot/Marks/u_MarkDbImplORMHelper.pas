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

unit u_MarkDbImplORMHelper;

interface

uses
  Windows,
  Classes,
  SysUtils,
  mORMot,
  SynCommons,
  t_MarkSystemORM,
  i_GeometryLonLat,
  i_GeometryToStream,
  i_GeometryFromStream,
  u_MarkSystemORMModel,
  u_MarkDbImplORMCache;

procedure DeleteMarkSQL(
  const AMarkID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache
);

procedure InsertMarkSQL(
  var AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AGeometryWriter: IGeometryToStream
);

procedure UpdateMarkSQL(
  const AOldMarkRec: TSQLMarkRec;
  var ANewMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AGeometryWriter: IGeometryToStream
);

function ReadMarkSQL(
  out AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AMarkID: TID;
  const ACategoryID: TID;
  const AMarkName: string;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AGeometryReader: IGeometryFromStream
): Boolean;

function SetMarkVisibleSQL(
  const AMarkID: TID;
  const AUserID: TID;
  const AVisible: Boolean;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AUseTransaction: Boolean
): Boolean;

function SetMarksInCategoryVisibleSQL(
  const AUserID: TID;
  const ACategoryID: TID;
  const AVisible: Boolean;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache
): Boolean;

function GetMarkRecArray(
  const AUserID: TID;
  const ACategoryId: TID;
  const AClient: TSQLRestClient;
  const AGeometryReader: IGeometryFromStream;
  const ACache: TSQLMarkDbCache;
  const AIncludeHiddenMarks: Boolean;
  const AIncludeGeometry: Boolean;
  out AMarkRecArray: TSQLMarkRecDynArray
): Integer;

implementation

uses
  t_GeoTypes,
  u_MarkSystemORMTools;

function _GeomertryFromBlob(
  const ABlob: TSQLRawBlob;
  const AGeometryReader: IGeometryFromStream
): IGeometryLonLat;
var
  VStream: TRawByteStringStream;
begin
  Assert(ABlob <> '');
  Assert(AGeometryReader <> nil);
  VStream := TRawByteStringStream.Create(ABlob);
  try
    Result := AGeometryReader.Parse(VStream);
  finally
    VStream.Free;
  end;
end;

function _GeomertryToBlob(
  const AGeometry: IGeometryLonLat;
  const AGeometryWriter: IGeometryToStream
): TSQLRawBlob;
var
  VStream: TRawByteStringStream;
begin
  Assert(AGeometry <> nil);
  Assert(AGeometryWriter <> nil);
  VStream := TRawByteStringStream.Create;
  try
    AGeometryWriter.Save(AGeometry, VStream);
    Result := VStream.DataString;
  finally
    VStream.Free;
  end;
end;

function _AddMarkImage(
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const APicName: string
): TID;
var
  VPicName: RawUTF8;
  VItem: PSQLMarkImageRow;
  VSQLMarkImage: TSQLMarkImage;
begin
  Result := 0;
  if ACache.FMarkImage.Find(APicName, VItem) then begin
    // found in cache
    Result := VItem.ImageId;
  end else begin
    VPicName := StringToUTF8(APicName);
    VSQLMarkImage := TSQLMarkImage.Create(AClient, 'Name=?', [VPicName]);
    try
      if VSQLMarkImage.ID = 0 then begin
        VSQLMarkImage.Name := VPicName;
        // add to db
        CheckID( AClient.Add(VSQLMarkImage, True) );
      end;
      Result := VSQLMarkImage.ID;
      // add to cache
      ACache.FMarkImage.AddOrIgnore(Result, APicName);
    finally
      VSQLMarkImage.Free;
    end;
  end;
end;

function _AddMarkAppearance(
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AColor1, AColor2: Cardinal;
  const AScale1, AScale2: Integer
): TID;
var
  VItem: PSQLMarkAppearanceRow;
  VSQLMarkAppearance: TSQLMarkAppearance;
begin
  Result := 0;
  if ACache.FMarkAppearance.Find(AColor1, AColor2, AScale1, AScale2, VItem) then begin
    // found in cache
    Result := VItem.AppearanceId;
  end else begin
    VSQLMarkAppearance := TSQLMarkAppearance.Create(
      AClient, 'Color1=? AND Color2=? AND Scale1=? AND Scale2=?',
      [AColor1, AColor2, AScale1, AScale2]
    );
    try
      if VSQLMarkAppearance.ID = 0 then begin
        VSQLMarkAppearance.Color1 := AColor1;
        VSQLMarkAppearance.Color2 := AColor2;
        VSQLMarkAppearance.Scale1 := AScale1;
        VSQLMarkAppearance.Scale2 := AScale2;
        // add to db
        CheckID( AClient.Add(VSQLMarkAppearance, True) );
      end;
      Result := VSQLMarkAppearance.ID;
      // add to cache
      ACache.FMarkAppearance.AddOrIgnore(Result, AColor1, AColor2, AScale1, AScale2);
    finally
      VSQLMarkAppearance.Free;
    end;
  end;
end;

procedure DeleteMarkSQL(
  const AMarkID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache
);
var
  VTransaction: TTransactionRec;
  VIndex: PSQLMarkIdIndexRec;
begin
  Assert(AMarkID > 0);
  Assert(AClient <> nil);

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    // delete view for all Users if exists
    AClient.Delete(TSQLMarkView, FormatUTF8('Mark=?', [], [AMarkID]));

    // delete rect
    CheckDeleteResult( AClient.Delete(TSQLMarkRTree, AMarkID) );

    // delete name and desc
    CheckDeleteResult( AClient.Delete(TSQLMarkFTS, AMarkID) );

    // delete mark
    CheckDeleteResult( AClient.Delete(TSQLMark, AMarkID) );

    // pic name and appearance are never deleted...

    CommitTransaction(AClient, VTransaction);

  except
    RollBackTransaction(AClient, VTransaction);
    raise;
  end;

  // delete from cache
  ACache.FMarkCache.Delete(AMarkID);
  ACache.FMarkGeometryCache.Delete(AMarkID);
  ACache.FMarkViewCache.Delete(AMarkID);
  if ACache.FMarkIdIndex.Find(AMarkID, VIndex) then begin
    ACache.FMarkIdIndex.Delete(AMarkID);
    ACache.FMarkIdByCategoryIndex.Delete(VIndex.CategoryId, AMarkID);
  end;
end;

procedure InsertMarkSQL(
  var AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AGeometryWriter: IGeometryToStream
);
var
  VRect: TDoubleRect;
  VGeometryBlob: TSQLRawBlob;
  VSQLMark: TSQLMark;
  VSQLMarkView: TSQLMarkView;
  VSQLMarkFTS: TSQLMarkFTS;
  VSQLMarkRTree: TSQLMarkRTree;
  VTransaction: TTransactionRec;
begin
  Assert(AClient <> nil);
  Assert(AGeometryWriter <> nil);

  VRect := AMarkRec.FGeometry.Bounds.Rect;
  VGeometryBlob := _GeomertryToBlob(AMarkRec.FGeometry, AGeometryWriter);
  CalcGeometrySize(VRect, AMarkRec.FGeoLonSize, AMarkRec.FGeoLatSize);

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    if AMarkRec.FPicName <> '' then begin
      AMarkRec.FPicId := _AddMarkImage(AClient, ACache, AMarkRec.FPicName);
    end else begin
      AMarkRec.FPicId := 0;
    end;

    AMarkRec.FAppearanceId := _AddMarkAppearance(
      AClient, ACache,
      AMarkRec.FColor1, AMarkRec.FColor2,
      AMarkRec.FScale1, AMarkRec.FScale2
    );

    VSQLMark := TSQLMark.Create;
    try
      VSQLMark.Category := Pointer(AMarkRec.FCategoryId);
      VSQLMark.Image := Pointer(AMarkRec.FPicId);
      VSQLMark.Appearance := Pointer(AMarkRec.FAppearanceId);

      VSQLMark.Name := StringToUTF8(AMarkRec.FName);
      VSQLMark.Desc := StringToUTF8(AMarkRec.FDesc);

      VSQLMark.GeoLonSize := AMarkRec.FGeoLonSize;
      VSQLMark.GeoLatSize := AMarkRec.FGeoLatSize;
      VSQLMark.GeoType := AMarkRec.FGeoType;
      VSQLMark.GeoCount := AMarkRec.FGeoCount;

      // add mark to db
      CheckID( AClient.Add(VSQLMark, True) );
      AMarkRec.FMarkId := VSQLMark.ID;
      // add geometry blob to db
      CheckUpdateResult( AClient.UpdateBlob(TSQLMark, AMarkRec.FMarkId, 'GeoWKB', VGeometryBlob) );
      // add to cache
      ACache.FMarkCache.AddOrUpdate(AMarkRec);
      ACache.FMarkGeometryCache.AddOrUpdate(AMarkRec.FMarkId, Length(VGeometryBlob), AMarkRec.FGeometry);
      ACache.FMarkIdIndex.AddOrUpdate(AMarkRec);
      ACache.FMarkIdByCategoryIndex.Add(AMarkRec.FCategoryId, AMarkRec.FMarkId);
    finally
      VSQLMark.Free;
    end;

    VSQLMarkFTS := TSQLMarkFTS.Create;
    try
      VSQLMarkFTS.DocID := AMarkRec.FMarkId;
      VSQLMarkFTS.Name := StringToUTF8(SysUtils.AnsiLowerCase(AMarkRec.FName));
      VSQLMarkFTS.Desc := StringToUTF8(SysUtils.AnsiLowerCase(AMarkRec.FDesc));
      // add name and desc to db (fts index)
      CheckID( AClient.Add(VSQLMarkFTS, True) );
    finally
      VSQLMarkFTS.Free;
    end;

    VSQLMarkRTree := TSQLMarkRTree.Create;
    try
      VSQLMarkRTree.IDValue := AMarkRec.FMarkId;
      VSQLMarkRTree.Left := VRect.Left;
      VSQLMarkRTree.Right := VRect.Right;
      VSQLMarkRTree.Top := VRect.Top;
      VSQLMarkRTree.Bottom := VRect.Bottom;
      // add rect to db (rtree index)
      CheckID( AClient.Add(VSQLMarkRTree, True) );
    finally
      VSQLMarkRTree.Free;
    end;

    if not AMarkRec.FVisible then begin
      VSQLMarkView := TSQLMarkView.Create;
      try
        VSQLMarkView.User := Pointer(AUserID);
        VSQLMarkView.Mark := Pointer(AMarkRec.FMarkId);
        VSQLMarkView.Visible := AMarkRec.FVisible;
        // add view to db
        CheckID( AClient.Add(VSQLMarkView, True) );
        AMarkRec.FViewId := VSQLMarkView.ID;
        // add view to cache
        ACache.FMarkViewCache.AddOrUpdate(AMarkRec);
      finally
        VSQLMarkView.Free;
      end;
    end;

    CommitTransaction(AClient, VTransaction);
  except
    RollBackTransaction(AClient, VTransaction);
    raise;
  end;
end;

procedure UpdateMarkSQL(
  const AOldMarkRec: TSQLMarkRec;
  var ANewMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AGeometryWriter: IGeometryToStream
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
  VRect: TDoubleRect;
  VGeometryBlob: TSQLRawBlob;
  VSQLMark: TSQLMark;
  VSQLMarkFTS: TSQLMarkFTS;
  VSQLMarkRTree: TSQLMarkRTree;
  VTransaction: TTransactionRec;
  VUpdatePic: Boolean;
  VUpdateGeo: Boolean;
  VUpdateName: Boolean;
  VUpdateDesc: Boolean;
  VUpdateCategory: Boolean;
  VUpdateAppearance: Boolean;
  VUpdateIdIndex: Boolean;
begin
  Assert(AClient <> nil);

  CheckID(AOldMarkRec.FMarkId);

  ANewMarkRec.FMarkId := AOldMarkRec.FMarkId;

  VUpdatePic := (AOldMarkRec.FPicName <> ANewMarkRec.FPicName);
  VUpdateGeo := not ANewMarkRec.FGeometry.IsSameGeometry(AOldMarkRec.FGeometry);
  VUpdateName := (AOldMarkRec.FName <> ANewMarkRec.FName);
  VUpdateDesc := (AOldMarkRec.FDesc <> ANewMarkRec.FDesc);
  VUpdateCategory := (AOldMarkRec.FCategoryId <> ANewMarkRec.FCategoryId);

  VUpdateAppearance :=
    (AOldMarkRec.FColor1 <> ANewMarkRec.FColor1) or
    (AOldMarkRec.FColor2 <> ANewMarkRec.FColor2) or
    (AOldMarkRec.FScale1 <> ANewMarkRec.FScale1) or
    (AOldMarkRec.FScale2 <> ANewMarkRec.FScale2);

  VUpdateIdIndex := VUpdateAppearance or VUpdatePic or VUpdateCategory;

  if VUpdateGeo then begin
    VRect := ANewMarkRec.FGeometry.Bounds.Rect;
    VGeometryBlob := _GeomertryToBlob(ANewMarkRec.FGeometry, AGeometryWriter);
    CalcGeometrySize(VRect, ANewMarkRec.FGeoLonSize, ANewMarkRec.FGeoLatSize);
  end;

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    if VUpdateIdIndex then begin
      if ANewMarkRec.FPicName <> '' then begin
        ANewMarkRec.FPicId := _AddMarkImage(AClient, ACache, ANewMarkRec.FPicName);
      end else begin
        ANewMarkRec.FPicId := 0;
      end;

      ANewMarkRec.FAppearanceId := _AddMarkAppearance(
        AClient, ACache,
        ANewMarkRec.FColor1, ANewMarkRec.FColor2,
        ANewMarkRec.FScale1, ANewMarkRec.FScale2
      );
    end;

    VSQLMark := TSQLMark.Create;
    try
      _ClearFields;

      VSQLMark.IDValue := ANewMarkRec.FMarkId;

      if VUpdateCategory then begin
        _AddField('Category');
        VSQLMark.Category := Pointer(ANewMarkRec.FCategoryId);
      end;
      if VUpdatePic then begin
        _AddField('Image');
        VSQLMark.Image := Pointer(ANewMarkRec.FPicId);
      end;
      if VUpdateAppearance then begin
        _AddField('Appearance');
        VSQLMark.Appearance := Pointer(ANewMarkRec.FAppearanceId);
      end;
      if VUpdateName then begin
        _AddField('Name');
        VSQLMark.Name := StringToUTF8(ANewMarkRec.FName);
      end;
      if VUpdateDesc then begin
        _AddField('Desc');
        VSQLMark.Desc := StringToUTF8(ANewMarkRec.FDesc);
      end;

      if VUpdateGeo then begin
        _AddField('GeoLonSize');
        _AddField('GeoLatSize');
        _AddField('GeoType');
        _AddField('GeoCount');
        VSQLMark.GeoLonSize := ANewMarkRec.FGeoLonSize;
        VSQLMark.GeoLatSize := ANewMarkRec.FGeoLatSize;
        VSQLMark.GeoType := ANewMarkRec.FGeoType;
        VSQLMark.GeoCount := ANewMarkRec.FGeoCount;
      end;

      if VFieldsCount > 0 then begin
        // update mark
        CheckUpdateResult( AClient.Update(VSQLMark, _FieldsNamesStr) );
        // update cache
        if VUpdateName or VUpdateDesc or VUpdateGeo then begin
          ACache.FMarkCache.AddOrUpdate(ANewMarkRec);
        end;
        if VUpdateIdIndex then begin
          ACache.FMarkIdIndex.AddOrUpdate(ANewMarkRec);
          if VUpdateCategory then begin
            ACache.FMarkIdByCategoryIndex.Delete(AOldMarkRec.FCategoryId, ANewMarkRec.FMarkId);
            ACache.FMarkIdByCategoryIndex.Add(ANewMarkRec.FCategoryId, ANewMarkRec.FMarkId);
          end;
        end;
      end;

      if VUpdateGeo then begin
        // update geometry blob
        CheckUpdateResult(
          AClient.UpdateBlob(TSQLMark, VSQLMark.ID, 'GeoWKB', VGeometryBlob)
        );
        // update cache
        ACache.FMarkGeometryCache.AddOrUpdate(
          ANewMarkRec.FMarkId, Length(VGeometryBlob), ANewMarkRec.FGeometry
        );
      end;
    finally
      VSQLMark.Free;
    end;

    VSQLMarkFTS := TSQLMarkFTS.Create;
    try
      _ClearFields;

      VSQLMarkFTS.DocID := ANewMarkRec.FMarkId;

      if VUpdateName then begin
        _AddField('Name');
        VSQLMarkFTS.Name := StringToUTF8(SysUtils.AnsiLowerCase(ANewMarkRec.FName));
      end;
      if VUpdateDesc then begin
        _AddField('Desc');
        VSQLMarkFTS.Desc := StringToUTF8(SysUtils.AnsiLowerCase(ANewMarkRec.FDesc));
      end;

      if VFieldsCount > 0 then begin
        // update name / desc (fts index)
        CheckUpdateResult( AClient.Update(VSQLMarkFTS, _FieldsNamesStr) );
      end;
    finally
      VSQLMarkFTS.Free;
    end;

    if VUpdateGeo then begin
      VSQLMarkRTree := TSQLMarkRTree.Create;
      try
        VSQLMarkRTree.IDValue := ANewMarkRec.FMarkId;
        VSQLMarkRTree.Left := VRect.Left;
        VSQLMarkRTree.Right := VRect.Right;
        VSQLMarkRTree.Top := VRect.Top;
        VSQLMarkRTree.Bottom := VRect.Bottom;
        // update rect (rtree index)
        CheckUpdateResult( AClient.Update(VSQLMarkRTree) );
      finally
        VSQLMarkRTree.Free;
      end;
    end;

    if AOldMarkRec.FVisible <> ANewMarkRec.FVisible then begin
      // update view
      SetMarkVisibleSQL(
        ANewMarkRec.FMarkId, AUserID, ANewMarkRec.FVisible, AClient, ACache, False
      );
    end;

    CommitTransaction(AClient, VTransaction);
  except
    RollBackTransaction(AClient, VTransaction);
    raise;
  end;
end;

function ReadMarkSQL(
  out AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AMarkID: TID;
  const ACategoryID: TID;
  const AMarkName: string;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AGeometryReader: IGeometryFromStream
): Boolean;
var
  VMarkID: TID;
  VSQLWhere: RawUTF8;
  VFieldsCSV: RawUTF8;
  VSQLMark: TSQLMark;
  VSQLMarkView: TSQLMarkView;
  VSQLMarkImage: TSQLMarkImage;
  VSQLMarkAppearance: TSQLMarkAppearance;
  VSQLBlobData: TSQLRawBlob;
  VIndexItem: PSQLMarkIdIndexRec;
  VCacheItem: PSQLMarkRow;
  VViewItem: PSQLMarkViewRow;
  VPicItem: PSQLMarkImageRow;
  VAppearanceItem: PSQLMarkAppearanceRow;
  VGeometry: IGeometryLonLat;
begin
  Assert( (AMarkID > 0) or (AMarkName <> '') );

  Result := False;

  AMarkRec := cEmptySQLMarkRec;

  VSQLMark := TSQLMark.Create;
  try
    VSQLWhere := '';
    if AMarkID > 0 then begin
      VMarkID := AMarkID;
      if ACache.FMarkIdIndex.Find(VMarkID, VIndexItem) then begin
        // fill id's from cache
        AMarkRec.FMarkId := VMarkID;
        AMarkRec.FCategoryId := VIndexItem.CategoryId;
        AMarkRec.FPicId := VIndexItem.ImageId;
        AMarkRec.FAppearanceId := VIndexItem.AppearanceId;
        if not ACache.FMarkCache.Find(VIndexItem.MarkId, VCacheItem) then begin
          // get main params from db
          VFieldsCSV := 'Name,Desc,GeoType,GeoCount';
          VSQLWhere := FormatUTF8('ID=?', [], [VMarkID]);
          if AClient.Retrieve(VSQLWhere, VSQLMark, VFieldsCSV) then begin
            // fill main params from db
            AMarkRec.FName := UTF8ToString(VSQLMark.Name);
            AMarkRec.FDesc := UTF8ToString(VSQLMark.Desc);
            AMarkRec.FGeoType := VSQLMark.GeoType;
            AMarkRec.FGeoCount := VSQLMark.GeoCount;
            // add to cache
            ACache.FMarkCache.AddOrUpdate(AMarkRec);
          end else begin
            DeleteMarkSQL(VMarkID, AClient, ACache);
            Exit;
          end;
        end else begin
          // fill main params from cache
          AMarkRec.FName := VCacheItem.Name;
          AMarkRec.FDesc := VCacheItem.Desc;
          AMarkRec.FGeoType := VCacheItem.GeoType;
          AMarkRec.FGeoCount := VCacheItem.GeoCount;
        end;
      end else begin
        VSQLWhere := FormatUTF8('ID=?', [], [VMarkID]);
      end;
    end else if AMarkName <> '' then begin
      if ACategoryID > 0 then begin
        VSQLWhere := FormatUTF8('Name=? AND Category=?', [], [AMarkName, ACategoryID]);
      end else begin
        VSQLWhere := FormatUTF8('Name=?', [], [AMarkName]);
      end;
    end else begin
      Exit;
    end;
    if VSQLWhere <> '' then begin
      // get all from db
      VFieldsCSV := 'ID,Category,Image,Appearance,Name,Desc,GeoType,GeoCount';
      if AClient.Retrieve(VSQLWhere, VSQLMark, VFieldsCSV) then begin
        // fill id's from db
        VMarkID := VSQLMark.ID;
        AMarkRec.FMarkId := VMarkID;
        AMarkRec.FCategoryId := TID(VSQLMark.Category.AsTSQLRecord);
        CheckID(AMarkRec.FCategoryId);
        AMarkRec.FPicId := TID(VSQLMark.Image.AsTSQLRecord); // = 0 is OK
        AMarkRec.FAppearanceId := TID(VSQLMark.Appearance.AsTSQLRecord);
        CheckID(AMarkRec.FAppearanceId);
        // fill main params from db
        AMarkRec.FName := UTF8ToString(VSQLMark.Name);
        AMarkRec.FDesc := UTF8ToString(VSQLMark.Desc);
        AMarkRec.FGeoType := VSQLMark.GeoType;
        AMarkRec.FGeoCount := VSQLMark.GeoCount;
        // add to cache
        ACache.FMarkCache.AddOrUpdate(AMarkRec);
        ACache.FMarkIdIndex.AddOrUpdate(AMarkRec);
        ACache.FMarkIdByCategoryIndex.Add(AMarkRec.FCategoryId, AMarkRec.FMarkId);
      end else begin
        Exit;
      end;
    end;
  finally
    VSQLMark.Free;
  end;

  VMarkID := AMarkRec.FMarkId;

  // read geometry blob
  if ACache.FMarkGeometryCache.Find(VMarkID, VGeometry) then begin
    // found in cache
    AMarkRec.FGeometry := VGeometry;
  end else begin
    // read from db
    CheckRetrieveResult( AClient.RetrieveBlob(TSQLMark, VMarkID, 'GeoWKB', VSQLBlobData) );
    AMarkRec.FGeometry := _GeomertryFromBlob(VSQLBlobData, AGeometryReader);
    // add to cache
    ACache.FMarkGeometryCache.AddOrUpdate(VMarkID, Length(VSQLBlobData), AMarkRec.FGeometry);
  end;

  // read view
  if ACache.FMarkViewCache.Find(VMarkID, VViewItem) then begin
    // found in cache
    AMarkRec.FViewId := VViewItem.ViewId;
    AMarkRec.FVisible := VViewItem.Visible;
  end else begin
    // read from db
    VSQLMarkView := TSQLMarkView.Create(AClient, 'Mark=? AND User=?', [VMarkID, AUserID]);
    try
      if VSQLMarkView.ID > 0 then begin
        AMarkRec.FVisible := VSQLMarkView.Visible;
      end else begin
        AMarkRec.FVisible := True;
      end;
      // add to cache
      ACache.FMarkViewCache.AddOrUpdate(AMarkRec);
    finally
      VSQLMarkView.Free;
    end;
  end;

  // read pic name
  if AMarkRec.FGeoType = gtPoint then begin
    if AMarkRec.FPicId > 0 then begin
      if ACache.FMarkImage.Find(AMarkRec.FPicId, VPicItem) then begin
        // found in cache
        AMarkRec.FPicName := VPicItem.Name;
      end else begin
        // read from db
        VSQLMarkImage := TSQLMarkImage.Create(AClient, AMarkRec.FPicId);
        try
          CheckID(VSQLMarkImage.ID);
          AMarkRec.FPicName := UTF8ToString(VSQLMarkImage.Name);
          // add to cache
          ACache.FMarkImage.AddOrIgnore(AMarkRec);
        finally
          VSQLMarkImage.Free;
        end;
      end;
    end else begin
      AMarkRec.FPicName := '';
    end;
  end;

  // read appearance
  if ACache.FMarkAppearance.Find(AMarkRec.FAppearanceId, VAppearanceItem) then begin
    // found in cache
    AMarkRec.FColor1 := VAppearanceItem.Color1;
    AMarkRec.FColor2 := VAppearanceItem.Color2;
    AMarkRec.FScale1 := VAppearanceItem.Scale1;
    AMarkRec.FScale2 := VAppearanceItem.Scale2;
  end else begin
    // read from db
    VSQLMarkAppearance := TSQLMarkAppearance.Create(AClient, AMarkRec.FAppearanceId);
    try
      CheckID(VSQLMarkAppearance.ID);
      AMarkRec.FColor1 := VSQLMarkAppearance.Color1;
      AMarkRec.FColor2 := VSQLMarkAppearance.Color2;
      AMarkRec.FScale1 := VSQLMarkAppearance.Scale1;
      AMarkRec.FScale2 := VSQLMarkAppearance.Scale2;
      // add to cache
      ACache.FMarkAppearance.AddOrIgnore(AMarkRec);
    finally
      VSQLMarkAppearance.Free;
    end;
  end;

  Result := True;
end;

function SetMarkVisibleSQL(
  const AMarkID: TID;
  const AUserID: TID;
  const AVisible: Boolean;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache;
  const AUseTransaction: Boolean
): Boolean;
var
  VSQLMarkView: TSQLMarkView;
  VTransaction: TTransactionRec;
begin
  Assert(AMarkID > 0);

  Result := False;

  if AUseTransaction then begin
    StartTransaction(AClient, VTransaction, TSQLMarkView);
  end;
  try
    VSQLMarkView := TSQLMarkView.Create(AClient, 'Mark=? AND User=?', [AMarkID, AUserID]);
    try
      if VSQLMarkView.ID > 0 then begin
        // update db
        if VSQLMarkView.Visible <> AVisible then begin
          Result := AClient.UpdateField(TSQLMarkView, VSQLMarkView.ID, 'Visible', [AVisible]);
          CheckUpdateResult(Result);
        end;
        // update cache
        ACache.FMarkViewCache.AddOrUpdate(AMarkID, VSQLMarkView.ID, AVisible);
      end else if not AVisible then begin
        VSQLMarkView.User := Pointer(AUserID);
        VSQLMarkView.Mark := Pointer(AMarkID);
        VSQLMarkView.Visible := AVisible;
        // add to db
        CheckID( AClient.Add(VSQLMarkView, True) );
        Result := True;
        // add to cache
        ACache.FMarkViewCache.AddOrUpdate(AMarkID, VSQLMarkView.ID, AVisible);
      end;
    finally
      VSQLMarkView.Free;
    end;
    if AUseTransaction then begin
      CommitTransaction(AClient, VTransaction);
    end;
  except
    if AUseTransaction then begin
      RollBackTransaction(AClient, VTransaction);
    end;
    raise;
  end;
end;

function FillPrepareMarkIdIndex(
  const ACategoryID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache
): Integer;
var
  I, J, K: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VByCategory: Boolean;
  VArray: TIDDynArray;
  VMarkIdArray: TIDDynArray;
  VMarkIdRows: TSQLMarkIdIndexRecDynArray;
  VCategory: TID;
  VCurrCategory: TID;
begin
  Result := 0;

  VByCategory := ACategoryID > 0;

  if VByCategory then begin
    if ACache.FMarkIdByCategoryIndex.Find(ACategoryID, VArray, VCount) then begin
      Result := VCount;
      Exit;
    end;
    VList := AClient.ExecuteList(
      [TSQLMark],
      FormatUTF8(
        'SELECT ID,Image,Appearance FROM Mark WHERE Category=?',
        [], [ACategoryID]
      )
    );
  end else begin
    if ACache.FMarkIdIndex.IsPrepared then begin
      Result := ACache.FMarkIdIndex.Count;
      Exit;
    end;
    VList := AClient.ExecuteList(
      [TSQLMark],
      RawUTF8('SELECT ID,Image,Appearance,Category FROM Mark ORDER BY Category')
    );
  end;
  if Assigned(VList) then
  try
    VCount := VList.RowCount;
    SetLength(VMarkIdArray, VCount);
    SetLength(VMarkIdRows, VCount);
    K := 0;
    VCurrCategory := 0;
    for I := 0 to VCount - 1 do begin
      J := I + 1;
      VMarkIdRows[I].MarkId := VList.GetAsInt64(J, 0);
      VMarkIdRows[I].ImageId := VList.GetAsInt64(J, 1);
      VMarkIdRows[I].AppearanceId := VList.GetAsInt64(J, 2);
      VMarkIdArray[I] := VMarkIdRows[I].MarkId;
      if VByCategory then begin
        VCategory := ACategoryID;
      end else begin
        VCategory := VList.GetAsInt64(J, 3);
        if VCurrCategory = 0 then begin
          VCurrCategory := VCategory;
        end else if VCurrCategory = VCategory then begin
          Inc(K);
        end else if VCurrCategory < VCategory then begin
          if K > 0 then begin
            ACache.FMarkIdByCategoryIndex.AddPrepared(VCurrCategory, VMarkIdArray, I-(K-1), K);
            K := 0;
          end;
          VCurrCategory := VCategory;
        end else begin
          Assert(False, 'List not ordered by Category!');
        end;
      end;
      VMarkIdRows[I].CategoryId := VCategory;
    end;
    if VByCategory then begin
      ACache.FMarkIdIndex.AddArray(VMarkIdRows);
      ACache.FMarkIdByCategoryIndex.AddPrepared(ACategoryID, VMarkIdArray, 0, VCount);
    end else begin
      ACache.FMarkIdIndex.AddPrepared(VMarkIdRows);
    end;
    Result := VCount;
  finally
    VList.Free;
  end;
end;

procedure FillPrepareMarkIdCache(
  const ACategoryID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache
);
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VArray: TSQLMarkRowDynArray;
begin
  if ACategoryID > 0 then begin
    if ACache.FMarkCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := AClient.ExecuteList(
      [TSQLMark],
      FormatUTF8(
        'SELECT ID,Name,Desc,GeoType,GeoCount FROM Mark WHERE Category=?',
        [], [ACategoryID]
      )
    );
  end else begin
    if ACache.FMarkCache.IsPrepared then begin
      Exit;
    end;
    VList := AClient.ExecuteList(
      [TSQLMark],
      RawUTF8('SELECT ID,Name,Desc,GeoType,GeoCount FROM Mark')
    );
  end;
  if Assigned(VList) then
  try
    VCount := VList.RowCount;
    SetLength(VArray, VCount);
    for I := 0 to VCount - 1 do begin
      J := I + 1;
      VArray[I].MarkId := VList.GetAsInt64(J, 0);
      VArray[I].Name := VList.GetString(J, 1);
      VArray[I].Desc := VList.GetString(J, 2);
      VArray[I].GeoType := TSQLGeoType(VList.GetAsInteger(J, 3));
      VArray[I].GeoCount := VList.GetAsInteger(J, 4);
    end;
    ACache.FMarkCache.AddPrepared(ACategoryID, VArray);
  finally
    VList.Free;
  end;
end;

procedure FillPrepareMarkGeometryCache(
  const ACategoryID: TID;
  const AClient: TSQLRestClient;
  const AGeometryReader: IGeometryFromStream;
  const ACache: TSQLMarkDbCache
);
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VArray: TSQLMarkGeometryRecDynArray;
  VBlob: TSQLRawBlob;
begin
  if ACategoryID > 0 then begin
    if ACache.FMarkGeometryCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := AClient.ExecuteList(
      [TSQLMark],
      FormatUTF8('SELECT ID,GeoWKB FROM Mark WHERE Category=?', [], [ACategoryID])
    );
  end else begin
    if ACache.FMarkGeometryCache.IsPrepared then begin
      Exit;
    end;
    VList := AClient.ExecuteList(
      [TSQLMark],
      RawUTF8('SELECT ID,GeoWKB FROM Mark')
    );
  end;
  if Assigned(VList) then
  try
    VCount := VList.RowCount;
    SetLength(VArray, VCount);
    for I := 0 to VCount - 1 do begin
      J := I + 1;
      VArray[I].MarkId := VList.GetAsInt64(J, 0);
      VBlob := VList.GetBlob(J, 1);
      VArray[I].Geometry := _GeomertryFromBlob(VBlob, AGeometryReader);
      VArray[I].Size := Length(VBlob);
    end;
    ACache.FMarkGeometryCache.AddPrepared(ACategoryID, VArray);
  finally
    VList.Free;
  end;
end;

procedure FillPrepareMarkViewCache(
  const AUserID: TID;
  const ACategoryID: TID;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache
);
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VRows: TSQLMarkViewRowDynArray;
begin
  if ACategoryID > 0 then begin
    if ACache.FMarkViewCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := AClient.ExecuteList(
      [TSQLMarkView, TSQLMark],
      FormatUTF8(
        'SELECT MarkView.ID,MarkView.Mark,MarkView.Visible' + ' '+
        'FROM MarkView,Mark' + ' ' +
        'WHERE MarkView.Mark=Mark.ID AND MarkView.User=? AND Mark.Category=?',
        [], [AUserID, ACategoryID]
      )
    );
  end else begin
    if ACache.FMarkViewCache.IsPrepared then begin
      Exit;
    end;
    VList := AClient.ExecuteList(
      [TSQLMarkView],
      RawUTF8('SELECT ID,Mark,Visible FROM MarkView')
    );
  end;
  if Assigned(VList) then
  try
    VCount := VList.RowCount;
    SetLength(VRows, VCount);
    for I := 0 to VCount - 1 do begin
      J := I + 1;
      VRows[I].ViewId := VList.GetAsInt64(J, 0);
      VRows[I].MarkId := VList.GetAsInt64(J, 1);
      VRows[I].Visible := (VList.GetAsInteger(J, 2) <> 0);
    end;
    ACache.FMarkViewCache.AddPrepared(ACategoryID, VRows);
  finally
    VList.Free;
  end;
end;

function GetMarkRecArray(
  const AUserID: TID;
  const ACategoryId: TID;
  const AClient: TSQLRestClient;
  const AGeometryReader: IGeometryFromStream;
  const ACache: TSQLMarkDbCache;
  const AIncludeHiddenMarks: Boolean;
  const AIncludeGeometry: Boolean;
  out AMarkRecArray: TSQLMarkRecDynArray
): Integer;

  function FillMarkRec(const AMarkRec: PSQLMarkRec; const AIndexRec: PSQLMarkIdIndexRec): Boolean;
  var
    VCacheItem: PSQLMarkRow;
    VViewItem: PSQLMarkViewRow;
    VGeomatry: IGeometryLonLat;
  begin
    Result := False;
    
    AMarkRec.FMarkId := AIndexRec.MarkId;
    AMarkRec.FCategoryId := AIndexRec.CategoryId;
    AMarkRec.FPicId := AIndexRec.ImageId;
    AMarkRec.FAppearanceId := AIndexRec.AppearanceId;

    if not ACache.FMarkCache.Find(AIndexRec.MarkId, VCacheItem) then begin
      Assert(False);
      Exit;
    end;

    AMarkRec.FName := VCacheItem.Name;
    AMarkRec.FDesc := VCacheItem.Desc;
    AMarkRec.FGeoType := VCacheItem.GeoType;
    AMarkRec.FGeoCount := VCacheItem.GeoCount;

    if ACache.FMarkViewCache.Find(AIndexRec.MarkId, VViewItem) then begin
      AMarkRec.FViewId := VViewItem.ViewId;
      AMarkRec.FVisible := VViewItem.Visible;
      if not AIncludeHiddenMarks then begin
        if not AMarkRec.FVisible then begin
          Exit;
        end;
      end;
    end;

    if AIncludeGeometry then begin
      if ACache.FMarkGeometryCache.Find(AIndexRec.MarkId, VGeomatry) then begin
        AMarkRec.FGeometry := VGeomatry;
      end else begin
        Assert(False);
        Exit;
      end;
    end;

    Result := True;
  end;

var
  I, J: Integer;
  VCount: Integer;
  VIdCount: Integer;
  VRows: TSQLMarkIdIndexRecDynArray;
  VArray: TIDDynArray;
  VIndexRec: PSQLMarkIdIndexRec;
begin
  J := 0;

  VCount := FillPrepareMarkIdIndex(ACategoryId, AClient, ACache);
  if VCount > 0 then begin
    FillPrepareMarkIdCache(ACategoryId, AClient, ACache);
    FillPrepareMarkViewCache(AUserID, ACategoryId, AClient, ACache);
    if AIncludeGeometry then begin
      FillPrepareMarkGeometryCache(ACategoryId, AClient, AGeometryReader, ACache);
    end;

    SetLength(AMarkRecArray, VCount);

    if ACategoryId > 0 then begin
      if ACache.FMarkIdByCategoryIndex.Find(ACategoryId, VArray, VIdCount) then begin
        Assert(VCount >= VIdCount);
        for I := 0 to VIdCount - 1 do begin
          if ACache.FMarkIdIndex.Find(VArray[I], VIndexRec) then begin
            AMarkRecArray[J] := cEmptySQLMarkRec;
            if FillMarkRec(@AMarkRecArray[J], VIndexRec) then begin
              Inc(J);
            end;
          end;
        end;
      end else begin
        Assert(False);
      end;
    end else begin
      VRows := ACache.FMarkIdIndex.Rows;
      for I := 0 to ACache.FMarkIdIndex.Count - 1 do begin
        AMarkRecArray[J] := cEmptySQLMarkRec;
        if FillMarkRec(@AMarkRecArray[J], @VRows[I]) then begin
          Inc(J);
        end;
      end;
    end;
    SetLength(AMarkRecArray, J);
  end;

  Result := J;
end;

function SetMarksInCategoryVisibleSQL(
  const AUserID: TID;
  const ACategoryID: TID;
  const AVisible: Boolean;
  const AClient: TSQLRestClient;
  const ACache: TSQLMarkDbCache
): Boolean;
var
  I: Integer;
  VCount: Integer;
  VArray: TIDDynArray;
  VTransaction: TTransactionRec;
begin
  Result := False;
  if FillPrepareMarkIdIndex(ACategoryID, AClient, ACache) > 0 then begin
    if ACache.FMarkIdByCategoryIndex.Find(ACategoryID, VArray, VCount) then begin
      StartTransaction(AClient, VTransaction, TSQLMarkView);
      try
        for I := 0 to VCount - 1 do begin
          if SetMarkVisibleSQL(VArray[I], AUserID, AVisible, AClient, ACache, False) then begin
            Result := True;
          end;
        end;
        CommitTransaction(AClient, VTransaction);
      except
        RollBackTransaction(AClient, VTransaction);
        raise;
      end;
    end else begin
      Assert(False);
    end;
  end;
end;

end.
