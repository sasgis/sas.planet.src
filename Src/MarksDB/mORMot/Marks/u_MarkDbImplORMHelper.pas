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
  t_GeoTypes,
  t_MarkSystemORM,
  i_GeometryLonLat,
  i_GeometryToStream,
  i_GeometryFromStream,
  i_MarkSystemImplORMClientProvider,
  u_MarkSystemORMModel,
  u_MarkDbImplORMCache;

type
  TMarkDbImplORMHelper = class
  private
    FUserID: TID;
    FClient: TSQLRestClient;
    FCache: TSQLMarkDbCache;
    FGeometryWriter: IGeometryToStream;
    FGeometryReader: IGeometryFromStream;
    FClientType: TMarkSystemImplORMClientType;
    FClientProvider: IMarkSystemImplORMClientProvider;
  private
    function _GeomertryFromBlob(const ABlob: TSQLRawBlob): IGeometryLonLat;
    function _GeomertryToBlob(const AGeometry: IGeometryLonLat): TSQLRawBlob;
    function _AddMarkImage(const APicName: string): TID;
    function _AddMarkAppearance(
      const AColor1, AColor2: Cardinal;
      const AScale1, AScale2: Integer
    ): TID;
    function _FillPrepareMarkIdIndex(const ACategoryID: TID): Integer;
    procedure _FillPrepareMarkIdCache(const ACategoryID: TID);
    procedure _FillPrepareMarkGeometryCache(const ACategoryID: TID);
    procedure _FillPrepareMarkViewCache(const ACategoryID: TID);
    function _GetMarkRecArrayByRectSQL(
      const ACategoryIDArray: TIDDynArray;
      const ARect: TDoubleRect;
      const AIncludeHiddenMarks: Boolean;
      const ALonLatSize: TDoublePoint;
      out AMarkRecArray: TSQLMarkRecDynArray
    ): Integer;
    function _GetMarkRecArrayByRectMongoDB(
      const ACategoryIDArray: TIDDynArray;
      const ARect: TDoubleRect;
      const AIncludeHiddenMarks: Boolean;
      const ALonLatSize: TDoublePoint;
      out AMarkRecArray: TSQLMarkRecDynArray
    ): Integer;
    function _GetMarkRecArrayByTextSQLite3(
      const ASearchText: string;
      const AMaxCount: Integer;
      const AIncludeHiddenMarks: Boolean;
      const ASearchInDescription: Boolean;
      out AIDArray: TIDDynArray
    ): Integer;
    function _GetMarkRecArrayByTextSQL(
      const ASearchText: string;
      const AMaxCount: Integer;
      const AIncludeHiddenMarks: Boolean;
      const ASearchInDescription: Boolean;
      out AIDArray: TIDDynArray
    ): Integer;
  public
    procedure DeleteMarkSQL(
      const AMarkID: TID
    );
    procedure InsertMarkSQL(
      var AMarkRec: TSQLMarkRec
    );
    procedure UpdateMarkSQL(
      const AOldMarkRec: TSQLMarkRec;
      var ANewMarkRec: TSQLMarkRec
    );
    function ReadMarkSQL(
      out AMarkRec: TSQLMarkRec;
      const AMarkID: TID;
      const ACategoryID: TID;
      const AMarkName: string
    ): Boolean;
    function SetMarkVisibleSQL(
      const AMarkID: TID;
      const AVisible: Boolean;
      const AUseTransaction: Boolean
    ): Boolean;
    function SetMarksInCategoryVisibleSQL(
      const ACategoryID: TID;
      const AVisible: Boolean
    ): Boolean;
    function GetMarkRecArray(
      const ACategoryId: TID;
      const AIncludeHiddenMarks: Boolean;
      const AIncludeGeometry: Boolean;
      out AMarkRecArray: TSQLMarkRecDynArray
    ): Integer;
    function GetMarkRecArrayByRect(
      const ACategoryIDArray: TIDDynArray;
      const ARect: TDoubleRect;
      const AIncludeHiddenMarks: Boolean;
      const ALonLatSize: TDoublePoint;
      out AMarkRecArray: TSQLMarkRecDynArray
    ): Integer;
    function GetMarkRecArrayByText(
      const ASearchText: string;
      const AMaxCount: Integer;
      const AIncludeHiddenMarks: Boolean;
      const ASearchInDescription: Boolean;
      out AMarkRecArray: TSQLMarkRecDynArray
    ): Integer;
  public
    constructor Create(
      const ACache: TSQLMarkDbCache;
      const AGeometryWriter: IGeometryToStream;
      const AGeometryReader: IGeometryFromStream;
      const AClientProvider: IMarkSystemImplORMClientProvider
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_MarkSystemORMTools;

{ TMarkDbImplORMHelper }

constructor TMarkDbImplORMHelper.Create(
  const ACache: TSQLMarkDbCache;
  const AGeometryWriter: IGeometryToStream;
  const AGeometryReader: IGeometryFromStream;
  const AClientProvider: IMarkSystemImplORMClientProvider
);
begin
  Assert(AGeometryWriter <> nil);
  Assert(AGeometryReader <> nil);
  inherited Create;
  FCache := ACache;
  FGeometryWriter := AGeometryWriter;
  FGeometryReader := AGeometryReader;
  FClientProvider := AClientProvider;
  FUserID := FClientProvider.UserID;
  FClient := FClientProvider.RestClient;
  FClientType := FClientProvider.RestClientType;
end;

destructor TMarkDbImplORMHelper.Destroy;
begin
  FClientProvider := nil;
  inherited Destroy;
end;

function TMarkDbImplORMHelper._GeomertryFromBlob(
  const ABlob: TSQLRawBlob
): IGeometryLonLat;
var
  VStream: TRawByteStringStream;
begin
  Assert(ABlob <> '');
  VStream := TRawByteStringStream.Create(ABlob);
  try
    Result := FGeometryReader.Parse(VStream);
  finally
    VStream.Free;
  end;
end;

function TMarkDbImplORMHelper._GeomertryToBlob(
  const AGeometry: IGeometryLonLat
): TSQLRawBlob;
var
  VStream: TRawByteStringStream;
begin
  Assert(AGeometry <> nil);
  VStream := TRawByteStringStream.Create;
  try
    FGeometryWriter.Save(AGeometry, VStream);
    Result := VStream.DataString;
  finally
    VStream.Free;
  end;
end;

function TMarkDbImplORMHelper._AddMarkImage(const APicName: string): TID;
var
  VPicName: RawUTF8;
  VItem: PSQLMarkImageRow;
  VSQLMarkImage: TSQLMarkImage;
begin
  Result := 0;
  if FCache.FMarkImage.Find(APicName, VItem) then begin
    // found in cache
    Result := VItem.ImageId;
  end else begin
    VPicName := StringToUTF8(APicName);
    VSQLMarkImage := TSQLMarkImage.Create(FClient, 'Name=?', [VPicName]);
    try
      if VSQLMarkImage.ID = 0 then begin
        VSQLMarkImage.Name := VPicName;
        // add to db
        CheckID( FClient.Add(VSQLMarkImage, True) );
      end;
      Result := VSQLMarkImage.ID;
      // add to cache
      FCache.FMarkImage.AddOrIgnore(Result, APicName);
    finally
      VSQLMarkImage.Free;
    end;
  end;
end;

function TMarkDbImplORMHelper._AddMarkAppearance(
  const AColor1, AColor2: Cardinal;
  const AScale1, AScale2: Integer
): TID;
var
  VItem: PSQLMarkAppearanceRow;
  VSQLMarkAppearance: TSQLMarkAppearance;
begin
  Result := 0;
  if FCache.FMarkAppearance.Find(AColor1, AColor2, AScale1, AScale2, VItem) then begin
    // found in cache
    Result := VItem.AppearanceId;
  end else begin
    VSQLMarkAppearance := TSQLMarkAppearance.Create(
      FClient, 'Color1=? AND Color2=? AND Scale1=? AND Scale2=?',
      [AColor1, AColor2, AScale1, AScale2]
    );
    try
      if VSQLMarkAppearance.ID = 0 then begin
        VSQLMarkAppearance.Color1 := AColor1;
        VSQLMarkAppearance.Color2 := AColor2;
        VSQLMarkAppearance.Scale1 := AScale1;
        VSQLMarkAppearance.Scale2 := AScale2;
        // add to db
        CheckID( FClient.Add(VSQLMarkAppearance, True) );
      end;
      Result := VSQLMarkAppearance.ID;
      // add to cache
      FCache.FMarkAppearance.AddOrIgnore(Result, AColor1, AColor2, AScale1, AScale2);
    finally
      VSQLMarkAppearance.Free;
    end;
  end;
end;

procedure TMarkDbImplORMHelper.DeleteMarkSQL(const AMarkID: TID);
var
  VTransaction: TTransactionRec;
  VIndex: PSQLMarkIdIndexRec;
begin
  if not (AMarkID > 0) then begin
    Assert(False);
    Exit;
  end;

  StartTransaction(FClient, VTransaction, TSQLMark);
  try
    // delete view for all Users if exists
    FClient.Delete(TSQLMarkView, FormatUTF8('Mark=?', [], [AMarkID]));

    // delete rect
    CheckDeleteResult( FClient.Delete(TSQLMarkRTree, AMarkID) );

    // delete name and desc
    CheckDeleteResult( FClient.Delete(TSQLMarkFTS, AMarkID) );

    // delete mark
    CheckDeleteResult( FClient.Delete(TSQLMark, AMarkID) );

    // pic name and appearance are never deleted...

    CommitTransaction(FClient, VTransaction);

  except
    RollBackTransaction(FClient, VTransaction);
    raise;
  end;

  // delete from cache
  FCache.FMarkCache.Delete(AMarkID);
  FCache.FMarkGeometryCache.Delete(AMarkID);
  FCache.FMarkViewCache.Delete(AMarkID);
  if FCache.FMarkIdIndex.Find(AMarkID, VIndex) then begin
    FCache.FMarkIdIndex.Delete(AMarkID);
    FCache.FMarkIdByCategoryIndex.Delete(VIndex.CategoryId, AMarkID);
  end;
end;

procedure TMarkDbImplORMHelper.InsertMarkSQL(var AMarkRec: TSQLMarkRec);
var
  VRect: TDoubleRect;
  VGeometryBlob: TSQLRawBlob;
  VSQLMark: TSQLMark;
  VSQLMarkView: TSQLMarkView;
  VSQLMarkFTS: TSQLMarkFTS;
  VSQLMarkRTree: TSQLMarkRTree;
  VTransaction: TTransactionRec;
begin
  VRect := AMarkRec.FGeometry.Bounds.Rect;
  VGeometryBlob := _GeomertryToBlob(AMarkRec.FGeometry);
  CalcGeometrySize(VRect, AMarkRec.FGeoLonSize, AMarkRec.FGeoLatSize);

  StartTransaction(FClient, VTransaction, TSQLMark);
  try
    if AMarkRec.FPicName <> '' then begin
      AMarkRec.FPicId := _AddMarkImage(AMarkRec.FPicName);
    end else begin
      AMarkRec.FPicId := 0;
    end;

    AMarkRec.FAppearanceId := _AddMarkAppearance(
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
      CheckID( FClient.Add(VSQLMark, True) );
      AMarkRec.FMarkId := VSQLMark.ID;
      // add geometry blob to db
      CheckUpdateResult( FClient.UpdateBlob(TSQLMark, AMarkRec.FMarkId, 'GeoWKB', VGeometryBlob) );
      // add to cache
      FCache.FMarkCache.AddOrUpdate(AMarkRec);
      FCache.FMarkGeometryCache.AddOrUpdate(AMarkRec.FMarkId, Length(VGeometryBlob), AMarkRec.FGeometry);
      FCache.FMarkIdIndex.AddOrUpdate(AMarkRec);
      FCache.FMarkIdByCategoryIndex.Add(AMarkRec.FCategoryId, AMarkRec.FMarkId);
    finally
      VSQLMark.Free;
    end;

    VSQLMarkFTS := TSQLMarkFTS.Create;
    try
      VSQLMarkFTS.DocID := AMarkRec.FMarkId;
      VSQLMarkFTS.Name := StringToUTF8(SysUtils.AnsiLowerCase(AMarkRec.FName));
      VSQLMarkFTS.Desc := StringToUTF8(SysUtils.AnsiLowerCase(AMarkRec.FDesc));
      // add name and desc to db (fts index)
      CheckID( FClient.Add(VSQLMarkFTS, True) );
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
      CheckID( FClient.Add(VSQLMarkRTree, True, True) );
    finally
      VSQLMarkRTree.Free;
    end;

    if not AMarkRec.FVisible then begin
      VSQLMarkView := TSQLMarkView.Create;
      try
        VSQLMarkView.User := Pointer(FUserID);
        VSQLMarkView.Mark := Pointer(AMarkRec.FMarkId);
        VSQLMarkView.Visible := AMarkRec.FVisible;
        // add view to db
        CheckID( FClient.Add(VSQLMarkView, True) );
        AMarkRec.FViewId := VSQLMarkView.ID;
        // add view to cache
        FCache.FMarkViewCache.AddOrUpdate(AMarkRec);
      finally
        VSQLMarkView.Free;
      end;
    end;

    CommitTransaction(FClient, VTransaction);
  except
    RollBackTransaction(FClient, VTransaction);
    raise;
  end;
end;

procedure TMarkDbImplORMHelper.UpdateMarkSQL(
  const AOldMarkRec: TSQLMarkRec;
  var ANewMarkRec: TSQLMarkRec
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
    VGeometryBlob := _GeomertryToBlob(ANewMarkRec.FGeometry);
    CalcGeometrySize(VRect, ANewMarkRec.FGeoLonSize, ANewMarkRec.FGeoLatSize);
  end;

  StartTransaction(FClient, VTransaction, TSQLMark);
  try
    if VUpdateIdIndex then begin
      if ANewMarkRec.FPicName <> '' then begin
        ANewMarkRec.FPicId := _AddMarkImage(ANewMarkRec.FPicName);
      end else begin
        ANewMarkRec.FPicId := 0;
      end;

      ANewMarkRec.FAppearanceId := _AddMarkAppearance(
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
        CheckUpdateResult( FClient.Update(VSQLMark, _FieldsNamesStr) );
        // update cache
        if VUpdateName or VUpdateDesc or VUpdateGeo then begin
          FCache.FMarkCache.AddOrUpdate(ANewMarkRec);
        end;
        if VUpdateIdIndex then begin
          FCache.FMarkIdIndex.AddOrUpdate(ANewMarkRec);
          if VUpdateCategory then begin
            FCache.FMarkIdByCategoryIndex.Delete(AOldMarkRec.FCategoryId, ANewMarkRec.FMarkId);
            FCache.FMarkIdByCategoryIndex.Add(ANewMarkRec.FCategoryId, ANewMarkRec.FMarkId);
          end;
        end;
      end;

      if VUpdateGeo then begin
        // update geometry blob
        CheckUpdateResult(
          FClient.UpdateBlob(TSQLMark, VSQLMark.ID, 'GeoWKB', VGeometryBlob)
        );
        // update cache
        FCache.FMarkGeometryCache.AddOrUpdate(
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
        CheckUpdateResult( FClient.Update(VSQLMarkFTS, _FieldsNamesStr) );
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
        CheckUpdateResult( FClient.Update(VSQLMarkRTree) );
      finally
        VSQLMarkRTree.Free;
      end;
    end;

    if AOldMarkRec.FVisible <> ANewMarkRec.FVisible then begin
      // update view
      SetMarkVisibleSQL(ANewMarkRec.FMarkId, ANewMarkRec.FVisible, False);
    end;

    CommitTransaction(FClient, VTransaction);
  except
    RollBackTransaction(FClient, VTransaction);
    raise;
  end;
end;

function TMarkDbImplORMHelper.ReadMarkSQL(
  out AMarkRec: TSQLMarkRec;
  const AMarkID: TID;
  const ACategoryID: TID;
  const AMarkName: string
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
      if FCache.FMarkIdIndex.Find(VMarkID, VIndexItem) then begin
        // fill id's from cache
        AMarkRec.FMarkId := VMarkID;
        AMarkRec.FCategoryId := VIndexItem.CategoryId;
        AMarkRec.FPicId := VIndexItem.ImageId;
        AMarkRec.FAppearanceId := VIndexItem.AppearanceId;
        if not FCache.FMarkCache.Find(VIndexItem.MarkId, VCacheItem) then begin
          // get main params from db
          VFieldsCSV := 'Name,Desc,GeoType,GeoCount';
          VSQLWhere := FormatUTF8('RowID=?', [], [VMarkID]);
          if FClient.Retrieve(VSQLWhere, VSQLMark, VFieldsCSV) then begin
            // fill main params from db
            AMarkRec.FName := UTF8ToString(VSQLMark.Name);
            AMarkRec.FDesc := UTF8ToString(VSQLMark.Desc);
            AMarkRec.FGeoType := VSQLMark.GeoType;
            AMarkRec.FGeoCount := VSQLMark.GeoCount;
            // add to cache
            FCache.FMarkCache.AddOrUpdate(AMarkRec);
          end else begin
            DeleteMarkSQL(VMarkID);
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
        VSQLWhere := FormatUTF8('RowID=?', [], [VMarkID]);
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
      VFieldsCSV := 'RowID,Category,Image,Appearance,Name,Desc,GeoType,GeoCount';
      if FClient.Retrieve(VSQLWhere, VSQLMark, VFieldsCSV) then begin
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
        FCache.FMarkCache.AddOrUpdate(AMarkRec);
        FCache.FMarkIdIndex.AddOrUpdate(AMarkRec);
        FCache.FMarkIdByCategoryIndex.Add(AMarkRec.FCategoryId, AMarkRec.FMarkId);
      end else begin
        Exit;
      end;
    end;
  finally
    VSQLMark.Free;
  end;

  VMarkID := AMarkRec.FMarkId;

  // read geometry blob
  if FCache.FMarkGeometryCache.Find(VMarkID, VGeometry) then begin
    // found in cache
    AMarkRec.FGeometry := VGeometry;
  end else begin
    // read from db
    CheckRetrieveResult( FClient.RetrieveBlob(TSQLMark, VMarkID, 'GeoWKB', VSQLBlobData) );
    AMarkRec.FGeometry := _GeomertryFromBlob(VSQLBlobData);
    // add to cache
    FCache.FMarkGeometryCache.AddOrUpdate(VMarkID, Length(VSQLBlobData), AMarkRec.FGeometry);
  end;

  // read view
  if FCache.FMarkViewCache.Find(VMarkID, VViewItem) then begin
    // found in cache
    AMarkRec.FViewId := VViewItem.ViewId;
    AMarkRec.FVisible := VViewItem.Visible;
  end else begin
    // read from db
    VSQLMarkView := TSQLMarkView.Create(FClient, 'Mark=? AND User=?', [VMarkID, FUserID]);
    try
      if VSQLMarkView.ID > 0 then begin
        AMarkRec.FVisible := VSQLMarkView.Visible;
      end else begin
        AMarkRec.FVisible := True;
      end;
      // add to cache
      FCache.FMarkViewCache.AddOrUpdate(AMarkRec);
    finally
      VSQLMarkView.Free;
    end;
  end;

  // read pic name
  if AMarkRec.FGeoType = gtPoint then begin
    if AMarkRec.FPicId > 0 then begin
      if FCache.FMarkImage.Find(AMarkRec.FPicId, VPicItem) then begin
        // found in cache
        AMarkRec.FPicName := VPicItem.Name;
      end else begin
        // read from db
        VSQLMarkImage := TSQLMarkImage.Create(FClient, AMarkRec.FPicId);
        try
          CheckID(VSQLMarkImage.ID);
          AMarkRec.FPicName := UTF8ToString(VSQLMarkImage.Name);
          // add to cache
          FCache.FMarkImage.AddOrIgnore(AMarkRec);
        finally
          VSQLMarkImage.Free;
        end;
      end;
    end else begin
      AMarkRec.FPicName := '';
    end;
  end;

  // read appearance
  if FCache.FMarkAppearance.Find(AMarkRec.FAppearanceId, VAppearanceItem) then begin
    // found in cache
    AMarkRec.FColor1 := VAppearanceItem.Color1;
    AMarkRec.FColor2 := VAppearanceItem.Color2;
    AMarkRec.FScale1 := VAppearanceItem.Scale1;
    AMarkRec.FScale2 := VAppearanceItem.Scale2;
  end else begin
    // read from db
    VSQLMarkAppearance := TSQLMarkAppearance.Create(FClient, AMarkRec.FAppearanceId);
    try
      CheckID(VSQLMarkAppearance.ID);
      AMarkRec.FColor1 := VSQLMarkAppearance.Color1;
      AMarkRec.FColor2 := VSQLMarkAppearance.Color2;
      AMarkRec.FScale1 := VSQLMarkAppearance.Scale1;
      AMarkRec.FScale2 := VSQLMarkAppearance.Scale2;
      // add to cache
      FCache.FMarkAppearance.AddOrIgnore(AMarkRec);
    finally
      VSQLMarkAppearance.Free;
    end;
  end;

  Result := True;
end;

function TMarkDbImplORMHelper.SetMarkVisibleSQL(
  const AMarkID: TID;
  const AVisible: Boolean;
  const AUseTransaction: Boolean
): Boolean;
var
  VSQLMarkView: TSQLMarkView;
  VTransaction: TTransactionRec;
begin
  Assert(AMarkID > 0);

  Result := False;

  if AUseTransaction then begin
    StartTransaction(FClient, VTransaction, TSQLMarkView);
  end;
  try
    VSQLMarkView := TSQLMarkView.Create(FClient, 'Mark=? AND User=?', [AMarkID, FUserID]);
    try
      if VSQLMarkView.ID > 0 then begin
        // update db
        if VSQLMarkView.Visible <> AVisible then begin
          Result := FClient.UpdateField(TSQLMarkView, VSQLMarkView.ID, 'Visible', [AVisible]);
          CheckUpdateResult(Result);
        end;
        // update cache
        FCache.FMarkViewCache.AddOrUpdate(AMarkID, VSQLMarkView.ID, AVisible);
      end else if not AVisible then begin
        VSQLMarkView.User := Pointer(FUserID);
        VSQLMarkView.Mark := Pointer(AMarkID);
        VSQLMarkView.Visible := AVisible;
        // add to db
        CheckID( FClient.Add(VSQLMarkView, True) );
        Result := True;
        // add to cache
        FCache.FMarkViewCache.AddOrUpdate(AMarkID, VSQLMarkView.ID, AVisible);
      end;
    finally
      VSQLMarkView.Free;
    end;
    if AUseTransaction then begin
      CommitTransaction(FClient, VTransaction);
    end;
  except
    if AUseTransaction then begin
      RollBackTransaction(FClient, VTransaction);
    end;
    raise;
  end;
end;

function TMarkDbImplORMHelper.SetMarksInCategoryVisibleSQL(
  const ACategoryID: TID;
  const AVisible: Boolean
): Boolean;
var
  I: Integer;
  VCount: Integer;
  VArray: TIDDynArray;
  VTransaction: TTransactionRec;
begin
  Result := False;
  if _FillPrepareMarkIdIndex(ACategoryID) > 0 then begin
    if FCache.FMarkIdByCategoryIndex.Find(ACategoryID, VArray, VCount) then begin
      StartTransaction(FClient, VTransaction, TSQLMarkView);
      try
        for I := 0 to VCount - 1 do begin
          if SetMarkVisibleSQL(VArray[I], AVisible, False) then begin
            Result := True;
          end;
        end;
        CommitTransaction(FClient, VTransaction);
      except
        RollBackTransaction(FClient, VTransaction);
        raise;
      end;
    end else begin
      Assert(False);
    end;
  end;
end;

function TMarkDbImplORMHelper._FillPrepareMarkIdIndex(const ACategoryID: TID): Integer;
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
    if FCache.FMarkIdByCategoryIndex.Find(ACategoryID, VArray, VCount) then begin
      Result := VCount;
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TSQLMark],
      FormatUTF8(
        'SELECT RowID,Image,Appearance FROM Mark WHERE Category=?',
        [], [ACategoryID]
      )
    );
  end else begin
    if FCache.FMarkIdIndex.IsPrepared then begin
      Result := FCache.FMarkIdIndex.Count;
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TSQLMark],
      RawUTF8('SELECT RowID,Image,Appearance,Category FROM Mark ORDER BY Category')
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
            FCache.FMarkIdByCategoryIndex.AddPrepared(VCurrCategory, VMarkIdArray, I-(K-1), K);
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
      FCache.FMarkIdIndex.AddArray(VMarkIdRows);
      FCache.FMarkIdByCategoryIndex.AddPrepared(ACategoryID, VMarkIdArray, 0, VCount);
    end else begin
      FCache.FMarkIdIndex.AddPrepared(VMarkIdRows);
    end;
    Result := VCount;
  finally
    VList.Free;
  end;
end;

procedure TMarkDbImplORMHelper._FillPrepareMarkIdCache(const ACategoryID: TID);
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VArray: TSQLMarkRowDynArray;
begin
  if ACategoryID > 0 then begin
    if FCache.FMarkCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TSQLMark],
      FormatUTF8(
        'SELECT RowID,Name,Desc,GeoType,GeoCount FROM Mark WHERE Category=?',
        [], [ACategoryID]
      )
    );
  end else begin
    if FCache.FMarkCache.IsPrepared then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TSQLMark],
      RawUTF8('SELECT RowID,Name,Desc,GeoType,GeoCount FROM Mark')
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
    FCache.FMarkCache.AddPrepared(ACategoryID, VArray);
  finally
    VList.Free;
  end;
end;

procedure TMarkDbImplORMHelper._FillPrepareMarkGeometryCache(const ACategoryID: TID);
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VArray: TSQLMarkGeometryRecDynArray;
  VBlob: TSQLRawBlob;
begin
  if ACategoryID > 0 then begin
    if FCache.FMarkGeometryCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TSQLMark],
      FormatUTF8('SELECT RowID,GeoWKB FROM Mark WHERE Category=?', [], [ACategoryID])
    );
  end else begin
    if FCache.FMarkGeometryCache.IsPrepared then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TSQLMark],
      RawUTF8('SELECT RowID,GeoWKB FROM Mark')
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
      VArray[I].Geometry := _GeomertryFromBlob(VBlob);
      VArray[I].Size := Length(VBlob);
    end;
    FCache.FMarkGeometryCache.AddPrepared(ACategoryID, VArray);
  finally
    VList.Free;
  end;
end;

procedure TMarkDbImplORMHelper._FillPrepareMarkViewCache(const ACategoryID: TID);
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
  VRows: TSQLMarkViewRowDynArray;
begin
  if (ACategoryID > 0) and (FClientType <> ctMongoDB) then begin
    if FCache.FMarkViewCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TSQLMarkView, TSQLMark],
      FormatUTF8(
        'SELECT MarkView.RowID,MarkView.Mark,MarkView.Visible' + ' '+
        'FROM MarkView,Mark' + ' ' +
        'WHERE MarkView.Mark=Mark.RowID AND MarkView.User=? AND Mark.Category=?',
        [], [FUserID, ACategoryID]
      )
    );
  end else begin
    if FCache.FMarkViewCache.IsPrepared then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TSQLMarkView],
      RawUTF8('SELECT RowID,Mark,Visible FROM MarkView')
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
    FCache.FMarkViewCache.AddPrepared(ACategoryID, VRows);
  finally
    VList.Free;
  end;
end;

function TMarkDbImplORMHelper.GetMarkRecArray(
  const ACategoryId: TID;
  const AIncludeHiddenMarks: Boolean;
  const AIncludeGeometry: Boolean;
  out AMarkRecArray: TSQLMarkRecDynArray
): Integer;

  function FillMarkRec(const AMarkRec: PSQLMarkRec; const AIndexRec: PSQLMarkIdIndexRec): Boolean;
  var
    VCacheItem: PSQLMarkRow;
    VViewItem: PSQLMarkViewRow;
    VGeometry: IGeometryLonLat;
  begin
    Result := False;

    AMarkRec.FMarkId := AIndexRec.MarkId;
    AMarkRec.FCategoryId := AIndexRec.CategoryId;
    AMarkRec.FPicId := AIndexRec.ImageId;
    AMarkRec.FAppearanceId := AIndexRec.AppearanceId;

    if not FCache.FMarkCache.Find(AIndexRec.MarkId, VCacheItem) then begin
      Assert(False);
      Exit;
    end;

    AMarkRec.FName := VCacheItem.Name;
    AMarkRec.FDesc := VCacheItem.Desc;
    AMarkRec.FGeoType := VCacheItem.GeoType;
    AMarkRec.FGeoCount := VCacheItem.GeoCount;

    if FCache.FMarkViewCache.Find(AIndexRec.MarkId, VViewItem) then begin
      AMarkRec.FViewId := VViewItem.ViewId;
      AMarkRec.FVisible := VViewItem.Visible;
      if not AIncludeHiddenMarks then begin
        if not AMarkRec.FVisible then begin
          Exit;
        end;
      end;
    end;

    if AIncludeGeometry then begin
      if FCache.FMarkGeometryCache.Find(AIndexRec.MarkId, VGeometry) then begin
        AMarkRec.FGeometry := VGeometry;
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

  VCount := _FillPrepareMarkIdIndex(ACategoryId);
  if VCount > 0 then begin
    _FillPrepareMarkIdCache(ACategoryId);
    _FillPrepareMarkViewCache(ACategoryId);
    if AIncludeGeometry then begin
      _FillPrepareMarkGeometryCache(ACategoryId);
    end;

    SetLength(AMarkRecArray, VCount);

    if ACategoryId > 0 then begin
      if FCache.FMarkIdByCategoryIndex.Find(ACategoryId, VArray, VIdCount) then begin
        Assert(VCount >= VIdCount);
        for I := 0 to VIdCount - 1 do begin
          if FCache.FMarkIdIndex.Find(VArray[I], VIndexRec) then begin
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
      VRows := FCache.FMarkIdIndex.Rows;
      for I := 0 to FCache.FMarkIdIndex.Count - 1 do begin
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

function TMarkDbImplORMHelper._GetMarkRecArrayByRectSQL(
  const ACategoryIDArray: TIDDynArray;
  const ARect: TDoubleRect;
  const AIncludeHiddenMarks: Boolean;
  const ALonLatSize: TDoublePoint;
  out AMarkRecArray: TSQLMarkRecDynArray
): Integer;
var
  I: Integer;
  VLen: Integer;
  VMarkId: TID;
  VCategoryId: TID;
  VCategoris: TDynArray;
  VCategoryIDArray: TIDDynArray;
  VSQLSelect: RawUTF8;
  VList: TSQLTableJSON;
  VFilterByCategories: Boolean;
  VFilterByOneCategory: Boolean;
  VLonSize, VLatSize: Cardinal;
begin
  I := 0;

  VLen := Length(ACategoryIDArray);
  Assert(VLen > 0);

  LonLatSizeToInternalSize(ALonLatSize, VLonSize, VLatSize);

  VFilterByCategories := (VLen > 1);
  VFilterByOneCategory := (VLen = 1) and (ACategoryIDArray[0] > 0);

  if VFilterByOneCategory then begin
    VSQLSelect := FormatUTF8(
      'SELECT Mark.RowID FROM Mark,MarkRTree ' +
      'WHERE Mark.RowID=MarkRTree.RowID AND Mark.Category=? ' +
      'AND Left<=? AND Right>=? AND Bottom<=? AND Top>=? ' +
      'AND (Mark.GeoType=? OR Mark.GeoLonSize>=? OR Mark.GeoLatSize>=?);',
      [],
      [
        ACategoryIDArray[0],
        ARect.Right, ARect.Left, ARect.Top, ARect.Bottom,
        Integer(gtPoint), VLonSize, VLatSize
      ]
    );
  end else if VFilterByCategories then begin
    VSQLSelect := FormatUTF8(
      'SELECT Mark.RowID,Mark.Category FROM Mark,MarkRTree ' +
      'WHERE Mark.RowID=MarkRTree.RowID ' +
      'AND Left<=? AND Right>=? AND Bottom<=? AND Top>=? ' +
      'AND (Mark.GeoType=? OR Mark.GeoLonSize>=? OR Mark.GeoLatSize>=?);',
      [], [ARect.Right, ARect.Left, ARect.Top, ARect.Bottom, Integer(gtPoint), VLonSize, VLatSize]
    );
  end else begin
    VSQLSelect := FormatUTF8(
      'SELECT Mark.RowID FROM Mark,MarkRTree ' +
      'WHERE Mark.RowID=MarkRTree.RowID ' +
      'AND Left<=? AND Right>=? AND Bottom<=? AND Top>=? ' +
      'AND (Mark.GeoType=? OR Mark.GeoLonSize>=? OR Mark.GeoLatSize>=?);',
      [], [ARect.Right, ARect.Left, ARect.Top, ARect.Bottom, Integer(gtPoint), VLonSize, VLatSize]
    );
  end;

  VList := FClient.ExecuteList([TSQLMark, TSQLMarkRTree], VSQLSelect);
  if Assigned(VList) then
  try
    SetLength(AMarkRecArray, VList.RowCount);

    if VFilterByCategories then begin
      VCategoris.Init(TypeInfo(TIDDynArray), VCategoryIDArray);
      VCategoris.CopyFrom(ACategoryIDArray, VLen);
      VCategoris.Compare := SortDynArrayInt64;
      VCategoris.Sort;
    end;

    while VList.Step do begin
      VMarkId := VList.Field(0);

      if VFilterByCategories then begin
        VCategoryId := VList.Field(1);
        if VCategoris.Find(VCategoryId) = -1 then begin
          Continue;
        end;
      end;

      if VMarkId > 0 then begin
        if ReadMarkSQL(AMarkRecArray[I], VMarkId, 0, '') then begin
          if not AIncludeHiddenMarks then begin
            if not AMarkRecArray[I].FVisible then begin
              Continue;
            end;
          end;
          Inc(I);
        end;
      end;
    end;

    SetLength(AMarkRecArray, I);
  finally
    VList.Free;
  end;

  Result := I;
end;

function TMarkDbImplORMHelper._GetMarkRecArrayByRectMongoDB(
  const ACategoryIDArray: TIDDynArray;
  const ARect: TDoubleRect;
  const AIncludeHiddenMarks: Boolean;
  const ALonLatSize: TDoublePoint;
  out AMarkRecArray: TSQLMarkRecDynArray
): Integer;
begin
  Result := 0;
  //ToDo
end;

function TMarkDbImplORMHelper.GetMarkRecArrayByRect(
  const ACategoryIDArray: TIDDynArray;
  const ARect: TDoubleRect;
  const AIncludeHiddenMarks: Boolean;
  const ALonLatSize: TDoublePoint;
  out AMarkRecArray: TSQLMarkRecDynArray
): Integer;
begin
  Result := 0;
  case FClientType of
    ctSQLite3, ctZeosDBMS: begin
      Result :=
        _GetMarkRecArrayByRectSQL(
          ACategoryIDArray,
          ARect,
          AIncludeHiddenMarks,
          ALonLatSize,
          AMarkRecArray
        );
    end;
    ctMongoDB: begin
      Result :=
        _GetMarkRecArrayByRectMongoDB(
          ACategoryIDArray,
          ARect,
          AIncludeHiddenMarks,
          ALonLatSize,
          AMarkRecArray
        );
    end;
  else
    Assert(False);
  end;
end;

function TMarkDbImplORMHelper._GetMarkRecArrayByTextSQLite3(
  const ASearchText: string;
  const AMaxCount: Integer;
  const AIncludeHiddenMarks: Boolean;
  const ASearchInDescription: Boolean;
  out AIDArray: TIDDynArray
): Integer;
var
  I, J: Integer;
  VLen: Integer;
  VLimit: RawUTF8;
  VSearch: RawUTF8;
  VSQLWhere: RawUTF8;
  VNameIDArray: TIDDynArray;
  VDescIDArray: TIDDynArray;
  VSkipDescSearch: Boolean;
begin
  if AMaxCount > 0 then begin
    VLimit := StringToUTF8(' LIMIT ' + IntToStr(AMaxCount));
  end else begin
    VLimit := '';
  end;

  VSearch := StringToUTF8('''' + SysUtils.AnsiLowerCase(ASearchText) + '''');

  VSQLWhere := RawUTF8('Name MATCH ') + VSearch + VLimit;
  if FClient.FTSMatch(TSQLMarkFTS, VSQLWhere, VNameIDArray) then begin
    VSkipDescSearch := (AMaxCount > 0) and (Length(VNameIDArray) >= AMaxCount);
  end else begin
    VSkipDescSearch := False;
  end;

  if ASearchInDescription and not VSkipDescSearch then begin
    VSQLWhere := RawUTF8('Desc MATCH ') + VSearch + VLimit;
    FClient.FTSMatch(TSQLMarkFTS, VSQLWhere, VDescIDArray);
  end;

  I := Length(VNameIDArray);
  J := Length(VDescIDArray);

  if (I > 0) and (J > 0) then begin
    VLen := I + J;
    SetLength(AIDArray, VLen);
    Move(VNameIDArray[0], AIDArray[0], I);
    Move(VDescIDArray[0], AIDArray[I], J);
    VLen := MergeSortRemoveDuplicates(AIDArray);
    SetLength(AIDArray, VLen);
  end else if I > 0 then begin
    AIDArray := VNameIDArray;
  end else if J > 0 then begin
    AIDArray := VDescIDArray;
  end;

  if (AMaxCount > 0) and (Length(AIDArray) > AMaxCount) then begin
    SetLength(AIDArray, AMaxCount);
  end;

  Result := Length(AIDArray);
end;

function TMarkDbImplORMHelper._GetMarkRecArrayByTextSQL(
  const ASearchText: string;
  const AMaxCount: Integer;
  const AIncludeHiddenMarks: Boolean;
  const ASearchInDescription: Boolean;
  out AIDArray: TIDDynArray
): Integer;
var
  I: Integer;
  VLimit: RawUTF8;
  VSearch: RawUTF8;
  VSQLSelect: RawUTF8;
  VList: TSQLTableJSON;
begin
  Result := 0;
  VLimit := '';

  VSearch := StringToUTF8('''' + SysUtils.AnsiLowerCase(ASearchText) + '''');

  if AMaxCount > 0 then begin
    VLimit := StringToUTF8(' LIMIT ' + IntToStr(AMaxCount));
  end;

  VSQLSelect := RawUTF8('SELECT RowID WHERE Name or Desc LIKE ') + VSearch + VLimit;

  VList := FClient.ExecuteList([TSQLMarkFTS], VSQLSelect);
  if Assigned(VList) then
  try
    I := 0;
    SetLength(AIDArray, VList.RowCount);
    while VList.Step do begin
      AIDArray[I] := VList.Field(0);
      Inc(I);
    end;
    SetLength(AIDArray, I);
    Result := I;
  finally
    VList.Free;
  end;
end;

function TMarkDbImplORMHelper.GetMarkRecArrayByText(
  const ASearchText: string;
  const AMaxCount: Integer;
  const AIncludeHiddenMarks: Boolean;
  const ASearchInDescription: Boolean;
  out AMarkRecArray: TSQLMarkRecDynArray
): Integer;
var
  I, J: Integer;
  VIDArray: TIDDynArray;
begin
  Result := 0;
  SetLength(VIDArray, 0);

  case FClientType of
    ctSQLite3: begin
      // db specific text search (by FTS index)
      Result :=
        _GetMarkRecArrayByTextSQLite3(
          ASearchText,
          AMaxCount,
          AIncludeHiddenMarks,
          ASearchInDescription,
          VIDArray
        );
    end;
    ctMongoDB, ctZeosDBMS: begin
      Result :=
        _GetMarkRecArrayByTextSQL(
          ASearchText,
          AMaxCount,
          AIncludeHiddenMarks,
          ASearchInDescription,
          VIDArray
        );
    end;
  else
    Assert(False);
  end;
  if Result > 0 then begin
    Assert(Length(VIDArray) >= Result);
    J := 0;
    SetLength(AMarkRecArray, Result);
    for I := 0 to Result - 1 do begin
      if VIDArray[I] > 0 then begin
        if ReadMarkSQL(AMarkRecArray[J], VIDArray[I], 0, '') then begin
          if not AIncludeHiddenMarks then begin
            if not AMarkRecArray[J].FVisible then begin
              Continue;
            end;
          end;
          Inc(J);
        end;
      end;
    end;
    SetLength(AMarkRecArray, J);
    Result := J;
  end;
end;

end.
