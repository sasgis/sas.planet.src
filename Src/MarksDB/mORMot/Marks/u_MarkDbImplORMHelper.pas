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

unit u_MarkDbImplORMHelper;

interface

{$I ..\MarkSystemORM.inc}

uses
  Types,
  Windows,
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.json,
  mormot.core.variants,
  mormot.core.os,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.server,
  mormot.orm.mongodb,
  mormot.db.nosql.bson,
  mormot.db.nosql.mongodb,
  {$IFDEF ENABLE_DBMS}
  mormot.db.sql,
  {$ENDIF}
  mormot.db.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.core.rtti,
  t_GeoTypes,
  t_MarkSystemORM,
  i_GeometryLonLat,
  i_GeometryToStream,
  i_GeometryFromStream,
  i_DoublePointsMeta,
  i_MarkSystemImplORMClientProvider,
  u_MarkSystemORMModel,
  u_MarkDbImplORMCache;

type
  TMarkDbImplORMHelper = class
  private
    type
      TMarkWithCategoryID = record
        MarkID: TID;
        CategoryID: TID;
      end;
      TMarkWithCategoryIDDynArray = array of TMarkWithCategoryID;
  private
    FUserID: TID;
    FRestClient: TRestClientDB;
    FClient: TRestOrm;
    FServer: TRestOrmServer;
    FCache: TOrmMarkDbCache;
    FIsReadOnly: Boolean;
    FTransaction: IMarkSystemORMTransaction;
    FGeometryReader: IGeometryFromStream;
    FGeometryMetaReader: IGeometryMetaFromStream;
    FGeometryPointsWriter: IGeometryPointsToStream;
    FGeometryMetaWriter: IGeometryMetaToStream;
    FClientType: TMarkSystemImplORMClientType;
    FClientProvider: IMarkSystemImplORMClientProvider;
    FOrmMarkClass: TOrmMarkClass;
    FOrmMarkName: RawUtf8;
  private
    function _GeomertryFromBlob(const ABlob: RawBlob; const AMeta: IDoublePointsMeta): IGeometryLonLat;
    function _GeomertryMetaFromBlob(const ABlob: RawBlob): IDoublePointsMeta;
    function _GeomertryPointsToBlob(const AGeometry: IGeometryLonLat): RawBlob;
    function _GeomertryMetaToBlob(const AGeometry: IGeometryLonLat): RawBlob;
    function _AddMarkImage(const APicName: string): TID;
    procedure _ReadMarkImage(var AMarkRec: TOrmMarkRec);
    function _AddMarkAppearance(
      const AColor1, AColor2: Cardinal;
      const AScale1, AScale2: Integer
    ): TID;
    procedure _ReadMarkAppearance(var AMarkRec: TOrmMarkRec);
    function _FillPrepareMarkIdIndex(const ACategoryID: TID): Integer;
    procedure _FillPrepareMarkIdCache(const ACategoryID: TID);
    procedure _FillPrepareMarkGeometryCache(const ACategoryID: TID);
    procedure _FillPrepareMarkViewCache(const ACategoryID: TID);
    function _GetMarkIDArrayByRectSQL(
      const ACategoryIDArray: TIDDynArray;
      const ARect: TDoubleRect;
      const ALonSize: Cardinal;
      const ALatSize: Cardinal;
      const AReciveCategoryID: Boolean;
      out AIDArray: TMarkWithCategoryIDDynArray
    ): Integer;
    {$IFDEF ENABLE_DBMS}
    function _GetMarkIDArrayByRectDBMS(
      const ACategoryIDArray: TIDDynArray;
      const ARect: TDoubleRect;
      const ALonSize: Cardinal;
      const ALatSize: Cardinal;
      const AReciveCategoryID: Boolean;
      out AIDArray: TMarkWithCategoryIDDynArray
    ): Integer;
    {$ENDIF}
    function _GetMarkIDArrayByRectMongoDB(
      const ACategoryIDArray: TIDDynArray;
      const ARect: TDoubleRect;
      const ALonSize: Cardinal;
      const ALatSize: Cardinal;
      const AReciveCategoryID: Boolean;
      out AIDArray: TMarkWithCategoryIDDynArray
    ): Integer;
    function _GetMarkRecArrayByTextSQLite3(
      const ASearch: RawUtf8;
      const AMaxCount: Integer;
      const ASearchInDescription: Boolean;
      out ANameIDArray: TIDDynArray;
      out ADescIDArray: TIDDynArray
    ): Integer;
    function _GetMarkRecArrayByTextSQL(
      const ASearch: RawUtf8;
      const AMaxCount: Integer;
      const ASearchInDescription: Boolean;
      out ANameIDArray: TIDDynArray;
      out ADescIDArray: TIDDynArray
    ): Integer;
    procedure SetReadOnly(const AValue: Boolean);
  public
    function DeleteMarkSQL(
      const AMarkID: TID;
      const AUseTransactions: Boolean
    ): Boolean; overload;
    function DeleteMarkSQL(
      const AMarkIDs: TIDDynArray
    ): Boolean; overload;
    function InsertMarkSQL(
      var AMarkRec: TOrmMarkRec;
      const AUseTransactions: Boolean
    ): Boolean;
    function UpdateMarkSQL(
      const AOldMarkRec: TOrmMarkRec;
      var ANewMarkRec: TOrmMarkRec
    ): Boolean;
    function ReadMarkSQL(
      out AMarkRec: TOrmMarkRec;
      const AMarkID: TID;
      const ACategoryID: TID;
      const AMarkName: string
    ): Boolean;
    function UpdateMarkView(
      const AMarkID: TID;
      const ACategoryID: TID;
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
      const AIncludeAppearance: Boolean;
      out AMarkRecArray: TOrmMarkRecDynArray
    ): Integer;
    function GetMarkRecArrayByRect(
      const ACategoryIDArray: TDynArray;
      const ARect: TDoubleRect;
      const AIncludeHiddenMarks: Boolean;
      const ALonLatSize: TDoublePoint;
      out AMarkRecArray: TOrmMarkRecDynArray
    ): Integer;
    function GetMarkRecArrayByText(
      const ASearchText: string;
      const AMaxCount: Integer;
      const AIncludeHiddenMarks: Boolean;
      const ASearchInDescription: Boolean;
      out AMarkRecArray: TOrmMarkRecDynArray
    ): Integer;
  public
    constructor Create(
      const AIsReadOnly: Boolean;
      const ACacheSizeMb: Cardinal;
      const AGeometryReader: IGeometryFromStream;
      const AGeometryMetaReader: IGeometryMetaFromStream;
      const AGeometryPointsWriter: IGeometryPointsToStream;
      const AGeometryMetaWriter: IGeometryMetaToStream;
      const AClientProvider: IMarkSystemImplORMClientProvider
    );
    destructor Destroy; override;
  public
    property IsReadOnly: Boolean read FIsReadOnly write SetReadOnly;
  end;

implementation

uses
  Math,
  u_MarkSystemORMTools;

const
  cOrmMarkTableClass: array[TMarkSystemImplORMClientType] of TOrmMarkClass = (
    TOrmMark, TOrmMarkMongoDB, TOrmMarkDBMS, TOrmMarkDBMS);

  cOrmMarkTableName: array[TMarkSystemImplORMClientType] of RawUtf8 = (
    'Mark', 'MarkMongoDB', 'MarkDBMS', 'MarkDBMS');

{ TMarkDbImplORMHelper }

constructor TMarkDbImplORMHelper.Create(
  const AIsReadOnly: Boolean;
  const ACacheSizeMb: Cardinal;
  const AGeometryReader: IGeometryFromStream;
  const AGeometryMetaReader: IGeometryMetaFromStream;
  const AGeometryPointsWriter: IGeometryPointsToStream;
  const AGeometryMetaWriter: IGeometryMetaToStream;
  const AClientProvider: IMarkSystemImplORMClientProvider
);
begin
  inherited Create;

  FIsReadOnly := AIsReadOnly;
  FCache.Init(Int64(ACacheSizeMb) * 1024 * 1024);

  FGeometryReader := AGeometryReader;
  FGeometryMetaReader := AGeometryMetaReader;
  FGeometryPointsWriter := AGeometryPointsWriter;
  FGeometryMetaWriter := AGeometryMetaWriter;

  FClientProvider := AClientProvider;
  FUserID := FClientProvider.UserID;
  FRestClient := FClientProvider.RestClient;
  FClient := FRestClient.OrmInstance;
  FServer := FRestClient.Server.OrmInstance as TRestOrmServer;
  FClientType := FClientProvider.RestClientType;
  FTransaction := FClientProvider.Transaction;
  FOrmMarkClass := cOrmMarkTableClass[FClientType];
  FOrmMarkName := cOrmMarkTableName[FClientType];
end;

destructor TMarkDbImplORMHelper.Destroy;
begin
  FCache.Done;
  inherited Destroy;
end;

function TMarkDbImplORMHelper._GeomertryFromBlob(
  const ABlob: RawBlob;
  const AMeta: IDoublePointsMeta
): IGeometryLonLat;
var
  VStream: TRawByteStringStream;
begin
  Assert(ABlob <> '');
  VStream := TRawByteStringStream.Create(ABlob);
  try
    Result := FGeometryReader.Parse(VStream, AMeta);
  finally
    VStream.Free;
  end;
end;

function TMarkDbImplORMHelper._GeomertryMetaFromBlob(
  const ABlob: RawBlob
): IDoublePointsMeta;
var
  VStream: TRawByteStringStream;
begin
  Result := nil;
  if ABlob = '' then begin
    Exit;
  end;
  VStream := TRawByteStringStream.Create(ABlob);
  try
    Result := FGeometryMetaReader.Parse(VStream);
  finally
    VStream.Free;
  end;
end;

function TMarkDbImplORMHelper._GeomertryPointsToBlob(
  const AGeometry: IGeometryLonLat
): RawBlob;
var
  VStream: TRawByteStringStream;
begin
  Assert(AGeometry <> nil);
  VStream := TRawByteStringStream.Create;
  try
    FGeometryPointsWriter.Save(AGeometry, VStream);
    Result := VStream.DataString;
  finally
    VStream.Free;
  end;
end;

function TMarkDbImplORMHelper._GeomertryMetaToBlob(
  const AGeometry: IGeometryLonLat
): RawBlob;
var
  VStream: TRawByteStringStream;
begin
  Assert(AGeometry <> nil);
  VStream := TRawByteStringStream.Create;
  try
    FGeometryMetaWriter.Save(AGeometry, VStream);
    Result := VStream.DataString;
  finally
    VStream.Free;
  end;
end;

function TMarkDbImplORMHelper._AddMarkImage(const APicName: string): TID;
var
  VPicName: RawUtf8;
  VItem: POrmMarkImageRow;
  VOrmMarkImage: TOrmMarkImage;
begin
  {$IF CompilerVersion < 33}
  Result := 0; // prevent compiler warning
  {$IFEND}
  if FCache.FMarkImageCache.Find(APicName, VItem) then begin
    // found in cache
    Result := VItem.ImageId;
  end else begin
    VPicName := StringToUtf8(APicName);
    VOrmMarkImage := TOrmMarkImage.Create(FClient, 'miName=?', [VPicName]);
    try
      if VOrmMarkImage.ID = 0 then begin
        VOrmMarkImage.FName := VPicName;
        // add to db
        CheckID( FClient.Add(VOrmMarkImage, True) );
      end;
      Result := VOrmMarkImage.ID;
      // add to cache
      FCache.FMarkImageCache.AddOrIgnore(Result, APicName);
    finally
      VOrmMarkImage.Free;
    end;
  end;
end;

procedure TMarkDbImplORMHelper._ReadMarkImage(var AMarkRec: TOrmMarkRec);
var
  VItem: POrmMarkImageRow;
  VOrmMarkImage: TOrmMarkImage;
begin
  if AMarkRec.FGeoType = gtPoint then begin
    if AMarkRec.FPicId > 0 then begin
      if FCache.FMarkImageCache.Find(AMarkRec.FPicId, VItem) then begin
        // found in cache
        AMarkRec.FPicName := VItem.Name;
      end else begin
        // read from db
        VOrmMarkImage := TOrmMarkImage.Create(FClient, AMarkRec.FPicId);
        try
          CheckID(VOrmMarkImage.ID);
          AMarkRec.FPicName := UTF8ToString(VOrmMarkImage.FName);
          // add to cache
          FCache.FMarkImageCache.AddOrIgnore(AMarkRec);
        finally
          VOrmMarkImage.Free;
        end;
      end;
    end else begin
      AMarkRec.FPicName := '';
    end;
  end;
end;

function TMarkDbImplORMHelper._AddMarkAppearance(
  const AColor1, AColor2: Cardinal;
  const AScale1, AScale2: Integer
): TID;
var
  VItem: POrmMarkAppearanceRow;
  VOrmMarkAppearance: TOrmMarkAppearance;
begin
  {$IF CompilerVersion < 33}
  Result := 0; // prevent compiler warning
  {$IFEND}
  if FCache.FMarkAppearanceCache.Find(AColor1, AColor2, AScale1, AScale2, VItem) then begin
    // found in cache
    Result := VItem.AppearanceId;
  end else begin
    VOrmMarkAppearance := TOrmMarkAppearance.Create(
      FClient, 'maColor1=? AND maColor2=? AND maScale1=? AND maScale2=?',
      [Int64(AColor1), Int64(AColor2), AScale1, AScale2]
    );
    try
      if VOrmMarkAppearance.ID = 0 then begin
        VOrmMarkAppearance.FColor1 := AColor1;
        VOrmMarkAppearance.FColor2 := AColor2;
        VOrmMarkAppearance.FScale1 := AScale1;
        VOrmMarkAppearance.FScale2 := AScale2;
        // add to db
        CheckID( FClient.Add(VOrmMarkAppearance, True) );
      end;
      Result := VOrmMarkAppearance.ID;
      // add to cache
      FCache.FMarkAppearanceCache.AddOrIgnore(Result, AColor1, AColor2, AScale1, AScale2);
    finally
      VOrmMarkAppearance.Free;
    end;
  end;
end;

procedure TMarkDbImplORMHelper._ReadMarkAppearance(var AMarkRec: TOrmMarkRec);
var
  VItem: POrmMarkAppearanceRow;
  VOrmMarkAppearance: TOrmMarkAppearance;
begin
  if FCache.FMarkAppearanceCache.Find(AMarkRec.FAppearanceId, VItem) then begin
    // found in cache
    AMarkRec.FColor1 := VItem.Color1;
    AMarkRec.FColor2 := VItem.Color2;
    AMarkRec.FScale1 := VItem.Scale1;
    AMarkRec.FScale2 := VItem.Scale2;
  end else begin
    // read from db
    VOrmMarkAppearance := TOrmMarkAppearance.Create(FClient, AMarkRec.FAppearanceId);
    try
      CheckID(VOrmMarkAppearance.ID);
      AMarkRec.FColor1 := VOrmMarkAppearance.FColor1;
      AMarkRec.FColor2 := VOrmMarkAppearance.FColor2;
      AMarkRec.FScale1 := VOrmMarkAppearance.FScale1;
      AMarkRec.FScale2 := VOrmMarkAppearance.FScale2;
      // add to cache
      FCache.FMarkAppearanceCache.AddOrIgnore(AMarkRec);
    finally
      VOrmMarkAppearance.Free;
    end;
  end;
end;

function TMarkDbImplORMHelper.DeleteMarkSQL(
  const AMarkID: TID;
  const AUseTransactions: Boolean
): Boolean;
var
  VTransaction: TTransactionRec;
  VIndex: POrmMarkIdIndexRec;
begin
  Result := False;

  if FIsReadOnly then begin
    Exit;
  end;

  if AMarkID <= 0 then begin
    Assert(False);
    Exit;
  end;

  // delete from cache
  FCache.FMarkCache.Delete(AMarkID);
  FCache.FMarkGeometryCache.Delete(AMarkID);
  FCache.FMarkViewCache.Delete(AMarkID);
  if FCache.FMarkIdIndex.Find(AMarkID, VIndex) then begin
    FCache.FMarkIdIndex.Delete(AMarkID);
    FCache.FMarkIdByCategoryIndex.Delete(VIndex.CategoryId, AMarkID);
  end;

  // delete from db
  if AUseTransactions then begin
    VTransaction := FTransaction.Start(FOrmMarkClass, FIsReadOnly);
  end;
  try
    // delete view for all Users if exists
    FClient.Delete(TOrmMarkView, FormatSql('mvMark=?', [], [AMarkID]));

    // delete rect
    if FClientType = ctSQLite3 then begin
      CheckDeleteResult( FClient.Delete(TOrmMarkRTree, AMarkID) );
    end;

    // delete name and desc
    CheckDeleteResult( FClient.Delete(TOrmMarkFTS, AMarkID) );

    // delete meta if exists
    FClient.Delete(TOrmMarkMeta, FormatSql('mMark=?', [], [AMarkID]));

    // delete mark
    CheckDeleteResult( FClient.Delete(FOrmMarkClass, AMarkID) );

    // pic name and appearance are never deleted...

    if AUseTransactions then begin
      FTransaction.Commit(VTransaction);
    end;
  except
    if AUseTransactions then begin
      FTransaction.RollBack(VTransaction);
    end;
    raise;
  end;

  Result := True;
end;

function TMarkDbImplORMHelper.DeleteMarkSQL(
  const AMarkIDs: TIDDynArray
): Boolean;
const
  cMongoDbBatchSize = 5 * 1000;
var
  I: Integer;
  VCount, VLen: Integer;
  VIds: RawUtf8;
  VTransaction: TTransactionRec;
  VMarkCol, VMarkMetaCol, VMarkViewCol, VMarkFtsCol: TMongoCollection;
begin
  Result := False;

  if FIsReadOnly then begin
    Exit;
  end;

  VLen := Length(AMarkIDs);
  if VLen = 0 then begin
    Result := True;
    Exit;
  end;

  // delete from cache
  FCache.FMarkCache.Reset;
  FCache.FMarkGeometryCache.Reset;
  FCache.FMarkViewCache.Reset;
  FCache.FMarkIdIndex.Reset;
  FCache.FMarkIdByCategoryIndex.Reset;

  // delete from db
  VTransaction := FTransaction.Start(FOrmMarkClass, FIsReadOnly);
  try
    if FClientType <> ctMongoDB then begin
      // SQLite3, DBMS
      I := 0;
      while I < VLen do begin
        VCount := Min(MAX_SQLPARAMS, VLen - I);
        VIds := Int64DynArrayToCSV(@AMarkIDs[I], VCount, '(', ');');
        Inc(I, VCount);

        if FClientType = ctSQLite3 then begin
          Result := FClient.Execute('DELETE FROM MarkRTree WHERE RowID IN ' + VIds);
        end else begin
          Result := True; // nothing to do (data embeded into "FOrmMarkClass" table)
        end;

        Result :=
          Result and
          FClient.Execute('DELETE FROM MarkFTS WHERE RowID IN ' + VIds) and
          FClient.Execute('DELETE FROM MarkMeta WHERE mMark IN ' + VIds) and
          FClient.Execute('DELETE FROM MarkView WHERE mvMark IN ' + VIds) and
          FClient.Execute('DELETE FROM ' + FOrmMarkName + ' WHERE RowID IN ' + VIds);

        CheckDeleteResult(Result);

        if FClientType <> ctSQLite3 then begin
          FTransaction.Commit(VTransaction);
          VTransaction := FTransaction.Start(FOrmMarkClass, FIsReadOnly);
        end;
      end;
    end else begin // MongoDB
      VMarkCol     := (FServer.GetStaticStorage(FOrmMarkClass) as TRestStorageMongoDB).Collection;
      VMarkFtsCol  := (FServer.GetStaticStorage(TOrmMarkFTS)   as TRestStorageMongoDB).Collection;
      VMarkMetaCol := (FServer.GetStaticStorage(TOrmMarkMeta)  as TRestStorageMongoDB).Collection;
      VMarkViewCol := (FServer.GetStaticStorage(TOrmMarkView)  as TRestStorageMongoDB).Collection;

      I := 0;
      while I < VLen do begin
        VCount := Min(cMongoDbBatchSize, VLen - I);

        // delete view
        VIds := Int64DynArrayToCSV(@AMarkIDs[I], VCount, '{mvMark:{$in:[', ']}}');
        VMarkViewCol.RemoveFmt(VIds, []);

        // delete meta
        VIds := Int64DynArrayToCSV(@AMarkIDs[I], VCount, '{mMark:{$in:[', ']}}');
        VMarkMetaCol.RemoveFmt(VIds, []);

        // delete mark, name and desc
        VIds := Int64DynArrayToCSV(@AMarkIDs[I], VCount, '{_id:{$in:[', ']}}');

        VMarkFTSCol.RemoveFmt(VIds, []);
        VMarkCol.RemoveFmt(VIds, []);

        Inc(I, VCount);
      end;
      Result := True;
    end;
    FTransaction.Commit(VTransaction);
  except
    FTransaction.RollBack(VTransaction);
    raise;
  end;
end;

function TMarkDbImplORMHelper.InsertMarkSQL(
  var AMarkRec: TOrmMarkRec;
  const AUseTransactions: Boolean
): Boolean;
var
  VRect: TDoubleRect;
  VIntRect: TRect;
  VGeometryPointsBlob: RawBlob;
  VGeometryMetaBlob: RawBlob;
  VOrmMark: TOrmMark;
  VOrmMarkMeta: TOrmMarkMeta;
  VOrmMarkDBMS: TOrmMarkDBMS;
  VOrmMarkView: TOrmMarkView;
  VOrmMarkFTS: TOrmMarkFTS;
  VOrmMarkRTree: TOrmMarkRTree;
  VTransaction: TTransactionRec;
begin
  Result := False;

  if FIsReadOnly then begin
    Exit;
  end;

  AMarkRec.FMarkId := 0;

  VRect := AMarkRec.FGeometry.Bounds.Rect;
  VGeometryPointsBlob := _GeomertryPointsToBlob(AMarkRec.FGeometry);
  VGeometryMetaBlob := _GeomertryMetaToBlob(AMarkRec.FGeometry);
  CalcGeometrySize(VRect, AMarkRec.FGeoLonSize, AMarkRec.FGeoLatSize);

  if AUseTransactions then begin
    VTransaction := FTransaction.Start(FOrmMarkClass, FIsReadOnly);
  end;
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

    VOrmMark := FOrmMarkClass.Create;
    try
      VOrmMark.FCategory := AMarkRec.FCategoryId;
      VOrmMark.FImage := AMarkRec.FPicId;
      VOrmMark.FAppearance := AMarkRec.FAppearanceId;

      VOrmMark.FName := StringToUtf8(AMarkRec.FName);
      VOrmMark.FDesc := StringToUtf8(AMarkRec.FDesc);

      VOrmMark.FGeoLonSize := AMarkRec.FGeoLonSize;
      VOrmMark.FGeoLatSize := AMarkRec.FGeoLatSize;
      VOrmMark.FGeoType := AMarkRec.FGeoType;
      VOrmMark.FGeoCount := AMarkRec.FGeoCount;

      if FClientType in [ctMongoDB, ctZDBC, ctODBC] then begin
        LonLatDoubleRectToRect(VRect, VIntRect);
        VOrmMarkDBMS := VOrmMark as TOrmMarkDBMS;
        VOrmMarkDBMS.FLeft := VIntRect.Left;
        VOrmMarkDBMS.FRight := VIntRect.Right;
        VOrmMarkDBMS.FTop := VIntRect.Top;
        VOrmMarkDBMS.FBottom := VIntRect.Bottom;
      end;

      // add mark to db
      CheckID( FClient.Add(VOrmMark, True) );
      AMarkRec.FMarkId := VOrmMark.ID;
      // add geometry points blob to db
      CheckUpdateResult( FClient.UpdateBlob(FOrmMarkClass, AMarkRec.FMarkId, 'mGeoWKB', VGeometryPointsBlob) );
      // add to cache
      FCache.FMarkCache.AddOrUpdate(AMarkRec);
      FCache.FMarkGeometryCache.AddOrUpdate(
        AMarkRec.FMarkId,
        Length(VGeometryPointsBlob) + Length(VGeometryMetaBlob),
        AMarkRec.FGeometry
      );
      FCache.FMarkIdIndex.AddOrUpdate(AMarkRec);
      FCache.FMarkIdByCategoryIndex.Add(AMarkRec.FCategoryId, AMarkRec.FMarkId);
    finally
      VOrmMark.Free;
    end;

    VOrmMarkFTS := TOrmMarkFTS.Create;
    try
      VOrmMarkFTS.DocID := AMarkRec.FMarkId;
      VOrmMarkFTS.FName := StringToUtf8(SysUtils.AnsiLowerCase(AMarkRec.FName));
      VOrmMarkFTS.FDesc := StringToUtf8(SysUtils.AnsiLowerCase(AMarkRec.FDesc));
      // add name and desc to db (fts index)
      CheckID( FClient.Add(VOrmMarkFTS, True, True) );
      Assert(VOrmMarkFTS.ID = AMarkRec.FMarkId);
    finally
      VOrmMarkFTS.Free;
    end;

    if FClientType = ctSQLite3 then begin
      VOrmMarkRTree := TOrmMarkRTree.Create;
      try
        VOrmMarkRTree.IDValue := AMarkRec.FMarkId;
        VOrmMarkRTree.FLeft := VRect.Left;
        VOrmMarkRTree.FRight := VRect.Right;
        VOrmMarkRTree.FTop := VRect.Top;
        VOrmMarkRTree.FBottom := VRect.Bottom;
        // add rect to db (rtree index)
        CheckID( FClient.Add(VOrmMarkRTree, True, True) );
        Assert(VOrmMarkRTree.ID = AMarkRec.FMarkId);
      finally
        VOrmMarkRTree.Free;
      end;
    end;

    if not AMarkRec.FVisible then begin
      VOrmMarkView := TOrmMarkView.Create;
      try
        VOrmMarkView.FUser := FUserID;
        VOrmMarkView.FMark := AMarkRec.FMarkId;
        VOrmMarkView.FCategory := AMarkRec.FCategoryId;
        VOrmMarkView.FVisible := AMarkRec.FVisible;
        // add view to db
        CheckID( FClient.Add(VOrmMarkView, True) );
        AMarkRec.FViewId := VOrmMarkView.ID;
      finally
        VOrmMarkView.Free;
      end;
    end;
    // add view to cache
    FCache.FMarkViewCache.AddOrUpdate(AMarkRec);

    // add geometry meta blob to db
    if Length(VGeometryMetaBlob) > 0 then begin
      VOrmMarkMeta := TOrmMarkMeta.Create;
      try
        VOrmMarkMeta.FMark := AMarkRec.FMarkId;
        VOrmMarkMeta.FMeta := VGeometryMetaBlob;
        // add
        CheckID( FClient.Add(VOrmMarkMeta, True) );
        CheckUpdateResult( FClient.UpdateBlob(TOrmMarkMeta, VOrmMarkMeta.ID, 'mMeta', VGeometryMetaBlob) );
      finally
        VOrmMarkMeta.Free;
      end;
    end;

    if AUseTransactions then begin
      FTransaction.Commit(VTransaction);
    end;
  except
    if AUseTransactions then begin
      FTransaction.RollBack(VTransaction);
    end;
    raise;
  end;

  Result := (AMarkRec.FMarkId > 0);
end;

function TMarkDbImplORMHelper.UpdateMarkSQL(
  const AOldMarkRec: TOrmMarkRec;
  var ANewMarkRec: TOrmMarkRec
): Boolean;
var
  VRect: TDoubleRect;
  VIntRect: TRect;
  VGeometryPointsBlob: RawBlob;
  VGeometryMetaBlob: RawBlob;
  VOrmMark: TOrmMark;
  VOrmMarkMeta: TOrmMarkMeta;
  VOrmMarkDBMS: TOrmMarkDBMS;
  VOrmMarkFTS: TOrmMarkFTS;
  VOrmMarkRTree: TOrmMarkRTree;
  VTransaction: TTransactionRec;
  VUpdatePic: Boolean;
  VUpdateGeo: Boolean;
  VUpdateName: Boolean;
  VUpdateDesc: Boolean;
  VUpdateCategory: Boolean;
  VUpdateAppearance: Boolean;
  VUpdateIdIndex: Boolean;
  VFieldsBuilder: TCSVFieldsBuilder;
begin
  Result := False;

  CheckID(AOldMarkRec.FMarkId);

  ANewMarkRec.FMarkId := AOldMarkRec.FMarkId;

  if FIsReadOnly then begin
    if AOldMarkRec.FVisible <> ANewMarkRec.FVisible then begin
      // update view
      Result := UpdateMarkView(ANewMarkRec.FMarkId, ANewMarkRec.FCategoryId, ANewMarkRec.FVisible, False);
    end;
    Exit;
  end;

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
    VGeometryPointsBlob := _GeomertryPointsToBlob(ANewMarkRec.FGeometry);
    VGeometryMetaBlob := _GeomertryMetaToBlob(ANewMarkRec.FGeometry);
    CalcGeometrySize(VRect, ANewMarkRec.FGeoLonSize, ANewMarkRec.FGeoLatSize);
  end;

  VTransaction := FTransaction.Start(FOrmMarkClass, FIsReadOnly);
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

    VOrmMark := FOrmMarkClass.Create;
    try
      VFieldsBuilder.Clear;

      VOrmMark.IDValue := ANewMarkRec.FMarkId;

      if VUpdateCategory then begin
        VFieldsBuilder.Add('mCategory');
        VOrmMark.FCategory := ANewMarkRec.FCategoryId;
      end;
      if VUpdatePic then begin
        VFieldsBuilder.Add('mImage');
        VOrmMark.FImage := ANewMarkRec.FPicId;
      end;
      if VUpdateAppearance then begin
        VFieldsBuilder.Add('mAppearance');
        VOrmMark.FAppearance := ANewMarkRec.FAppearanceId;
      end;
      if VUpdateName then begin
        VFieldsBuilder.Add('mName');
        VOrmMark.FName := StringToUtf8(ANewMarkRec.FName);
      end;
      if VUpdateDesc then begin
        VFieldsBuilder.Add('mDesc');
        VOrmMark.FDesc := StringToUtf8(ANewMarkRec.FDesc);
      end;

      if VUpdateGeo then begin
        VFieldsBuilder.Add('mGeoLonSize');
        VFieldsBuilder.Add('mGeoLatSize');
        VFieldsBuilder.Add('mGeoType');
        VFieldsBuilder.Add('mGeoCount');
        VOrmMark.FGeoLonSize := ANewMarkRec.FGeoLonSize;
        VOrmMark.FGeoLatSize := ANewMarkRec.FGeoLatSize;
        VOrmMark.FGeoType := ANewMarkRec.FGeoType;
        VOrmMark.FGeoCount := ANewMarkRec.FGeoCount;
        if FClientType in [ctMongoDB, ctZDBC, ctODBC] then begin
          VFieldsBuilder.Add('mLeft');
          VFieldsBuilder.Add('mRight');
          VFieldsBuilder.Add('mTop');
          VFieldsBuilder.Add('mBottom');
          LonLatDoubleRectToRect(VRect, VIntRect);
          VOrmMarkDBMS := VOrmMark as TOrmMarkDBMS;
          VOrmMarkDBMS.FLeft := VIntRect.Left;
          VOrmMarkDBMS.FRight := VIntRect.Right;
          VOrmMarkDBMS.FTop := VIntRect.Top;
          VOrmMarkDBMS.FBottom := VIntRect.Bottom;
        end;
      end;

      if VFieldsBuilder.Count > 0 then begin
        // update mark
        CheckUpdateResult( FClient.Update(VOrmMark, VFieldsBuilder.Build) );
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
        Result := True;
      end;

      if VUpdateGeo then begin
        // update geometry blob
        CheckUpdateResult(
          FClient.UpdateBlob(FOrmMarkClass, VOrmMark.ID, 'mGeoWKB', VGeometryPointsBlob)
        );
        // update cache
        FCache.FMarkGeometryCache.AddOrUpdate(
          ANewMarkRec.FMarkId,
          Length(VGeometryPointsBlob) + Length(VGeometryMetaBlob),
          ANewMarkRec.FGeometry
        );
        Result := True;
      end;
    finally
      VOrmMark.Free;
    end;

    VOrmMarkFTS := TOrmMarkFTS.Create;
    try
      VFieldsBuilder.Clear;

      VOrmMarkFTS.DocID := ANewMarkRec.FMarkId;

      if VUpdateName then begin
        VFieldsBuilder.Add('mName');
        VOrmMarkFTS.FName := StringToUtf8(SysUtils.AnsiLowerCase(ANewMarkRec.FName));
      end;
      if VUpdateDesc then begin
        VFieldsBuilder.Add('mDesc');
        VOrmMarkFTS.FDesc := StringToUtf8(SysUtils.AnsiLowerCase(ANewMarkRec.FDesc));
      end;

      if VFieldsBuilder.Count > 0 then begin
        // update name / desc (fts index)
        CheckUpdateResult( FClient.Update(VOrmMarkFTS, VFieldsBuilder.Build) );
      end;
    finally
      VOrmMarkFTS.Free;
    end;

    if VUpdateGeo and (FClientType = ctSQLite3) then begin
      VOrmMarkRTree := TOrmMarkRTree.Create;
      try
        VOrmMarkRTree.IDValue := ANewMarkRec.FMarkId;
        VOrmMarkRTree.FLeft := VRect.Left;
        VOrmMarkRTree.FRight := VRect.Right;
        VOrmMarkRTree.FTop := VRect.Top;
        VOrmMarkRTree.FBottom := VRect.Bottom;
        // update rect (rtree index)
        CheckUpdateResult( FClient.Update(VOrmMarkRTree) );
      finally
        VOrmMarkRTree.Free;
      end;
    end;

    if (AOldMarkRec.FVisible <> ANewMarkRec.FVisible) or VUpdateCategory then begin
      // update view
      if UpdateMarkView(ANewMarkRec.FMarkId, ANewMarkRec.FCategoryId, ANewMarkRec.FVisible, False) then begin
        Result := True;
      end;
    end;

    // update geometry meta blob
    if VUpdateGeo then begin
      VOrmMarkMeta := TOrmMarkMeta.Create(FClient, 'mMark=?', [ANewMarkRec.FMarkId]);
      try
        if Length(VGeometryMetaBlob) = 0 then begin
          if VOrmMarkMeta.ID > 0 then begin
            CheckDeleteResult( FClient.Delete(TOrmMarkMeta, VOrmMarkMeta.ID) );
            Result := True;
          end;
        end else begin
          if VOrmMarkMeta.ID = 0 then begin
            // add new
            VOrmMarkMeta.FMark := ANewMarkRec.FMarkId;
            VOrmMarkMeta.FMeta := VGeometryMetaBlob;
            CheckID( FClient.Add(VOrmMarkMeta, True) );
          end;
          CheckUpdateResult(
            FClient.UpdateBlob(TOrmMarkMeta, VOrmMarkMeta.ID, 'mMeta', VGeometryMetaBlob)
          );
          Result := True;
        end;
      finally
        VOrmMarkMeta.Free;
      end;
    end;

    FTransaction.Commit(VTransaction);
  except
    FTransaction.RollBack(VTransaction);
    raise;
  end;
end;

function TMarkDbImplORMHelper.ReadMarkSQL(
  out AMarkRec: TOrmMarkRec;
  const AMarkID: TID;
  const ACategoryID: TID;
  const AMarkName: string
): Boolean;
var
  VMarkID: TID;
  VSQLWhere: RawUtf8;
  VFieldsCSV: RawUtf8;
  VOrmMark: TOrmMark;
  VOrmMarkMeta: TOrmMarkMeta;
  VOrmMarkView: TOrmMarkView;
  VPointsBlobData: RawBlob;
  VMetaBlobData: RawBlob;
  VIndexItem: POrmMarkIdIndexRec;
  VCacheItem: POrmMarkRow;
  VViewItem: POrmMarkViewRow;
  VGeometry: IGeometryLonLat;
  VMeta: IDoublePointsMeta;
begin
  Assert( (AMarkID > 0) or (AMarkName <> '') );

  Result := False;

  AMarkRec := cEmptyOrmMarkRec;

  VOrmMark := FOrmMarkClass.Create;
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
          VFieldsCSV := 'mName,mDesc,mGeoType,mGeoCount';
          VSQLWhere := FormatSql('RowID=?', [], [VMarkID]);
          if FClient.Retrieve(VSQLWhere, VOrmMark, VFieldsCSV) then begin
            // fill main params from db
            AMarkRec.FName := UTF8ToString(VOrmMark.FName);
            AMarkRec.FDesc := UTF8ToString(VOrmMark.FDesc);
            AMarkRec.FGeoType := VOrmMark.FGeoType;
            AMarkRec.FGeoCount := VOrmMark.FGeoCount;
            // add to cache
            FCache.FMarkCache.AddOrUpdate(AMarkRec);
            VSQLWhere := '';
          end else begin
            DeleteMarkSQL(VMarkID, True);
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
        VSQLWhere := FormatSql('RowID=?', [], [VMarkID]);
      end;
    end else if AMarkName <> '' then begin
      if ACategoryID > 0 then begin
        VSQLWhere := FormatSql('mName=? AND mCategory=?', [], [AMarkName, ACategoryID]);
      end else begin
        VSQLWhere := FormatSql('mName=?', [], [AMarkName]);
      end;
    end else begin
      Exit;
    end;
    if VSQLWhere <> '' then begin
      // get all from db
      VFieldsCSV := 'RowID,mCategory,mImage,mAppearance,mName,mDesc,mGeoType,mGeoCount';
      if FClient.Retrieve(VSQLWhere, VOrmMark, VFieldsCSV) then begin
        // fill id's from db
        VMarkID := VOrmMark.ID;
        AMarkRec.FMarkId := VMarkID;
        AMarkRec.FCategoryId := VOrmMark.FCategory;
        CheckID(AMarkRec.FCategoryId);
        AMarkRec.FPicId := VOrmMark.FImage; // = 0 is OK
        AMarkRec.FAppearanceId := VOrmMark.FAppearance;
        CheckID(AMarkRec.FAppearanceId);
        // fill main params from db
        AMarkRec.FName := UTF8ToString(VOrmMark.FName);
        AMarkRec.FDesc := UTF8ToString(VOrmMark.FDesc);
        AMarkRec.FGeoType := VOrmMark.FGeoType;
        AMarkRec.FGeoCount := VOrmMark.FGeoCount;
        // add to cache
        FCache.FMarkCache.AddOrUpdate(AMarkRec);
        FCache.FMarkIdIndex.AddOrUpdate(AMarkRec);
        FCache.FMarkIdByCategoryIndex.Add(AMarkRec.FCategoryId, AMarkRec.FMarkId);
      end else begin
        Exit;
      end;
    end;
  finally
    VOrmMark.Free;
  end;

  VMarkID := AMarkRec.FMarkId;

  // read geometry blob
  if FCache.FMarkGeometryCache.Find(VMarkID, VGeometry) then begin
    // found in cache
    AMarkRec.FGeometry := VGeometry;
  end else begin
    // read from db

    VMeta := nil;
    VOrmMarkMeta := TOrmMarkMeta.Create(FClient, 'mMark=?', [VMarkID]);
    try
      if VOrmMarkMeta.ID > 0 then begin
        if FClient.RetrieveBlob(TOrmMarkMeta, VOrmMarkMeta.ID, 'mMeta', VMetaBlobData) then begin
          VMeta := _GeomertryMetaFromBlob(VMetaBlobData);
        end;
      end;
    finally
      VOrmMarkMeta.Free;
    end;

    CheckRetrieveResult( FClient.RetrieveBlob(FOrmMarkClass, VMarkID, 'mGeoWKB', VPointsBlobData) );
    AMarkRec.FGeometry := _GeomertryFromBlob(VPointsBlobData, VMeta);

    // add to cache
    FCache.FMarkGeometryCache.AddOrUpdate(VMarkID, Length(VPointsBlobData), AMarkRec.FGeometry);
  end;

  // read view
  if FCache.FMarkViewCache.Find(VMarkID, VViewItem) then begin
    // found in cache
    AMarkRec.FViewId := VViewItem.ViewId;
    AMarkRec.FVisible := VViewItem.Visible;
  end else begin
    if not FCache.FMarkViewCache.IsPrepared then begin
      // read from db
      VOrmMarkView := TOrmMarkView.Create(FClient, 'mvMark=? AND mvUser=?', [VMarkID, FUserID]);
      try
        if VOrmMarkView.ID > 0 then begin
          AMarkRec.FViewId := VOrmMarkView.ID;
          AMarkRec.FVisible := VOrmMarkView.FVisible;
        end else begin
          AMarkRec.FVisible := True;
        end;
      finally
        VOrmMarkView.Free;
      end;
    end;
    // add to cache
    FCache.FMarkViewCache.AddOrUpdate(AMarkRec);
  end;

  // read pic name
  _ReadMarkImage(AMarkRec);

  // read appearance
  _ReadMarkAppearance(AMarkRec);

  Result := True;
end;

function TMarkDbImplORMHelper.UpdateMarkView(
  const AMarkID: TID;
  const ACategoryID: TID;
  const AVisible: Boolean;
  const AUseTransaction: Boolean
): Boolean;
var
  VFind: Boolean;
  VItem: POrmMarkViewRow;
  VCategory: TID;
  VUpdateCache: Boolean;
  VSQLWhere: RawUtf8;
  VOrmMarkView: TOrmMarkView;
  VTransaction: TTransactionRec;
  VFieldsBuilder: TCSVFieldsBuilder;
begin
  Assert(AMarkID > 0);

  Result := False;

  if AUseTransaction then begin
    VTransaction := FTransaction.Start(TOrmMarkView, FIsReadOnly);
  end;
  try
    VOrmMarkView := TOrmMarkView.Create;
    try
      VUpdateCache := True;
      VOrmMarkView.IDValue := 0;
      VFind := FCache.FMarkViewCache.Find(AMarkID, VItem);
      if VFind then begin
        VOrmMarkView.IDValue := VItem.ViewId;
        VOrmMarkView.FUser := FUserID;
        VOrmMarkView.FMark := VItem.MarkId;
        VOrmMarkView.FCategory := VItem.CategoryID;
        VOrmMarkView.FVisible := VItem.Visible;
        VUpdateCache := (VItem.CategoryID <> ACategoryID) or (VItem.Visible <> AVisible);
      end else if not (FCache.FMarkViewCache.IsPrepared or FCache.FMarkViewCache.IsCategoryPrepared(ACategoryID)) then begin
        VSQLWhere := FormatSql('mvMark=? AND mvUser=?', [], [AMarkID, FUserID]);
        VFind := FClient.Retrieve(VSQLWhere, VOrmMarkView, 'RowID,mvCategory,mvVisible');
      end;
      if not FIsReadOnly then begin
        if VFind and (VOrmMarkView.ID > 0) then begin
          VFieldsBuilder.Clear;
          if VOrmMarkView.FVisible <> AVisible then begin
            VFieldsBuilder.Add('mvVisible');
            VOrmMarkView.FVisible := AVisible;
          end;
          VCategory := VOrmMarkView.FCategory;
          CheckID(VCategory);
          if (ACategoryID > 0) and (VCategory <> ACategoryID) then begin
            VFieldsBuilder.Add('mvCategory');
            VOrmMarkView.FCategory := ACategoryID;
          end;
          if VFieldsBuilder.Count > 0 then begin
            // update db
            Result := FClient.Update(VOrmMarkView, VFieldsBuilder.Build);
            CheckUpdateResult(Result);
          end;
        end else if not AVisible then begin
          VOrmMarkView.FUser := FUserID;
          VOrmMarkView.FMark := AMarkID;
          VOrmMarkView.FCategory := ACategoryID;
          VOrmMarkView.FVisible := AVisible;
          // add to db
          CheckID( FClient.Add(VOrmMarkView, True) );
          Result := True;
        end else { AVisible = True } begin
          // Marks visible by default, so we can not add an entry to the db
        end;
      end else begin
        Result := True;
      end;
      if VUpdateCache then begin
        // update cache
        FCache.FMarkViewCache.AddOrUpdate(AMarkID, VOrmMarkView.ID {can be zero}, ACategoryID, AVisible);
      end;
    finally
      VOrmMarkView.Free;
    end;
    if AUseTransaction then begin
      FTransaction.Commit(VTransaction);
    end;
  except
    if AUseTransaction then begin
      FTransaction.RollBack(VTransaction);
    end;
    raise;
  end;
end;

function TMarkDbImplORMHelper.SetMarksInCategoryVisibleSQL(
  const ACategoryID: TID;
  const AVisible: Boolean
): Boolean;

  function _TryFastUpdate(const AMarkID: TID): Boolean;
  var
    VView: POrmMarkViewRow;
  begin
    Result := False;
    if FCache.FMarkViewCache.Find(AMarkID, VView) then begin
      if VView.Visible = AVisible then begin
        Result := True; // nothing to change in db
      end else if FIsReadOnly then begin
        FCache.FMarkViewCache.AddOrUpdate(AMarkID, VView.ViewId, ACategoryID, AVisible);
        Result := True;
      end;
    end else if FIsReadOnly then begin
      FCache.FMarkViewCache.AddOrUpdate(AMarkID, 0 {use fake id}, ACategoryID, AVisible);
      Result := True;
    end;
  end;

var
  I: Integer;
  VCount: Integer;
  VArray: TIDDynArray;
  VTransaction: TTransactionRec;
  VSQLRequest: RawUtf8;
begin
  Result := False;
  CheckID(ACategoryID);

  if not FIsReadOnly and (FClientType in [ctSQLite3, ctODBC, ctZDBC]) then begin
    // UPDATE: SQLite3 and DBMS
    FCache.FMarkViewCache.Reset;

    VSQLRequest := FormatSql(
      'UPDATE MarkView SET mvVisible=% WHERE mvCategory=? AND mvUser=?',
      [AVisible], [ACategoryID, FUserID]
    );

    Result := FClient.Execute(VSQLRequest);
    CheckUpdateResult(Result);
  end;

  if not FIsReadOnly and not AVisible and (FClientType = ctSQLite3) then begin
    // INSERT: SQLite3
    VSQLRequest := FormatSql(
      'INSERT OR IGNORE INTO MarkView (mvUser,mvMark,mvCategory,mvVisible) SELECT %,RowID,%,% FROM Mark WHERE mCategory=?',
      [FUserID, ACategoryID, AVisible], [ACategoryID]
    );

    Result := FClient.Execute(VSQLRequest);
    CheckExecuteResult(Result);
  end else
  if FIsReadOnly or not AVisible or (FClientType = ctMongoDB) then begin
    // INSERT: DBMS (if not ReadOnly)
    // UPDATE or INSERT: MongoDB (if not ReadOnly)
    // AddOrUpdate: FMarkViewCache
    if _FillPrepareMarkIdIndex(ACategoryID) > 0 then begin
      _FillPrepareMarkViewCache(ACategoryID);
      if FCache.FMarkIdByCategoryIndex.Find(ACategoryID, VArray, VCount) then begin
        VTransaction := FTransaction.Start(TOrmMarkView, FIsReadOnly);
        try
          for I := 0 to VCount - 1 do begin
            if not _TryFastUpdate(VArray[I]) then begin
              UpdateMarkView(VArray[I], ACategoryID, AVisible, False);
            end;
          end;
          FTransaction.Commit(VTransaction);
          Result := True;
        except
          FTransaction.RollBack(VTransaction);
          raise;
        end;
      end else begin
        Assert(False);
      end;
    end;
  end;
end;

function TMarkDbImplORMHelper._FillPrepareMarkIdIndex(const ACategoryID: TID): Integer;
var
  {$IFDEF DEBUG}
  Z: Integer;
  {$ENDIF}
  I, J, K: Integer;
  VCount: Integer;
  VList: TOrmTable;
  VByCategory: Boolean;
  VArray: TIDDynArray;
  VMarkIdArray: TIDDynArray;
  VMarkIdRows: TOrmMarkIdIndexRecDynArray;
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
      [FOrmMarkClass],
      FormatSql(
        'SELECT RowID,mImage,mAppearance FROM % WHERE mCategory=?',
        [FOrmMarkName], [ACategoryID]
      )
    );
  end else begin
    if FCache.FMarkIdIndex.IsPrepared then begin
      Result := FCache.FMarkIdIndex.Count;
      Exit;
    end;
    VList := FClient.ExecuteList(
      [FOrmMarkClass],
      FormatSql(
        'SELECT RowID,mImage,mAppearance,mCategory FROM % ORDER BY mCategory',
        [FOrmMarkName], []
      )
    );
  end;
  if Assigned(VList) then
  try
    VCount := VList.RowCount;
    SetLength(VMarkIdArray, VCount);
    SetLength(VMarkIdRows, VCount);
    K := 0;
    VCurrCategory := 0;
    if not VByCategory then begin
      // Ensure that result is sorted by Category
      // (MongoDB 3.2 issue: http://www.sasgis.org/mantis/view.php?id=2970)
      VList.SortFields(3, True, nil, oftID);
    end;
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
        end;
        if VCurrCategory = VCategory then begin
          Inc(K);
        end else if VCurrCategory < VCategory then begin
          if K > 0 then begin
            {$IFDEF DEBUG}
            Assert(I-K >= 0);
            for Z := I - K to I - 1 do begin
              Assert(VMarkIdRows[Z].CategoryId = VCurrCategory);
            end;
            if I > K then begin
              Assert(VMarkIdRows[I-K-1].CategoryId < VCurrCategory);
            end;
            {$ENDIF}
            FCache.FMarkIdByCategoryIndex.AddPrepared(VCurrCategory, VMarkIdArray, I-K, K);
            K := 0;
          end;
          VCurrCategory := VCategory;
          Inc(K);
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
  VList: TOrmTable;
  VArray: TOrmMarkRowDynArray;
begin
  if ACategoryID > 0 then begin
    if FCache.FMarkCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [FOrmMarkClass],
      FormatSql(
        'SELECT RowID,mName,mDesc,mGeoType,mGeoCount FROM % WHERE mCategory=?',
        [FOrmMarkName], [ACategoryID]
      )
    );
  end else begin
    if FCache.FMarkCache.IsPrepared then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [FOrmMarkClass],
      RawUtf8('SELECT RowID,mName,mDesc,mGeoType,mGeoCount FROM ') + FOrmMarkName
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
      VArray[I].GeoType := TOrmGeoType(VList.GetAsInteger(J, 3));
      VArray[I].GeoCount := VList.GetAsInteger(J, 4);
    end;
    FCache.FMarkCache.AddPrepared(ACategoryID, VArray);
  finally
    VList.Free;
  end;
end;

procedure TMarkDbImplORMHelper._FillPrepareMarkGeometryCache(const ACategoryID: TID);
type
  TMarkMetaRec = packed record
    Id: TID;
    Blob: RawBlob;
  end;
  TMarkMetaRecDynArray = array of TMarkMetaRec;
var
  I, J: Integer;
  VCount: Integer;
  VList: TOrmTable;
  VListMeta: TOrmTable;
  VArray: TOrmMarkGeometryRecDynArray;
  VBlob: RawBlob;
  VIndex: Integer;
  VMetaArr: TMarkMetaRecDynArray;
  VMetaDynArr: TDynArray;
  VMetaArrCount: Integer;
  VMeta: IDoublePointsMeta;
  VMetaBlobSize: Integer;
begin
  if ACategoryID > 0 then begin
    if FCache.FMarkGeometryCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [FOrmMarkClass],
      FormatSql('SELECT RowID,mGeoWKB FROM % WHERE mCategory=?', [FOrmMarkName], [ACategoryID])
    );
    VListMeta := FClient.ExecuteList(
      [TOrmMarkMeta, FOrmMarkClass],
      FormatSql(
        'SELECT A.mMark,A.mMeta FROM MarkMeta A WHERE A.mMark IN (SELECT B.RowID FROM % B WHERE B.mCategory=?)',
        [FOrmMarkName], [ACategoryID]
      )
    );
  end else begin
    if FCache.FMarkGeometryCache.IsPrepared then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [FOrmMarkClass],
      RawUtf8('SELECT RowID,mGeoWKB FROM ') + FOrmMarkName
    );
    VListMeta := FClient.ExecuteList(
      [TOrmMarkMeta],
      RawUtf8('SELECT mMark,mMeta FROM MarkMeta')
    );
  end;

  VMetaArr := nil;
  if Assigned(VListMeta) then
  try
    VMetaArrCount := VListMeta.RowCount;
    SetLength(VMetaArr, VMetaArrCount);
    for I := 0 to VMetaArrCount - 1 do begin
      J := I + 1;
      VMetaArr[I].Id := VListMeta.GetAsInt64(J, 0);
      VMetaArr[I].Blob := VListMeta.GetBlob(J, 1);
    end;
    VMetaDynArr.InitSpecific(TypeInfo(TMarkMetaRecDynArray), VMetaArr, ptInt64, @VMetaArrCount);
    VMetaDynArr.Count := Length(VMetaArr);
    VMetaDynArr.Sort;
  finally
    VListMeta.Free;
  end;

  if Assigned(VList) then
  try
    VCount := VList.RowCount;
    SetLength(VArray, VCount);
    for I := 0 to VCount - 1 do begin
      J := I + 1;
      VArray[I].MarkId := VList.GetAsInt64(J, 0);
      VBlob := VList.GetBlob(J, 1);

      VMeta := nil;
      VMetaBlobSize := 0;
      if (Length(VMetaArr) > 0) and VMetaDynArr.FastLocateSorted(VArray[I].MarkId, VIndex) then begin
        VMeta := _GeomertryMetaFromBlob(VMetaArr[VIndex].Blob);
        if VMeta <> nil then begin
          VMetaBlobSize := Length(VMetaArr[VIndex].Blob);
        end;
      end;

      VArray[I].Geometry := _GeomertryFromBlob(VBlob, VMeta);
      VArray[I].Size := Length(VBlob) + VMetaBlobSize;
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
  VList: TOrmTable;
  VRows: TOrmMarkViewRowDynArray;
begin
  if ACategoryID > 0 then begin
    if FCache.FMarkViewCache.IsCategoryPrepared(ACategoryID) then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TOrmMarkView, FOrmMarkClass],
      FormatSql(
        'SELECT RowID,mvMark,mvVisible ' +
        'FROM MarkView ' +
        'WHERE mvCategory=? AND mvUser=?',
        [], [ACategoryID, FUserID]
      )
    );
  end else begin
    if FCache.FMarkViewCache.IsPrepared then begin
      Exit;
    end;
    VList := FClient.ExecuteList(
      [TOrmMarkView],
      FormatSql(
        'SELECT RowID,mvMark,mvVisible,mvCategory FROM MarkView WHERE mvUser=?',
        [], [FUserID]
      )
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
      if ACategoryID > 0 then begin
        VRows[I].CategoryId := ACategoryID;
      end else begin
        VRows[I].CategoryId := VList.GetAsInt64(J, 3);
      end;
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
  const AIncludeAppearance: Boolean;
  out AMarkRecArray: TOrmMarkRecDynArray
): Integer;

  function FillMarkRec(var AMarkRec: TOrmMarkRec; const AIndexRec: POrmMarkIdIndexRec): Boolean;
  var
    VCacheItem: POrmMarkRow;
    VViewItem: POrmMarkViewRow;
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

    if AIncludeAppearance then begin
      _ReadMarkImage(AMarkRec);
      _ReadMarkAppearance(AMarkRec);
    end;

    Result := True;
  end;

var
  I, J: Integer;
  VCount: Integer;
  VIdCount: Integer;
  VRows: TOrmMarkIdIndexRecDynArray;
  VArray: TIDDynArray;
  VIndexRec: POrmMarkIdIndexRec;
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
            AMarkRecArray[J] := cEmptyOrmMarkRec;
            if FillMarkRec(AMarkRecArray[J], VIndexRec) then begin
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
        AMarkRecArray[J] := cEmptyOrmMarkRec;
        if FillMarkRec(AMarkRecArray[J], @VRows[I]) then begin
          Inc(J);
        end;
      end;
    end;
    SetLength(AMarkRecArray, J);
  end;

  Result := J;
end;

function TMarkDbImplORMHelper._GetMarkIDArrayByRectSQL(
  const ACategoryIDArray: TIDDynArray;
  const ARect: TDoubleRect;
  const ALonSize: Cardinal;
  const ALatSize: Cardinal;
  const AReciveCategoryID: Boolean;
  out AIDArray: TMarkWithCategoryIDDynArray
): Integer;
var
  I: Integer;
  VLen: Integer;
  VSQLSelect: RawUtf8;
  VSelectedRows: RawUtf8;
  VCategoryWhere: RawUtf8;
  VList: TOrmTable;
begin
  Result := 0;

  VLen := Length(ACategoryIDArray);

  if AReciveCategoryID then begin
    VSelectedRows := FormatSql('%.RowID,%.mCategory', [FOrmMarkName, FOrmMarkName], []);
  end else begin
    VSelectedRows := FormatSql('%.RowID', [FOrmMarkName], []);
  end;

  if VLen = 1 then begin
    if ACategoryIDArray[0] <= 0 then begin
      VCategoryWhere := '';
      Assert(False);
    end else begin
      VCategoryWhere := FormatSql('AND %.mCategory=? ', [FOrmMarkName], [ACategoryIDArray[0]]);
    end;
  end else if VLen > 1 then begin
    VCategoryWhere := Int64DynArrayToCSV(@ACategoryIDArray[0], VLen);
    VCategoryWhere := FormatUtf8('AND %.mCategory IN (%) ', [FOrmMarkName, VCategoryWhere]);
  end else begin
    VCategoryWhere := '';
  end;

  VSQLSelect :=
    FormatSql(
      'SELECT % FROM %,MarkRTree ' +
      'WHERE %.RowID=MarkRTree.RowID ' + VCategoryWhere +
      'AND mLeft<=? AND mRight>=? AND mBottom<=? AND mTop>=? ' +
      'AND (%.mGeoType=? OR %.mGeoLonSize>=? OR %.mGeoLatSize>=?);',
      [VSelectedRows,FOrmMarkName,FOrmMarkName,FOrmMarkName,FOrmMarkName,FOrmMarkName],
      [ARect.Right,ARect.Left,ARect.Top,ARect.Bottom,Integer(gtPoint),Int64(ALonSize),Int64(ALatSize)]
    );

  VList := FClient.ExecuteList([FOrmMarkClass, TOrmMarkRTree], VSQLSelect);
  if Assigned(VList) then
  try
    VLen := VList.RowCount;
    SetLength(AIDArray, VLen);
    for I := 0 to VLen - 1 do begin
      AIDArray[I].MarkID := VList.GetAsInt64(I+1, 0);
      if AReciveCategoryID then begin
        AIDArray[I].CategoryID := VList.GetAsInt64(I+1, 1);
      end;
    end;
    Result := VLen;
  finally
    VList.Free;
  end;
end;

{$IFDEF ENABLE_DBMS}
function TMarkDbImplORMHelper._GetMarkIDArrayByRectDBMS(
  const ACategoryIDArray: TIDDynArray;
  const ARect: TDoubleRect;
  const ALonSize: Cardinal;
  const ALatSize: Cardinal;
  const AReciveCategoryID: Boolean;
  out AIDArray: TMarkWithCategoryIDDynArray
): Integer;
var
  I: Integer;
  VLen: Integer;
  VIntRect: TRect;
  VSQLSelect: RawUtf8;
  VSelectedRows: RawUtf8;
  VCategoryWhere: RawUtf8;
  VList: TOrmTable;
  VRows: ISQLDBRows;
  VJsonBuffer: RawUtf8;
  VProps: TSQLDBConnectionProperties;
begin
  LonLatDoubleRectToRect(ARect, VIntRect);

  VLen := Length(ACategoryIDArray);

  if AReciveCategoryID then begin
    VSelectedRows := RawUtf8('ID,mCategory');
  end else begin
    VSelectedRows := RawUtf8('ID');
  end;

  if VLen = 1 then begin
    if ACategoryIDArray[0] <= 0 then begin
      VCategoryWhere := '';
      Assert(False);
    end else begin
      VCategoryWhere := FormatSql('mCategory=? AND ', [], [ACategoryIDArray[0]]);
    end;
  end else if VLen > 1 then begin
    VCategoryWhere := Int64DynArrayToCSV(@ACategoryIDArray[0], VLen);
    VCategoryWhere := FormatUtf8('mCategory IN (%) AND ', [VCategoryWhere]);
  end else begin
    VCategoryWhere := '';
  end;

  VSQLSelect :=
    FormatSql(
      'SELECT % FROM Mark ' +
      'WHERE ' + VCategoryWhere +
      'mLeft<=? AND mRight>=? AND mBottom<=? AND mTop>=? ' +
      'AND (mGeoType=? OR mGeoLonSize>=? OR mGeoLatSize>=?);',
      [VSelectedRows],
      [VIntRect.Right,VIntRect.Left,VIntRect.Top,VIntRect.Bottom,Integer(gtPoint),Int64(ALonSize),Int64(ALatSize)]
    );

  VProps :=
    FRestClient.Server.Model.Props[FOrmMarkClass].ExternalDB.ConnectionProperties as TSQLDBConnectionProperties;

  VRows := VProps.ExecuteInlined(VSQLSelect, True);

  VJsonBuffer := VRows.FetchAllAsJSON(False); // ToDo: use VRows.Step

  VList := TOrmTableJson.Create('', Pointer(VJsonBuffer), Length(VJsonBuffer));
  try
    VLen := VList.RowCount;
    SetLength(AIDArray, VLen);
    for I := 0 to VLen - 1 do begin
      AIDArray[I].MarkID := VList.GetAsInt64(I+1, 0);
      if AReciveCategoryID then begin
        AIDArray[I].CategoryID := VList.GetAsInt64(I+1, 1);
      end;
    end;
    Result := VLen;
  finally
    VList.Free;
  end;
end;
{$ENDIF}

function TMarkDbImplORMHelper._GetMarkIDArrayByRectMongoDB(
  const ACategoryIDArray: TIDDynArray;
  const ARect: TDoubleRect;
  const ALonSize: Cardinal;
  const ALatSize: Cardinal;
  const AReciveCategoryID: Boolean;
  out AIDArray: TMarkWithCategoryIDDynArray
): Integer;
var
  I, J: Integer;
  VLen: Integer;
  VIntRect: TRect;
  VSelectedRows: RawUtf8;
  VCategoryWhere: RawUtf8;
  VCollection: TMongoCollection;
  VArray: TVariantDynArray;
begin
  LonLatDoubleRectToRect(ARect, VIntRect);

  VLen := Length(ACategoryIDArray);

  if AReciveCategoryID then begin
    VSelectedRows := RawUtf8('_id,mCategory');
  end else begin
    VSelectedRows := RawUtf8('_id');
  end;

  if VLen = 1 then begin
    if ACategoryIDArray[0] <= 0 then begin
      VCategoryWhere := '';
      Assert(False);
    end else begin
      VCategoryWhere := '{mCategory:' + Int64ToUtf8(ACategoryIDArray[0]) + '},';
    end;
  end else if VLen > 1 then begin
    VCategoryWhere := Int64DynArrayToCSV(@ACategoryIDArray[0], VLen, '{mCategory:{$in:[', ']}},');
  end else begin
    VCategoryWhere := '';
  end;

  VCollection :=
    (FServer.GetStaticStorage(FOrmMarkClass) as TRestStorageMongoDB).Collection;

  VCollection.FindDocs(
    '{$and:[' +
      '{$and:[{mLeft:{$lte:?}},{mRight:{$gte:?}},{mBottom:{$lte:?}},{mTop:{$gte:?}}]},' +
      VCategoryWhere +
      '{$or:[{mGeoType:?},{mGeoLonSize:{$gte:?}},{mGeoLatSize:{$gte:?}}]}' +
    ']}',
    [VIntRect.Right, VIntRect.Left, VIntRect.Top, VIntRect.Bottom, Integer(gtPoint), Int64(ALonSize), Int64(ALatSize)],
    VArray, VSelectedRows
  );

  J := 0;
  SetLength(AIDArray, Length(VArray));
  for I := 0 to Length(VArray) - 1 do begin
    AIDArray[J].MarkID := DocVariantData(VArray[I]).I['_id'];
    if AIDArray[J].MarkID > 0 then begin
      if AReciveCategoryID then begin
        AIDArray[J].CategoryID := DocVariantData(VArray[I]).I['mCategory'];
        CheckID(AIDArray[J].CategoryID);
      end;
      Inc(J);
    end;
  end;
  SetLength(AIDArray, J);
  Result := J;
end;

function TMarkDbImplORMHelper.GetMarkRecArrayByRect(
  const ACategoryIDArray: TDynArray;
  const ARect: TDoubleRect;
  const AIncludeHiddenMarks: Boolean;
  const ALonLatSize: TDoublePoint;
  out AMarkRecArray: TOrmMarkRecDynArray
): Integer;
const
  cMaxArrayLen = 128;
var
  I: Integer;
  VCount: Integer;
  VId: TID;
  VMarksCount: Integer;
  VLonSize, VLatSize: Cardinal;
  VFilterByCategory: Boolean;
  VIDArray: TMarkWithCategoryIDDynArray;
  VCategoryIDArray: TIDDynArray;
begin
  LonLatSizeToInternalSize(ALonLatSize, VLonSize, VLatSize);

  // If there are too many categories, we fetch Mark IDs from all categories
  // for the given Rect from the DB, and then filter the results manually
  VFilterByCategory := (ACategoryIDArray.Count >= cMaxArrayLen);

  if VFilterByCategory then begin
    Finalize(VCategoryIDArray);
    if not ACategoryIDArray.Sorted then begin
      ACategoryIDArray.Compare := SortDynArrayInt64;
      ACategoryIDArray.Sort;
    end;
  end else begin
    ACategoryIDArray.Slice(VCategoryIDArray, cMaxArrayLen);
  end;

  // search mark id's
  case FClientType of
    ctMongoDB: begin
      VCount := _GetMarkIDArrayByRectMongoDB(
        VCategoryIDArray,
        ARect,
        VLonSize,
        VLatSize,
        VFilterByCategory,
        VIDArray
      );
    end;
    ctSQLite3: begin
      VCount := _GetMarkIDArrayByRectSQL(
        VCategoryIDArray,
        ARect,
        VLonSize,
        VLatSize,
        VFilterByCategory,
        VIDArray
      );
    end;
    {$IFDEF ENABLE_DBMS}
    ctZDBC, ctODBC: begin
      VCount := _GetMarkIDArrayByRectDBMS(
        VCategoryIDArray,
        ARect,
        VLonSize,
        VLatSize,
        VFilterByCategory,
        VIDArray
      );
    end;
    {$ENDIF}
  else
    Assert(False);
    Result := 0;
    Exit;
  end;

  Assert(Length(VIDArray) >= VCount);

  // read marks data
  VMarksCount := 0;
  SetLength(AMarkRecArray, VCount);
  for I := 0 to VCount - 1 do begin
    if VFilterByCategory then begin
      VId := VIDArray[I].CategoryID;
      Assert(VId > 0);
      if not (ACategoryIDArray.Find(VId) >= 0) then begin
        Continue;
      end;
    end;
    VId := VIDArray[I].MarkID;
    if VId > 0 then begin
      if ReadMarkSQL(AMarkRecArray[VMarksCount], VId, 0, '') then begin
        if not AIncludeHiddenMarks then begin
          if not AMarkRecArray[VMarksCount].FVisible then begin
            Continue;
          end;
        end;
        Inc(VMarksCount);
      end;
    end;
  end;
  SetLength(AMarkRecArray, VMarksCount);
  Result := VMarksCount;
end;

function TMarkDbImplORMHelper._GetMarkRecArrayByTextSQLite3(
  const ASearch: RawUtf8;
  const AMaxCount: Integer;
  const ASearchInDescription: Boolean;
  out ANameIDArray: TIDDynArray;
  out ADescIDArray: TIDDynArray
): Integer;
var
  VLimit: RawUtf8;
  VSQLWhere: RawUtf8;
begin
  Result := 0;

  VLimit := '';

  if AMaxCount > 0 then begin
    VLimit := StringToUtf8(' LIMIT ' + IntToStr(AMaxCount));
  end;

  VSQLWhere := RawUtf8('mName MATCH ') + ASearch + VLimit;

  if FClient.FtsMatch(TOrmMarkFTS, VSQLWhere, ANameIDArray) then begin
    Inc(Result, Length(ANameIDArray));
  end;

  if not ASearchInDescription or ( (AMaxCount > 0) and (Result >= AMaxCount) ) then begin
    Exit;
  end;

  VSQLWhere := RawUtf8('mDesc MATCH ') + ASearch + VLimit;

  if FClient.FtsMatch(TOrmMarkFTS, VSQLWhere, ADescIDArray) then begin
    Inc(Result, Length(ADescIDArray));
  end;
end;

function TMarkDbImplORMHelper._GetMarkRecArrayByTextSQL(
  const ASearch: RawUtf8;
  const AMaxCount: Integer;
  const ASearchInDescription: Boolean;
  out ANameIDArray: TIDDynArray;
  out ADescIDArray: TIDDynArray
): Integer;
var
  VLimit: RawUtf8;
  VSQLSelect: RawUtf8;
  VList: TOrmTable;
begin
  Result := 0;

  VLimit := '';

  if AMaxCount > 0 then begin
    VLimit := StringToUtf8(' LIMIT ' + IntToStr(AMaxCount));
  end;

  VSQLSelect := RawUtf8('SELECT RowID FROM MarkFTS WHERE mName LIKE ') + ASearch + VLimit;

  VList := FClient.ExecuteList([TOrmMarkFTS], VSQLSelect);
  if Assigned(VList) then
  try
    VList.GetRowValues(0, TInt64DynArray(ANameIDArray));
    Inc(Result, Length(ANameIDArray));
  finally
    VList.Free;
  end;

  if not ASearchInDescription or ( (AMaxCount > 0) and (Result >= AMaxCount) ) then begin
    Exit;
  end;

  if AMaxCount > 0 then begin
    VLimit := StringToUtf8(' LIMIT ' + IntToStr(AMaxCount - Result));
  end;

  VSQLSelect := RawUtf8('SELECT RowID FROM MarkFTS WHERE mDesc LIKE ') + ASearch + VLimit;

  VList := FClient.ExecuteList([TOrmMarkFTS], VSQLSelect);
  if Assigned(VList) then
  try
    VList.GetRowValues(0, TInt64DynArray(ADescIDArray));
    Inc(Result, Length(ADescIDArray));
  finally
    VList.Free;
  end;
end;

function TMarkDbImplORMHelper.GetMarkRecArrayByText(
  const ASearchText: string;
  const AMaxCount: Integer;
  const AIncludeHiddenMarks: Boolean;
  const ASearchInDescription: Boolean;
  out AMarkRecArray: TOrmMarkRecDynArray
): Integer;

  function MergeArrUnique(const A, B: TIDDynArray; out C: TIDDynArray): Integer;
  var
    I, J, R: Integer;
    P: PInt64Array;
    VArr1, VArr2: TIDDynArray;
  begin
    I := Length(A);
    J := Length(B);
    SetLength(C, I+J);
    if I >= J then begin
      VArr1 := A;
      VArr2 := B;
    end else begin
      VArr1 := B;
      VArr2 := A;
    end;
    J := Length(VArr1);
    R := J - 1;
    P := PInt64Array(VArr1);
    QuickSortInt64(P, 0, R);
    MoveFast(VArr1[0], C[0], J * SizeOf(VArr1[0]));
    for I := 0 to Length(VArr2) - 1 do begin
      if FastFindInt64Sorted(P, R, VArr2[I]) < 0 then begin
        C[J] := VArr2[I];
        Inc(J);
      end;
    end;
    SetLength(C, J);
    P := PInt64Array(C);
    QuickSortInt64(P, 0, J-1);
    Result := J;
  end;

var
  I, J, K: Integer;
  VSearch: RawUtf8;
  VIDArray: TIDDynArray;
  VNameIDArray: TIDDynArray;
  VDescIDArray: TIDDynArray;
begin
  Result := 0;

  SetLength(VNameIDArray, 0);
  SetLength(VDescIDArray, 0);

  VSearch := StringToUtf8('''' + SysUtils.AnsiLowerCase(ASearchText) + '''');

  // search mark id's
  case FClientType of
    ctSQLite3: begin
      J := _GetMarkRecArrayByTextSQLite3(
        VSearch,
        AMaxCount,
        ASearchInDescription,
        VNameIDArray,
        VDescIDArray
      );
    end;
  else
    J := _GetMarkRecArrayByTextSQL(
      VSearch,
      AMaxCount,
      ASearchInDescription,
      VNameIDArray,
      VDescIDArray
    );
  end;

  // read marks data
  if J > 0 then begin
    I := Length(VNameIDArray);
    J := Length(VDescIDArray);

    if (I > 0) and (J > 0) then begin
      K := MergeArrUnique(VNameIDArray, VDescIDArray, VIDArray);
    end else if I > 0 then begin
      K := I;
      VIDArray := VNameIDArray;
    end else if J > 0 then begin
      K := J;
      VIDArray := VDescIDArray;
    end else begin
      K := 0;
    end;

    J := 0;
    SetLength(AMarkRecArray, K);
    for I := 0 to K - 1 do begin
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

procedure TMarkDbImplORMHelper.SetReadOnly(const AValue: Boolean);
begin
  if FIsReadOnly <> AValue then begin
    if FIsReadOnly then begin      
      FCache.FMarkViewCache.Reset; // remove possible fake id's
    end;
    FIsReadOnly := AValue;
  end;
end;

end.
