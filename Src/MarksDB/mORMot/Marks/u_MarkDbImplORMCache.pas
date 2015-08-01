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

unit u_MarkDbImplORMCache;

interface

{$I ..\MarkSystemORM.inc}

uses
  Windows,
  SynCommons,
  t_MarkSystemORM,
  i_GeometryLonLat,
  u_MarkSystemORMModel,
  u_MarkSystemORMCacheBase;

type
  (****************************************************************************)
  (*                         TSQLMarkImageCache                               *)
  (****************************************************************************)

  TSQLMarkImageRow = packed record
    ImageId: TID;
    Name: string;
  end;
  PSQLMarkImageRow = ^TSQLMarkImageRow;
  TSQLMarkImageRowDynArray = array of TSQLMarkImageRow;

  TSQLMarkImageCache = class(TSQLCacheBase)
  private
    FRows: TSQLMarkImageRowDynArray;
  public
    function Find(const AID: TID; out AItem: PSQLMarkImageRow): Boolean; overload;
    function Find(const AName: string; out AItem: PSQLMarkImageRow): Boolean; overload;
    procedure AddOrIgnore(const ARec: TSQLMarkRec); overload;
    procedure AddOrIgnore(const AImageID: TID; const AName: string); overload;
    procedure Delete(const AID: TID); override;
  public
    property Rows: TSQLMarkImageRowDynArray read FRows;
  public
    constructor Create(const AMaxRamSize: Integer = 0);
  end;

  (****************************************************************************)
  (*                       TSQLMarkAppearanceCache                            *)
  (****************************************************************************)

  TSQLMarkAppearanceRow = packed record
    AppearanceId: TID;
    Color1: Cardinal;
    Color2: Cardinal;
    Scale1: Integer;
    Scale2: Integer;
  end;
  PSQLMarkAppearanceRow = ^TSQLMarkAppearanceRow;
  TSQLMarkAppearanceRowDynArray = array of TSQLMarkAppearanceRow;

  TSQLMarkAppearanceCache = class(TSQLCacheBase)
  private
    FRows: TSQLMarkAppearanceRowDynArray;
  public
    function Find(const AID: TID; out AItem: PSQLMarkAppearanceRow): Boolean; overload;
    function Find(const AColor1, AColor2: Cardinal; const AScale1, AScale2: Integer;
      out AItem: PSQLMarkAppearanceRow): Boolean; overload;
    procedure AddOrIgnore(const ARec: TSQLMarkRec); overload;
    procedure AddOrIgnore(
      const AID: TID;
      const AColor1, AColor2: Cardinal;
      const AScale1, AScale2: Integer
    ); overload;
  public
    property Rows: TSQLMarkAppearanceRowDynArray read FRows;
  public
    constructor Create(const AMaxRamSize: Integer = 0);
  end;

  (****************************************************************************)
  (*                           TSQLMarkViewCache                              *)
  (****************************************************************************)

  TSQLMarkViewRow = packed record
    MarkId: TID;
    ViewId: TID;
    Visible: Boolean;
  end;
  PSQLMarkViewRow = ^TSQLMarkViewRow;
  TSQLMarkViewRowDynArray = array of TSQLMarkViewRow;

  TSQLMarkViewCache = class(TSQLCacheBaseWithPreparedByCategory)
  private
    FRows: TSQLMarkViewRowDynArray;
  public
    function Find(const AMarkID: TID; out AItem: PSQLMarkViewRow): Boolean;
    procedure AddOrUpdate(const ARec: TSQLMarkRec); overload;
    procedure AddOrUpdate(const AMarkID, AViewID: TID; const AVisible: Boolean); overload;
    procedure AddPrepared(const ACategoryID: TID; const AArr: TSQLMarkViewRowDynArray);
    class procedure MarkRecToViewRow(
      const AMarkRec: TSQLMarkRec;
      const AViewRow: PSQLMarkViewRow
    ); inline;
  public
    property Rows: TSQLMarkViewRowDynArray read FRows;
  public
    constructor Create(const AMaxRamSize: Integer = 0);
  end;

  (****************************************************************************)
  (*                            TSQLMarkIdIndex                               *)
  (****************************************************************************)

  TSQLMarkIdIndexRec = packed record
    MarkId: TID;
    CategoryId: TID;
    ImageId: TID;
    AppearanceId: TID;
  end;
  PSQLMarkIdIndexRec = ^TSQLMarkIdIndexRec;
  TSQLMarkIdIndexRecDynArray = array of TSQLMarkIdIndexRec;

  TSQLMarkIdIndex = class(TSQLCacheBase)
  private
    FRows: TSQLMarkIdIndexRecDynArray;
  public
    function Find(const AID: TID; out AItem: PSQLMarkIdIndexRec): Boolean;
    procedure AddOrUpdate(const ARec: TSQLMarkRec);
    procedure AddPrepared(const AArr: TSQLMarkIdIndexRecDynArray);
    procedure AddArray(
      const AArray: TSQLMarkIdIndexRecDynArray;
      const AStartIndex: Integer = 0;
      const ACount: Integer = -1
    );
    class procedure MarkRecToIndexRec(
      const AMarkRec: TSQLMarkRec;
      const AIndexRec: PSQLMarkIdIndexRec
    ); inline;
  public
    property Rows: TSQLMarkIdIndexRecDynArray read FRows;
  public
    constructor Create;
  end;

  (****************************************************************************)
  (*                       TSQLMarkIdByCategoryIndex                          *)
  (****************************************************************************)

  TSQLMarkIdByCategoryIndex = class
  private
    FDynArray: TDynArrayByRecWithPointer;
  public
    function Find(
      const ACategoryID: TID;
      out AMarkIDArray: TIDDynArray;
      out AMarkIDArrayCount: Integer
    ): Boolean;
    procedure AddPrepared(
      const ACategoryID: TID;
      const AMarkIDArray: TIDDynArray;
      const AStartIndex: Integer;
      const ACount: Integer
    );
    procedure Add(const ACategoryID: TID; const AMarkID: TID);
    procedure Delete(const ACategoryID: TID; const AMarkID: TID);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  (****************************************************************************)
  (*                            TSQLMarkCache                                 *)
  (****************************************************************************)

  TSQLMarkRow = packed record
    MarkId: TID;
    Name: string;
    Desc: string;
    GeoType: TSQLGeoType;
    GeoCount: Integer;
  end;
  PSQLMarkRow = ^TSQLMarkRow;
  TSQLMarkRowDynArray = array of TSQLMarkRow;

  TSQLMarkCache = class(TSQLCacheBaseWithPreparedByCategory)
  private
    FRows: TSQLMarkRowDynArray;
  public
    function Find(const AID: TID; out AItem: PSQLMarkRow): Boolean;
    procedure AddOrUpdate(const ARec: TSQLMarkRec);
    procedure AddPrepared(const ACategoryID: TID; const AArr: TSQLMarkRowDynArray);
    class procedure MarkRecToMarkRow(
      const AMarkRec: TSQLMarkRec;
      out AMarkRow: TSQLMarkRow
    ); inline;
  public
    property Rows: TSQLMarkRowDynArray read FRows;
  public
    constructor Create(const AMaxRamSize: Integer = 0);
  end;

  (****************************************************************************)
  (*                        TSQLMarkGeometryCache                             *)
  (****************************************************************************)
  
  TSQLMarkGeometryRec = packed record
    MarkID: TID;
    Size: Integer;
    Geometry: IGeometryLonLat;
  end;
  PSQLMarkGeometryRec = ^TSQLMarkGeometryRec;
  TSQLMarkGeometryRecDynArray = array of TSQLMarkGeometryRec;

  TSQLMarkGeometryCache = class
  private
    FIsPrepared: Boolean;
    FPreparedCategories: TIDDynArrayObject;
    FGeometryArray: TDynArrayByRecWithPointer;
  public
    function IsCategoryPrepared(const ACategoryID: TID): Boolean;
    function Find(const AMarkID: TID; out AGeometry: IGeometryLonLat): Boolean;
    procedure AddOrUpdate(
      const AMarkID: TID;
      const ASize: Integer;
      const AGeometry: IGeometryLonLat
    );
    procedure AddPrepared(
      const ACategoryID: TID;
      const AArr: TSQLMarkGeometryRecDynArray
    );
    procedure Delete(const AMarkID: TID);
    procedure Reset;
  public
    property IsPrepared: Boolean read FIsPrepared;
  public
    constructor Create(const AMaxRamSize: Integer = 0);
    destructor Destroy; override;
  end;

  (****************************************************************************)
  (*                            TSQLMarkDbCache                               *)
  (****************************************************************************)

  TSQLMarkDbCache = record
    FMarkCache: TSQLMarkCache;
    FMarkGeometryCache: TSQLMarkGeometryCache;
    FMarkViewCache: TSQLMarkViewCache;
    FMarkImage: TSQLMarkImageCache;
    FMarkAppearance: TSQLMarkAppearanceCache;
    FMarkIdIndex: TSQLMarkIdIndex;
    FMarkIdByCategoryIndex: TSQLMarkIdByCategoryIndex;
    procedure Init(const AMaxCacheSize: Int64 = 100*1024*1024); // 100Mb
    procedure Done;
  end;

implementation

uses
  Classes,
  SysUtils,
  u_MarkSystemORMLog;

{ TSQLMarkDbCache }

procedure TSQLMarkDbCache.Init(const AMaxCacheSize: Int64);
var
  VMarkCacheSize: Int64;
  VMarkGeometryCacheSize: Int64;
begin
  VMarkCacheSize := Round(0.2 * AMaxCacheSize); // 20 %
  VMarkGeometryCacheSize := Round(0.7 * AMaxCacheSize); // 70%
  // + keep 10% reserved for View/Image/Appearance/Index caches
  FMarkCache := TSQLMarkCache.Create(VMarkCacheSize);
  FMarkGeometryCache := TSQLMarkGeometryCache.Create(VMarkGeometryCacheSize);
  FMarkViewCache := TSQLMarkViewCache.Create;
  FMarkImage := TSQLMarkImageCache.Create;
  FMarkAppearance := TSQLMarkAppearanceCache.Create;
  FMarkIdIndex := TSQLMarkIdIndex.Create;
  FMarkIdByCategoryIndex := TSQLMarkIdByCategoryIndex.Create;
end;

procedure TSQLMarkDbCache.Done;
begin
  FreeAndNil(FMarkIdByCategoryIndex);
  FreeAndNil(FMarkIdIndex);
  FreeAndNil(FMarkAppearance);
  FreeAndNil(FMarkImage);
  FreeAndNil(FMarkViewCache);
  FreeAndNil(FMarkGeometryCache);
  FreeAndNil(FMarkCache);
end;

{ TSQLMarkImageCache }

constructor TSQLMarkImageCache.Create(const AMaxRamSize: Integer);
begin
  inherited Create(
    TypeInfo(TSQLMarkImageRowDynArray), FRows, djInt64, AMaxRamSize
  );
end;

function TSQLMarkImageCache.Find(const AID: TID; out AItem: PSQLMarkImageRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AID);
  if I >=0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find ID=%, Result=%, Count=%', [AID, Result, FCount], Self);
  {$ENDIF}
end;

function TSQLMarkImageCache.Find(const AName: string; out AItem: PSQLMarkImageRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  for I := 0 to FCount - 1 do begin
    AItem := @FRows[I];
    if AnsiSameText(AItem.Name, AName) then begin
      Result := True;
      Break;
    end;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find Name=%, Result=%, Count=%', [AName, Result, FCount], Self);
  {$ENDIF}
end;

procedure TSQLMarkImageCache.AddOrIgnore(const ARec: TSQLMarkRec);
begin
  AddOrIgnore(ARec.FPicId, ARec.FPicName);
end;

procedure TSQLMarkImageCache.AddOrIgnore(const AImageID: TID; const AName: string);
var
  I: Integer;
  VRec: TSQLMarkImageRow;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddOrIgnore');
  {$ENDIF}
  CheckCacheSize;
  if not FRow.FastLocateSorted(AImageID, I) then begin
    // add
    if I >= 0 then begin
      VRec.ImageId := AImageID;
      VRec.Name := AName;
      FRow.FastAddSorted(I, VRec);
      Inc(FDataSize, Length(VRec.Name)*SizeOf(Char));
      {$IFDEF SQL_LOG_CACHE_RESULT}
      SQLLogCache('Add ID=%, Name=%, NewCount=%', [AImageID, AName, FCount], Self);
      {$ENDIF}
    end else begin
      Assert(False);
    end;
  end;
end;

procedure TSQLMarkImageCache.Delete(const AID: TID);
var
  I: Integer;
  VItemDataSize: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Delete');
  {$ENDIF}
  I := FRow.Find(AID);
  if I >= 0 then begin
    VItemDataSize := Length(FRows[I].Name)*SizeOf(Char);
    DeleteByIndex(I);
    Dec(FDataSize, VItemDataSize);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Del ID=%, NewCount=%', [AID, FCount], Self);
    {$ENDIF}
  end;
end;

{ TSQLMarkAppearanceCache }

constructor TSQLMarkAppearanceCache.Create(const AMaxRamSize: Integer);
begin
  inherited Create(
    TypeInfo(TSQLMarkAppearanceRowDynArray), FRows, djInt64, AMaxRamSize
  );
end;

function TSQLMarkAppearanceCache.Find(
  const AID: TID;
  out AItem: PSQLMarkAppearanceRow
): Boolean;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AID);
  if I >=0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find ID=%, Result=%, Count=%', [AID, Result, FCount], Self);
  {$ENDIF}
end;

function TSQLMarkAppearanceCache.Find(
  const AColor1, AColor2: Cardinal;
  const AScale1, AScale2: Integer;
  out AItem: PSQLMarkAppearanceRow
): Boolean;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  for I := 0 to FCount - 1 do begin
    AItem := @FRows[I];
    if (AItem.Color1 = AColor1) and (AItem.Color2 = AColor2) and
       (AItem.Scale1 = AScale1) and (AItem.Scale2 = AScale2) then
    begin
      Result := True;
      Break;
    end;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find Result=%, Count=%', [Result, FCount], Self);
  {$ENDIF}
end;

procedure TSQLMarkAppearanceCache.AddOrIgnore(const ARec: TSQLMarkRec);
begin
  AddOrIgnore(ARec.FAppearanceId, ARec.FColor1, ARec.FColor2, ARec.FScale1, ARec.FScale2);
end;

procedure TSQLMarkAppearanceCache.AddOrIgnore(
  const AID: TID;
  const AColor1, AColor2: Cardinal;
  const AScale1, AScale2: Integer
);
var
  I: Integer;
  VRec: TSQLMarkAppearanceRow;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddOrIgnore');
  {$ENDIF}
  CheckCacheSize;
  if not FRow.FastLocateSorted(AID, I) then begin
    // add
    if I >= 0 then begin
      VRec.AppearanceId := AID;
      VRec.Color1 := AColor1;
      VRec.Color2 := AColor2;
      VRec.Scale1 := AScale1;
      VRec.Scale2 := AScale2;
      FRow.FastAddSorted(I, VRec);
      {$IFDEF SQL_LOG_CACHE_RESULT}
      SQLLogCache('Add ID=%, NewCount=%', [AID, FCount], Self);
      {$ENDIF}
    end else begin
      Assert(False);
    end;
  end;
end;

{ TSQLMarkIdIndex }

constructor TSQLMarkIdIndex.Create;
begin
  inherited Create(TypeInfo(TSQLMarkIdIndexRecDynArray), FRows, djInt64, 0);
end;

function TSQLMarkIdIndex.Find(const AID: TID; out AItem: PSQLMarkIdIndexRec): Boolean;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AID);
  if I >=0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find ID=%, Result=%, Count=%', [AID, Result, FCount], Self);
  {$ENDIF}
end;

procedure TSQLMarkIdIndex.AddOrUpdate(const ARec: TSQLMarkRec);
var
  I: Integer;
  VRec: TSQLMarkIdIndexRec;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  if not FRow.FastLocateSorted(ARec.FMarkId, I) then begin
    // add
    if I >= 0 then begin
      MarkRecToIndexRec(ARec, @VRec);
      FRow.FastAddSorted(I, VRec);
      {$IFDEF SQL_LOG_CACHE_RESULT}
      SQLLogCache('Add ID=%, NewCount=%', [ARec.FMarkId, FCount], Self);
      {$ENDIF}
    end else begin
      Assert(False);
    end;
  end else begin
    // update
    FRows[I].CategoryId := ARec.FCategoryId;
    FRows[I].ImageId := ARec.FPicId;
    FRows[I].AppearanceId := ARec.FAppearanceId;
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Update ID=%, Count=%', [ARec.FMarkId, FCount], Self);
    {$ENDIF}
  end;
end;

procedure TSQLMarkIdIndex.AddPrepared(const AArr: TSQLMarkIdIndexRecDynArray);
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  Reset;
  if Length(AArr) > 0 then begin
    FRow.AddArray(AArr);
    FRow.Sort;
  end;
  FIsPrepared := True;
end;

procedure TSQLMarkIdIndex.AddArray(
  const AArray: TSQLMarkIdIndexRecDynArray;
  const AStartIndex: Integer;
  const ACount: Integer
);
var
  I, J: Integer;
  VSize: Integer;
  VCount: Integer;
  VBits: TBits;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddArray');
  {$ENDIF}
  if ACount < 0 then begin
    VSize := Length(AArray) - AStartIndex;
  end else begin
    VSize := ACount;
  end;
  if VSize <= 0 then begin
    Exit;
  end;
  if FCount = 0 then begin
    FRow.AddArray(AArray, AStartIndex, ACount);
    FRow.Sort;
  end else begin
    VBits := TBits.Create;
    try
      VCount := 0;
      VBits.Size := VSize;
      for I := 0 to VSize - 1 do begin
        if FRow.Find(AArray[AStartIndex+I]) < 0 then begin
          VBits[I] := True;
          Inc(VCount);
        end;
      end;
      if VCount = VSize then begin
        FRow.AddArray(AArray, AStartIndex, ACount);
        FRow.Sort;
      end else if VCount > 0 then begin
        FRow.Capacity := FCount + VCount;
        for I := 0 to VSize - 1 do begin
          if VBits[I] then begin
            J := FRow.New;
            FRows[J] := AArray[AStartIndex+I];
          end;
        end;
        FRow.Sort;
      end;
    finally
      VBits.Free;
    end;
  end;
end;

class procedure TSQLMarkIdIndex.MarkRecToIndexRec(
  const AMarkRec: TSQLMarkRec;
  const AIndexRec: PSQLMarkIdIndexRec
);
begin
  AIndexRec.MarkId := AMarkRec.FMarkId;
  AIndexRec.CategoryId := AMarkRec.FCategoryId;
  AIndexRec.ImageId := AMarkRec.FPicId;
  AIndexRec.AppearanceId := AMarkRec.FAppearanceId;
end;

{ TSQLMarkIdByCategoryIndex }

constructor TSQLMarkIdByCategoryIndex.Create;
begin
  inherited Create;
  FDynArray := TDynArrayByRecWithPointer.Create(dpObj, nil, 0);
end;

destructor TSQLMarkIdByCategoryIndex.Destroy;
begin
  FDynArray.Free;
  inherited Destroy;
end;

function TSQLMarkIdByCategoryIndex.Find(
  const ACategoryID: TID;
  out AMarkIDArray: TIDDynArray;
  out AMarkIDArrayCount: Integer
): Boolean;
var
  VItem: PRecWithPointer;
  VArray: TIDDynArrayObject;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  if FDynArray.Find(ACategoryID, VItem) then begin
    Assert(VItem.Data <> nil);
    VArray := TIDDynArrayObject(VItem.Data);
    AMarkIDArray := VArray.IDArray;
    AMarkIDArrayCount := VArray.Count;
    Result := True;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find CategoryID=%, Result=%, OutArrCount=%', [ACategoryID, Result, AMarkIDArrayCount], Self);
  {$ENDIF}
end;

procedure TSQLMarkIdByCategoryIndex.AddPrepared(
  const ACategoryID: TID;
  const AMarkIDArray: TIDDynArray;
  const AStartIndex: Integer;
  const ACount: Integer
);
var
  VItem: PRecWithPointer;
  VArray: TIDDynArrayObject;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  VArrayCount: Integer;
  {$ENDIF}
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  if FDynArray.Find(ACategoryID, VItem) then begin
    Assert(VItem.Data <> nil);
    VArray := TIDDynArrayObject(VItem.Data);
    VArray.Reset;
    VArray.AddArray(AMarkIDArray);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    VArrayCount := VArray.Count;
    {$ENDIF}
  end else begin
    VArray := TIDDynArrayObject.Create;
    try
      VArray.AddArray(AMarkIDArray);
      {$IFDEF SQL_LOG_CACHE_RESULT}
      VArrayCount := VArray.Count;
      {$ENDIF}
      FDynArray.Add(ACategoryID, Pointer(VArray), 0, True);
      VArray := nil;
    finally
      VArray.Free;
    end;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('AddPrepared CategoryID=%, MarksCountNew=%, Count=%', [ACategoryID, VArrayCount, FDynArray.Count], Self);
  {$ENDIF}
end;

procedure TSQLMarkIdByCategoryIndex.Add(const ACategoryID: TID; const AMarkID: TID);
var
  VItem: PRecWithPointer;
  VArray: TIDDynArrayObject;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Add');
  {$ENDIF}
  if FDynArray.Find(ACategoryID, VItem) then begin
    Assert(VItem.Data <> nil);
    VArray := TIDDynArrayObject(VItem.Data);
    VArray.Add(AMarkID);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Add CategoryID=%, MarkID=%, MarksCountNew=%', [ACategoryID, AMarkID, VArray.Count], Self);
    {$ENDIF}
  end;
end;

procedure TSQLMarkIdByCategoryIndex.Delete(const ACategoryID: TID; const AMarkID: TID);
var
  VItem: PRecWithPointer;
  VArray: TIDDynArrayObject;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Delete');
  {$ENDIF}
  if FDynArray.Find(ACategoryID, VItem) then begin
    Assert(VItem.Data <> nil);
    VArray := TIDDynArrayObject(VItem.Data);
    VArray.Delete(AMarkID);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Del CategoryID=%, MarkID=%, MarksCountNew=%', [ACategoryID, AMarkID, VArray.Count], Self);
    {$ENDIF}
  end;
end;

{ TSQLMarkCache }

constructor TSQLMarkCache.Create(const AMaxRamSize: Integer);
begin
  inherited Create(TypeInfo(TSQLMarkRowDynArray), FRows, djInt64, AMaxRamSize);
end;

function TSQLMarkCache.Find(const AID: TID; out AItem: PSQLMarkRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AID);
  if I >=0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find ID=%, Result=%, Count=%', [AID, Result, FCount], Self);
  {$ENDIF}
end;

procedure TSQLMarkCache.AddOrUpdate(const ARec: TSQLMarkRec);
var
  I: Integer;
  VSize: Integer;
  VRow: TSQLMarkRow;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  VSize := (Length(ARec.FName) + Length(ARec.FDesc)) * SizeOf(Char);
  if FRow.FastLocateSorted(ARec.FMarkId, I) then begin
    // update
    VSize := VSize - (Length(FRows[I].Name) + Length(FRows[I].Desc)) * SizeOf(Char);
    MarkRecToMarkRow(ARec, FRows[I]);
    Inc(FDataSize, VSize);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Update ID=%, SizeDiff=%, Count=%', [ARec.FMarkId, VSize, FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    MarkRecToMarkRow(ARec, VRow);
    FRow.FastAddSorted(I, VRow);
    Inc(FDataSize, VSize);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Add ID=%, Size=%, NewCount=%', [ARec.FMarkId, VSize, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

procedure TSQLMarkCache.AddPrepared(const ACategoryID: TID; const AArr: TSQLMarkRowDynArray);
var
  I, J: Integer;
  VSize: Integer;
  VCount: Integer;
  VBits: TBits;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  CheckCacheSize;
  VCount := Length(AArr);
  if ACategoryID > 0 then begin
    VBits := TBits.Create;
    try
      if not FPreparedCategoriesDynArr.FastLocateSorted(ACategoryID, I) then begin
        if I >= 0 then begin
          FPreparedCategoriesDynArr.FastAddSorted(I, ACategoryID);
          {$IFDEF SQL_LOG_CACHE_RESULT}
          SQLLogCache('PreparedCategoryID=%, PreparedCount=%', [ACategoryID, FPreparedCategoriesDynArr.Count], Self);
          {$ENDIF}
        end else begin
          Assert(False);
          Exit;
        end;
      end;
      J := 0;
      VBits.Size := VCount;
      for I := 0 to VCount - 1 do begin
        if FRow.Find(AArr[I].MarkId) < 0 then begin
          VBits[I] := True;
          Inc(J);
        end;
      end;
      if J = VCount then begin
        FRow.AddArray(AArr);
        FRow.Sort;
        for I := 0 to Length(AArr) - 1 do begin
          VSize := (Length(AArr[I].Name) + Length(AArr[I].Desc)) * SizeOf(Char);
          Inc(FDataSize, VSize);
        end;
      end else if J > 0 then begin
        for I := 0 to VCount - 1 do begin
          if VBits[I] then begin
            J := FRow.New;
            FRows[J] := AArr[I];
            VSize := (Length(AArr[I].Name) + Length(AArr[I].Desc)) * SizeOf(Char);
            Inc(FDataSize, VSize);
          end;
        end;
        FRow.Sort;
      end;
    finally
      VBits.Free;
    end;
  end else begin
    Reset;
    if VCount > 0 then begin
      FRow.AddArray(AArr);
      FRow.Sort;
    end;
    FIsPrepared := True;
    for I := 0 to VCount - 1 do begin
      VSize := (Length(AArr[I].Name) + Length(AArr[I].Desc)) * SizeOf(Char);
      Inc(FDataSize, VSize);
    end;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('AddPrepared CategoryID=%, IsPrepared=%, InArrCount=%, Count=%', [ACategoryID, FIsPrepared, VCount, FCount], Self);
  {$ENDIF}
end;

class procedure TSQLMarkCache.MarkRecToMarkRow(
  const AMarkRec: TSQLMarkRec;
  out AMarkRow: TSQLMarkRow
);
begin
  AMarkRow.MarkId := AMarkRec.FMarkId;
  AMarkRow.Name := AMarkRec.FName;
  AMarkRow.Desc := AMarkRec.FDesc;
  AMarkRow.GeoType := AMarkRec.FGeoType;
  AMarkRow.GeoCount := AMarkRec.FGeoCount;
end;

{ TSQLMarkViewCache }

constructor TSQLMarkViewCache.Create(const AMaxRamSize: Integer);
begin
  inherited Create(
    TypeInfo(TSQLMarkViewRowDynArray), FRows, djInt64, AMaxRamSize
  );
end;

function TSQLMarkViewCache.Find(const AMarkID: TID; out AItem: PSQLMarkViewRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AMarkID);
  if I >=0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find ID=%, Result=%, Count=%', [AMarkID, Result, FCount], Self);
  {$ENDIF}
end;

procedure TSQLMarkViewCache.AddOrUpdate(const ARec: TSQLMarkRec);
begin
  AddOrUpdate(ARec.FMarkId, ARec.FViewId, ARec.FVisible);
end;

procedure TSQLMarkViewCache.AddOrUpdate(
  const AMarkID, AViewID: TID;
  const AVisible: Boolean
);
var
  I: Integer;
  VRow: TSQLMarkViewRow;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  if FRow.FastLocateSorted(AMarkID, I) then begin
    // update
    FRows[I].ViewId := AViewID;
    FRows[I].Visible := AVisible;
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Update AMarkID=%, Count=%', [AMarkID, FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    VRow.MarkId := AMarkID;
    VRow.ViewId := AViewID;
    VRow.Visible := AVisible;
    FRow.FastAddSorted(I, VRow);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Add AMarkID=%, NewCount=%', [AMarkID, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

procedure TSQLMarkViewCache.AddPrepared(const ACategoryID: TID; const AArr: TSQLMarkViewRowDynArray);
var
  I, J: Integer;
  VCount: Integer;
  VBits: TBits;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  CheckCacheSize;
  VCount := Length(AArr);
  if ACategoryID > 0 then begin
    VBits := TBits.Create;
    try
      if not FPreparedCategoriesDynArr.FastLocateSorted(ACategoryID, I) then begin
        if I >= 0 then begin
          FPreparedCategoriesDynArr.FastAddSorted(I, ACategoryID);
          {$IFDEF SQL_LOG_CACHE_RESULT}
          SQLLogCache('PreparedCategoryID=%, PreparedCount=%', [ACategoryID, FPreparedCategoriesDynArr.Count], Self);
          {$ENDIF}
        end else begin
          Assert(False);
          Exit;
        end;
      end;
      J := 0;
      VBits.Size := VCount;
      for I := 0 to VCount - 1 do begin
        if FRow.Find(AArr[I].MarkId) < 0 then begin
          VBits[I] := True;
          Inc(J);
        end;
      end;
      if J = VCount then begin
        FRow.AddArray(AArr);
        FRow.Sort;
      end else if J > 0 then begin
        for I := 0 to VCount - 1 do begin
          if VBits[I] then begin
            J := FRow.New;
            FRows[J] := AArr[I];
          end;
        end;
        FRow.Sort;
      end;
    finally
      VBits.Free;
    end;
  end else begin
    Reset;
    if VCount > 0 then begin
      FRow.AddArray(AArr);
      FRow.Sort;
    end;
    FIsPrepared := True;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('AddPrepared CategoryID=%, IsPrepared=%, InArrCount=%, Count=%', [ACategoryID, FIsPrepared, VCount, FCount], Self);
  {$ENDIF}
end;

class procedure TSQLMarkViewCache.MarkRecToViewRow(
  const AMarkRec: TSQLMarkRec;
  const AViewRow: PSQLMarkViewRow
);
begin
  AViewRow.MarkId := AMarkRec.FMarkId;
  AViewRow.Visible := AMarkRec.FVisible;
  AViewRow.ViewId := AMarkRec.FViewId;
end;

{ TSQLMarkGeometryCache }

constructor TSQLMarkGeometryCache.Create(const AMaxRamSize: Integer);
begin
  inherited Create;
  FIsPrepared := False;
  FPreparedCategories := TIDDynArrayObject.Create;
  FGeometryArray := TDynArrayByRecWithPointer.Create(dpIntf, nil, AMaxRamSize);
end;

destructor TSQLMarkGeometryCache.Destroy;
begin
  FGeometryArray.Free;
  FPreparedCategories.Free;
  inherited Destroy;
end;

function TSQLMarkGeometryCache.IsCategoryPrepared(const ACategoryID: TID): Boolean;
begin
  Result := FIsPrepared or FPreparedCategories.Find(ACategoryID);
end;

function TSQLMarkGeometryCache.Find(
  const AMarkID: TID;
  out AGeometry: IGeometryLonLat
): Boolean;
var
  VItem: PRecWithPointer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := FGeometryArray.Find(AMarkID, VItem);
  if Result then begin
    AGeometry := IGeometryLonLat(VItem.Data);
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find ID=%, Result=%, Count=%', [AMarkID, Result, FGeometryArray.Count], Self);
  {$ENDIF}
end;

procedure TSQLMarkGeometryCache.AddOrUpdate(
  const AMarkID: TID;
  const ASize: Integer;
  const AGeometry: IGeometryLonLat
);
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  FGeometryArray.Add(AMarkID, Pointer(AGeometry), ASize, True);
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Add/Update ID=%, Size=%, (New)Count=%', [AMarkID, ASize, FGeometryArray.Count], Self);
  {$ENDIF}
end;

procedure TSQLMarkGeometryCache.AddPrepared(
  const ACategoryID: TID;
  const AArr: TSQLMarkGeometryRecDynArray
);
var
  I: Integer;
  VCount: Integer;
  VArray: TRecWithPointerDynArray;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  VCount := Length(AArr);
  SetLength(VArray, VCount);
  for I := 0 to VCount - 1 do begin
    VArray[I].ID := AArr[I].MarkID;
    VArray[I].Size := AArr[I].Size;
    VArray[I].Data := Pointer(AArr[I].Geometry);
  end;
  FGeometryArray.AddArray(VArray);
  if ACategoryID > 0 then begin
    FPreparedCategories.Add(ACategoryID);
  end else begin
    FIsPrepared := True;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('AddPrepared CategoryID=%, IsPrepared=%, InArrCount=%, Count=%', [ACategoryID, FIsPrepared, VCount, FGeometryArray.Count], Self);
  {$ENDIF}
end;

procedure TSQLMarkGeometryCache.Delete(const AMarkID: TID);
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Delete');
  {$ENDIF}
  FGeometryArray.Delete(AMarkID);
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Del ID=%, NewCount=%', [AMarkID, FGeometryArray.Count], Self);
  {$ENDIF}
end;

procedure TSQLMarkGeometryCache.Reset;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Reset');
  {$ENDIF}
  FGeometryArray.Reset;
  FPreparedCategories.Reset;
end;

end.
