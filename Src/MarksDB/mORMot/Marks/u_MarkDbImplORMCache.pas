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

unit u_MarkDbImplORMCache;

interface

{$I ..\MarkSystemORM.inc}

uses
  Windows,
  mormot.core.base,
  mormot.core.data,
  mormot.core.rtti,
  {$IFDEF ORM_LOG_ENABLE}
  mormot.core.unicode,
  {$ENDIF}
  t_MarkSystemORM,
  i_GeometryLonLat,
  u_MarkSystemORMModel,
  u_MarkSystemORMCacheBase;

type
  (****************************************************************************)
  (*                         TOrmMarkImageCache                               *)
  (****************************************************************************)

  TOrmMarkImageRow = packed record
    ImageId: TID;
    Name: string;
  end;
  POrmMarkImageRow = ^TOrmMarkImageRow;
  TOrmMarkImageRowDynArray = array of TOrmMarkImageRow;

  TOrmMarkImageCache = class(TOrmCacheBase)
  private
    FRows: TOrmMarkImageRowDynArray;
  public
    function Find(const AID: TID; out AItem: POrmMarkImageRow): Boolean; overload;
    function Find(const AName: string; out AItem: POrmMarkImageRow): Boolean; overload;
    procedure AddOrIgnore(const ARec: TOrmMarkRec); overload;
    procedure AddOrIgnore(const AImageID: TID; const AName: string); overload;
    procedure Delete(const AID: TID); override;
  public
    property Rows: TOrmMarkImageRowDynArray read FRows;
  public
    constructor Create(const AMaxCacheSize: Int64);
  end;

  (****************************************************************************)
  (*                       TOrmMarkAppearanceCache                            *)
  (****************************************************************************)

  TOrmMarkAppearanceRow = packed record
    AppearanceId: TID;
    Color1: Cardinal;
    Color2: Cardinal;
    Scale1: Integer;
    Scale2: Integer;
  end;
  POrmMarkAppearanceRow = ^TOrmMarkAppearanceRow;
  TOrmMarkAppearanceRowDynArray = array of TOrmMarkAppearanceRow;

  TOrmMarkAppearanceCache = class(TOrmCacheBase)
  private
    FRows: TOrmMarkAppearanceRowDynArray;
  public
    function Find(const AID: TID; out AItem: POrmMarkAppearanceRow): Boolean; overload;
    function Find(const AColor1, AColor2: Cardinal; const AScale1, AScale2: Integer;
      out AItem: POrmMarkAppearanceRow): Boolean; overload;
    procedure AddOrIgnore(const ARec: TOrmMarkRec); overload;
    procedure AddOrIgnore(
      const AID: TID;
      const AColor1, AColor2: Cardinal;
      const AScale1, AScale2: Integer
    ); overload;
  public
    property Rows: TOrmMarkAppearanceRowDynArray read FRows;
  public
    constructor Create(const AMaxCacheSize: Int64);
  end;

  (****************************************************************************)
  (*                           TOrmMarkViewCache                              *)
  (****************************************************************************)

  TOrmMarkViewRow = packed record
    MarkId: TID;
    ViewId: TID;
    CategoryID: TID;
    Visible: Boolean;
  end;
  POrmMarkViewRow = ^TOrmMarkViewRow;
  TOrmMarkViewRowDynArray = array of TOrmMarkViewRow;

  TOrmMarkViewCache = class(TOrmCacheBaseWithPreparedByCategory)
  private
    FRows: TOrmMarkViewRowDynArray;
  public
    function Find(const AMarkID: TID; out AItem: POrmMarkViewRow): Boolean;
    procedure AddOrUpdate(const ARec: TOrmMarkRec); overload;
    procedure AddOrUpdate(const AMarkID, AViewID, ACategoryID: TID; const AVisible: Boolean); overload;
    procedure AddPrepared(const ACategoryID: TID; const AArr: TOrmMarkViewRowDynArray);
  public
    property Rows: TOrmMarkViewRowDynArray read FRows;
  public
    constructor Create(const AMaxCacheSize: Int64);
  end;

  (****************************************************************************)
  (*                            TOrmMarkIdIndex                               *)
  (****************************************************************************)

  TOrmMarkIdIndexRec = packed record
    MarkId: TID;
    CategoryId: TID;
    ImageId: TID;
    AppearanceId: TID;
  end;
  POrmMarkIdIndexRec = ^TOrmMarkIdIndexRec;
  TOrmMarkIdIndexRecDynArray = array of TOrmMarkIdIndexRec;

  TOrmMarkIdIndex = class(TOrmCacheBase)
  private
    FRows: TOrmMarkIdIndexRecDynArray;
  public
    function Find(const AID: TID; out AItem: POrmMarkIdIndexRec): Boolean;
    procedure AddOrUpdate(const ARec: TOrmMarkRec);
    procedure AddPrepared(const AArr: TOrmMarkIdIndexRecDynArray);
    procedure AddArray(
      const AArray: TOrmMarkIdIndexRecDynArray;
      const AStartIndex: Integer = 0;
      const ACount: Integer = -1
    );
    class procedure MarkRecToIndexRec(
      const AMarkRec: TOrmMarkRec;
      const AIndexRec: POrmMarkIdIndexRec
    ); inline;
  public
    property Rows: TOrmMarkIdIndexRecDynArray read FRows;
  public
    constructor Create(const AMaxCacheSize: Int64);
  end;

  (****************************************************************************)
  (*                       TOrmMarkIdByCategoryIndex                          *)
  (****************************************************************************)

  TOrmMarkIdByCategoryIndex = class
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
    procedure Reset;
  public
    constructor Create(const AMaxCacheSize: Int64);
    destructor Destroy; override;
  end;

  (****************************************************************************)
  (*                            TOrmMarkCache                                 *)
  (****************************************************************************)

  TOrmMarkRow = packed record
    MarkId: TID;
    Name: string;
    Desc: string;
    GeoType: TOrmGeoType;
    GeoCount: Integer;
  end;
  POrmMarkRow = ^TOrmMarkRow;
  TOrmMarkRowDynArray = array of TOrmMarkRow;

  TOrmMarkCache = class(TOrmCacheBaseWithPreparedByCategory)
  private
    FRows: TOrmMarkRowDynArray;
  public
    function Find(const AID: TID; out AItem: POrmMarkRow): Boolean;
    procedure AddOrUpdate(const ARec: TOrmMarkRec);
    procedure AddPrepared(const ACategoryID: TID; const AArr: TOrmMarkRowDynArray);
    class procedure MarkRecToMarkRow(
      const AMarkRec: TOrmMarkRec;
      out AMarkRow: TOrmMarkRow
    ); inline;
  public
    property Rows: TOrmMarkRowDynArray read FRows;
  public
    constructor Create(const AMaxCacheSize: Int64);
  end;

  (****************************************************************************)
  (*                        TOrmMarkGeometryCache                             *)
  (****************************************************************************)

  TOrmMarkGeometryRec = packed record
    MarkID: TID;
    Size: Integer;
    Geometry: IGeometryLonLat;
  end;
  POrmMarkGeometryRec = ^TOrmMarkGeometryRec;
  TOrmMarkGeometryRecDynArray = array of TOrmMarkGeometryRec;

  TOrmMarkGeometryCache = class
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
      const AArr: TOrmMarkGeometryRecDynArray
    );
    procedure Delete(const AMarkID: TID);
    procedure Reset;
  public
    property IsPrepared: Boolean read FIsPrepared;
  public
    constructor Create(const AMaxCacheSize: Int64);
    destructor Destroy; override;
  end;

  (****************************************************************************)
  (*                            TOrmMarkDbCache                               *)
  (****************************************************************************)

  TOrmMarkDbCache = record
    FMarkCache: TOrmMarkCache;
    FMarkGeometryCache: TOrmMarkGeometryCache;
    FMarkViewCache: TOrmMarkViewCache;
    FMarkImageCache: TOrmMarkImageCache;
    FMarkAppearanceCache: TOrmMarkAppearanceCache;

    FMarkIdIndex: TOrmMarkIdIndex;
    FMarkIdByCategoryIndex: TOrmMarkIdByCategoryIndex;

    procedure Init(const AMaxCacheSize: Int64);
    procedure Done;
  end;

implementation

uses
  Classes,
  SysUtils,
  u_MarkSystemORMLog;

{ TOrmMarkDbCache }

procedure TOrmMarkDbCache.Init(const AMaxCacheSize: Int64);
var
  VMarkCacheSize: Int64;
  VMarkGeometryCacheSize: Int64;
begin
  VMarkCacheSize := Round(0.3 * AMaxCacheSize); // 30 %
  VMarkGeometryCacheSize := Round(0.7 * AMaxCacheSize); // 70%

  FMarkCache := TOrmMarkCache.Create(VMarkCacheSize);
  FMarkGeometryCache := TOrmMarkGeometryCache.Create(VMarkGeometryCacheSize);

  // no limits for View/Image/Appearance caches
  FMarkViewCache := TOrmMarkViewCache.Create(CUnlimCacheSize);
  FMarkImageCache := TOrmMarkImageCache.Create(CUnlimCacheSize);
  FMarkAppearanceCache := TOrmMarkAppearanceCache.Create(CUnlimCacheSize);

  // and no limits for indexes
  FMarkIdIndex := TOrmMarkIdIndex.Create(CUnlimCacheSize);
  FMarkIdByCategoryIndex := TOrmMarkIdByCategoryIndex.Create(CUnlimCacheSize);
end;

procedure TOrmMarkDbCache.Done;
begin
  FreeAndNil(FMarkIdByCategoryIndex);
  FreeAndNil(FMarkIdIndex);
  FreeAndNil(FMarkAppearanceCache);
  FreeAndNil(FMarkImageCache);
  FreeAndNil(FMarkViewCache);
  FreeAndNil(FMarkGeometryCache);
  FreeAndNil(FMarkCache);
end;

{ TOrmMarkImageCache }

constructor TOrmMarkImageCache.Create(const AMaxCacheSize: Int64);
begin
  inherited Create(
    TypeInfo(TOrmMarkImageRowDynArray), FRows, ptInt64, AMaxCacheSize
  );
end;

function TOrmMarkImageCache.Find(const AID: TID; out AItem: POrmMarkImageRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AID);
  if I >= 0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find ID=%, Result=%, Count=%', [AID, Result, FCount], Self);
  {$ENDIF}
end;

function TOrmMarkImageCache.Find(const AName: string; out AItem: POrmMarkImageRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  for I := 0 to FCount - 1 do begin
    AItem := @FRows[I];
    if AnsiSameText(AItem.Name, AName) then begin
      Result := True;
      Break;
    end;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find Name=%, Result=%, Count=%', [StringToUtf8(AName), Result, FCount], Self);
  {$ENDIF}
end;

procedure TOrmMarkImageCache.AddOrIgnore(const ARec: TOrmMarkRec);
begin
  AddOrIgnore(ARec.FPicId, ARec.FPicName);
end;

procedure TOrmMarkImageCache.AddOrIgnore(const AImageID: TID; const AName: string);
var
  I: Integer;
  VRec: TOrmMarkImageRow;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddOrIgnore');
  {$ENDIF}
  CheckCacheSize;
  if FRow.FastLocateSorted(AImageID, I) then begin
    // ignore
    Exit;
  end else if I >= 0 then begin
    // add
    VRec.ImageId := AImageID;
    VRec.Name := AName;
    FRow.FastAddSorted(I, VRec);
    Inc(FDataSize, Length(VRec.Name)*SizeOf(Char));
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Add ID=%, Name=%, NewCount=%', [AImageID, StringToUtf8(AName), FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

procedure TOrmMarkImageCache.Delete(const AID: TID);
var
  I: Integer;
  VItemDataSize: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Delete');
  {$ENDIF}
  I := FRow.Find(AID);
  if I >= 0 then begin
    VItemDataSize := Length(FRows[I].Name)*SizeOf(Char);
    DeleteByIndex(I);
    Dec(FDataSize, VItemDataSize);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Del ID=%, NewCount=%', [AID, FCount], Self);
    {$ENDIF}
  end;
end;

{ TOrmMarkAppearanceCache }

constructor TOrmMarkAppearanceCache.Create(const AMaxCacheSize: Int64);
begin
  inherited Create(
    TypeInfo(TOrmMarkAppearanceRowDynArray), FRows, ptInt64, AMaxCacheSize
  );
end;

function TOrmMarkAppearanceCache.Find(
  const AID: TID;
  out AItem: POrmMarkAppearanceRow
): Boolean;
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AID);
  if I >= 0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find ID=%, Result=%, Count=%', [AID, Result, FCount], Self);
  {$ENDIF}
end;

function TOrmMarkAppearanceCache.Find(
  const AColor1, AColor2: Cardinal;
  const AScale1, AScale2: Integer;
  out AItem: POrmMarkAppearanceRow
): Boolean;
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
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
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find Result=%, Count=%', [Result, FCount], Self);
  {$ENDIF}
end;

procedure TOrmMarkAppearanceCache.AddOrIgnore(const ARec: TOrmMarkRec);
begin
  AddOrIgnore(ARec.FAppearanceId, ARec.FColor1, ARec.FColor2, ARec.FScale1, ARec.FScale2);
end;

procedure TOrmMarkAppearanceCache.AddOrIgnore(
  const AID: TID;
  const AColor1, AColor2: Cardinal;
  const AScale1, AScale2: Integer
);
var
  I: Integer;
  VRec: TOrmMarkAppearanceRow;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddOrIgnore');
  {$ENDIF}
  CheckCacheSize;
  if FRow.FastLocateSorted(AID, I) then begin
    // ignore
    Exit;
  end else if I >= 0 then begin
    // add
    VRec.AppearanceId := AID;
    VRec.Color1 := AColor1;
    VRec.Color2 := AColor2;
    VRec.Scale1 := AScale1;
    VRec.Scale2 := AScale2;
    FRow.FastAddSorted(I, VRec);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Add ID=%, NewCount=%', [AID, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

{ TOrmMarkIdIndex }

constructor TOrmMarkIdIndex.Create(const AMaxCacheSize: Int64);
begin
  inherited Create(
    TypeInfo(TOrmMarkIdIndexRecDynArray), FRows, ptInt64, AMaxCacheSize
  );
end;

function TOrmMarkIdIndex.Find(const AID: TID; out AItem: POrmMarkIdIndexRec): Boolean;
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AID);
  if I >= 0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find ID=%, Result=%, Count=%', [AID, Result, FCount], Self);
  {$ENDIF}
end;

procedure TOrmMarkIdIndex.AddOrUpdate(const ARec: TOrmMarkRec);
var
  I: Integer;
  VRec: TOrmMarkIdIndexRec;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  if FRow.FastLocateSorted(ARec.FMarkId, I) then begin
    // update
    FRows[I].CategoryId := ARec.FCategoryId;
    FRows[I].ImageId := ARec.FPicId;
    FRows[I].AppearanceId := ARec.FAppearanceId;
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Update ID=%, Count=%', [ARec.FMarkId, FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    MarkRecToIndexRec(ARec, @VRec);
    FRow.FastAddSorted(I, VRec);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Add ID=%, NewCount=%', [ARec.FMarkId, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

procedure TOrmMarkIdIndex.AddPrepared(const AArr: TOrmMarkIdIndexRecDynArray);
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  Reset;
  I := Length(AArr);
  if I = 1 then begin
    FRow.FastAddSorted(0, AArr[0]);
  end else if I > 1 then begin
    FRow.AddArray(AArr);
    FRow.Sort;
  end;
  FIsPrepared := True;
end;

procedure TOrmMarkIdIndex.AddArray(
  const AArray: TOrmMarkIdIndexRecDynArray;
  const AStartIndex: Integer;
  const ACount: Integer
);
var
  I, J: Integer;
  VSize: Integer;
  VCount: Integer;
  VBits: TBits;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddArray');
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

class procedure TOrmMarkIdIndex.MarkRecToIndexRec(
  const AMarkRec: TOrmMarkRec;
  const AIndexRec: POrmMarkIdIndexRec
);
begin
  AIndexRec.MarkId := AMarkRec.FMarkId;
  AIndexRec.CategoryId := AMarkRec.FCategoryId;
  AIndexRec.ImageId := AMarkRec.FPicId;
  AIndexRec.AppearanceId := AMarkRec.FAppearanceId;
end;

{ TOrmMarkIdByCategoryIndex }

constructor TOrmMarkIdByCategoryIndex.Create(const AMaxCacheSize: Int64);
begin
  inherited Create;
  FDynArray := TDynArrayByRecWithPointer.Create(dpObj, nil, AMaxCacheSize);
end;

destructor TOrmMarkIdByCategoryIndex.Destroy;
begin
  FDynArray.Free;
  inherited Destroy;
end;

function TOrmMarkIdByCategoryIndex.Find(
  const ACategoryID: TID;
  out AMarkIDArray: TIDDynArray;
  out AMarkIDArrayCount: Integer
): Boolean;
var
  VItem: PRecWithPointer;
  VArray: TIDDynArrayObject;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  if FDynArray.Find(ACategoryID, VItem) then begin
    Assert(VItem.Data <> nil);
    VArray := TIDDynArrayObject(VItem.Data);
    AMarkIDArray := VArray.IDArray;
    AMarkIDArrayCount := VArray.Count;
    Result := True;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find CategoryID=%, Result=%, OutArrCount=%', [ACategoryID, Result, AMarkIDArrayCount], Self);
  {$ENDIF}
end;

procedure TOrmMarkIdByCategoryIndex.AddPrepared(
  const ACategoryID: TID;
  const AMarkIDArray: TIDDynArray;
  const AStartIndex: Integer;
  const ACount: Integer
);
var
  VItem: PRecWithPointer;
  VArray: TIDDynArrayObject;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  VArrayCount: Integer;
  {$ENDIF}
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  if FDynArray.Find(ACategoryID, VItem) then begin
    Assert(VItem.Data <> nil);
    VArray := TIDDynArrayObject(VItem.Data);
    VArray.Reset;
    VArray.AddArray(AMarkIDArray, AStartIndex, ACount);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    VArrayCount := VArray.Count;
    {$ENDIF}
  end else begin
    VArray := TIDDynArrayObject.Create;
    try
      VArray.AddArray(AMarkIDArray, AStartIndex, ACount);
      {$IFDEF ORM_LOG_CACHE_RESULT}
      VArrayCount := VArray.Count;
      {$ENDIF}
      FDynArray.Add(ACategoryID, Pointer(VArray), 0, True);
      VArray := nil;
    finally
      VArray.Free;
    end;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('AddPrepared CategoryID=%, MarksCountNew=%, Count=%', [ACategoryID, VArrayCount, FDynArray.Count], Self);
  {$ENDIF}
end;

procedure TOrmMarkIdByCategoryIndex.Add(const ACategoryID: TID; const AMarkID: TID);
var
  VItem: PRecWithPointer;
  VArray: TIDDynArrayObject;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Add');
  {$ENDIF}
  if FDynArray.Find(ACategoryID, VItem) then begin
    Assert(VItem.Data <> nil);
    VArray := TIDDynArrayObject(VItem.Data);
    VArray.Add(AMarkID);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Add CategoryID=%, MarkID=%, MarksCountNew=%', [ACategoryID, AMarkID, VArray.Count], Self);
    {$ENDIF}
  end;
end;

procedure TOrmMarkIdByCategoryIndex.Delete(const ACategoryID: TID; const AMarkID: TID);
var
  VItem: PRecWithPointer;
  VArray: TIDDynArrayObject;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Delete');
  {$ENDIF}
  if FDynArray.Find(ACategoryID, VItem) then begin
    Assert(VItem.Data <> nil);
    VArray := TIDDynArrayObject(VItem.Data);
    VArray.Delete(AMarkID);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Del CategoryID=%, MarkID=%, MarksCountNew=%', [ACategoryID, AMarkID, VArray.Count], Self);
    {$ENDIF}
  end;
end;

procedure TOrmMarkIdByCategoryIndex.Reset;
begin
  FDynArray.Reset;
end;

{ TOrmMarkCache }

constructor TOrmMarkCache.Create(const AMaxCacheSize: Int64);
begin
  inherited Create(TypeInfo(TOrmMarkRowDynArray), FRows, ptInt64, AMaxCacheSize);
end;

function TOrmMarkCache.Find(const AID: TID; out AItem: POrmMarkRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AID);
  if I >= 0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find ID=%, Result=%, Count=%', [AID, Result, FCount], Self);
  {$ENDIF}
end;

procedure TOrmMarkCache.AddOrUpdate(const ARec: TOrmMarkRec);
var
  I: Integer;
  VSize: Integer;
  VRow: TOrmMarkRow;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  VSize := (Length(ARec.FName) + Length(ARec.FDesc)) * SizeOf(Char);
  if FRow.FastLocateSorted(ARec.FMarkId, I) then begin
    // update
    VSize := VSize - (Length(FRows[I].Name) + Length(FRows[I].Desc)) * SizeOf(Char);
    MarkRecToMarkRow(ARec, FRows[I]);
    Inc(FDataSize, VSize);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Update ID=%, SizeDiff=%, Count=%', [ARec.FMarkId, VSize, FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    MarkRecToMarkRow(ARec, VRow);
    FRow.FastAddSorted(I, VRow);
    Inc(FDataSize, VSize);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Add ID=%, Size=%, NewCount=%', [ARec.FMarkId, VSize, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

procedure TOrmMarkCache.AddPrepared(const ACategoryID: TID; const AArr: TOrmMarkRowDynArray);
var
  I, J: Integer;
  VSize: Integer;
  VCount: Integer;
  VBits: TBits;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  CheckCacheSize;
  VCount := Length(AArr);
  if ACategoryID > 0 then begin
    if not FPreparedCategoriesDynArr.FastLocateSorted(ACategoryID, I) then begin
      if I >= 0 then begin
        FPreparedCategoriesDynArr.FastAddSorted(I, ACategoryID);
        {$IFDEF ORM_LOG_CACHE_RESULT}
        OrmLogCache('PreparedCategoryID=%, PreparedCount=%', [ACategoryID, FPreparedCategoriesDynArr.Count], Self);
        {$ENDIF}
      end else begin
        Assert(False);
        Exit;
      end;
    end;
    if VCount = 0 then begin
      Exit;
    end;
    VBits := TBits.Create;
    try
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
    if VCount = 1 then begin
      FRow.FastAddSorted(0, AArr[0]);
    end else if VCount > 1 then begin
      FRow.AddArray(AArr);
      FRow.Sort;
    end;
    FIsPrepared := True;
    for I := 0 to VCount - 1 do begin
      VSize := (Length(AArr[I].Name) + Length(AArr[I].Desc)) * SizeOf(Char);
      Inc(FDataSize, VSize);
    end;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('AddPrepared CategoryID=%, IsPrepared=%, InArrCount=%, Count=%', [ACategoryID, FIsPrepared, VCount, FCount], Self);
  {$ENDIF}
end;

class procedure TOrmMarkCache.MarkRecToMarkRow(
  const AMarkRec: TOrmMarkRec;
  out AMarkRow: TOrmMarkRow
);
begin
  AMarkRow.MarkId := AMarkRec.FMarkId;
  AMarkRow.Name := AMarkRec.FName;
  AMarkRow.Desc := AMarkRec.FDesc;
  AMarkRow.GeoType := AMarkRec.FGeoType;
  AMarkRow.GeoCount := AMarkRec.FGeoCount;
end;

{ TOrmMarkViewCache }

constructor TOrmMarkViewCache.Create(const AMaxCacheSize: Int64);
begin
  inherited Create(
    TypeInfo(TOrmMarkViewRowDynArray), FRows, ptInt64, AMaxCacheSize
  );
end;

function TOrmMarkViewCache.Find(const AMarkID: TID; out AItem: POrmMarkViewRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  I := FRow.Find(AMarkID);
  if I >= 0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find ID=%, Result=%, Count=%', [AMarkID, Result, FCount], Self);
  {$ENDIF}
end;

procedure TOrmMarkViewCache.AddOrUpdate(const ARec: TOrmMarkRec);
begin
  AddOrUpdate(ARec.FMarkId, ARec.FViewId, ARec.FCategoryId, ARec.FVisible);
end;

procedure TOrmMarkViewCache.AddOrUpdate(
  const AMarkID, AViewID, ACategoryID: TID;
  const AVisible: Boolean
);
var
  I: Integer;
  VRow: TOrmMarkViewRow;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  if FRow.FastLocateSorted(AMarkID, I) then begin
    // update
    FRows[I].ViewId := AViewID;
    FRows[I].CategoryID := ACategoryID;
    FRows[I].Visible := AVisible;
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Update AMarkID=%, Count=%', [AMarkID, FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    VRow.MarkId := AMarkID;
    VRow.ViewId := AViewID;
    VRow.CategoryID := ACategoryID;
    VRow.Visible := AVisible;
    FRow.FastAddSorted(I, VRow);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Add AMarkID=%, NewCount=%', [AMarkID, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

procedure TOrmMarkViewCache.AddPrepared(const ACategoryID: TID; const AArr: TOrmMarkViewRowDynArray);
var
  I, J: Integer;
  VCount: Integer;
  VBits: TBits;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddPrepared');
  {$ENDIF}
  CheckCacheSize;
  VCount := Length(AArr);
  if ACategoryID > 0 then begin
    if not FPreparedCategoriesDynArr.FastLocateSorted(ACategoryID, I) then begin
      if I >= 0 then begin
        FPreparedCategoriesDynArr.FastAddSorted(I, ACategoryID);
        {$IFDEF ORM_LOG_CACHE_RESULT}
        OrmLogCache('PreparedCategoryID=%, PreparedCount=%', [ACategoryID, FPreparedCategoriesDynArr.Count], Self);
        {$ENDIF}
      end else begin
        Assert(False);
        Exit;
      end;
    end;
    if VCount <= 0 then begin
      Exit;
    end;
    VBits := TBits.Create;
    try
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
    if VCount = 1 then begin
      FRow.FastAddSorted(0, AArr[0]);
    end else if VCount > 1 then begin
      FRow.AddArray(AArr);
      FRow.Sort;
    end;
    FIsPrepared := True;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('AddPrepared CategoryID=%, IsPrepared=%, InArrCount=%, Count=%', [ACategoryID, FIsPrepared, VCount, FCount], Self);
  {$ENDIF}
end;

{ TOrmMarkGeometryCache }

constructor TOrmMarkGeometryCache.Create(const AMaxCacheSize: Int64);
begin
  inherited Create;
  FIsPrepared := False;
  FPreparedCategories := TIDDynArrayObject.Create;
  FGeometryArray := TDynArrayByRecWithPointer.Create(dpIntf, nil, AMaxCacheSize);
end;

destructor TOrmMarkGeometryCache.Destroy;
begin
  FGeometryArray.Free;
  FPreparedCategories.Free;
  inherited Destroy;
end;

function TOrmMarkGeometryCache.IsCategoryPrepared(const ACategoryID: TID): Boolean;
begin
  Result := FIsPrepared or FPreparedCategories.Find(ACategoryID);
end;

function TOrmMarkGeometryCache.Find(
  const AMarkID: TID;
  out AGeometry: IGeometryLonLat
): Boolean;
var
  VItem: PRecWithPointer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := FGeometryArray.Find(AMarkID, VItem);
  if Result then begin
    AGeometry := IGeometryLonLat(VItem.Data);
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find ID=%, Result=%, Count=%', [AMarkID, Result, FGeometryArray.Count], Self);
  {$ENDIF}
end;

procedure TOrmMarkGeometryCache.AddOrUpdate(
  const AMarkID: TID;
  const ASize: Integer;
  const AGeometry: IGeometryLonLat
);
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  FGeometryArray.Add(AMarkID, Pointer(AGeometry), ASize, True);
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Add/Update ID=%, Size=%, (New)Count=%', [AMarkID, ASize, FGeometryArray.Count], Self);
  {$ENDIF}
end;

procedure TOrmMarkGeometryCache.AddPrepared(
  const ACategoryID: TID;
  const AArr: TOrmMarkGeometryRecDynArray
);
var
  I: Integer;
  VCount: Integer;
  VArray: TRecWithPointerDynArray;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddPrepared');
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
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('AddPrepared CategoryID=%, IsPrepared=%, InArrCount=%, Count=%', [ACategoryID, FIsPrepared, VCount, FGeometryArray.Count], Self);
  {$ENDIF}
end;

procedure TOrmMarkGeometryCache.Delete(const AMarkID: TID);
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Delete');
  {$ENDIF}
  FGeometryArray.Delete(AMarkID);
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Del ID=%, NewCount=%', [AMarkID, FGeometryArray.Count], Self);
  {$ENDIF}
end;

procedure TOrmMarkGeometryCache.Reset;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Reset');
  {$ENDIF}
  FGeometryArray.Reset;
  FPreparedCategories.Reset;
end;

end.
