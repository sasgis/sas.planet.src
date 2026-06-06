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

unit u_MarkCategoryDbImplORMCache;

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
  u_MarkSystemORMCacheBase;

type
  (****************************************************************************)
  (*                         TOrmCategoryCache                                *)
  (****************************************************************************)

  TOrmCategoryRow = packed record
    CategoryId: TID;
    Name: string;
  end;
  POrmCategoryRow = ^TOrmCategoryRow;
  TOrmCategoryRowDynArray = array of TOrmCategoryRow;

  TOrmCategoryCache = class(TOrmCacheBase)
  private
    FRows: TOrmCategoryRowDynArray;
  public
    function Find(const AID: TID; out AItem: POrmCategoryRow): Boolean; overload;
    function Find(const AName: string; out AItem: POrmCategoryRow): Boolean; overload;
    procedure AddOrUpdate(const ARec: TOrmCategoryRec);
    procedure AddPrepared(const AArr: TOrmCategoryRowDynArray);
  public
    property Rows: TOrmCategoryRowDynArray read FRows;
  public
    constructor Create(const AMaxCacheSize: Int64);
  end;

  (****************************************************************************)
  (*                         TOrmCategoryViewCache                            *)
  (****************************************************************************)

  TOrmCategoryViewRow = packed record
    CategoryId: TID;
    ViewId: TID;
    Visible: Boolean;
    MinZoom: Byte;
    MaxZoom: Byte;
  end;
  POrmCategoryViewRow = ^TOrmCategoryViewRow;
  TOrmCategoryViewRowDynArray = array of TOrmCategoryViewRow;

  TOrmCategoryViewCache = class(TOrmCacheBase)
  private
    FRows: TOrmCategoryViewRowDynArray;
  public
    function Find(const AID: TID; out AItem: POrmCategoryViewRow): Boolean;
    procedure AddOrUpdate(const ARec: TOrmCategoryRec);
    procedure AddPrepared(const AArr: TOrmCategoryViewRowDynArray);
  public
    property Rows: TOrmCategoryViewRowDynArray read FRows;
  public
    constructor Create(const AMaxCacheSize: Int64);
  end;

  (****************************************************************************)
  (*                         TOrmCategoryDbCache                              *)
  (****************************************************************************)

  TOrmCategoryDbCache = record
    FCategoryCache: TOrmCategoryCache;
    FCategoryViewCache: TOrmCategoryViewCache;
    procedure Init;
    procedure Done;
  end;

implementation

uses
  SysUtils,
  u_MarkSystemORMLog,
  u_MarkSystemORMModel;

{ TOrmCategoryDbCache }

procedure TOrmCategoryDbCache.Init;
begin
  FCategoryCache := TOrmCategoryCache.Create(CUnlimCacheSize);
  FCategoryViewCache := TOrmCategoryViewCache.Create(CUnlimCacheSize);
end;

procedure TOrmCategoryDbCache.Done;
begin
  FreeAndNil(FCategoryViewCache);
  FreeAndNil(FCategoryCache);
end;

{ TOrmCategoryCache }

constructor TOrmCategoryCache.Create(const AMaxCacheSize: Int64);
begin
  inherited Create(TypeInfo(TOrmCategoryRowDynArray), FRows, ptInt64, AMaxCacheSize);
end;

procedure TOrmCategoryCache.AddOrUpdate(const ARec: TOrmCategoryRec);
var
  I: Integer;
  VSize: Integer;
  VRow: TOrmCategoryRow;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  VSize := Length(ARec.FName) * SizeOf(Char);
  if FRow.FastLocateSorted(ARec.FCategoryId, I) then begin
    // update
    VSize := VSize - Length(FRows[I].Name) * SizeOf(Char);
    FRows[I].Name := ARec.FName;
    Inc(FDataSize, VSize);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Update ID=%, Name=%, Count=%', [ARec.FCategoryId, StringToUtf8(ARec.FName), FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    VRow.CategoryId := ARec.FCategoryId;
    VRow.Name := ARec.FName;
    FRow.FastAddSorted(I, VRow);
    Inc(FDataSize, VSize);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Add ID=%, Name=%, NewCount=%', [ARec.FCategoryId, StringToUtf8(ARec.FName), FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

procedure TOrmCategoryCache.AddPrepared(const AArr: TOrmCategoryRowDynArray);
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
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('AddPrepared ArrCount=%, NewCount=%', [Length(AArr), FCount], Self);
  {$ENDIF}
end;

function TOrmCategoryCache.Find(const AID: TID; out AItem: POrmCategoryRow): Boolean;
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

function TOrmCategoryCache.Find(const AName: string; out AItem: POrmCategoryRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  for I := 0 to FCount - 1 do begin
    if AnsiSameText(AName, FRows[I].Name) then begin
      AItem := @FRows[I];
      Result := True;
      Break;
    end;
  end;
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('Find Name=%, Result=%, Count=%', [StringToUtf8(AName), Result, FCount], Self);
  {$ENDIF}
end;

{ TOrmCategoryViewCache }

constructor TOrmCategoryViewCache.Create(const AMaxCacheSize: Int64);
begin
  inherited Create(TypeInfo(TOrmCategoryViewRowDynArray), FRows, ptInt64, AMaxCacheSize);
end;

procedure TOrmCategoryViewCache.AddOrUpdate(const ARec: TOrmCategoryRec);
var
  I: Integer;
  VRow: TOrmCategoryViewRow;
begin
  {$IFDEF ORM_LOG_CACHE_ENTER}
  OrmLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  if FRow.FastLocateSorted(ARec.FCategoryId, I) then begin
    // update
    FRows[I].ViewId := ARec.FViewId;
    FRows[I].Visible := ARec.FVisible;
    FRows[I].MinZoom := ARec.FMinZoom;
    FRows[I].MaxZoom := ARec.FMaxZoom;
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Update CategoryID=%, ViewID=%, Count=%', [ARec.FCategoryId, ARec.FViewId, FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    VRow.ViewId := ARec.FViewId;
    VRow.CategoryId := ARec.FCategoryId;
    VRow.Visible := ARec.FVisible;
    VRow.MinZoom := ARec.FMinZoom;
    VRow.MaxZoom := ARec.FMaxZoom;
    FRow.FastAddSorted(I, VRow);
    {$IFDEF ORM_LOG_CACHE_RESULT}
    OrmLogCache('Add CategoryID=%, ViewID=%, NewCount=%', [ARec.FCategoryId, ARec.FViewId, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

function TOrmCategoryViewCache.Find(const AID: TID; out AItem: POrmCategoryViewRow): Boolean;
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

procedure TOrmCategoryViewCache.AddPrepared(const AArr: TOrmCategoryViewRowDynArray);
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
  {$IFDEF ORM_LOG_CACHE_RESULT}
  OrmLogCache('AddPrepared ArrCount=%, NewCount=%', [Length(AArr), FCount], Self);
  {$ENDIF}
end;

end.
