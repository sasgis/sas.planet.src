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

unit u_MarkCategoryDbImplORMCache;

interface

{$I ..\MarkSystemORM.inc}

uses
  Windows,
  SynCommons,
  t_MarkSystemORM,
  u_MarkSystemORMCacheBase;

type
  (****************************************************************************)
  (*                         TSQLCategoryCache                                *)
  (****************************************************************************)

  TSQLCategoryRow = packed record
    CategoryId: TID;
    Name: string;
  end;
  PSQLCategoryRow = ^TSQLCategoryRow;
  TSQLCategoryRowDynArray = array of TSQLCategoryRow;

  TSQLCategoryCache = class(TSQLCacheBase)
  private
    FRows: TSQLCategoryRowDynArray;
  public
    function Find(const AID: TID; out AItem: PSQLCategoryRow): Boolean; overload;
    function Find(const AName: string; out AItem: PSQLCategoryRow): Boolean; overload;
    procedure AddOrUpdate(const ARec: TSQLCategoryRec);
    procedure AddPrepared(const AArr: TSQLCategoryRowDynArray);
  public
    property Rows: TSQLCategoryRowDynArray read FRows;
  public
    constructor Create;
  end;

  (****************************************************************************)
  (*                         TSQLCategoryViewCache                            *)
  (****************************************************************************)

  TSQLCategoryViewRow = packed record
    CategoryId: TID;
    ViewId: TID;
    Visible: Boolean;
    MinZoom: Byte;
    MaxZoom: Byte;
  end;
  PSQLCategoryViewRow = ^TSQLCategoryViewRow;
  TSQLCategoryViewRowDynArray = array of TSQLCategoryViewRow;

  TSQLCategoryViewCache = class(TSQLCacheBase)
  private
    FRows: TSQLCategoryViewRowDynArray;
  public
    function Find(const AID: TID; out AItem: PSQLCategoryViewRow): Boolean;
    procedure AddOrUpdate(const ARec: TSQLCategoryRec);
    procedure AddPrepared(const AArr: TSQLCategoryViewRowDynArray);
  public
    property Rows: TSQLCategoryViewRowDynArray read FRows;
  public
    constructor Create;
  end;

  (****************************************************************************)
  (*                         TSQLCategoryDbCache                              *)
  (****************************************************************************)

  TSQLCategoryDbCache = record
    FCategoryCache: TSQLCategoryCache;
    FCategoryViewCache: TSQLCategoryViewCache;
    procedure Init;
    procedure Done;
  end;

implementation

uses
  SysUtils,
  u_MarkSystemORMLog,
  u_MarkSystemORMModel;

{ TSQLCategoryDbCache }

procedure TSQLCategoryDbCache.Init;
begin
  FCategoryCache := TSQLCategoryCache.Create;
  FCategoryViewCache := TSQLCategoryViewCache.Create;
end;

procedure TSQLCategoryDbCache.Done;
begin
  FreeAndNil(FCategoryViewCache);
  FreeAndNil(FCategoryCache);
end;

{ TSQLCategoryCache }

constructor TSQLCategoryCache.Create;
begin
  inherited Create(TypeInfo(TSQLCategoryRowDynArray), FRows, djInt64, 0);
end;

procedure TSQLCategoryCache.AddOrUpdate(const ARec: TSQLCategoryRec);
var
  I: Integer;
  VSize: Integer;
  VRow: TSQLCategoryRow;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  VSize := Length(ARec.FName) * SizeOf(Char);
  if FRow.FastLocateSorted(ARec.FCategoryId, I) then begin
    // update
    VSize := VSize - Length(FRows[I].Name) * SizeOf(Char);
    FRows[I].Name := ARec.FName;
    Inc(FDataSize, VSize);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Update ID=%, Name=%, Count=%', [ARec.FCategoryId, ARec.FName, FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    VRow.CategoryId := ARec.FCategoryId;
    VRow.Name := ARec.FName;
    FRow.FastAddSorted(I, VRow);
    Inc(FDataSize, VSize);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Add ID=%, Name=%, NewCount=%', [ARec.FCategoryId, ARec.FName, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

procedure TSQLCategoryCache.AddPrepared(const AArr: TSQLCategoryRowDynArray);
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
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('AddPrepared ArrCount=%, NewCount=%', [Length(AArr), FCount], Self);
  {$ENDIF}
end;

function TSQLCategoryCache.Find(const AID: TID; out AItem: PSQLCategoryRow): Boolean;
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

function TSQLCategoryCache.Find(const AName: string; out AItem: PSQLCategoryRow): Boolean;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Find');
  {$ENDIF}
  Result := False;
  for I := 0 to FCount - 1 do begin
    if AnsiSameText(AName, FRows[I].Name) then begin
      AItem := @FRows[I];
      Result := True;
      Break;
    end;
  end;
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('Find Name=%, Result=%, Count=%', [AName, Result, FCount], Self);
  {$ENDIF}
end;

{ TSQLCategoryViewCache }

constructor TSQLCategoryViewCache.Create;
begin
  inherited Create(TypeInfo(TSQLCategoryViewRowDynArray), FRows, djInt64, 0);
end;

procedure TSQLCategoryViewCache.AddOrUpdate(const ARec: TSQLCategoryRec);
var
  I: Integer;
  VRow: TSQLCategoryViewRow;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'AddOrUpdate');
  {$ENDIF}
  CheckCacheSize;
  if FRow.FastLocateSorted(ARec.FCategoryId, I) then begin
    // update
    FRows[I].ViewId := ARec.FViewId;
    FRows[I].Visible := ARec.FVisible;
    FRows[I].MinZoom := ARec.FMinZoom;
    FRows[I].MaxZoom := ARec.FMaxZoom;
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Update CategoryID=%, ViewID=%, Count=%', [ARec.FCategoryId, ARec.FViewId, FCount], Self);
    {$ENDIF}
  end else if I >= 0 then begin
    // add
    VRow.ViewId := ARec.FViewId;
    VRow.CategoryId := ARec.FCategoryId;
    VRow.Visible := ARec.FVisible;
    VRow.MinZoom := ARec.FMinZoom;
    VRow.MaxZoom := ARec.FMaxZoom;
    FRow.FastAddSorted(I, VRow);
    {$IFDEF SQL_LOG_CACHE_RESULT}
    SQLLogCache('Add CategoryID=%, ViewID=%, NewCount=%', [ARec.FCategoryId, ARec.FViewId, FCount], Self);
    {$ENDIF}
  end else begin
    Assert(False);
  end;
end;

function TSQLCategoryViewCache.Find(const AID: TID; out AItem: PSQLCategoryViewRow): Boolean;
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

procedure TSQLCategoryViewCache.AddPrepared(const AArr: TSQLCategoryViewRowDynArray);
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
  {$IFDEF SQL_LOG_CACHE_RESULT}
  SQLLogCache('AddPrepared ArrCount=%, NewCount=%', [Length(AArr), FCount], Self);
  {$ENDIF}
end;

end.
