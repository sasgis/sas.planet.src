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

uses
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
  VRow: PSQLCategoryRow;
begin
  VSize := Length(ARec.FName) * SizeOf(Char);
  if FRow.FastLocateSorted(ARec.FCategoryId, I) then begin
    // update
    VSize := VSize - Length(FRows[I].Name) * SizeOf(Char);
    FRows[I].Name := ARec.FName;
    Inc(FDataSize, VSize);
  end else if I >= 0 then begin
    // add
    CheckCacheSize;
    New(VRow);
    try
      VRow.CategoryId := ARec.FCategoryId;
      VRow.Name := ARec.FName;
      FRow.Insert(I, VRow^);
      FRow.Sorted := True;
      Inc(FDataSize, VSize);
    finally
      Dispose(VRow);
    end;
  end else begin
    Assert(False);
  end;
end;

procedure TSQLCategoryCache.AddPrepared(const AArr: TSQLCategoryRowDynArray);
begin
  Reset;
  FRow.AddArray(AArr);
  FRow.Sort;
  FIsPrepared := True;
end;

function TSQLCategoryCache.Find(const AID: TID; out AItem: PSQLCategoryRow): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := FRow.Find(AID);
  if I >=0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
end;

function TSQLCategoryCache.Find(const AName: string; out AItem: PSQLCategoryRow): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FCount - 1 do begin
    if SameText(AName, FRows[I].Name) then begin
      AItem := @FRows[I];
      Result := True;
      Break;
    end;
  end;
end;

{ TSQLCategoryViewCache }

constructor TSQLCategoryViewCache.Create;
begin
  inherited Create(TypeInfo(TSQLCategoryViewRowDynArray), FRows, djInt64, 0);
end;

procedure TSQLCategoryViewCache.AddOrUpdate(const ARec: TSQLCategoryRec);
var
  I: Integer;
  VRow: PSQLCategoryViewRow;
begin
  if FRow.FastLocateSorted(ARec.FCategoryId, I) then begin
    // update
    FRows[I].Visible := ARec.FVisible;
    FRows[I].MinZoom := ARec.FMinZoom;
    FRows[I].MaxZoom := ARec.FMaxZoom;
  end else if I >= 0 then begin
    // add
    CheckCacheSize;
    New(VRow);
    try
      VRow.ViewId := ARec.FViewId;
      VRow.CategoryId := ARec.FCategoryId;
      VRow.Visible := ARec.FVisible;
      VRow.MinZoom := ARec.FMinZoom;
      VRow.MaxZoom := ARec.FMaxZoom;

      FRow.Insert(I, VRow^);
      FRow.Sorted := True;
    finally
      Dispose(VRow);
    end;
  end else begin
    Assert(False);
  end;
end;

function TSQLCategoryViewCache.Find(const AID: TID; out AItem: PSQLCategoryViewRow): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := FRow.Find(AID);
  if I >=0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
end;

procedure TSQLCategoryViewCache.AddPrepared(const AArr: TSQLCategoryViewRowDynArray);
begin
  Reset;
  FRow.AddArray(AArr);
  FRow.Sort;
  FIsPrepared := True;
end;

end.
