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
  mORMot,
  SynCommons,
  t_MarkSystemORM;

type
  TSQLCacheBase = class
  protected
    FCount: Integer;
    FRow: TDynArray;
    FAutoSort: Boolean;
    FIsPrepared: Boolean;
    FIndex: TIntegerDynArray;
    FCompare: TDynArraySortCompare;
    function GetCount: Integer;
    procedure SetAutoSort(const AValue: Boolean);
  public
    procedure Sort;
    procedure Delete(const AID: TID);
    property Count: Integer read GetCount;
    property IsPrepared: Boolean read FIsPrepared;
    property AutoSort: Boolean read FAutoSort write SetAutoSort;
  public
    constructor Create;
    destructor Destroy; override;
  end;

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
    function FillPrepare(
      const AClient: TSQLRestClient;
      const AForceUpdate: Boolean = False
    ): Integer;
    function Find(const AID: TID; out AItem: PSQLCategoryRow): Boolean; overload;
    function Find(const AName: string; out AItem: PSQLCategoryRow): Boolean; overload;
    procedure AddOrUpdate(const ARec: TSQLCategoryRec);
    property Rows: TSQLCategoryRowDynArray read FRows;
  public
    constructor Create;
  end;

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
    constructor Create;
  public
    function FillPrepare(
      const AClient: TSQLRestClient;
      const AUserID: TID;
      const AForceUpdate: Boolean = False
    ): Integer;
    function Find(const AID: TID; out AItem: PSQLCategoryViewRow): Boolean;
    procedure AddOrUpdate(const ARec: TSQLCategoryRec);
    property Rows: TSQLCategoryViewRowDynArray read FRows;
  end;

implementation

uses
  SysUtils,
  u_MarkSystemORMModel;

{ TSQLCacheBase }

constructor TSQLCacheBase.Create;
begin
  inherited Create;
  FAutoSort := True;
  FIsPrepared := False;
  FCount := 0;
  FCompare := @SortDynArrayInt64;
  SetLength(FIndex, 0);
end;

destructor TSQLCacheBase.Destroy;
begin
  FRow.Clear;
  inherited Destroy;
end;

function TSQLCacheBase.GetCount: Integer;
begin
  Result := FRow.Count;
end;

procedure TSQLCacheBase.Sort;
var
  VCount: Integer;
begin
  VCount := FRow.Count;
  SetLength(FIndex, VCount);
  FillIncreasing(Pointer(FIndex), 0, VCount);
  FRow.CreateOrderedIndex(FIndex, FCompare);
end;

procedure TSQLCacheBase.Delete(const AID: TID);
var
  I: Integer;
begin
  I := FRow.Find(AID, FIndex, FCompare);
  if I >= 0 then begin
    FRow.Delete(I);
    if FAutoSort then begin
      Sort;
    end;
  end;
end;

procedure TSQLCacheBase.SetAutoSort(const AValue: Boolean);
begin
  if FAutoSort <> AValue then begin
    FAutoSort := AValue;
    if FAutoSort then begin
      Sort;
    end;
  end;
end;

{ TSQLCategoryCache }

constructor TSQLCategoryCache.Create;
begin
  inherited Create;
  SetLength(FRows, 0);
  FRow.InitSpecific(TypeInfo(TSQLCategoryRowDynArray), FRows, djInt64, @FCount);
end;

procedure TSQLCategoryCache.AddOrUpdate(const ARec: TSQLCategoryRec);
var
  I: Integer;
  VAdd: Boolean;
begin
  VAdd := False;
  I := FRow.Find(ARec.FCategoryId, FIndex, FCompare);

  if I < 0 then begin
    VAdd := True;
    I := FRow.New;
    FRows[I].CategoryId := ARec.FCategoryId;
  end;

  FRows[I].Name := ARec.FName;

  if VAdd and FAutoSort then begin
    Sort;
  end;
end;

function TSQLCategoryCache.Find(const AID: TID; out AItem: PSQLCategoryRow): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := FRow.Find(AID, FIndex, FCompare);
  if I >= 0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
end;

function TSQLCategoryCache.Find(const AName: string; out AItem: PSQLCategoryRow): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FRow.Count - 1 do begin
    if SameText(AName, FRows[I].Name) then begin
      AItem := @FRows[I];
      Result := True;
      Break;
    end;
  end;
end;

function TSQLCategoryCache.FillPrepare(
  const AClient: TSQLRestClient;
  const AForceUpdate: Boolean
): Integer;
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
begin
  if not FIsPrepared or AForceUpdate then begin
    FIsPrepared := False;
    FRow.Clear;

    VList := AClient.ExecuteList(
      [TSQLCategory],
      'SELECT ID,Name FROM Category'
    );

    if Assigned(VList) then
    try
      FIsPrepared := True;
      VCount := VList.RowCount;
      if VCount > 0 then begin
        FRow.Capacity := VCount;
        for I := 1 to VCount do begin
          J := FRow.New;
          FRows[J].CategoryId := VList.GetAsInt64(I, 0);
          FRows[J].Name := VList.GetString(I, 1);
        end;
        Sort;
      end;
    finally
      VList.Free;
    end;
  end;

  Result := FRow.Count;
end;

{ TSQLCategoryViewCache }

constructor TSQLCategoryViewCache.Create;
begin
  inherited Create;
  SetLength(FRows, 0);
  FRow.InitSpecific(TypeInfo(TSQLCategoryViewRowDynArray), FRows, djInt64, @FCount);
end;

procedure TSQLCategoryViewCache.AddOrUpdate(const ARec: TSQLCategoryRec);
var
  I: Integer;
  VAdd: Boolean;
begin
  VAdd := False;
  I := FRow.Find(ARec.FCategoryId, FIndex, FCompare);

  if I < 0 then begin
    VAdd := True;
    I := FRow.New;
    FRows[I].ViewId := ARec.FViewId;
    FRows[I].CategoryId := ARec.FCategoryId;
  end;

  FRows[I].Visible := ARec.FVisible;
  FRows[I].MinZoom := ARec.FMinZoom;
  FRows[I].MaxZoom := ARec.FMaxZoom;

  if VAdd and FAutoSort then begin
    Sort;
  end;
end;

function TSQLCategoryViewCache.Find(const AID: TID; out AItem: PSQLCategoryViewRow): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := FRow.Find(AID, FIndex, FCompare);
  if I >= 0 then begin
    AItem := @FRows[I];
    Result := True;
  end;
end;

function TSQLCategoryViewCache.FillPrepare(
  const AClient: TSQLRestClient;
  const AUserID: TID;
  const AForceUpdate: Boolean
): Integer;
var
  I, J: Integer;
  VCount: Integer;
  VList: TSQLTableJSON;
begin
  if not FIsPrepared or AForceUpdate then begin
    FIsPrepared := False;
    FRow.Clear;

    VList := AClient.ExecuteList(
      [TSQLCategoryView],
      FormatUTF8('SELECT ID,Category,Visible,MinZoom,MaxZoom FROM CategoryView WHERE User=?', [], [AUserID])
    );

    if Assigned(VList) then
    try
      FIsPrepared := True;
      VCount := VList.RowCount;
      if VCount > 0 then begin
        FRow.Capacity := VCount;
        for I := 1 to VCount do begin
          J := FRow.New;
          FRows[J].ViewId := VList.GetAsInt64(I, 0);
          FRows[J].CategoryId := VList.GetAsInt64(I, 1);
          FRows[J].Visible := VList.GetAsInteger(I, 2) <> 0;
          FRows[J].MaxZoom := VList.GetAsInteger(I, 3);
          FRows[J].MinZoom := VList.GetAsInteger(I, 4);
        end;
        Sort;
      end;
    finally
      VList.Free;
    end;
  end;

  Result := FRow.Count;
end;

end.
