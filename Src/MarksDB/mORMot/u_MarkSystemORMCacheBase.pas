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

unit u_MarkSystemORMCacheBase;

interface

{$I MarkSystemORM.inc}

uses
  SysUtils,
  SynCommons,
  t_MarkSystemORM;

type
  (****************************************************************************)
  (*                            TSQLCacheBase                                 *)
  (****************************************************************************)

  TSQLCacheBase = class
  protected
    FCount: Integer;
    FRow: TDynArray;
    FIsPrepared: Boolean;
    FDataSize: Int64;
    FMaxCacheRamUsed: Int64;
    function GetSize: Int64;
    function GetIsSorted: Boolean; inline;
    procedure CheckCacheSize; inline;
    procedure DeleteByIndex(const AIndex: Integer); inline;
  public
    procedure Sort;
    procedure Reset;
    procedure Delete(const AID: TID); virtual;
  public
    property Count: Integer read FCount;
    property IsPrepared: Boolean read FIsPrepared;
    property IsSorted: Boolean read GetIsSorted;
    property Size: Int64 read GetSize;
  public
    constructor Create(
      const ATypeInfo: pointer;
      var AValue;
      const AKind: TDynArrayKind;
      const AMaxCacheRamUsed: Int64
    );
    destructor Destroy; override;
  end;

  (****************************************************************************)
  (*                TSQLCacheBaseWithPreparedByCategory                       *)
  (****************************************************************************)

  TSQLCacheBaseWithPreparedByCategory = class(TSQLCacheBase)
  protected
    FPreparedCategoriesCount: Integer;
    FPreparedCategories: TIDDynArray;
    FPreparedCategoriesDynArr: TDynArray;
  public
    function IsCategoryPrepared(const ACategoryID: TID): Boolean;
  public
    constructor Create(
      const ATypeInfo: pointer;
      var AValue;
      const AKind: TDynArrayKind;
      const AMaxCacheRamUsed: Int64
    );
    destructor Destroy; override;
  end;

  (****************************************************************************)
  (*                         TIDDynArrayObject                                *)
  (****************************************************************************)

  TIDDynArrayObject = class
  private
    FCount: Integer;
    FArray: TIDDynArray;
    FDynArray: TDynArray;
  public
    function Find(const AID: TID): Boolean;
    procedure Add(const AID: TID);
    procedure AddArray(
      const AArray: TIDDynArray;
      const AStartIndex: Integer = 0;
      const ACount: Integer = -1
    );
    procedure Delete(const AID: TID);
    procedure Reset;
  public
    property IDArray: TIDDynArray read FArray;
    property Count: Integer read FCount;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  (****************************************************************************)
  (*                     TDynArrayByRecWithPointer                            *)
  (****************************************************************************)

  TRecWithPointer = packed record
    ID: TID;
    Size: Integer;
    Data: Pointer;
  end;
  PRecWithPointer = ^TRecWithPointer;
  TRecWithPointerDynArray = array of TRecWithPointer;

  TRecWithPointerKnownTypes = (dpCustom, dpObj, dpIntf);

  TOnDynArrayItemDelete = procedure(var AData: Pointer) of object;

  TDynArrayByRecWithPointer = class
  private
    FCount: Integer;
    FArray: TRecWithPointerDynArray;
    FDynArray: TDynArray;
    FOnDelete: TOnDynArrayItemDelete;
    FDataType: TRecWithPointerKnownTypes;
    FDataSize: Int64;
    FMaxCacheRamUsed: Int64;
    function GetSize: Int64;
    procedure CheckCacheSize; inline;
  public
    function Find(const AID: TID; out ARec: PRecWithPointer): Boolean;
    procedure Add(
      const AID: TID;
      const AData: Pointer;
      const ASize: Integer;
      const AUpdateIfExists: Boolean
    );
    procedure AddArray(const AArray: TRecWithPointerDynArray);
    procedure Delete(const AID: TID);
    procedure Reset;
  public
    property Rows: TRecWithPointerDynArray read FArray;
    property Count: Integer read FCount;
    property Size: Int64 read GetSize;
  public
    constructor Create(
      const ADataType: TRecWithPointerKnownTypes;
      const AOnItemDelete: TOnDynArrayItemDelete;
      const AMaxCacheRamUsed: Int64
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  u_MarkSystemORMLog;

{ TSQLCacheBase }

constructor TSQLCacheBase.Create(
  const ATypeInfo: pointer;
  var AValue;
  const AKind: TDynArrayKind;
  const AMaxCacheRamUsed: Int64
);
begin
  inherited Create;
  FMaxCacheRamUsed := AMaxCacheRamUsed;
  FDataSize := 0;
  FIsPrepared := False;
  FCount := 0;
  FRow.InitSpecific(ATypeInfo, AValue, AKind, @FCount);
  FRow.Sorted := True;
end;

destructor TSQLCacheBase.Destroy;
begin
  Reset;
  inherited Destroy;
end;

function TSQLCacheBase.GetSize: Int64;
begin
  Result := InstanceSize + Integer(FRow.ElemSize) * FCount + FDataSize;
end;

procedure TSQLCacheBase.Reset;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Reset');
  {$ENDIF}
  FRow.Clear;
  FRow.Sorted := True;
  FDataSize := 0;
  FIsPrepared := False;
end;

procedure TSQLCacheBase.CheckCacheSize;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'CheckCacheSize');
  {$ENDIF}
  if FMaxCacheRamUsed > 0 then begin
    if Self.Size > FMaxCacheRamUsed then begin
      {$IFDEF SQL_LOG_CACHE_SIZE}
      SQLLogInfo('Init auto-reset: CurSize=%, MaxSize=%', [Self.Size, FMaxCacheRamUsed], Self);
      {$ENDIF}
      Reset;
    end;
  end;
  {$IFDEF SQL_LOG_CACHE_SIZE}
  SQLLogDebug('CurSize=%, MaxSize=%', [Self.Size, FMaxCacheRamUsed], Self);
  {$ENDIF}
end;

function TSQLCacheBase.GetIsSorted: Boolean;
begin
  Result := FRow.Sorted;
end;

procedure TSQLCacheBase.Sort;
begin
  FRow.Sort;
end;

procedure TSQLCacheBase.DeleteByIndex(const AIndex: Integer);
begin
  Assert(AIndex >= 0);
  Assert(AIndex < FCount);
  Assert(FRow.Sorted);
  FRow.FastDeleteSorted(AIndex);
end;

procedure TSQLCacheBase.Delete(const AID: TID);
var
  I: Integer;
begin
  I := FRow.Find(AID);
  if I >= 0 then begin
    DeleteByIndex(I);
  end;
end;

{ TSQLCacheBaseWithPreparedByCategory }

constructor TSQLCacheBaseWithPreparedByCategory.Create(
  const ATypeInfo: Pointer;
  var AValue;
  const AKind: TDynArrayKind;
  const AMaxCacheRamUsed: Int64
);
begin
  inherited Create(ATypeInfo, AValue, AKind, AMaxCacheRamUsed);
  FPreparedCategoriesCount := 0;
  FPreparedCategoriesDynArr.InitSpecific(
    TypeInfo(TIDDynArray), FPreparedCategories, djInt64, @FPreparedCategoriesCount
  );
  FPreparedCategoriesDynArr.Sorted := True;
end;

destructor TSQLCacheBaseWithPreparedByCategory.Destroy;
begin
  FPreparedCategoriesDynArr.Clear;
  inherited Destroy;
end;

function TSQLCacheBaseWithPreparedByCategory.IsCategoryPrepared(const ACategoryID: TID): Boolean;
begin
  Result := (FPreparedCategoriesDynArr.Find(ACategoryID) >= 0);
end;

{ TIDDynArrayObject }

constructor TIDDynArrayObject.Create;
begin
  inherited Create;
  FCount := 0;
  FDynArray.InitSpecific(TypeInfo(TIDDynArray), FArray, djInt64, @FCount);
  FDynArray.Sorted := True;
end;

destructor TIDDynArrayObject.Destroy;
begin
  Reset;
  inherited Destroy;
end;

function TIDDynArrayObject.Find(const AID: TID): Boolean;
begin
  Result := FDynArray.Find(AID) >= 0;
end;

procedure TIDDynArrayObject.Add(const AID: TID);
begin
  FDynArray.FastLocateOrAddSorted(AID);
end;

procedure TIDDynArrayObject.AddArray(
  const AArray: TIDDynArray;
  const AStartIndex: Integer;
  const ACount: Integer
);
var
  I, J: Integer;
  VSize: Integer;
  VCount: Integer;
  VBits: TBits;
begin
  if ACount < 0 then begin
    VSize := Length(AArray) - AStartIndex;
  end else begin
    VSize := ACount;
  end;
  if VSize <= 0 then begin
    Exit;
  end;
  if FCount = 0 then begin
    FDynArray.AddArray(AArray, AStartIndex, ACount);
    FDynArray.Sort;
  end else begin
    VBits := TBits.Create;
    try
      VCount := 0;
      VBits.Size := VSize;
      for I := 0 to VSize - 1 do begin
        if FDynArray.Find(AArray[AStartIndex+I]) < 0 then begin
          VBits[I] := True;
          Inc(VCount);
        end;
      end;
      if VCount = VSize then begin
        FDynArray.AddArray(AArray, AStartIndex, ACount);
        FDynArray.Sort;
      end else if VCount > 0 then begin
        FDynArray.Capacity := FCount + VCount;
        for I := 0 to VSize - 1 do begin
          if VBits[I] then begin
            J := FDynArray.New;
            FArray[J] := AArray[AStartIndex+I];
          end;
        end;
        FDynArray.Sort;
      end;
    finally
      VBits.Free;
    end;
  end;
end;

procedure TIDDynArrayObject.Delete(const AID: TID);
var
  I: Integer;
begin
  I := FDynArray.Find(AID);
  if I >= 0 then begin
    FDynArray.Delete(I);
    FDynArray.Sorted := True;
  end;
end;

procedure TIDDynArrayObject.Reset;
begin
  FDynArray.Clear;
  FDynArray.Sorted := True;
end;

{ TDynArrayByRecWithPointer }

constructor TDynArrayByRecWithPointer.Create(
  const ADataType: TRecWithPointerKnownTypes;
  const AOnItemDelete: TOnDynArrayItemDelete;
  const AMaxCacheRamUsed: Int64
);
begin
  inherited Create;
  FDataType := ADataType;
  FOnDelete := AOnItemDelete;
  FMaxCacheRamUsed := AMaxCacheRamUsed;
  if FDataType = dpCustom then begin
    Assert(Assigned(FOnDelete));
  end;
  FCount := 0;
  FDynArray.InitSpecific(TypeInfo(TRecWithPointerDynArray), FArray, djInt64, @FCount);
  FDynArray.Sorted := True;
end;

destructor TDynArrayByRecWithPointer.Destroy;
begin
  Reset;
  inherited Destroy;
end;

function TDynArrayByRecWithPointer.Find(const AID: TID; out ARec: PRecWithPointer): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := FDynArray.Find(AID);
  if I >= 0 then begin
    ARec := @FArray[I];
    Result := True;
  end;
end;

procedure TDynArrayByRecWithPointer.Add(
  const AID: TID;
  const AData: Pointer;
  const ASize: Integer;
  const AUpdateIfExists: Boolean
);
var
  I: Integer;
  VRec: TRecWithPointer;
begin
  CheckCacheSize;
  if not FDynArray.FastLocateSorted(AID, I) then begin
    if I >= 0 then begin
      VRec.ID := AID;
      VRec.Size := ASize;
      VRec.Data := AData;
      FDynArray.Insert(I, VRec);
      if Assigned(AData) then begin
        if FDataType = dpIntf then begin
          IInterface(FArray[I].Data)._AddRef;
        end;
        Inc(FDataSize, ASize);
      end;
      FDynArray.Sorted := True;
    end else begin
      Assert(False);
    end;
  end else if AUpdateIfExists then begin
    if Assigned(FArray[I].Data) then begin
      if Assigned(FOnDelete) then begin
        FOnDelete(FArray[I].Data);
      end else begin
        case FDataType of
          dpObj: begin
            TObject(FArray[I].Data).Free;
            FArray[I].Data := nil;
          end;
          dpIntf: begin
            IInterface(FArray[I].Data)._Release;
            FArray[I].Data := nil;
          end;
        else
          Assert(False);
        end;
      end;
      Dec(FDataSize, FArray[I].Size);
    end;
    FArray[I].Data := AData;
    FArray[I].Size := ASize;
    if Assigned(AData) then begin
      if FDataType = dpIntf then begin
        IInterface(FArray[I].Data)._AddRef;
      end;
      Inc(FDataSize, ASize);
    end;
  end;
end;

procedure TDynArrayByRecWithPointer.AddArray(const AArray: TRecWithPointerDynArray);

  procedure AddFull;
  var
    I: Integer;
  begin
    FDynArray.AddArray(AArray);
    FDynArray.Sort;
    for I := 0 to Length(AArray) - 1 do begin
      if AArray[I].Data <> nil then begin
        if FDataType = dpIntf then begin
          IInterface(AArray[I].Data)._AddRef;
        end;
        Inc(FDataSize, AArray[I].Size);
      end;
    end;
  end;

var
  I, J: Integer;
  VSize: Integer;
  VCount: Integer;
  VBits: TBits;
begin
  CheckCacheSize;
  VSize := Length(AArray);
  if VSize <= 0 then begin
    Exit;
  end;
  if FCount = 0 then begin
    AddFull;
  end else begin
    VBits := TBits.Create;
    try
      VCount := 0;
      VBits.Size := VSize;
      for I := 0 to VSize - 1 do begin
        if FDynArray.Find(AArray[I].ID) < 0 then begin
          VBits[I] := True;
          Inc(VCount);
        end;
      end;
      if VCount = VSize then begin
        AddFull;
      end else if VCount > 0 then begin
        FDynArray.Capacity := FCount + VCount;
        for I := 0 to VSize - 1 do begin
          if VBits[I] then begin
            J := FDynArray.New;
            FArray[J] := AArray[I];
            if FArray[J].Data <> nil then begin
              if FDataType = dpIntf then begin
                IInterface(FArray[J].Data)._AddRef;
              end;
              Inc(FDataSize, FArray[J].Size);
            end;
          end;
        end;
        FDynArray.Sort;
      end;
    finally
      VBits.Free;
    end;
  end;
end;

procedure TDynArrayByRecWithPointer.Delete(const AID: TID);
var
  I: Integer;
begin
  I := FDynArray.Find(AID);
  if I >= 0 then begin
    if Assigned(FArray[I].Data) then begin
      if Assigned(FOnDelete) then begin
        FOnDelete(FArray[I].Data);
      end else begin
        case FDataType of
          dpObj: begin
            TObject(FArray[I].Data).Free;
            FArray[I].Data := nil;
          end;
          dpIntf: begin
            IInterface(FArray[I].Data)._Release;
            FArray[I].Data := nil;
          end;
        else
          Assert(False);
        end;
      end;
      Dec(FDataSize, FArray[I].Size);
    end;
    FDynArray.Delete(I);
    FDynArray.Sorted := True;
  end;
end;

procedure TDynArrayByRecWithPointer.Reset;
var
  I: Integer;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'Reset');
  {$ENDIF}
  if Assigned(FOnDelete) then begin
    for I := 0 to FCount - 1 do begin
      if Assigned(FArray[I].Data) then begin
        FOnDelete(FArray[I].Data);
      end;
    end;
  end else begin
    case FDataType of
      dpObj: begin
        for I := 0 to FCount - 1 do begin
          if Assigned(FArray[I].Data) then begin
            TObject(FArray[I].Data).Free;
            FArray[I].Data := nil;
          end;
        end;
      end;
      dpIntf: begin
        for I := 0 to FCount - 1 do begin
          if Assigned(FArray[I].Data) then begin
            IUnknown(FArray[I].Data)._Release;
            FArray[I].Data := nil;
          end;
        end;
      end;
    else
      Assert(False);
    end;
  end;
  FDynArray.Clear;
  FDynArray.Sorted := True;
  FDataSize := 0;
end;

function TDynArrayByRecWithPointer.GetSize: Int64;
begin
  Result := InstanceSize + Integer(FDynArray.ElemSize) * FCount + FDataSize;
end;

procedure TDynArrayByRecWithPointer.CheckCacheSize;
begin
  {$IFDEF SQL_LOG_CACHE_ENTER}
  SQLLogEnter(Self, 'CheckCacheSize');
  {$ENDIF}
  if FMaxCacheRamUsed > 0 then begin
    if Self.Size > FMaxCacheRamUsed then begin
      {$IFDEF SQL_LOG_CACHE_SIZE}
      SQLLogInfo('Init auto-reset: CurSize=%, MaxSize=%', [Self.Size, FMaxCacheRamUsed], Self);
      {$ENDIF}
      Reset;
    end;
  end;
  {$IFDEF SQL_LOG_CACHE_SIZE}
  SQLLogDebug('CurSize=%, MaxSize=%', [Self.Size, FMaxCacheRamUsed], Self);
  {$ENDIF}
end;

end.
