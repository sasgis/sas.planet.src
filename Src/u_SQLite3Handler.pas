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

unit u_SQLite3Handler;

interface

uses
  Windows,
  SysUtils,
  libsqlite3;

const
  cLogicalSortingCollation = 'LOGICALSORTING';

type
  PSQLite3StmtData = ^TSQLite3StmtData;

  TSQLite3StmtData = record
  public
    Stmt: PSQLite3Stmt;
    Cancelled: Boolean;
  public
    procedure Init; inline;

    function IsNull(const ACol: Integer): Boolean; inline;

    function ColumnInt(const ACol: Integer): Integer; inline;
    function ColumnInt64(const ACol: Integer): Int64; inline;
    function ColumnDouble(const ACol: Integer): Double; inline;
    function ColumnIntDef(const ACol, AValueIfNull: Integer): Integer; inline;
    function ColumnAsString(const ACol: Integer): string;  inline;
    function ColumnAsAnsiString(const ACol: Integer): AnsiString; inline;
    function ColumnBlobSize(const ACol: Integer): Integer; inline;
    function ColumnBlobData(const ACol: Integer): Pointer; inline;
    function ColumnCount: Integer; inline;
    function ColumnName(const ACol: Integer): PUTF8Char; inline;
    function ColumnType(const ACol: Integer): Integer; inline;
    function ColumnDeclType(const ACol: Integer): PUTF8Char; inline;

    function BindInt(const ACol, AValue: Integer): Boolean; inline;
    function BindInt64(const ACol: Integer; const AValue: Int64): Boolean; inline;
    function BindBlob(const ACol: Integer; const ABlob: Pointer; const ASize: Integer): Boolean; inline;
    function BindBlobCopy(const ACol: Integer; const ABlob: Pointer; const ASize: Integer): Boolean; inline;
    function BindText(const ACol: Integer; const AText: PUTF8Char; const ALen: Integer): Boolean; overload; inline;
    function BindTextCopy(const ACol: Integer; const AText: PUTF8Char; const ALen: Integer): Boolean; overload; inline;
    function BindText(const ACol: Integer; const AText: UTF8String): Boolean; overload; inline;
    function BindTextCopy(const ACol: Integer; const AText: UTF8String): Boolean; overload; inline;

    function ClearBindings: Boolean; inline;
    function Reset: Boolean; inline;
    function Fin: Boolean; inline;
  end;

  PSQLite3DbHandler = ^TSQLite3DbHandler;

  TSQLiteOpenStatementProc = procedure (
    const AHandler: PSQLite3DbHandler;
    const ACallbackPtr: Pointer;
    const AStmtData: PSQLite3StmtData
  ) of object;

  TSQLite3DbHandler = record
  private
    FHandle: PSQLite3; // database connection handle
    procedure CheckResult(const AResult: Integer); inline;
    procedure RegisterCollationNeededCallback;
  public
    function Init: Boolean;
    function LibVersionInfo: string;

    // Opening a new database connection
    procedure Open(
      const ADbFileName: string;
      const AOpenFlags: Integer;
      const ASupportLogicalCollation: Boolean = False
    );

    // Closing a database connection
    procedure Close;

    function IsOpened: Boolean; inline;

    procedure ExecSql(
      const ASqlText: UTF8String;
      const ARowsAffectedPtr: PInteger = nil
    ); inline;

    procedure ExecSqlWithBlob(
      const ASqlText: UTF8String;
      const ABufferPtr: Pointer;
      const ABufferLen: Integer;
      const ARowsAffectedPtr: PInteger = nil
    );

    procedure ExecSqlWithText(
      const ASqlText: UTF8String;
      const AWithText: Boolean;
      const ATextBuffer: PUTF8Char;
      const ATextLength: Integer;
      const ARowsAffectedPtr: PInteger = nil
    ); inline;

    function OpenSql(
      const ASqlText: UTF8String;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer;
      const ARaiseOnOpenError: Boolean = True
    ): Integer; inline;

    function OpenSqlWithText(
      const ASqlText: UTF8String;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer;
      const ARaiseOnOpenError: Boolean;
      const AWithText: Boolean;
      const ATextBuffer: PUTF8Char;
      const ATextLength: Integer;
      const ARowsAffectedPtr: PInteger = nil
    ): Integer;

    function DeclareSql(
      const ASqlText: UTF8String;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer;
      const ARaiseOnOpenError: Boolean
    ): Integer;

    function PrepareStatement(
      const AStmtData: PSQLite3StmtData;
      const ASqlText: UTF8String
    ): Boolean; inline;

    function FetchPrepared(
      const AStmtData: PSQLite3StmtData;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer
    ): Boolean; inline;

    function ExecPrepared(
      const AStmtData: PSQLite3StmtData
    ): Boolean; inline;

    function ClosePrepared(
      const AStmtData: PSQLite3StmtData
    ): Boolean; inline;

    function LastInsertedRowId: Int64; inline;

    procedure RaiseSQLite3Error;

    procedure SetBusyTryCount(const ATryCount: Integer);

    procedure SetProgressHandler(
      const nOps: Integer;
      const xProgress: TSQLite3ProgressCallback;
      const pArg: Pointer
    ); inline;

    procedure BeginTransaction; inline;
    procedure RollbackTransaction; inline;
    procedure CommitTransaction; inline;
  end;

  ESQLite3Exception     = class(Exception);
  ESQLite3SimpleError   = class(ESQLite3Exception);
  ESQLite3ErrorWithCode = class(ESQLite3Exception);

implementation

uses
  u_Dialogs,
  u_GlobalDllName;

function LocalSQLiteBusyHandler(ptr: Pointer; count: Integer): Integer; cdecl;
begin
  if count < NativeInt(ptr) then begin
    // repeat
    Sleep(1);
    Result := 1;
  end else begin
    // failed
    Result := 0;
  end;
end;

function CompareLogicallyAnsi(n1: Integer; const z1: PAnsiChar; n2: Integer; const z2: PAnsiChar): Integer;
var
  V1IsInt, V2IsInt: Boolean;
  V1Cursor, V2Cursor: PAnsiChar;
  V1Int, V2Int, VCounter, V1IntCount, V2IntCount: Integer;
  VSingleByte: Byte;
begin
  // Проверка на пустые строки
  if (z1 = nil) or (n1 = 0) then begin
    if (z2 = nil) or (n2 = 0) then begin
      Result := 0;  // z1 = z2
      Exit;
    end else begin
      Result := -1; // z1 < z2
      Exit;
    end;
  end else if (z2 = nil) or (n2 = 0) then begin
    Result := 1;    // z1 > z2
    Exit;
  end;

  V1Cursor := z1;
  V2Cursor := z2;

  while True do begin
    // проверки на конец строки
    if (V1Cursor^ = #0) or (n1 = 0) then begin
      if (V2Cursor^ = #0) or (n2 = 0) then begin
        Result := 0;
        Exit;
      end else begin
        Result := -1;
        Exit;
      end;
    end else if (V2Cursor^ = #0) or (n2 = 0) then begin
      Result := 1;
      Exit;
    end;

    // проверка на начало числа в обеих строках
    V1IsInt := CharInSet(V1Cursor^, ['0'..'9']);
    V2IsInt := CharInSet(V2Cursor^, ['0'..'9']);
    if V1IsInt and not V2IsInt then begin
      Result := -1;
      Exit;
    end else if not V1IsInt and V2IsInt then begin
      Result := 1;
      Exit;
    end;

    // посимвольное сравнение
    if not (V1IsInt and V2IsInt) then begin
      if V1Cursor^ = V2Cursor^ then begin
        Inc(V1Cursor);
        Inc(V2Cursor);
        Dec(n1);
        Dec(n2);
        Continue;
      end;
      if (V1Cursor^ < V2Cursor^) then begin
        Result := -1;
        Exit;
      end else begin
        Result := 1;
        Exit;
      end;
    end;

    // вытаскиваем числа из обоих строк и сравниваем
    V1Int := 0;
    VCounter := 1;
    V1IntCount := 0;
    repeat
      Inc(V1IntCount);
      VSingleByte := Byte(V1Cursor^) - Byte('0');
      V1Int := V1Int * VCounter + VSingleByte;
      Inc(V1Cursor);
      Dec(n1);
      VCounter := 10;
    until not CharInSet(V1Cursor^, ['0'..'9']);

    V2Int := 0;
    VCounter := 1;
    V2IntCount := 0;
    repeat
      VSingleByte := Byte(V2Cursor^) - Byte('0');
      Inc(V2IntCount);
      V2Int := V2Int * VCounter + VSingleByte;
      Inc(V2Cursor);
      Dec(n2);
      VCounter := 10;
    until not CharInSet(V2Cursor^, ['0'..'9']);

    if V1Int = V2Int then begin
      if V1Int = 0 then begin
        if V1IntCount < V2IntCount then begin
          Result := -1;
          Exit;
        end else if V1IntCount > V2IntCount then begin
          Result := 1;
          Exit;
        end;
      end;
      Continue;
    end else if V1Int < V2Int then begin
      Result := -1;
      Exit;
    end else begin
      Result := 1;
      Exit;
    end;
  end;
end;

function LogicalCollactionCompareUTF8(pUser: Pointer; n1: Integer; const z1: Pointer; n2: Integer;
  const z2: Pointer): Integer; cdecl;
begin
  Result := CompareLogicallyAnsi(n1, z1, n2, z2);
end;

function LogicalCollactionCompareUTF16LE(pUser: Pointer; n1: Integer; const z1: Pointer; n2: Integer;
  const z2: Pointer): Integer; cdecl;
begin
  Result := CompareLogicallyAnsi(n1, z1, n2, z2);
end;

function LogicalCollactionCompareUTF16BE(pUser: Pointer; n1: Integer; const z1: Pointer; n2: Integer;
  const z2: Pointer): Integer; cdecl;
begin
  Result := CompareLogicallyAnsi(n1, z1, n2, z2);
end;

procedure Callback4CollationNeeded(pCollNeededArg: Pointer; db: PSQLite3; eTextRep: Integer;
  const zExternal: PAnsiChar); cdecl;
begin
  case eTextRep of
    SQLITE_UTF8: begin
      sqlite3_create_collation(
        db,
        zExternal,
        SQLITE_UTF8,
        nil,
        @LogicalCollactionCompareUTF8
      );
    end;

    SQLITE_UTF16LE: begin
      sqlite3_create_collation(
        db,
        zExternal,
        SQLITE_UTF8,
        nil,
        @LogicalCollactionCompareUTF16LE
      );
    end;

    SQLITE_UTF16BE: begin
      sqlite3_create_collation(
        db,
        zExternal,
        SQLITE_UTF8,
        nil,
        @LogicalCollactionCompareUTF16BE
      );
    end;
  else
    Assert(False, IntToStr(eTextRep));
  end;
end;

{ TSQLite3StmtData }

function TSQLite3StmtData.ColumnAsString(const ACol: Integer): string;
var
  VValue: PAnsiChar;
begin
  VValue := sqlite3_column_text(Stmt, ACol); // return UTF-8
  if VValue = nil then begin
    Result := '';
  end else begin
    Result := UTF8ToString(VValue);
  end;
end;

function TSQLite3StmtData.BindInt(const ACol, AValue: Integer): Boolean;
begin
  Result := sqlite3_bind_int(Stmt, ACol, AValue) = SQLITE_OK;
end;

function TSQLite3StmtData.BindInt64(const ACol: Integer; const AValue: Int64): Boolean;
begin
  Result := sqlite3_bind_int64(Stmt, ACol, AValue) = SQLITE_OK;
end;

function TSQLite3StmtData.BindBlob(const ACol: Integer; const ABlob: Pointer; const ASize: Integer): Boolean;
begin
  Result := sqlite3_bind_blob(Stmt, ACol, ABlob, ASize, SQLITE_STATIC) = SQLITE_OK;
end;

function TSQLite3StmtData.BindBlobCopy(const ACol: Integer; const ABlob: Pointer; const ASize: Integer): Boolean;
begin
  Result := sqlite3_bind_blob(Stmt, ACol, ABlob, ASize, SQLITE_TRANSIENT) = SQLITE_OK;
end;

function TSQLite3StmtData.BindText(const ACol: Integer; const AText: PUTF8Char; const ALen: Integer): Boolean;
begin
  Result := sqlite3_bind_text(Stmt, ACol, AText, ALen, SQLITE_STATIC) = SQLITE_OK;
end;

function TSQLite3StmtData.BindTextCopy(const ACol: Integer; const AText: PUTF8Char; const ALen: Integer): Boolean;
begin
  Result := sqlite3_bind_text(Stmt, ACol, AText, ALen, SQLITE_TRANSIENT) = SQLITE_OK;
end;

function TSQLite3StmtData.BindText(const ACol: Integer; const AText: UTF8String): Boolean;
begin
  Result := sqlite3_bind_text(Stmt, ACol, PUTF8Char(AText), Length(AText), SQLITE_STATIC) = SQLITE_OK;
end;

function TSQLite3StmtData.BindTextCopy(const ACol: Integer; const AText: UTF8String): Boolean;
begin
  Result := sqlite3_bind_text(Stmt, ACol, PUTF8Char(AText), Length(AText), SQLITE_TRANSIENT) = SQLITE_OK;
end;

function TSQLite3StmtData.ClearBindings: Boolean;
begin
  Result := sqlite3_clear_bindings(Stmt) = SQLITE_OK;
end;

function TSQLite3StmtData.ColumnAsAnsiString(const ACol: Integer): AnsiString;
begin
  Result := AnsiString(ColumnAsString(ACol));
end;

function TSQLite3StmtData.ColumnBlobData(const ACol: Integer): Pointer;
begin
  Result := sqlite3_column_blob(Stmt, ACol);
end;

function TSQLite3StmtData.ColumnBlobSize(const ACol: Integer): Integer;
begin
  Result := sqlite3_column_bytes(Stmt, ACol);
end;

function TSQLite3StmtData.ColumnCount: Integer;
begin
  Result := sqlite3_column_count(Stmt);
end;

function TSQLite3StmtData.ColumnDeclType(const ACol: Integer): PUTF8Char;
begin
  Result := sqlite3_column_decltype(Stmt, ACol);
end;

function TSQLite3StmtData.ColumnDouble(const ACol: Integer): Double;
begin
  Result := sqlite3_column_double(Stmt, ACol);
end;

function TSQLite3StmtData.ColumnInt(const ACol: Integer): Integer;
begin
  Result := sqlite3_column_int(Stmt, ACol);
end;

function TSQLite3StmtData.ColumnInt64(const ACol: Integer): Int64;
begin
  Result := sqlite3_column_int64(Stmt, ACol);
end;

function TSQLite3StmtData.ColumnIntDef(const ACol, AValueIfNull: Integer): Integer;
begin
  if IsNull(ACol) then begin
    Result := AValueIfNull;
  end else begin
    Result := sqlite3_column_int(Stmt, ACol);
  end;
end;

function TSQLite3StmtData.ColumnName(const ACol: Integer): PUTF8Char;
begin
  Result := sqlite3_column_name(Stmt, ACol);
end;

function TSQLite3StmtData.ColumnType(const ACol: Integer): Integer;
begin
  Result := sqlite3_column_type(Stmt, ACol);
end;

procedure TSQLite3StmtData.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TSQLite3StmtData.Fin: Boolean;
begin
  Result := sqlite3_finalize(Stmt) = SQLITE_OK;
end;

function TSQLite3StmtData.IsNull(const ACol: Integer): Boolean;
begin
  Result := sqlite3_column_type(Stmt, ACol) = SQLITE_NULL;
end;

function TSQLite3StmtData.Reset: Boolean;
begin
  Result := sqlite3_reset(Stmt) = SQLITE_OK;
end;

{ TSQLite3DbHandler }

procedure TSQLite3DbHandler.BeginTransaction;
begin
  ExecSql('BEGIN TRANSACTION');
end;

procedure TSQLite3DbHandler.RollbackTransaction;
begin
  ExecSql('ROLLBACK TRANSACTION');
end;

procedure TSQLite3DbHandler.CommitTransaction;
begin
  ExecSql('COMMIT TRANSACTION');
end;

procedure TSQLite3DbHandler.CheckResult(const AResult: Integer);
begin
  if AResult <> SQLITE_OK then begin
    RaiseSQLite3Error;
  end;
end;

procedure TSQLite3DbHandler.RaiseSQLite3Error;
begin
  if Assigned(FHandle) then begin
    raise ESQLite3ErrorWithCode.Create(
      sqlite3_errmsg(FHandle) + ' ( error code: ' + IntToStr(sqlite3_errcode(FHandle)) + ')'
    );
  end else begin
    raise ESQLite3SimpleError.Create('SQLite3 error');
  end;
end;

procedure TSQLite3DbHandler.Close;
begin
  if FHandle <> nil then begin
    sqlite3_close(FHandle);
    FHandle := nil;
  end;
end;

function TSQLite3DbHandler.IsOpened: Boolean;
begin
  Result := FHandle <> nil;
end;

function TSQLite3DbHandler.ClosePrepared(const AStmtData: PSQLite3StmtData): Boolean;
begin
  Result := sqlite3_finalize(AStmtData.Stmt) = SQLITE_OK;
end;

function TSQLite3DbHandler.DeclareSql(
  const ASqlText: UTF8String;
  const ACallbackProc: TSQLiteOpenStatementProc;
  const ACallbackPtr: Pointer;
  const ARaiseOnOpenError: Boolean
): Integer;
var
  VStmtData: TSQLite3StmtData;
begin
  VStmtData.Init;

  Result := sqlite3_prepare_v2(FHandle, PUTF8Char(ASqlText), Length(ASqlText), VStmtData.Stmt, nil);

  if Result <> SQLITE_OK then begin
    if ARaiseOnOpenError then begin
      RaiseSQLite3Error;
    end else begin
      Exit;
    end;
  end;

  try
    Result := sqlite3_step(VStmtData.Stmt);

    // SQLITE_ROW
    if Assigned(ACallbackProc) then begin
      ACallbackProc(@Self, ACallbackPtr, @VStmtData);
    end;
  finally
    CheckResult(sqlite3_finalize(VStmtData.Stmt));
  end;
end;

procedure TSQLite3DbHandler.ExecSql(
  const ASqlText: UTF8String;
  const ARowsAffectedPtr: PInteger
);
begin
  OpenSqlWithText(ASqlText, nil, nil, True, False, nil, 0, ARowsAffectedPtr);
end;

procedure TSQLite3DbHandler.ExecSqlWithBlob(
  const ASqlText: UTF8String;
  const ABufferPtr: Pointer;
  const ABufferLen: Integer;
  const ARowsAffectedPtr: PInteger
);
var
  VStmt: PSQLite3Stmt;
begin
  CheckResult(
    sqlite3_prepare_v2(FHandle, PUTF8Char(ASqlText), Length(ASqlText), VStmt, nil)
  );
  try
    CheckResult(
      sqlite3_bind_blob(VStmt, 1, ABufferPtr, ABufferLen, SQLITE_STATIC)
    );
    if not (sqlite3_step(VStmt) in [SQLITE_DONE, SQLITE_ROW]) then begin
      RaiseSQLite3Error;
    end;
    if ARowsAffectedPtr <> nil then begin
      ARowsAffectedPtr^ := sqlite3_changes(FHandle);
    end;
  finally
    CheckResult(sqlite3_finalize(VStmt));
  end;
end;

procedure TSQLite3DbHandler.ExecSqlWithText(
  const ASqlText: UTF8String;
  const AWithText: Boolean;
  const ATextBuffer: PUTF8Char;
  const ATextLength: Integer;
  const ARowsAffectedPtr: PInteger
);
begin
  OpenSqlWithText(ASqlText, nil, nil, True, AWithText, ATextBuffer, ATextLength, ARowsAffectedPtr);
end;

function TSQLite3DbHandler.FetchPrepared(
  const AStmtData: PSQLite3StmtData;
  const ACallbackProc: TSQLiteOpenStatementProc;
  const ACallbackPtr: Pointer
): Boolean;
begin
  Result := sqlite3_step(AStmtData.Stmt) = SQLITE_ROW;

  if not Result then begin
    // SQLITE_DONE or error
    Exit;
  end;

  ACallbackProc(@Self, ACallbackPtr, AStmtData);
end;

function TSQLite3DbHandler.ExecPrepared(const AStmtData: PSQLite3StmtData): Boolean;
begin
  Result :=
    (sqlite3_step(AStmtData.Stmt) = SQLITE_DONE) and
    (sqlite3_reset(AStmtData.Stmt) = SQLITE_OK);
end;

function TSQLite3DbHandler.Init: Boolean;
begin
  FillChar(Self, SizeOf(Self), 0);
  try
    InitLibSQLite3(GDllName.Sqlite3);
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      ShowErrorMessageSync(E.Message);
    end;
  end;
end;

function TSQLite3DbHandler.LastInsertedRowId: Int64;
begin
  Result := sqlite3_last_insert_rowid(FHandle);
end;

function TSQLite3DbHandler.LibVersionInfo: string;
var
  VHandle: THandle;
begin
  VHandle := GetModuleHandle(PChar(GDllName.Sqlite3));
  if (VHandle <> 0) and (Addr(sqlite3_libversion) <> nil) then begin
    Result := string(AnsiString(sqlite3_libversion)) + ' at ' + GetModuleName(VHandle);
  end else begin
    Result := 'not loaded!';
  end;
end;

procedure TSQLite3DbHandler.Open(
  const ADbFileName: string;
  const AOpenFlags: Integer;
  const ASupportLogicalCollation: Boolean
);
var
  VDbFileName: UTF8String;
begin
  Close;
  try
    VDbFileName := Utf8Encode(ADbFileName);
    CheckResult(
      sqlite3_open_v2(PUTF8Char(VDbFileName), FHandle, AOpenFlags, nil)
    );
    if ASupportLogicalCollation then begin
      RegisterCollationNeededCallback;
    end;
  except
    Close;
    raise;
  end;
end;

function TSQLite3DbHandler.OpenSql(
  const ASqlText: UTF8String;
  const ACallbackProc: TSQLiteOpenStatementProc;
  const ACallbackPtr: Pointer;
  const ARaiseOnOpenError: Boolean
): Integer;
begin
  Result := OpenSqlWithText(ASqlText, ACallbackProc, ACallbackPtr, ARaiseOnOpenError, False, nil, 0, nil);
end;

function TSQLite3DbHandler.OpenSqlWithText(
  const ASqlText: UTF8String;
  const ACallbackProc: TSQLiteOpenStatementProc;
  const ACallbackPtr: Pointer;
  const ARaiseOnOpenError: Boolean;
  const AWithText: Boolean;
  const ATextBuffer: PUTF8Char;
  const ATextLength: Integer;
  const ARowsAffectedPtr: PInteger
): Integer;
var
  VStmtData: TSQLite3StmtData;
begin
  VStmtData.Init;

  Result := sqlite3_prepare_v2(FHandle, PUTF8Char(ASqlText), Length(ASqlText), VStmtData.Stmt, nil);

  if Result <> SQLITE_OK then begin
    if ARaiseOnOpenError then begin
      RaiseSQLite3Error;
    end else begin
      Exit;
    end;
  end;

  try
    if AWithText then begin
      CheckResult(
        sqlite3_bind_text(VStmtData.Stmt, 1, ATextBuffer, ATextLength, SQLITE_STATIC)
      );
    end;

    repeat
      Result := sqlite3_step(VStmtData.Stmt);

      if Result <> SQLITE_ROW then begin
        if Result = SQLITE_DONE then begin
          if ARowsAffectedPtr <> nil then begin
            ARowsAffectedPtr^ := sqlite3_changes(FHandle);
          end;
          Break;
        end;
        RaiseSQLite3Error;
      end;

      // SQLITE_ROW
      if Assigned(ACallbackProc) then begin
        ACallbackProc(@Self, ACallbackPtr, @VStmtData);
        if VStmtData.Cancelled then begin
          Break;
        end;
      end;

    until False;
  finally
    CheckResult(sqlite3_finalize(VStmtData.Stmt));
  end;
end;

function TSQLite3DbHandler.PrepareStatement(
  const AStmtData: PSQLite3StmtData;
  const ASqlText: UTF8String
): Boolean;
begin
  AStmtData.Init;
  Result := sqlite3_prepare_v2(FHandle, PUTF8Char(ASqlText), Length(ASqlText), AStmtData.Stmt, nil) = SQLITE_OK;
end;

procedure TSQLite3DbHandler.RegisterCollationNeededCallback;
begin
  sqlite3_collation_needed(FHandle, nil {@Self}, @Callback4CollationNeeded);
end;

procedure TSQLite3DbHandler.SetBusyTryCount(const ATryCount: Integer);
begin
  sqlite3_busy_handler(FHandle, LocalSQLiteBusyHandler, Pointer(ATryCount));
end;

procedure TSQLite3DbHandler.SetProgressHandler(
  const nOps: Integer;
  const xProgress: TSQLite3ProgressCallback;
  const pArg: Pointer
);
begin
  sqlite3_progress_handler(FHandle, nOps, xProgress, pArg);
end;

end.
