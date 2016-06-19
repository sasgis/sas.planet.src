unit SQLite3Handler;

interface

uses
  Windows,
  SysUtils,
  AlSqlite3Wrapper,
  Classes;

{$IFNDEF UNICODE}
type
  UnicodeString = WideString;
{$ENDIF}

const
  c_SQLite_Ext = '.sqlitedb';
  cLogicalSortingCollation = 'LOGICALSORTING';

  SQLITE_OPEN_READONLY  = AlSqlite3Wrapper.SQLITE_OPEN_READONLY;
  SQLITE_OPEN_READWRITE = AlSqlite3Wrapper.SQLITE_OPEN_READWRITE;
  SQLITE_OPEN_CREATE    = AlSqlite3Wrapper.SQLITE_OPEN_CREATE;

type
  PSQLite3StmtData = ^TSQLite3StmtData;
  TSQLite3StmtData = record
  public
    Stmt: PSQLite3Stmt;
    Cancelled: Boolean;
  public
    procedure Init;
    function IsNull(const iCol: Integer): Boolean; //inline;
    function ColumnInt(const iCol: Integer): Integer; //inline;
    function ColumnInt64(const iCol: Integer): Int64; //inline;
    function ColumnDouble(const iCol: Integer): Double; //inline;
    function ColumnIntDef(const iCol, AValueIfNull: Integer): Integer;
    function ColumnAsString(const iCol: Integer): string;
    function ColumnAsAnsiString(const iCol: Integer): AnsiString;
    function ColumnAsWideString(const iCol: Integer): UnicodeString;
    function ColumnBlobSize(const iCol: Integer): Integer;
    function ColumnBlobData(const iCol: Integer): Pointer;
    function ColumnCount: Integer;
    function ColumnName(const iCol: Integer): PAnsiChar;
    function ColumnType(const iCol: Integer): Integer;
    function ColumnDeclType(const iCol: Integer): PAnsiChar;
  end;

  PSQLite3DbHandler = ^TSQLite3DbHandler;

  TSQLiteOpenStatementProc = procedure (
    const AHandler: PSQLite3DbHandler;
    const ACallbackPtr: Pointer;
    const AStmtData: PSQLite3StmtData
  ) of object;

  // подключение к БД SQLite
  TSQLite3DbHandler = record
  private
    Sqlite3Handle: PSQLite3;
  private
    procedure RegisterCollationNeededCallback;
  public
    function Init: Boolean;
    function LibVersionInfo: string;

    procedure Open(
      const ADbFileName: String;
      const AOpenFlags: Integer;
      const ASupportLogicalCollation: Boolean = False
    );
    procedure OpenW(
      const ADbFileName: WideString;
      const ASupportLogicalCollation: Boolean = False
    );
    procedure Close;
    function Opened: Boolean; inline;
    function Closed: Boolean; inline;

    procedure ExecSQL(
      const ASQLText: AnsiString;
      const ARowsAffectedPtr: PInteger = nil
    ); inline;

    procedure ExecSQLWithBLOB(
      const ASQLText: AnsiString;
      const ABufferPtr: Pointer;
      const ABufferLen: Integer;
      const ARowsAffectedPtr: PInteger = nil
    );

    procedure ExecSQLWithTEXTW(
      const ASQLText: AnsiString;
      const AWithTEXTW: Boolean;
      const AWideTextBuffer: PWideChar;
      const AWideTextLength: Integer;
      const ARowsAffectedPtr: PInteger = nil
    ); inline;

    function OpenSQL(
      const ASQLText: AnsiString;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer;
      const ARaiseOnOpenError: Boolean = TRUE
    ): Integer; //inline;

    function OpenSQLWithTEXTW(
      const ASQLText: AnsiString;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer;
      const ARaiseOnOpenError: Boolean;
      const AWithTEXTW: Boolean;
      const AWideTextBuffer: PWideChar;
      const AWideTextLength: Integer;
      const ARowsAffectedPtr: PInteger = nil
    ): Integer;

    function DeclareSQL(
      const ASQLText: AnsiString;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer;
      const ARaiseOnOpenError: Boolean
    ): Integer;

    function PrepareStatement(
      const AStmtData: PSQLite3StmtData;
      const ASQLText: AnsiString
    ): Boolean;

    function FetchPrepared(
      const AStmtData: PSQLite3StmtData;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer
    ): Boolean;

    function ClosePrepared(
      const AStmtData: PSQLite3StmtData
    ): Boolean;

    function LastInsertedRowId: Int64; //inline;

    procedure CheckError(const AHasError: Boolean);

    procedure SetBusyTryCount(const ATryCount: Integer);

    procedure SetProgressHandler(
      const nOps: Integer;
      const xProgress: TSQLite3ProgressCallback;
      const pArg: Pointer
    );

    procedure SetExclusiveLockingMode; inline;
    procedure SetNormalLockingMode; inline;

    procedure BeginTran; inline;
    procedure Rollback; inline;
    procedure Commit; inline;
  end;

  ESQLite3Exception     = class(Exception);
  ESQLite3SimpleError   = class(ESQLite3Exception);
  ESQLite3ErrorWithCode = class(ESQLite3Exception);

function QuotedStrA(const S: string): AnsiString;
  
implementation

uses
  ALString;

var
  g_Sqlite3Library: TALSqlite3Library;

function QuotedStrA(const S: string): AnsiString;
begin
  Result := AnsiToUtf8(QuotedStr(S));
end;

function InternalInitLib: Boolean;
begin
  // инициализация при первом обращении
  if (nil=g_Sqlite3Library) then begin
    g_Sqlite3Library := TALSqlite3Library.Create;
    Result := g_Sqlite3Library.Load;
  end else begin
    // уже загружено
    Result := g_Sqlite3Library.Loaded;
  end;
end;

function LocalSQLiteBusyHandler(ptr: Pointer; count: Integer): Integer; cdecl;
begin
  if (count < Integer(ptr)) then begin
    // repeat
    Sleep(1);
    Result := 1;
  end else begin
    // failed
    Result := 0;
  end;
end;

{$IF CompilerVersion < 23}
function CharInSet(AChar: AnsiChar; const ASet: TSysCharSet): Boolean; inline;
begin
  Result := AChar in ASet;
end;
{$IFEND}

function CompareLogicallyAnsi(n1: Integer; const z1: PAnsiChar; n2: Integer;
  const z2: PAnsiChar): Integer;
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

function LogicalCollactionCompareUTF8(
  pUser: Pointer;
  n1: Integer;
  const z1: Pointer;
  n2: Integer;
  const z2: Pointer
): integer; cdecl;
begin
  Result := CompareLogicallyAnsi(n1, z1, n2, z2);
end;

function LogicalCollactionCompareUTF16LE(
  pUser: Pointer;
  n1: Integer;
  const z1: Pointer;
  n2: Integer;
  const z2: Pointer
): integer; cdecl;
begin
  Result := CompareLogicallyAnsi(n1, z1, n2, z2);
end;

function LogicalCollactionCompareUTF16BE(
  pUser: Pointer;
  n1: Integer;
  const z1: Pointer;
  n2: Integer;
  const z2: Pointer
): integer; cdecl;
begin
  Result := CompareLogicallyAnsi(n1, z1, n2, z2);
end;

procedure Callback4CollationNeeded(
  pCollNeededArg: Pointer;
  db: PSQLite3;
  eTextRep: Integer;
  const zExternal: PAnsiChar
); cdecl;
begin
  case eTextRep of
    SQLITE_UTF8: begin
      g_Sqlite3Library.sqlite3_create_collation(
        db,
        zExternal,
        SQLITE_UTF8,
        nil,
        @LogicalCollactionCompareUTF8
      );
    end;
    SQLITE_UTF16LE: begin
      g_Sqlite3Library.sqlite3_create_collation(
        db,
        zExternal,
        SQLITE_UTF8,
        nil,
        @LogicalCollactionCompareUTF16LE
      );
    end;
    SQLITE_UTF16BE: begin
      g_Sqlite3Library.sqlite3_create_collation(
        db,
        zExternal,
        SQLITE_UTF8,
        nil,
        @LogicalCollactionCompareUTF16BE
      );
    end;
  else
    begin
      Assert(False, IntToStr(eTextRep));
    end;
  end;
end;

{ TSQLite3StmtData }

function TSQLite3StmtData.ColumnAsString(const iCol: Integer): string;
var
  VValue: PAnsiChar;
begin
  VValue := g_Sqlite3Library.sqlite3_column_text(Stmt, iCol); // return UTF-8
  if VValue = nil then begin
    Result := '';
  end else begin
    Result :=
      {$IFDEF UNICODE}
      UTF8ToString(VValue);
      {$ELSE}
      UTf8ToAnsi(VValue);
      {$ENDIF}
  end;
end;

function TSQLite3StmtData.ColumnAsAnsiString(const iCol: Integer): AnsiString;
begin
  Result := AnsiString(ColumnAsString(iCol));
end;

function TSQLite3StmtData.ColumnAsWideString(const iCol: Integer): UnicodeString;
var
  VValue: PWideChar;
begin
  VValue := g_Sqlite3Library.sqlite3_column_text16(Stmt, iCol); // return UTF-16
  if VValue = nil then begin
    Result := '';
  end else begin
    Result := UnicodeString(VValue);
  end;
end;

function TSQLite3StmtData.ColumnBlobData(const iCol: Integer): Pointer;
begin
  Result := g_Sqlite3Library.sqlite3_column_blob(Stmt, iCol)
end;

function TSQLite3StmtData.ColumnBlobSize(const iCol: Integer): Integer;
begin
  Result := g_Sqlite3Library.sqlite3_column_bytes(Stmt, iCol)
end;

function TSQLite3StmtData.ColumnCount: Integer;
begin
  Result := g_Sqlite3Library.sqlite3_column_count(Stmt);
end;

function TSQLite3StmtData.ColumnDeclType(const iCol: Integer): PAnsiChar;
begin
  Result := g_Sqlite3Library.sqlite3_column_decltype(Stmt, iCol);
end;

function TSQLite3StmtData.ColumnDouble(const iCol: Integer): Double;
begin
  Result := g_Sqlite3Library.sqlite3_column_double(Stmt, iCol)
end;

function TSQLite3StmtData.ColumnInt(const iCol: Integer): Integer;
begin
  Result := g_Sqlite3Library.sqlite3_column_int(Stmt, iCol)
end;

function TSQLite3StmtData.ColumnInt64(const iCol: Integer): Int64;
begin
  Result := g_Sqlite3Library.sqlite3_column_int64(Stmt, iCol)
end;

function TSQLite3StmtData.ColumnIntDef(const iCol, AValueIfNull: Integer): Integer;
begin
  if IsNull(iCol) then
    Result := AValueIfNull
  else
    Result := g_Sqlite3Library.sqlite3_column_int(Stmt, iCol)
end;

function TSQLite3StmtData.ColumnName(const iCol: Integer): PAnsiChar;
begin
  Result := g_Sqlite3Library.sqlite3_column_name(Stmt, iCol);
end;

function TSQLite3StmtData.ColumnType(const iCol: Integer): Integer;
begin
  Result := g_Sqlite3Library.sqlite3_column_type(Stmt, iCol);
end;

procedure TSQLite3StmtData.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TSQLite3StmtData.IsNull(const iCol: Integer): Boolean;
begin
  Result := (SQLITE_NULL = g_Sqlite3Library.sqlite3_column_type(Stmt, iCol))
end;

{ TSQLite3DbHandler }

procedure TSQLite3DbHandler.BeginTran;
begin
  ExecSQL('BEGIN TRANSACTION');
end;

procedure TSQLite3DbHandler.CheckError(const AHasError: Boolean);
begin
  if (not AHasError) then
    Exit;

  if Assigned(Sqlite3Handle) and Assigned(g_Sqlite3Library) then begin
    raise ESQLite3ErrorWithCode.Create(
        g_Sqlite3Library.sqlite3_errmsg(Sqlite3Handle) +
        ' ( error code: ' +
        IntToStr(g_Sqlite3Library.sqlite3_errcode(Sqlite3Handle)) + ')'
      );
  end else begin
    raise ESQLite3SimpleError.Create('SQLite3 error');
  end;
end;

procedure TSQLite3DbHandler.Close;
begin
  if (Sqlite3Handle<>nil) then begin
    g_Sqlite3Library.sqlite3_close(Sqlite3Handle);
    Sqlite3Handle := nil;
  end;
end;

function TSQLite3DbHandler.Closed: Boolean;
begin
  Result := (nil=Sqlite3Handle)
end;

function TSQLite3DbHandler.ClosePrepared(const AStmtData: PSQLite3StmtData): Boolean;
begin
  Result := (SQLITE_OK = g_Sqlite3Library.sqlite3_finalize(AStmtData^.Stmt));
end;

procedure TSQLite3DbHandler.Commit;
begin
  ExecSQL('COMMIT TRANSACTION');
end;

function TSQLite3DbHandler.DeclareSQL(
  const ASQLText: AnsiString;
  const ACallbackProc: TSQLiteOpenStatementProc;
  const ACallbackPtr: Pointer;
  const ARaiseOnOpenError: Boolean
): Integer;
var
  VStmtData: TSQLite3StmtData;
begin
  VStmtData.Init;

  Result := g_Sqlite3Library.sqlite3_prepare_v2(
    Sqlite3Handle,
    PAnsiChar(ASQLText),
    Length(ASQLText),
    VStmtData.Stmt,
    nil
  );

  if (SQLITE_OK<>Result) then begin
    // ошибка
    if ARaiseOnOpenError then
      CheckError(TRUE)
    else
      Exit;
  end;

  try
    Result := g_Sqlite3Library.sqlite3_step(VStmtData.Stmt);

    // SQLITE_ROW
    if Assigned(ACallbackProc) then begin
      ACallbackProc(@Self, ACallbackPtr, @VStmtData);
    end;
  finally
    CheckError(g_Sqlite3Library.sqlite3_finalize(VStmtData.Stmt) <> SQLITE_OK);
  end;
end;

procedure TSQLite3DbHandler.ExecSQL(
  const ASQLText: AnsiString;
  const ARowsAffectedPtr: PInteger
);
begin
  OpenSQLWithTEXTW(
    ASQLText,
    nil,
    nil,
    TRUE,
    FALSE,
    nil,
    0,
    ARowsAffectedPtr
  )
end;

procedure TSQLite3DbHandler.ExecSQLWithBLOB(
  const ASQLText: AnsiString;
  const ABufferPtr: Pointer;
  const ABufferLen: Integer;
  const ARowsAffectedPtr: PInteger
);
var
  VStmt: PSQLite3Stmt;
begin
  CheckError(
    g_Sqlite3Library.sqlite3_prepare_v2(
    Sqlite3Handle,
    PAnsiChar(ASQLText),
    Length(ASQLText),
    VStmt,
    nil
    ) <> SQLITE_OK
  );
  try
    CheckError(
      g_Sqlite3Library.sqlite3_bind_blob(
      VStmt,
      1,
      ABufferPtr,
      ABufferLen,
      SQLITE_STATIC
      ) <> SQLITE_OK
    );
    CheckError(not (g_Sqlite3Library.sqlite3_step(VStmt) in [SQLITE_DONE, SQLITE_ROW]));
    if (ARowsAffectedPtr<>nil) then begin
      ARowsAffectedPtr^ := g_Sqlite3Library.sqlite3_changes(Sqlite3Handle);
    end;
  finally
    CheckError(g_Sqlite3Library.sqlite3_finalize(VStmt) <> SQLITE_OK);
  end;
end;

procedure TSQLite3DbHandler.ExecSQLWithTEXTW(
  const ASQLText: AnsiString;
  const AWithTEXTW: Boolean;
  const AWideTextBuffer: PWideChar;
  const AWideTextLength: Integer;
  const ARowsAffectedPtr: PInteger
);
begin
  OpenSQLWithTEXTW(
    ASQLText,
    nil,
    nil,
    TRUE,
    AWithTEXTW,
    AWideTextBuffer,
    AWideTextLength,
    ARowsAffectedPtr
  )
end;

function TSQLite3DbHandler.FetchPrepared(
  const AStmtData: PSQLite3StmtData;
  const ACallbackProc: TSQLiteOpenStatementProc;
  const ACallbackPtr: Pointer
): Boolean;
begin
  Result := (SQLITE_ROW = g_Sqlite3Library.sqlite3_step(AStmtData^.Stmt));

  if (not Result) then begin
    // SQLITE_DONE or error
    Exit;
  end;

  ACallbackProc(@Self, ACallbackPtr, AStmtData);
end;

function TSQLite3DbHandler.Init: Boolean;
begin
  FillChar(Self, SizeOf(Self), 0);
  Result := InternalInitLib;
end;

function TSQLite3DbHandler.LastInsertedRowId: Int64;
begin
  Result := g_Sqlite3Library.sqlite3_last_insert_rowid(Sqlite3Handle)
end;

function TSQLite3DbHandler.LibVersionInfo: string;
var FDLL: THandle;
begin
  FDLL := GetModuleHandle('sqlite3.dll');
  if (FDLL<>0) then begin
    Result := string(AnsiString(g_Sqlite3Library.sqlite3_libversion));
    Result := Result + ' at ' + GetModuleName(FDLL);
  end else begin
    Result := 'not loaded!';
  end;
end;

procedure TSQLite3DbHandler.Open(
  const ADbFileName: String;
  const AOpenFlags: Integer;
  const ASupportLogicalCollation: Boolean
);
var
  VDBFileName: AnsiString;
begin
  Close;
  try
    VDBFileName := AnsiToUtf8(ADbFileName);
    CheckError(
      g_Sqlite3Library.sqlite3_open_v2(
      PAnsiChar(VDBFileName),
      Sqlite3Handle,
      AOpenFlags, // SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE // SQLITE_OPEN_READWRITE
      nil
      ) <> SQLITE_OK
    );
    if ASupportLogicalCollation then begin
      RegisterCollationNeededCallback;
    end;
  except
    Close;
    raise;
  end;
end;

procedure TSQLite3DbHandler.OpenW(
  const ADbFileName: WideString;
  const ASupportLogicalCollation: Boolean
);
begin
  Close;
  try
    CheckError(
      g_Sqlite3Library.sqlite3_open16(
      PWideChar(ADbFileName),
      Sqlite3Handle
      ) <> SQLITE_OK
    );
    if ASupportLogicalCollation then begin
      RegisterCollationNeededCallback;
    end;
  except
    Close;
    raise;
  end;
end;

function TSQLite3DbHandler.Opened: Boolean;
begin
  Result := (nil<>Sqlite3Handle)
end;

function TSQLite3DbHandler.OpenSQL(
  const ASQLText: AnsiString;
  const ACallbackProc: TSQLiteOpenStatementProc;
  const ACallbackPtr: Pointer;
  const ARaiseOnOpenError: Boolean): Integer;
begin
  Result := OpenSQLWithTEXTW(
    ASQLText,
    ACallbackProc,
    ACallbackPtr,
    ARaiseOnOpenError,
    FALSE,
    nil,
    0
  );
end;

function TSQLite3DbHandler.OpenSQLWithTEXTW(
  const ASQLText: AnsiString;
  const ACallbackProc: TSQLiteOpenStatementProc;
  const ACallbackPtr: Pointer;
  const ARaiseOnOpenError: Boolean;
  const AWithTEXTW: Boolean;
  const AWideTextBuffer: PWideChar;
  const AWideTextLength: Integer;
  const ARowsAffectedPtr: PInteger
): Integer;
var
  VStmtData: TSQLite3StmtData;
begin
  VStmtData.Init;

  Result := g_Sqlite3Library.sqlite3_prepare_v2(
    Sqlite3Handle,
    PAnsiChar(ASQLText),
    Length(ASQLText),
    VStmtData.Stmt,
    nil
  );

  if (SQLITE_OK <> Result) then begin
    // ошибка
    if ARaiseOnOpenError then
      CheckError(TRUE)
    else
      Exit;
  end;

  try
    if AWithTEXTW then begin
      CheckError(
        g_Sqlite3Library.sqlite3_bind_text16(
        VStmtData.Stmt,
        1,
        AWideTextBuffer,
        AWideTextLength*SizeOf(WideChar),
        SQLITE_STATIC
        ) <> SQLITE_OK
      );
    end;

    repeat
      Result := g_Sqlite3Library.sqlite3_step(VStmtData.Stmt);

      if (SQLITE_ROW <> Result) then begin
        if (SQLITE_DONE = Result) then begin
          if (ARowsAffectedPtr <> nil) then begin
            ARowsAffectedPtr^ := g_Sqlite3Library.sqlite3_changes(Sqlite3Handle);
          end;
          break;
        end;
        CheckError(True);
      end;

      // SQLITE_ROW
      if Assigned(ACallbackProc) then begin
        ACallbackProc(@Self, ACallbackPtr, @VStmtData);
        if VStmtData.Cancelled then
          break;
      end;

    until FALSE;
  finally
    CheckError(g_Sqlite3Library.sqlite3_finalize(VStmtData.Stmt) <> SQLITE_OK);
  end;
end;

function TSQLite3DbHandler.PrepareStatement(
  const AStmtData: PSQLite3StmtData;
  const ASQLText: AnsiString
): Boolean;
begin
  Result := (SQLITE_OK = g_Sqlite3Library.sqlite3_prepare_v2(
    Sqlite3Handle,
    PAnsiChar(ASQLText),
    Length(ASQLText),
    AStmtData^.Stmt,
    nil
  ));
end;

procedure TSQLite3DbHandler.RegisterCollationNeededCallback;
begin
  g_Sqlite3Library.sqlite3_collation_needed(
    Sqlite3Handle,
    nil, // @Self
    @Callback4CollationNeeded
  );
end;

procedure TSQLite3DbHandler.Rollback;
begin
  ExecSQL('ROLLBACK TRANSACTION');
end;

procedure TSQLite3DbHandler.SetBusyTryCount(const ATryCount: Integer);
begin
  g_Sqlite3Library.sqlite3_busy_handler(
    Sqlite3Handle,
    LocalSQLiteBusyHandler,
    Pointer(ATryCount)
  );
end;

procedure TSQLite3DbHandler.SetExclusiveLockingMode;
begin
  ExecSQL('PRAGMA locking_mode='+'EXCLUSIVE')
end;

procedure TSQLite3DbHandler.SetNormalLockingMode;
begin
  ExecSQL('PRAGMA locking_mode='+'NORMAL')
end;

procedure TSQLite3DbHandler.SetProgressHandler(
  const nOps: Integer;
  const xProgress: TSQLite3ProgressCallback;
  const pArg: Pointer
);
begin
  g_Sqlite3Library.sqlite3_progress_handler(
    Sqlite3Handle,
    nOps,
    xProgress,
    pArg
  );
end;

initialization
  g_Sqlite3Library := nil;

finalization
  FreeAndNil(g_Sqlite3Library);

end.