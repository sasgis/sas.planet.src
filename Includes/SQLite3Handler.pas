unit SQLite3Handler;

interface

uses
  SysUtils,
  AlSqlite3Wrapper,
  Classes;

const
  c_SQLite_Ext = '.sqlitedb';

  SQLITE_OPEN_READONLY       = AlSqlite3Wrapper.SQLITE_OPEN_READONLY;
  SQLITE_OPEN_READWRITE      = AlSqlite3Wrapper.SQLITE_OPEN_READWRITE;
  SQLITE_OPEN_CREATE         = AlSqlite3Wrapper.SQLITE_OPEN_CREATE;

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
    function ColumnIntDef(const iCol, AValueIfNull: Integer): Integer;
    function ColumnText(const iCol: Integer): PAnsiChar; // inline;
    function ColumnAsString(const iCol: Integer): AnsiString;
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
  public
    function Init: Boolean;

    procedure Open(const ADbFileName: AnsiString; const AOpenFlags: Integer);
    procedure OpenW(const ADbFileName: WideString);
    procedure Close;
    function Opened: Boolean; inline;
    function Closed: Boolean; inline;

    procedure ExecSQL(
      const ASQLText: AnsiString;
      const ALastInsertedRowId: PInt64 = nil
    );

    procedure ExecSQLWithBLOB(
      const ASQLText: AnsiString;
      const ABufferPtr: Pointer;
      const ABufferLen: Integer
    );

    function OpenSQL(
      const ASQLText: AnsiString;
      const ACallbackProc: TSQLiteOpenStatementProc;
      const ACallbackPtr: Pointer;
      const ARaiseOnOpenError: Boolean = TRUE
    ): Integer;

    procedure CheckError(const AHasError: Boolean);

    procedure SetExclusiveLockingMode; inline;
    procedure SetNormalLockingMode; inline;

    procedure BeginTran; inline;
    procedure Rollback; inline;
    procedure Commit; inline;
  end;

  ESQLite3Exception     = class(Exception);
  ESQLite3SimpleError   = class(ESQLite3Exception);
  ESQLite3ErrorWithCode = class(ESQLite3Exception);
  
implementation

var
  g_Sqlite3Library: TALSqlite3Library;

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

{ TSQLite3StmtData }

function TSQLite3StmtData.ColumnAsString(const iCol: Integer): AnsiString;
var
  VValue: PAnsiChar;
begin
  VValue := ColumnText(iCol);
  if (nil=VValue) then
    Result := ''
  else
    Result := AnsiString(VValue);
end;

function TSQLite3StmtData.ColumnInt(const iCol: Integer): Integer;
begin
  Result := g_Sqlite3Library.sqlite3_column_int(Stmt, iCol)
end;

function TSQLite3StmtData.ColumnIntDef(const iCol, AValueIfNull: Integer): Integer;
begin
  if IsNull(iCol) then
    Result := AValueIfNull
  else
    Result := g_Sqlite3Library.sqlite3_column_int(Stmt, iCol)
end;

function TSQLite3StmtData.ColumnText(const iCol: Integer): PAnsiChar;
begin
  Result := g_Sqlite3Library.sqlite3_column_text(Stmt, iCol)
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

procedure TSQLite3DbHandler.Commit;
begin
  ExecSQL('COMMIT TRANSACTION');
end;

procedure TSQLite3DbHandler.ExecSQL(const ASQLText: AnsiString; const ALastInsertedRowId: PInt64);
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
    CheckError(not (g_Sqlite3Library.sqlite3_step(VStmt) in [SQLITE_DONE, SQLITE_ROW]));

    if (nil<>ALastInsertedRowId) then begin
      ALastInsertedRowId^ := g_Sqlite3Library.sqlite3_last_insert_rowid(VStmt);
    end;
  finally
    CheckError(g_Sqlite3Library.sqlite3_finalize(VStmt) <> SQLITE_OK);
  end;
end;

procedure TSQLite3DbHandler.ExecSQLWithBLOB(
  const ASQLText: AnsiString;
  const ABufferPtr: Pointer;
  const ABufferLen: Integer
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
  finally
    CheckError(g_Sqlite3Library.sqlite3_finalize(VStmt) <> SQLITE_OK);
  end;
end;

function TSQLite3DbHandler.Init: Boolean;
begin
  FillChar(Self, SizeOf(Self), 0);
  Result := InternalInitLib;
end;

procedure TSQLite3DbHandler.Open(const ADbFileName: AnsiString; const AOpenFlags: Integer);
begin
  Close;
  try
    CheckError(
      g_Sqlite3Library.sqlite3_open_v2(
      PAnsiChar(ADbFileName),
      Sqlite3Handle,
      AOpenFlags, // SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE // SQLITE_OPEN_READWRITE
      nil
      ) <> SQLITE_OK
    );
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
    repeat
      Result := g_Sqlite3Library.sqlite3_step(VStmtData.Stmt);

      if (SQLITE_DONE=Result) then begin
        break;
      end;

      if (SQLITE_ROW<>Result) then begin
        CheckError(TRUE);
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

procedure TSQLite3DbHandler.OpenW(const ADbFileName: WideString);
begin
  Close;
  try
    CheckError(
      g_Sqlite3Library.sqlite3_open16(
      PWideChar(ADbFileName),
      Sqlite3Handle
      ) <> SQLITE_OK
    );
  except
    Close;
    raise;
  end;
end;

procedure TSQLite3DbHandler.Rollback;
begin
  ExecSQL('ROLLBACK TRANSACTION');
end;

procedure TSQLite3DbHandler.SetExclusiveLockingMode;
begin
  ExecSQL('PRAGMA locking_mode='+'EXCLUSIVE')
end;

procedure TSQLite3DbHandler.SetNormalLockingMode;
begin
  ExecSQL('PRAGMA locking_mode='+'NORMAL')
end;

initialization
  g_Sqlite3Library := nil;
finalization
  FreeAndNil(g_Sqlite3Library);
end.