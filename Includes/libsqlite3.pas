{
  Based on:
    - https://github.com/plashenkov/SQLite3-Delphi-FPC (MIT license)
    - https://github.com/MagicFoundation/Alcinoe/ (Apache-2.0 license)
}

unit libsqlite3;

interface

const
  libsqlite3_dll = 'sqlite3.dll';

type
  PPUTF8Chat = ^PUTF8Char;

  PPUTF8CharArray = ^TPUTF8CharArray;
  TPUTF8CharArray = array[0..MaxInt div SizeOf(PUTF8Char) - 1] of PUTF8Char;

  PSQLite3 = type Pointer;

  TSQLite3Callback = function(pArg: Pointer; nCol: Integer; argv: PPUTF8CharArray; colv: PPUTF8CharArray): Integer; cdecl;

const
  SQLITE_OK          = 0;   // Successful result
  SQLITE_ERROR       = 1;   // SQL error or missing database
  SQLITE_INTERNAL    = 2;   // Internal logic error in SQLite
  SQLITE_PERM        = 3;   // Access permission denied
  SQLITE_ABORT       = 4;   // Callback routine requested an abort
  SQLITE_BUSY        = 5;   // The database file is locked
  SQLITE_LOCKED      = 6;   // A table in the database is locked
  SQLITE_NOMEM       = 7;   // A malloc() failed
  SQLITE_READONLY    = 8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT   = 9;   // Operation terminated by sqlite3_interrupt(
  SQLITE_IOERR      = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT    = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND   = 12;   // Unknown opcode in sqlite3_file_control()
  SQLITE_FULL       = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN   = 14;   // Unable to open the database file
  SQLITE_PROTOCOL   = 15;   // Database lock protocol error
  SQLITE_EMPTY      = 16;   // Database is empty
  SQLITE_SCHEMA     = 17;   // The database schema changed
  SQLITE_TOOBIG     = 18;   // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT = 19;   // Abort due to constraint violation
  SQLITE_MISMATCH   = 20;   // Data type mismatch
  SQLITE_MISUSE     = 21;   // Library used incorrectly
  SQLITE_NOLFS      = 22;   // Uses OS features not supported on host
  SQLITE_AUTH       = 23;   // Authorization denied
  SQLITE_FORMAT     = 24;   // Auxiliary database format error
  SQLITE_RANGE      = 25;   // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB     = 26;   // File opened that is not a database file
  SQLITE_NOTICE     = 27;   // Notifications from sqlite3_log()
  SQLITE_WARNING    = 28;   // Warnings from sqlite3_log()
  SQLITE_ROW        = 100;  // sqlite3_step() has another row ready
  SQLITE_DONE       = 101;  // sqlite3_step() has finished executing

  SQLITE_IOERR_READ              = SQLITE_IOERR or (1 shl 8);
  SQLITE_IOERR_SHORT_READ        = SQLITE_IOERR or (2 shl 8);
  SQLITE_IOERR_WRITE             = SQLITE_IOERR or (3 shl 8);
  SQLITE_IOERR_FSYNC             = SQLITE_IOERR or (4 shl 8);
  SQLITE_IOERR_DIR_FSYNC         = SQLITE_IOERR or (5 shl 8);
  SQLITE_IOERR_TRUNCATE          = SQLITE_IOERR or (6 shl 8);
  SQLITE_IOERR_FSTAT             = SQLITE_IOERR or (7 shl 8);
  SQLITE_IOERR_UNLOCK            = SQLITE_IOERR or (8 shl 8);
  SQLITE_IOERR_RDLOCK            = SQLITE_IOERR or (9 shl 8);
  SQLITE_IOERR_DELETE            = SQLITE_IOERR or (10 shl 8);
  SQLITE_IOERR_BLOCKED           = SQLITE_IOERR or (11 shl 8);
  SQLITE_IOERR_NOMEM             = SQLITE_IOERR or (12 shl 8);
  SQLITE_IOERR_ACCESS            = SQLITE_IOERR or (13 shl 8);
  SQLITE_IOERR_CHECKRESERVEDLOCK = SQLITE_IOERR or (14 shl 8);
  SQLITE_IOERR_LOCK              = SQLITE_IOERR or (15 shl 8);
  SQLITE_IOERR_CLOSE             = SQLITE_IOERR or (16 shl 8);
  SQLITE_IOERR_DIR_CLOSE         = SQLITE_IOERR or (17 shl 8);
  SQLITE_IOERR_SHMOPEN           = SQLITE_IOERR or (18 shl 8);
  SQLITE_IOERR_SHMSIZE           = SQLITE_IOERR or (19 shl 8);
  SQLITE_IOERR_SHMLOCK           = SQLITE_IOERR or (20 shl 8);
  SQLITE_IOERR_SHMMAP            = SQLITE_IOERR or (21 shl 8);
  SQLITE_IOERR_SEEK              = SQLITE_IOERR or (22 shl 8);
  SQLITE_IOERR_DELETE_NOENT      = SQLITE_IOERR or (23 shl 8);
  SQLITE_IOERR_MMAP              = SQLITE_IOERR or (24 shl 8);
  SQLITE_IOERR_GETTEMPPATH       = SQLITE_IOERR or (25 shl 8);
  SQLITE_IOERR_CONVPATH          = SQLITE_IOERR or (26 shl 8);
  SQLITE_LOCKED_SHAREDCACHE      = SQLITE_LOCKED or  (1 shl 8);
  SQLITE_BUSY_RECOVERY           = SQLITE_BUSY   or  (1 shl 8);
  SQLITE_BUSY_SNAPSHOT           = SQLITE_BUSY   or  (2 shl 8);
  SQLITE_CANTOPEN_NOTEMPDIR      = SQLITE_CANTOPEN or (1 shl 8);
  SQLITE_CANTOPEN_ISDIR          = SQLITE_CANTOPEN or (2 shl 8);
  SQLITE_CANTOPEN_FULLPATH       = SQLITE_CANTOPEN or (3 shl 8);
  SQLITE_CANTOPEN_CONVPATH       = SQLITE_CANTOPEN or (4 shl 8);
  SQLITE_CORRUPT_VTAB            = SQLITE_CORRUPT or (1 shl 8);
  SQLITE_READONLY_RECOVERY       = SQLITE_READONLY or (1 shl 8);
  SQLITE_READONLY_CANTLOCK       = SQLITE_READONLY or (2 shl 8);
  SQLITE_READONLY_ROLLBACK       = SQLITE_READONLY or (3 shl 8);
  SQLITE_READONLY_DBMOVED        = SQLITE_READONLY or (4 shl 8);
  SQLITE_ABORT_ROLLBACK          = SQLITE_ABORT or (2 shl 8);
  SQLITE_CONSTRAINT_CHECK        = SQLITE_CONSTRAINT or (1 shl 8);
  SQLITE_CONSTRAINT_COMMITHOOK   = SQLITE_CONSTRAINT or (2 shl 8);
  SQLITE_CONSTRAINT_FOREIGNKEY   = SQLITE_CONSTRAINT or (3 shl 8);
  SQLITE_CONSTRAINT_FUNCTION     = SQLITE_CONSTRAINT or (4 shl 8);
  SQLITE_CONSTRAINT_NOTNULL      = SQLITE_CONSTRAINT or (5 shl 8);
  SQLITE_CONSTRAINT_PRIMARYKEY   = SQLITE_CONSTRAINT or (6 shl 8);
  SQLITE_CONSTRAINT_TRIGGER      = SQLITE_CONSTRAINT or (7 shl 8);
  SQLITE_CONSTRAINT_UNIQUE       = SQLITE_CONSTRAINT or (8 shl 8);
  SQLITE_CONSTRAINT_VTAB         = SQLITE_CONSTRAINT or (9 shl 8);
  SQLITE_CONSTRAINT_ROWID        = SQLITE_CONSTRAINT or(10 shl 8);
  SQLITE_NOTICE_RECOVER_WAL      = SQLITE_NOTICE or (1 shl 8);
  SQLITE_NOTICE_RECOVER_ROLLBACK = SQLITE_NOTICE or (2 shl 8);
  SQLITE_WARNING_AUTOINDEX       = SQLITE_WARNING or (1 shl 8);
  SQLITE_AUTH_USER               = SQLITE_AUTH or (1 shl 8);

  SQLITE_OPEN_READONLY         = $00000001; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_READWRITE        = $00000002; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_CREATE           = $00000004; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_DELETEONCLOSE    = $00000008; // VFS only
  SQLITE_OPEN_EXCLUSIVE        = $00000010; // VFS only
  SQLITE_OPEN_AUTOPROXY        = $00000020; // VFS only
  SQLITE_OPEN_URI              = $00000040; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_MEMORY           = $00000080; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_MAIN_DB          = $00000100; // VFS only
  SQLITE_OPEN_TEMP_DB          = $00000200; // VFS only
  SQLITE_OPEN_TRANSIENT_DB     = $00000400; // VFS only
  SQLITE_OPEN_MAIN_JOURNAL     = $00000800; // VFS only
  SQLITE_OPEN_TEMP_JOURNAL     = $00001000; // VFS only
  SQLITE_OPEN_SUBJOURNAL       = $00002000; // VFS only
  SQLITE_OPEN_MASTER_JOURNAL   = $00004000; // VFS only
  SQLITE_OPEN_NOMUTEX          = $00008000; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_FULLMUTEX        = $00010000; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_SHAREDCACHE      = $00020000; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_PRIVATECACHE     = $00040000; // Ok for sqlite3_open_v2()
  SQLITE_OPEN_WAL              = $00080000; // VFS only

  SQLITE_IOCAP_ATOMIC                 = $00000001;
  SQLITE_IOCAP_ATOMIC512              = $00000002;
  SQLITE_IOCAP_ATOMIC1K               = $00000004;
  SQLITE_IOCAP_ATOMIC2K               = $00000008;
  SQLITE_IOCAP_ATOMIC4K               = $00000010;
  SQLITE_IOCAP_ATOMIC8K               = $00000020;
  SQLITE_IOCAP_ATOMIC16K              = $00000040;
  SQLITE_IOCAP_ATOMIC32K              = $00000080;
  SQLITE_IOCAP_ATOMIC64K              = $00000100;
  SQLITE_IOCAP_SAFE_APPEND            = $00000200;
  SQLITE_IOCAP_SEQUENTIAL             = $00000400;
  SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN  = $00000800;
  SQLITE_IOCAP_POWERSAFE_OVERWRITE    = $00001000;
  SQLITE_IOCAP_IMMUTABLE              = $00002000;

  SQLITE_LOCK_NONE          = 0;
  SQLITE_LOCK_SHARED        = 1;
  SQLITE_LOCK_RESERVED      = 2;
  SQLITE_LOCK_PENDING       = 3;
  SQLITE_LOCK_EXCLUSIVE     = 4;

  SQLITE_SYNC_NORMAL       = $00002;
  SQLITE_SYNC_FULL         = $00003;
  SQLITE_SYNC_DATAONLY     = $00010;

type
  PSQLite3File = ^TSQLite3File;
  PSQLite3IOMethods = ^TSQLite3IOMethods;

  TSQLite3File = record
    pMethods: PSQLite3IOMethods;
  end;

  TSQLite3IOMethods = record
    iVersion: Integer;
    xClose: function(id: PSQLite3File): Integer; cdecl;
    xRead: function(id: PSQLite3File; pBuf: Pointer; iAmt: Integer; iOfst: Int64): Integer; cdecl;
    xWrite: function(id: PSQLite3File; const pBuf: Pointer; iAmt: Integer; iOfst: Int64): Integer; cdecl;
    xTruncate: function(id: PSQLite3File; size: Int64): Integer; cdecl;
    xSync: function(id: PSQLite3File; flags: Integer): Integer; cdecl;
    xFileSize: function(id: PSQLite3File; var pSize: Int64): Integer; cdecl;
    xLock: function(id: PSQLite3File; locktype: Integer): Integer; cdecl;
    xUnlock: function(id: PSQLite3File; locktype: Integer): Integer; cdecl;
    xCheckReservedLock: function(f: PSQLite3File; var pResOut: Integer): Integer; cdecl;
    xFileControl: function(id: PSQLite3File; op: Integer; pArg: Pointer): Integer; cdecl;
    xSectorSize: function(id: PSQLite3File): Integer; cdecl;
    xDeviceCharacteristics: function(id: PSQLite3File): Integer; cdecl;
  end;

const
  SQLITE_FCNTL_LOCKSTATE              = 1;
  SQLITE_FCNTL_GET_LOCKPROXYFILE      = 2;
  SQLITE_FCNTL_SET_LOCKPROXYFILE      = 3;
  SQLITE_FCNTL_LAST_ERRNO             = 4;
  SQLITE_FCNTL_SIZE_HINT              = 5;
  SQLITE_FCNTL_CHUNK_SIZE             = 6;
  SQLITE_FCNTL_FILE_POINTER           = 7;
  SQLITE_FCNTL_SYNC_OMITTED           = 8;
  SQLITE_FCNTL_WIN32_AV_RETRY         = 9;
  SQLITE_FCNTL_PERSIST_WAL           = 10;
  SQLITE_FCNTL_OVERWRITE             = 11;
  SQLITE_FCNTL_VFSNAME               = 12;
  SQLITE_FCNTL_POWERSAFE_OVERWRITE   = 13;
  SQLITE_FCNTL_PRAGMA                = 14;
  SQLITE_FCNTL_BUSYHANDLER           = 15;
  SQLITE_FCNTL_TEMPFILENAME          = 16;
  SQLITE_FCNTL_MMAP_SIZE             = 18;
  SQLITE_FCNTL_TRACE                 = 19;
  SQLITE_FCNTL_HAS_MOVED             = 20;
  SQLITE_FCNTL_SYNC                  = 21;
  SQLITE_FCNTL_COMMIT_PHASETWO       = 22;
  SQLITE_FCNTL_WIN32_SET_HANDLE      = 23;
  SQLITE_FCNTL_WAL_BLOCK             = 24;
  SQLITE_FCNTL_ZIPVFS                = 25;
  SQLITE_FCNTL_RBU                   = 26;

type
  PSQLite3Mutex = type Pointer;

type
  PSQLite3VFS = ^TSQLite3VFS;
  TSQLite3VFS = record
    iVersion: Integer;
    szOsFile: Integer;
    mxPathname: Integer;
    pNext: PSQLite3VFS;
    zName: PUTF8Char;
    pAppData: Pointer;
    xOpen: function(pVfs: PSQLite3VFS; const zName: PUTF8Char; id: PSQLite3File; flags: Integer; pOutFlags: PInteger): Integer; cdecl;
    xDelete: function(pVfs: PSQLite3VFS; const zName: PUTF8Char; syncDir: Integer): Integer; cdecl;
    xAccess: function(pVfs: PSQLite3VFS; const zName: PUTF8Char; flags: Integer; var pResOut: Integer): Integer; cdecl;
    xFullPathname: function(pVfs: PSQLite3VFS; const zName: PUTF8Char; nOut: Integer; zOut: PUTF8Char): Integer; cdecl;
    xDlOpen: function(pVfs: PSQLite3VFS; const zFilename: PUTF8Char): Pointer; cdecl;
    xDlError: procedure(pVfs: PSQLite3VFS; nByte: Integer; zErrMsg: PUTF8Char); cdecl;
    xDlSym: function(pVfs: PSQLite3VFS; pHandle: Pointer; const zSymbol: PUTF8Char): Pointer; cdecl;
    xDlClose: procedure(pVfs: PSQLite3VFS; pHandle: Pointer); cdecl;
    xRandomness: function(pVfs: PSQLite3VFS; nByte: Integer; zOut: PUTF8Char): Integer; cdecl;
    xSleep: function(pVfs: PSQLite3VFS; microseconds: Integer): Integer; cdecl;
    xCurrentTime: function(pVfs: PSQLite3VFS; var prNow: Double): Integer; cdecl;
    xGetLastError: function(pVfs: PSQLite3VFS; nBuf: Integer; zBuf: PUTF8Char): Integer; cdecl;
  end;

const
  SQLITE_ACCESS_EXISTS    = 0;
  SQLITE_ACCESS_READWRITE = 1;   // Used by PRAGMA temp_store_directory
  SQLITE_ACCESS_READ      = 2;   // Unused

  SQLITE_SHM_UNLOCK       = 1;
  SQLITE_SHM_LOCK         = 2;
  SQLITE_SHM_SHARED       = 4;
  SQLITE_SHM_EXCLUSIVE    = 8;

  SQLITE_SHM_NLOCK        = 8;

type
  TSQLite3MemMethods = record
    xMalloc: function(nByte: Integer): Pointer; cdecl;
    xFree: procedure(pPrior: Pointer); cdecl;
    xRealloc: function(pPrior: Pointer; nByte: Integer): Pointer; cdecl;
    xSize: function(pPrior: Pointer): Integer; cdecl;
    xRoundup: function(n: Integer): Integer; cdecl;
    xInit: function(NotUsed: Pointer): Integer; cdecl;
    xShutdown: procedure(NotUsed: Pointer); cdecl;
    pAppData: Pointer;
  end;

const
  SQLITE_CONFIG_SINGLETHREAD         = 1;  // nil
  SQLITE_CONFIG_MULTITHREAD          = 2;  // nil
  SQLITE_CONFIG_SERIALIZED           = 3;  // nil
  SQLITE_CONFIG_MALLOC               = 4;  // sqlite3_mem_methods*
  SQLITE_CONFIG_GETMALLOC            = 5;  // sqlite3_mem_methods*
  SQLITE_CONFIG_SCRATCH              = 6;  // void*, int sz, int N
  SQLITE_CONFIG_PAGECACHE            = 7;  // void*, int sz, int N
  SQLITE_CONFIG_HEAP                 = 8;  // void*, int nByte, int min
  SQLITE_CONFIG_MEMSTATUS            = 9;  // boolean
  SQLITE_CONFIG_MUTEX               = 10;  // sqlite3_mutex_methods*
  SQLITE_CONFIG_GETMUTEX            = 11;  // sqlite3_mutex_methods*
  SQLITE_CONFIG_LOOKASIDE           = 13;  // int int
  SQLITE_CONFIG_PCACHE              = 14;  // no-op
  SQLITE_CONFIG_GETPCACHE           = 15;  // no-op
  SQLITE_CONFIG_LOG                 = 16;  // xFunc, void*
  SQLITE_CONFIG_URI                 = 17;  // int
  SQLITE_CONFIG_PCACHE2             = 18;  // sqlite3_pcache_methods2*
  SQLITE_CONFIG_GETPCACHE2          = 19;  // sqlite3_pcache_methods2*
  SQLITE_CONFIG_COVERING_INDEX_SCAN = 20;  // int
  SQLITE_CONFIG_SQLLOG              = 21;  // xSqllog, void*
  SQLITE_CONFIG_MMAP_SIZE           = 22;  // sqlite3_int64, sqlite3_int64
  SQLITE_CONFIG_WIN32_HEAPSIZE      = 23;  // int nByte
  SQLITE_CONFIG_PCACHE_HDRSZ        = 24;  // int *psz
  SQLITE_CONFIG_PMASZ               = 25;  // unsigned int szPma

  SQLITE_DBCONFIG_LOOKASIDE       = 1001;  // void* int int
  SQLITE_DBCONFIG_ENABLE_FKEY     = 1002;  // int int*
  SQLITE_DBCONFIG_ENABLE_TRIGGER  = 1003;  // int int*

type
  TSQLite3BusyCallback = function(ptr: Pointer; count: Integer): Integer; cdecl;

  TSQLite3AuthorizerCallback = function(pAuthArg: Pointer; code: Integer; const zTab: PUTF8Char; const zCol: PUTF8Char; const zDb: PUTF8Char; const zAuthContext: PUTF8Char): Integer; cdecl;

const
  SQLITE_DENY   = 1;   // Abort the SQL statement with an error
  SQLITE_IGNORE = 2;   // Don't allow access, but don't generate an error

  SQLITE_CREATE_INDEX         = 1;   // Index Name      Table Name
  SQLITE_CREATE_TABLE         = 2;   // Table Name      NULL
  SQLITE_CREATE_TEMP_INDEX    = 3;   // Index Name      Table Name
  SQLITE_CREATE_TEMP_TABLE    = 4;   // Table Name      NULL
  SQLITE_CREATE_TEMP_TRIGGER  = 5;   // Trigger Name    Table Name
  SQLITE_CREATE_TEMP_VIEW     = 6;   // View Name       NULL
  SQLITE_CREATE_TRIGGER       = 7;   // Trigger Name    Table Name
  SQLITE_CREATE_VIEW          = 8;   // View Name       NULL
  SQLITE_DELETE               = 9;   // Table Name      NULL
  SQLITE_DROP_INDEX          = 10;   // Index Name      Table Name
  SQLITE_DROP_TABLE          = 11;   // Table Name      NULL
  SQLITE_DROP_TEMP_INDEX     = 12;   // Index Name      Table Name
  SQLITE_DROP_TEMP_TABLE     = 13;   // Table Name      NULL
  SQLITE_DROP_TEMP_TRIGGER   = 14;   // Trigger Name    Table Name
  SQLITE_DROP_TEMP_VIEW      = 15;   // View Name       NULL
  SQLITE_DROP_TRIGGER        = 16;   // Trigger Name    Table Name
  SQLITE_DROP_VIEW           = 17;   // View Name       NULL
  SQLITE_INSERT              = 18;   // Table Name      NULL
  SQLITE_PRAGMA              = 19;   // Pragma Name     1st arg or NULL
  SQLITE_READ                = 20;   // Table Name      Column Name
  SQLITE_SELECT              = 21;   // NULL            NULL
  SQLITE_TRANSACTION         = 22;   // Operation       NULL
  SQLITE_UPDATE              = 23;   // Table Name      Column Name
  SQLITE_ATTACH              = 24;   // Filename        NULL
  SQLITE_DETACH              = 25;   // Database Name   NULL
  SQLITE_ALTER_TABLE         = 26;   // Database Name   Table Name
  SQLITE_REINDEX             = 27;   // Index Name      NULL
  SQLITE_ANALYZE             = 28;   // Table Name      NULL
  SQLITE_CREATE_VTABLE       = 29;   // Table Name      Module Name
  SQLITE_DROP_VTABLE         = 30;   // Table Name      Module Name
  SQLITE_FUNCTION            = 31;   // NULL            Function Name
  SQLITE_SAVEPOINT           = 32;   // Operation       Savepoint Name
  SQLITE_COPY                = 0;    // No longer used
  SQLITE_RECURSIVE           = 33;   // NULL            NULL

type
  TSQLite3TraceCallback = procedure(pTraceArg: Pointer; const zTrace: PUTF8Char); cdecl;
  TSQLite3ProfileCallback = procedure(pProfileArg: Pointer; const zSql: PUTF8Char; elapseTime: UInt64); cdecl;

  TSQLite3ProgressCallback = function(pProgressArg: Pointer): Integer; cdecl;

  PSQLite3Stmt = type Pointer;

const
  SQLITE_LIMIT_LENGTH                  = 0;
  SQLITE_LIMIT_SQL_LENGTH              = 1;
  SQLITE_LIMIT_COLUMN                  = 2;
  SQLITE_LIMIT_EXPR_DEPTH              = 3;
  SQLITE_LIMIT_COMPOUND_SELECT         = 4;
  SQLITE_LIMIT_VDBE_OP                 = 5;
  SQLITE_LIMIT_FUNCTION_ARG            = 6;
  SQLITE_LIMIT_ATTACHED                = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH     = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER         = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH           = 10;
  SQLITE_LIMIT_WORKER_THREADS          = 11;

type
  TSQLite3Value = type Pointer;
  PSQLite3Value = ^TSQLite3Value;

  PPSQLite3ValueArray = ^TPSQLite3ValueArray;
  TPSQLite3ValueArray = array[0..MaxInt div SizeOf(PSQLite3Value) - 1] of PSQLite3Value;

  PSQLite3Context = type Pointer;

  TSQLite3DestructorType = procedure(p: Pointer); cdecl;

const
  SQLITE_STATIC    = Pointer(0);
  SQLITE_TRANSIENT = Pointer(-1);

  SQLITE_INTEGER  = 1;
  SQLITE_FLOAT    = 2;
  SQLITE_BLOB     = 4;
  SQLITE_NULL     = 5;
  SQLITE_TEXT     = 3;
  SQLITE3_TEXT    = 3;

type
  TSQLite3RegularFunction = procedure(ctx: PSQLite3Context; n: Integer; apVal: PPSQLite3ValueArray); cdecl;
  TSQLite3AggregateStep = procedure(ctx: PSQLite3Context; n: Integer; apVal: PPSQLite3ValueArray); cdecl;
  TSQLite3AggregateFinalize = procedure(ctx: PSQLite3Context); cdecl;

const
  SQLITE_UTF8          = 1;    // IMP: R-37514-35566
  SQLITE_UTF16LE       = 2;    // IMP: R-03371-37637
  SQLITE_UTF16BE       = 3;    // IMP: R-51971-34154
  SQLITE_UTF16         = 4;    // Use native byte order
  SQLITE_ANY           = 5;    // Deprecated
  SQLITE_UTF16_ALIGNED = 8;    // sqlite3_create_collation only

  SQLITE_DETERMINISTIC    = $800;

type
  TSQLite3AuxDataDestructor = procedure(pAux: Pointer); cdecl;

  TSQLite3CollationCompare = function(pUser: Pointer; n1: Integer; const z1: Pointer; n2: Integer; const z2: Pointer): Integer; cdecl;
  TSQLite3CollationDestructor = procedure(pUser: Pointer); cdecl;

  TSQLite3CollationNeededCallback = procedure(pCollNeededArg: Pointer; db: PSQLite3; eTextRep: Integer; const zExternal: PUTF8Char); cdecl;
  TSQLite3CollationNeededCallback16 = procedure(pCollNeededArg: Pointer; db: PSQLite3; eTextRep: Integer; const zExternal: PWideChar); cdecl;

  TSQLite3CommitCallback = function(pCommitArg: Pointer): Integer; cdecl;
  TSQLite3RollbackCallback = procedure(pRollbackArg: Pointer); cdecl;

  TSQLite3UpdateCallback = procedure(pUpdateArg: Pointer; op: Integer; const zDb: PUTF8Char; const zTbl: PUTF8Char; iKey: Int64); cdecl;

  TSQLiteAutoExtensionEntryPoint = procedure; cdecl;

  TSQLite3FTS3Func = procedure(pContext: PSQLite3Context; argc: Integer; argv: PPSQLite3ValueArray); cdecl;

  PSQLite3VTab = ^TSQLite3VTab;
  PSQLite3IndexInfo = ^TSQLite3IndexInfo;
  PSQLite3VTabCursor = ^TSQLite3VTabCursor;
  PSQLite3Module = ^TSQLite3Module;

  TSQLite3Module = record
    iVersion: Integer;
    xCreate: function(db: PSQLite3; pAux: Pointer; argc: Integer; const argv: PPUTF8CharArray; var ppVTab: PSQLite3VTab; var pzErr: PUTF8Char): Integer; cdecl;
    xConnect: function(db: PSQLite3; pAux: Pointer; argc: Integer; const argv: PPUTF8CharArray; var ppVTab: PSQLite3VTab; var pzErr: PUTF8Char): Integer; cdecl;
    xBestIndex: function(pVTab: PSQLite3VTab; pInfo: PSQLite3IndexInfo): Integer; cdecl;
    xDisconnect: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xDestroy: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xOpen: function(pVTab: PSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer; cdecl;
    xClose: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xFilter: function(pVtabCursor: PSQLite3VTabCursor; idxNum: Integer; const idxStr: PUTF8Char; argc: Integer; argv: PPSQLite3ValueArray): Integer; cdecl;
    xNext: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xEof: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xColumn: function(pVtabCursor: PSQLite3VTabCursor; sContext: PSQLite3Context; p2: Integer): Integer; cdecl;
    xRowid: function(pVtabCursor: PSQLite3VTabCursor; var pRowid: Int64): Integer; cdecl;
    xUpdate: function(pVtab: PSQLite3VTab; nArg: Integer; ppArg: PPSQLite3ValueArray; var pRowid: Int64): Integer; cdecl;
    xBegin: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xSync: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xCommit: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xRollback: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xFindFunction: function(pVtab: PSQLite3VTab; nArg: Integer; const zName: PUTF8Char; var pxFunc: TSQLite3FTS3Func; var ppArg: Pointer): Integer; cdecl;
    xRename: function(pVtab: PSQLite3VTab; const zNew: PUTF8Char): Integer; cdecl;
  end;

  TSQLite3IndexConstraint = record
    iColumn: Integer;
    op: Byte;
    usable: Byte;
    iTermOffset: Integer;
  end;

  PSQLite3IndexConstraintArray = ^TSQLite3IndexConstraintArray;
  TSQLite3IndexConstraintArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraint) - 1] of TSQLite3IndexConstraint;

  TSQLite3IndexOrderBy = record
    iColumn: Integer;
    desc: Byte;
  end;

  PSQLite3IndexOrderByArray = ^TSQLite3IndexOrderByArray;
  TSQLite3IndexOrderByArray = array[0..MaxInt div SizeOf(TSQLite3IndexOrderBy) - 1] of TSQLite3IndexOrderBy;

  TSQLite3IndexConstraintUsage = record
    argvIndex: Integer;
    omit: Byte;
  end;

  PSQLite3IndexConstraintUsageArray = ^TSQLite3IndexConstraintUsageArray;
  TSQLite3IndexConstraintUsageArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraintUsage) - 1] of TSQLite3IndexConstraintUsage;

  TSQLite3IndexInfo = record
    nConstraint: Integer;
    aConstraint: PSQLite3IndexConstraintArray;
    nOrderBy: Integer;
    aOrderBy: PSQLite3IndexOrderByArray;
    aConstraintUsage: PSQLite3IndexConstraintUsageArray;
    idxNum: Integer;
    idxStr: PUTF8Char;
    needToFreeIdxStr: Integer;
    orderByConsumed: Integer;
    estimatedCost: Double;
  end;

  TSQLite3VTab = record
    pModule: PSQLite3Module;
    nRef: Integer;
    zErrMsg: PUTF8Char;
  end;

  TSQLite3VTabCursor = record
    pVtab: PSQLite3VTab;
  end;

const
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;

type
  PSQLite3Blob = type Pointer;

  TSQLite3ModuleDestructor = procedure(pAux: Pointer); cdecl;

  TSQLite3MutexMethods = record
    xMutexInit: function: Integer; cdecl;
    xMutexEnd: function: Integer; cdecl;
    xMutexAlloc: function(id: Integer): PSQLite3Mutex; cdecl;
    xMutexFree: procedure(p: PSQLite3Mutex); cdecl;
    xMutexEnter: procedure(p: PSQLite3Mutex); cdecl;
    xMutexTry: function(p: PSQLite3Mutex): Integer; cdecl;
    xMutexLeave: procedure(p: PSQLite3Mutex); cdecl;
    xMutexHeld: function(p: PSQLite3Mutex): Integer; cdecl;
    xMutexNotheld: function(p: PSQLite3Mutex): Integer; cdecl;
  end;

const
  SQLITE_MUTEX_FAST           =  0;
  SQLITE_MUTEX_RECURSIVE      =  1;
  SQLITE_MUTEX_STATIC_MASTER  =  2;
  SQLITE_MUTEX_STATIC_MEM     =  3;  // sqlite3_malloc()
  SQLITE_MUTEX_STATIC_MEM2    =  4;  // NOT USED
  SQLITE_MUTEX_STATIC_OPEN    =  4;  // sqlite3BtreeOpen()
  SQLITE_MUTEX_STATIC_PRNG    =  5;  // sqlite3_random()
  SQLITE_MUTEX_STATIC_LRU     =  6;  // lru page list
  SQLITE_MUTEX_STATIC_LRU2    =  7;  // NOT USED
  SQLITE_MUTEX_STATIC_PMEM    =  7;  // sqlite3PageMalloc()
  SQLITE_MUTEX_STATIC_APP1    =  8;  // For use by application
  SQLITE_MUTEX_STATIC_APP2    =  9;  // For use by application
  SQLITE_MUTEX_STATIC_APP3    = 10;  // For use by application
  SQLITE_MUTEX_STATIC_VFS1    = 11;  // For use by built-in VFS
  SQLITE_MUTEX_STATIC_VFS2    = 12;  // For use by extension VFS
  SQLITE_MUTEX_STATIC_VFS3    = 13;  // For use by application VFS

  SQLITE_TESTCTRL_FIRST                  =  5;
  SQLITE_TESTCTRL_PRNG_SAVE              =  5;
  SQLITE_TESTCTRL_PRNG_RESTORE           =  6;
  SQLITE_TESTCTRL_PRNG_RESET             =  7;
  SQLITE_TESTCTRL_BITVEC_TEST            =  8;
  SQLITE_TESTCTRL_FAULT_INSTALL          =  9;
  SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS    = 10;
  SQLITE_TESTCTRL_PENDING_BYTE           = 11;
  SQLITE_TESTCTRL_ASSERT                 = 12;
  SQLITE_TESTCTRL_ALWAYS                 = 13;
  SQLITE_TESTCTRL_RESERVE                = 14;
  SQLITE_TESTCTRL_OPTIMIZATIONS          = 15;
  SQLITE_TESTCTRL_ISKEYWORD              = 16;
  SQLITE_TESTCTRL_SCRATCHMALLOC          = 17;
  SQLITE_TESTCTRL_LOCALTIME_FAULT        = 18;
  SQLITE_TESTCTRL_EXPLAIN_STMT           = 19;  // NOT USED
  SQLITE_TESTCTRL_NEVER_CORRUPT          = 20;
  SQLITE_TESTCTRL_VDBE_COVERAGE          = 21;
  SQLITE_TESTCTRL_BYTEORDER              = 22;
  SQLITE_TESTCTRL_ISINIT                 = 23;
  SQLITE_TESTCTRL_SORTER_MMAP            = 24;
  SQLITE_TESTCTRL_IMPOSTER               = 25;
  SQLITE_TESTCTRL_LAST                   = 25;

  SQLITE_STATUS_MEMORY_USED         = 0;
  SQLITE_STATUS_PAGECACHE_USED      = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW  = 2;
  SQLITE_STATUS_SCRATCH_USED        = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW    = 4;
  SQLITE_STATUS_MALLOC_SIZE         = 5;
  SQLITE_STATUS_PARSER_STACK        = 6;
  SQLITE_STATUS_PAGECACHE_SIZE      = 7;
  SQLITE_STATUS_SCRATCH_SIZE        = 8;
  SQLITE_STATUS_MALLOC_COUNT        = 9;

  SQLITE_DBSTATUS_LOOKASIDE_USED      =  0;
  SQLITE_DBSTATUS_CACHE_USED          =  1;
  SQLITE_DBSTATUS_SCHEMA_USED         =  2;
  SQLITE_DBSTATUS_STMT_USED           =  3;
  SQLITE_DBSTATUS_LOOKASIDE_HIT       =  4;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE =  5;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL =  6;
  SQLITE_DBSTATUS_CACHE_HIT           =  7;
  SQLITE_DBSTATUS_CACHE_MISS          =  8;
  SQLITE_DBSTATUS_CACHE_WRITE         =  9;
  SQLITE_DBSTATUS_DEFERRED_FKS        = 10;
  SQLITE_DBSTATUS_MAX                 = 10;   // Largest defined DBSTATUS

  SQLITE_STMTSTATUS_FULLSCAN_STEP     = 1;
  SQLITE_STMTSTATUS_SORT              = 2;
  SQLITE_STMTSTATUS_AUTOINDEX         = 3;
  SQLITE_STMTSTATUS_VM_STEP           = 4;

  SQLITE_CHECKPOINT_PASSIVE  = 0;  // Do as much as possible w/o blocking
  SQLITE_CHECKPOINT_FULL     = 1;  // Wait for writers, then checkpoint
  SQLITE_CHECKPOINT_RESTART  = 2;  // Like FULL but wait for for readers
  SQLITE_CHECKPOINT_TRUNCATE = 3;  // Like RESTART but also truncate WAL

  SQLITE_VTAB_CONSTRAINT_SUPPORT = 1;

  SQLITE_ROLLBACK = 1;
  SQLITE_FAIL     = 3;
  SQLITE_REPLACE  = 5;

  SQLITE_SCANSTAT_NLOOP    = 0;
  SQLITE_SCANSTAT_NVISIT   = 1;
  SQLITE_SCANSTAT_EST      = 2;
  SQLITE_SCANSTAT_NAME     = 3;
  SQLITE_SCANSTAT_EXPLAIN  = 4;
  SQLITE_SCANSTAT_SELECTID = 5;

  NOT_WITHIN      = 0;  // Object completely outside of query region
  PARTLY_WITHIN   = 1;  // Object partially overlaps query region
  FULLY_WITHIN    = 2;  // Object fully contained within query region

type
  PSQLite3PCache = type Pointer;

  TSQLite3PCacheMethods = record
    pArg: Pointer;
    xInit: function(pArg: Pointer): Integer; cdecl;
    xShutdown: procedure(pArg: Pointer); cdecl;
    xCreate: function(szPage: Integer; bPurgeable: Integer): PSQLite3PCache; cdecl;
    xCachesize: procedure(pCache: PSQLite3PCache; nCachesize: Integer); cdecl;
    xPagecount: function(pCache: PSQLite3PCache): Integer; cdecl;
    xFetch: function(pCache: PSQLite3PCache; key: Cardinal; createFlag: Integer): Pointer; cdecl;
    xUnpin: procedure(pCache: PSQLite3PCache; pPg: Pointer; discard: Integer); cdecl;
    xRekey: procedure(pCache: PSQLite3PCache; pPg: Pointer; oldKey: Cardinal; newKey: Cardinal); cdecl;
    xTruncate: procedure(pCache: PSQLite3PCache; iLimit: Cardinal); cdecl;
    xDestroy: procedure(pCache: PSQLite3PCache); cdecl;
  end;

  PSQLite3Backup = type Pointer;

  TSQLite3UnlockNotifyCallback = procedure(apArg: PPointerArray; nArg: Integer); cdecl;

var
  sqlite3_libversion: function: PUTF8Char; cdecl;
  sqlite3_sourceid: function: PUTF8Char; cdecl;
  sqlite3_libversion_number: function: Integer; cdecl;
  sqlite3_threadsafe: function: Integer; cdecl;
  sqlite3_close: function(db: PSQLite3): Integer; cdecl;
  sqlite3_close_v2: function(db: PSQLite3): Integer; cdecl;
  sqlite3_exec: function(db: PSQLite3; const sql: PUTF8Char; callback: TSQLite3Callback; pArg: Pointer; errmsg: PPAnsiChar): Integer; cdecl;
  sqlite3_initialize: function: Integer; cdecl;
  sqlite3_shutdown: function: Integer; cdecl;
  sqlite3_os_init: function: Integer; cdecl;
  sqlite3_os_end: function: Integer; cdecl;
  sqlite3_config: function(op: Integer{; ...}): Integer; cdecl;
  sqlite3_db_config: function(db: PSQLite3; op: Integer{; ...}): Integer; cdecl;
  sqlite3_extended_result_codes: function(db: PSQLite3; onoff: Integer): Integer; cdecl;
  sqlite3_last_insert_rowid: function(db: PSQLite3): Int64; cdecl;
  sqlite3_changes: function(db: PSQLite3): Integer; cdecl;
  sqlite3_total_changes: function(db: PSQLite3): Integer; cdecl;
  sqlite3_interrupt: procedure(db: PSQLite3); cdecl;
  sqlite3_complete: function(const sql: PUTF8Char): Integer; cdecl;
  sqlite3_complete16: function(const sql: PWideChar): Integer; cdecl;
  sqlite3_busy_handler: function(db: PSQLite3; xBusy: TSQLite3BusyCallback; pArg: Pointer): Integer; cdecl;
  sqlite3_busy_timeout: function(db: PSQLite3; ms: Integer): Integer; cdecl;
  sqlite3_get_table: function(db: PSQLite3; const zSql: PUTF8Char; var pazResult: PPUTF8CharArray; pnRow: PInteger; pnColumn: PInteger; pzErrmsg: PPAnsiChar): Integer; cdecl;
  sqlite3_free_table: procedure(result: PPUTF8CharArray); cdecl;
  sqlite3_mprintf: function(const zFormat: PUTF8Char{; ...}): PUTF8Char; cdecl;
  sqlite3_vmprintf: function(const zFormat: PUTF8Char; ap: Pointer{va_list}): PUTF8Char; cdecl;
  sqlite3_snprintf: function(n: Integer; zBuf: PUTF8Char; const zFormat: PUTF8Char{; ...}): PUTF8Char; cdecl;
  sqlite3_malloc: function(n: Integer): Pointer; cdecl;
  sqlite3_realloc: function(pOld: Pointer; n: Integer): Pointer; cdecl;
  sqlite3_free: procedure(p: Pointer); cdecl;
  sqlite3_memory_used: function: Int64; cdecl;
  sqlite3_memory_highwater: function(resetFlag: Integer): Int64; cdecl;
  sqlite3_randomness: procedure(N: Integer; P: Pointer); cdecl;
  sqlite3_set_authorizer: function(db: PSQLite3; xAuth: TSQLite3AuthorizerCallback; pUserData: Pointer): Integer; cdecl;
  sqlite3_trace: function(db: PSQLite3; xTrace: TSQLite3TraceCallback; pArg: Pointer): Pointer; cdecl;
  sqlite3_profile: function(db: PSQLite3; xProfile: TSQLite3ProfileCallback; pArg: Pointer): Pointer; cdecl;
  sqlite3_progress_handler: procedure(db: PSQLite3; nOps: Integer; xProgress: TSQLite3ProgressCallback; pArg: Pointer); cdecl;
  sqlite3_open: function(const filename: PUTF8Char; var ppDb: PSQLite3): Integer; cdecl;
  sqlite3_open16: function(const filename: PWideChar; var ppDb: PSQLite3): Integer; cdecl;
  sqlite3_open_v2: function(const filename: PUTF8Char; var ppDb: PSQLite3; flags: Integer; const zVfs: PUTF8Char): Integer; cdecl;
  sqlite3_errcode: function(db: PSQLite3): Integer; cdecl;
  sqlite3_extended_errcode: function(db: PSQLite3): Integer; cdecl;
  sqlite3_errmsg: function(db: PSQLite3): PUTF8Char; cdecl;
  sqlite3_errmsg16: function(db: PSQLite3): PWideChar; cdecl;
  sqlite3_limit: function(db: PSQLite3; limitId: Integer; newLimit: Integer): Integer; cdecl;
  sqlite3_prepare: function(db: PSQLite3; const zSql: PUTF8Char; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPAnsiChar): Integer; cdecl;
  sqlite3_prepare_v2: function(db: PSQLite3; const zSql: PUTF8Char; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPAnsiChar): Integer; cdecl;
  sqlite3_prepare16: function(db: PSQLite3; const zSql: PWideChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPWideChar): Integer; cdecl;
  sqlite3_prepare16_v2: function(db: PSQLite3; const zSql: PWideChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPWideChar): Integer; cdecl;
  sqlite3_sql: function(pStmt: PSQLite3Stmt): PUTF8Char; cdecl;
  sqlite3_bind_blob: function(pStmt: PSQLite3Stmt; i: Integer; const zData: Pointer; n: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
  sqlite3_bind_double: function(pStmt: PSQLite3Stmt; i: Integer; rValue: Double): Integer; cdecl;
  sqlite3_bind_int: function(p: PSQLite3Stmt; i: Integer; iValue: Integer): Integer; cdecl;
  sqlite3_bind_int64: function(pStmt: PSQLite3Stmt; i: Integer; iValue: Int64): Integer; cdecl;
  sqlite3_bind_null: function(pStmt: PSQLite3Stmt; i: Integer): Integer; cdecl;
  sqlite3_bind_text: function(pStmt: PSQLite3Stmt; i: Integer; const zData: PUTF8Char; n: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
  sqlite3_bind_text16: function(pStmt: PSQLite3Stmt; i: Integer; const zData: PWideChar; nData: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
  sqlite3_bind_value: function(pStmt: PSQLite3Stmt; i: Integer; const pValue: PSQLite3Value): Integer; cdecl;
  sqlite3_bind_zeroblob: function(pStmt: PSQLite3Stmt; i: Integer; n: Integer): Integer; cdecl;
  sqlite3_bind_parameter_count: function(pStmt: PSQLite3Stmt): Integer; cdecl;
  sqlite3_bind_parameter_name: function(pStmt: PSQLite3Stmt; i: Integer): PUTF8Char; cdecl;
  sqlite3_bind_parameter_index: function(pStmt: PSQLite3Stmt; const zName: PUTF8Char): Integer; cdecl;
  sqlite3_clear_bindings: function(pStmt: PSQLite3Stmt): Integer; cdecl;
  sqlite3_column_count: function(pStmt: PSQLite3Stmt): Integer; cdecl;
  sqlite3_column_name: function(pStmt: PSQLite3Stmt; N: Integer): PUTF8Char; cdecl;
  sqlite3_column_name16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
  sqlite3_column_database_name: function(pStmt: PSQLite3Stmt; N: Integer): PUTF8Char; cdecl;
  sqlite3_column_database_name16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
  sqlite3_column_table_name: function(pStmt: PSQLite3Stmt; N: Integer): PUTF8Char; cdecl;
  sqlite3_column_table_name16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
  sqlite3_column_origin_name: function(pStmt: PSQLite3Stmt; N: Integer): PUTF8Char; cdecl;
  sqlite3_column_origin_name16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
  sqlite3_column_decltype: function(pStmt: PSQLite3Stmt; N: Integer): PUTF8Char; cdecl;
  sqlite3_column_decltype16: function(pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
  sqlite3_step: function(pStmt: PSQLite3Stmt): Integer; cdecl;
  sqlite3_data_count: function(pStmt: PSQLite3Stmt): Integer; cdecl;
  sqlite3_column_blob: function(pStmt: PSQLite3Stmt; iCol: Integer): Pointer; cdecl;
  sqlite3_column_bytes: function(pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
  sqlite3_column_bytes16: function(pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
  sqlite3_column_double: function(pStmt: PSQLite3Stmt; iCol: Integer): Double; cdecl;
  sqlite3_column_int: function(pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
  sqlite3_column_int64: function(pStmt: PSQLite3Stmt; iCol: Integer): Int64; cdecl;
  sqlite3_column_text: function(pStmt: PSQLite3Stmt; iCol: Integer): PUTF8Char; cdecl;
  sqlite3_column_text16: function(pStmt: PSQLite3Stmt; iCol: Integer): PWideChar; cdecl;
  sqlite3_column_type: function(pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
  sqlite3_column_value: function(pStmt: PSQLite3Stmt; iCol: Integer): PSQLite3Value; cdecl;
  sqlite3_finalize: function(pStmt: PSQLite3Stmt): Integer; cdecl;
  sqlite3_reset: function(pStmt: PSQLite3Stmt): Integer; cdecl;
  sqlite3_create_function: function(db: PSQLite3; const zFunctionName: PUTF8Char; nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: TSQLite3RegularFunction; xStep: TSQLite3AggregateStep; xFinal: TSQLite3AggregateFinalize): Integer; cdecl;
  sqlite3_create_function16: function(db: PSQLite3; const zFunctionName: PWideChar; nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: TSQLite3RegularFunction; xStep: TSQLite3AggregateStep; xFinal: TSQLite3AggregateFinalize): Integer; cdecl;
  sqlite3_value_blob: function(pVal: PSQLite3Value): Pointer; cdecl;
  sqlite3_value_bytes: function(pVal: PSQLite3Value): Integer; cdecl;
  sqlite3_value_bytes16: function(pVal: PSQLite3Value): Integer; cdecl;
  sqlite3_value_double: function(pVal: PSQLite3Value): Double; cdecl;
  sqlite3_value_int: function(pVal: PSQLite3Value): Integer; cdecl;
  sqlite3_value_int64: function(pVal: PSQLite3Value): Int64; cdecl;
  sqlite3_value_text: function(pVal: PSQLite3Value): PUTF8Char; cdecl;
  sqlite3_value_text16: function(pVal: PSQLite3Value): PWideChar; cdecl;
  sqlite3_value_text16le: function(pVal: PSQLite3Value): Pointer; cdecl;
  sqlite3_value_text16be: function(pVal: PSQLite3Value): Pointer; cdecl;
  sqlite3_value_type: function(pVal: PSQLite3Value): Integer; cdecl;
  sqlite3_value_numeric_type: function(pVal: PSQLite3Value): Integer; cdecl;
  sqlite3_aggregate_context: function(p: PSQLite3Context; nBytes: Integer): Pointer; cdecl;
  sqlite3_user_data: function(p: PSQLite3Context): Pointer; cdecl;
  sqlite3_context_db_handle: function(p: PSQLite3Context): PSQLite3; cdecl;
  sqlite3_get_auxdata: function(pCtx: PSQLite3Context; N: Integer): Pointer; cdecl;
  sqlite3_set_auxdata: procedure(pCtx: PSQLite3Context; N: Integer; pAux: Pointer; xDelete: TSQLite3AuxDataDestructor); cdecl;
  sqlite3_result_blob: procedure(pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
  sqlite3_result_double: procedure(pCtx: PSQLite3Context; rVal: Double); cdecl;
  sqlite3_result_error: procedure(pCtx: PSQLite3Context; const z: PUTF8Char; n: Integer); cdecl;
  sqlite3_result_error16: procedure(pCtx: PSQLite3Context; const z: PWideChar; n: Integer); cdecl;
  sqlite3_result_error_toobig: procedure(pCtx: PSQLite3Context); cdecl;
  sqlite3_result_error_nomem: procedure(pCtx: PSQLite3Context); cdecl;
  sqlite3_result_error_code: procedure(pCtx: PSQLite3Context; errCode: Integer); cdecl;
  sqlite3_result_int: procedure(pCtx: PSQLite3Context; iVal: Integer); cdecl;
  sqlite3_result_int64: procedure(pCtx: PSQLite3Context; iVal: Int64); cdecl;
  sqlite3_result_null: procedure(pCtx: PSQLite3Context); cdecl;
  sqlite3_result_text: procedure(pCtx: PSQLite3Context; const z: PUTF8Char; n: Integer; xDel: TSQLite3DestructorType); cdecl;
  sqlite3_result_text16: procedure(pCtx: PSQLite3Context; const z: PWideChar; n: Integer; xDel: TSQLite3DestructorType); cdecl;
  sqlite3_result_text16le: procedure(pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
  sqlite3_result_text16be: procedure(pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
  sqlite3_result_value: procedure(pCtx: PSQLite3Context; pValue: PSQLite3Value); cdecl;
  sqlite3_result_zeroblob: procedure(pCtx: PSQLite3Context; n: Integer); cdecl;
  sqlite3_create_collation: function(db: PSQLite3; const zName: PUTF8Char; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare): Integer; cdecl;
  sqlite3_create_collation_v2: function(db: PSQLite3; const zName: PUTF8Char; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare; xDestroy: TSQLite3CollationDestructor): Integer; cdecl;
  sqlite3_create_collation16: function(db: PSQLite3; const zName: PWideChar; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare): Integer; cdecl;
  sqlite3_collation_needed: function(db: PSQLite3; pCollNeededArg: Pointer; xCollNeeded: TSQLite3CollationNeededCallback): Integer; cdecl;
  sqlite3_collation_needed16: function(db: PSQLite3; pCollNeededArg: Pointer; xCollNeeded16: TSQLite3CollationNeededCallback16): Integer; cdecl;
  sqlite3_sleep: function(ms: Integer): Integer; cdecl;
  sqlite3_get_autocommit: function(db: PSQLite3): Integer; cdecl;
  sqlite3_db_handle: function(pStmt: PSQLite3Stmt): PSQLite3; cdecl;
  sqlite3_next_stmt: function(pDb: PSQLite3; pStmt: PSQLite3Stmt): PSQLite3Stmt; cdecl;
  sqlite3_commit_hook: function(db: PSQLite3; xCallback: TSQLite3CommitCallback; pArg: Pointer): Pointer; cdecl;
  sqlite3_rollback_hook: function(db: PSQLite3; xCallback: TSQLite3RollbackCallback; pArg: Pointer): Pointer; cdecl;
  sqlite3_update_hook: function(db: PSQLite3; xCallback: TSQLite3UpdateCallback; pArg: Pointer): Pointer; cdecl;
  sqlite3_enable_shared_cache: function(enable: Integer): Integer; cdecl;
  sqlite3_release_memory: function(n: Integer): Integer; cdecl;
  sqlite3_soft_heap_limit64: function(n: Int64): int64; cdecl;
  sqlite3_table_column_metadata: function(db: PSQLite3; const zDbName: PUTF8Char; const zTableName: PUTF8Char; const zColumnName: PUTF8Char; const pzDataType: PPAnsiChar; const pzCollSeq: PPAnsiChar; pNotNull: PInteger; pPrimaryKey: PInteger; pAutoinc: PInteger): Integer; cdecl;
  sqlite3_load_extension: function(db: PSQLite3; const zFile: PUTF8Char; const zProc: PUTF8Char; pzErrMsg: PPAnsiChar): Integer; cdecl;
  sqlite3_enable_load_extension: function(db: PSQLite3; onoff: Integer): Integer; cdecl;
  sqlite3_auto_extension: function(xEntryPoint: TSQLiteAutoExtensionEntryPoint): Integer; cdecl;
  sqlite3_reset_auto_extension: procedure; cdecl;
  sqlite3_create_module: function(db: PSQLite3; const zName: PUTF8Char; const p: PSQLite3Module; pClientData: Pointer): Integer; cdecl;
  sqlite3_create_module_v2: function(db: PSQLite3; const zName: PUTF8Char; const p: PSQLite3Module; pClientData: Pointer; xDestroy: TSQLite3ModuleDestructor): Integer; cdecl;
  sqlite3_declare_vtab: function(db: PSQLite3; const zSQL: PUTF8Char): Integer; cdecl;
  sqlite3_overload_function: function(db: PSQLite3; const zFuncName: PUTF8Char; nArg: Integer): Integer; cdecl;
  sqlite3_blob_open: function(db: PSQLite3; const zDb: PUTF8Char; const zTable: PUTF8Char; const zColumn: PUTF8Char; iRow: Int64; flags: Integer; var ppBlob: PSQLite3Blob): Integer; cdecl;
  sqlite3_blob_close: function(pBlob: PSQLite3Blob): Integer; cdecl;
  sqlite3_blob_bytes: function(pBlob: PSQLite3Blob): Integer; cdecl;
  sqlite3_blob_read: function(pBlob: PSQLite3Blob; Z: Pointer; N: Integer; iOffset: Integer): Integer; cdecl;
  sqlite3_blob_write: function(pBlob: PSQLite3Blob; const z: Pointer; n: Integer; iOffset: Integer): Integer; cdecl;
  sqlite3_vfs_find: function(const zVfsName: PUTF8Char): PSQLite3VFS; cdecl;
  sqlite3_vfs_register: function(pVfs: PSQLite3VFS; makeDflt: Integer): Integer; cdecl;
  sqlite3_vfs_unregister: function(pVfs: PSQLite3VFS): Integer; cdecl;
  sqlite3_mutex_alloc: function(id: Integer): PSQLite3Mutex; cdecl;
  sqlite3_mutex_free: procedure(p: PSQLite3Mutex); cdecl;
  sqlite3_mutex_enter: procedure(p: PSQLite3Mutex); cdecl;
  sqlite3_mutex_try: function(p: PSQLite3Mutex): Integer; cdecl;
  sqlite3_mutex_leave: procedure(p: PSQLite3Mutex); cdecl;
  sqlite3_db_mutex: function(db: PSQLite3): PSQLite3Mutex; cdecl;
  sqlite3_file_control: function(db: PSQLite3; const zDbName: PUTF8Char; op: Integer; pArg: Pointer): Integer; cdecl;
  sqlite3_test_control: function(op: Integer{; ...}): Integer; cdecl;
  sqlite3_status: function(op: Integer; var pCurrent: Integer; var pHighwater: Integer; resetFlag: Integer): Integer; cdecl;
  sqlite3_db_status: function(db: PSQLite3; op: Integer; var pCur: Integer; var pHiwtr: Integer; resetFlg: Integer): Integer; cdecl;
  sqlite3_stmt_status: function(pStmt: PSQLite3Stmt; op: Integer; resetFlg: Integer): Integer; cdecl;
  sqlite3_backup_init: function(pDest: PSQLite3; const zDestName: PUTF8Char; pSource: PSQLite3; const zSourceName: PUTF8Char): PSQLite3Backup; cdecl;
  sqlite3_backup_step: function(p: PSQLite3Backup; nPage: Integer): Integer; cdecl;
  sqlite3_backup_finish: function(p: PSQLite3Backup): Integer; cdecl;
  sqlite3_backup_remaining: function(p: PSQLite3Backup): Integer; cdecl;
  sqlite3_backup_pagecount: function(p: PSQLite3Backup): Integer; cdecl;
  sqlite3_strnicmp: function(const zLeft: PUTF8Char; const zRight: PUTF8Char; N: Integer): Integer; cdecl;

function LibSQLite3Load(const ALibName: string = libsqlite3_dll; const AInitialize: Boolean = False): Boolean;
procedure LibSQLite3Unload;

implementation

uses
  Windows,
  SysUtils,
  SyncObjs;

var
  GHandle: THandle = 0;
  GLock: TCriticalSection = nil;
  GIsLoadError: Boolean = False;
  GIsInitialized: Boolean = False;

function GetProcAddr(const AProcName: PAnsiChar): Pointer; inline;
begin
  Result := GetProcAddress(GHandle, AProcName);
  if Result = nil then begin
    raise Exception.CreateFmt('SQLite3: Unable to find proc "%s"', [AProcName]);
  end;
end;

function LibSQLite3Load(const ALibName: string; const AInitialize: Boolean): Boolean;
Begin
  Result := GIsInitialized;

  if GIsLoadError or GIsInitialized then begin
    Exit;
  end;

  GLock.Acquire;
  try
    if GIsLoadError or GIsInitialized then begin
      Exit;
    end;

    if GHandle = 0 then begin
      GHandle := SafeLoadLibrary(PChar(ALibName));
      if GHandle = 0 then begin
        GIsLoadError := True;
        raise Exception.CreateFmt(
          'SQLite3: Unable to load library %s - %s', [ALibName, SysErrorMessage(GetLastError)]
        );
      end;
    end;

    GIsInitialized := True;

    try
      sqlite3_libversion := GetProcAddr('sqlite3_libversion');
      sqlite3_sourceid := GetProcAddr('sqlite3_sourceid');
      sqlite3_libversion_number := GetProcAddr('sqlite3_libversion_number');
      sqlite3_threadsafe := GetProcAddr('sqlite3_threadsafe');
      sqlite3_close := GetProcAddr('sqlite3_close');
      sqlite3_close_v2 := GetProcAddr('sqlite3_close_v2');
      sqlite3_exec := GetProcAddr('sqlite3_exec');
      sqlite3_initialize := GetProcAddr('sqlite3_initialize');
      sqlite3_shutdown := GetProcAddr('sqlite3_shutdown');
      sqlite3_os_init := GetProcAddr('sqlite3_os_init');
      sqlite3_os_end := GetProcAddr('sqlite3_os_end');
      sqlite3_config := GetProcAddr('sqlite3_config');
      sqlite3_db_config := GetProcAddr('sqlite3_db_config');
      sqlite3_extended_result_codes := GetProcAddr('sqlite3_extended_result_codes');
      sqlite3_last_insert_rowid := GetProcAddr('sqlite3_last_insert_rowid');
      sqlite3_changes := GetProcAddr('sqlite3_changes');
      sqlite3_total_changes := GetProcAddr('sqlite3_total_changes');
      sqlite3_interrupt := GetProcAddr('sqlite3_interrupt');
      sqlite3_complete := GetProcAddr('sqlite3_complete');
      sqlite3_complete16 := GetProcAddr('sqlite3_complete16');
      sqlite3_busy_handler := GetProcAddr('sqlite3_busy_handler');
      sqlite3_busy_timeout := GetProcAddr('sqlite3_busy_timeout');
      sqlite3_get_table := GetProcAddr('sqlite3_get_table');
      sqlite3_free_table := GetProcAddr('sqlite3_free_table');
      sqlite3_mprintf := GetProcAddr('sqlite3_mprintf');
      sqlite3_vmprintf := GetProcAddr('sqlite3_vmprintf');
      sqlite3_snprintf := GetProcAddr('sqlite3_snprintf');
      sqlite3_malloc := GetProcAddr('sqlite3_malloc');
      sqlite3_realloc := GetProcAddr('sqlite3_realloc');
      sqlite3_free := GetProcAddr('sqlite3_free');
      sqlite3_memory_used := GetProcAddr('sqlite3_memory_used');
      sqlite3_memory_highwater := GetProcAddr('sqlite3_memory_highwater');
      sqlite3_randomness := GetProcAddr('sqlite3_randomness');
      sqlite3_set_authorizer := GetProcAddr('sqlite3_set_authorizer');
      sqlite3_trace := GetProcAddr('sqlite3_trace');
      sqlite3_profile := GetProcAddr('sqlite3_profile');
      sqlite3_progress_handler := GetProcAddr('sqlite3_progress_handler');
      sqlite3_open := GetProcAddr('sqlite3_open');
      sqlite3_open16 := GetProcAddr('sqlite3_open16');
      sqlite3_open_v2 := GetProcAddr('sqlite3_open_v2');
      sqlite3_errcode := GetProcAddr('sqlite3_errcode');
      sqlite3_extended_errcode := GetProcAddr('sqlite3_extended_errcode');
      sqlite3_errmsg := GetProcAddr('sqlite3_errmsg');
      sqlite3_errmsg16 := GetProcAddr('sqlite3_errmsg16');
      sqlite3_limit := GetProcAddr('sqlite3_limit');
      sqlite3_prepare := GetProcAddr('sqlite3_prepare');
      sqlite3_prepare_v2 := GetProcAddr('sqlite3_prepare_v2');
      sqlite3_prepare16 := GetProcAddr('sqlite3_prepare16');
      sqlite3_prepare16_v2 := GetProcAddr('sqlite3_prepare16_v2');
      sqlite3_sql := GetProcAddr('sqlite3_sql');
      sqlite3_bind_blob := GetProcAddr('sqlite3_bind_blob');
      sqlite3_bind_double := GetProcAddr('sqlite3_bind_double');
      sqlite3_bind_int := GetProcAddr('sqlite3_bind_int');
      sqlite3_bind_int64 := GetProcAddr('sqlite3_bind_int64');
      sqlite3_bind_null := GetProcAddr('sqlite3_bind_null');
      sqlite3_bind_text := GetProcAddr('sqlite3_bind_text');
      sqlite3_bind_text16 := GetProcAddr('sqlite3_bind_text16');
      sqlite3_bind_value := GetProcAddr('sqlite3_bind_value');
      sqlite3_bind_zeroblob := GetProcAddr('sqlite3_bind_zeroblob');
      sqlite3_bind_parameter_count := GetProcAddr('sqlite3_bind_parameter_count');
      sqlite3_bind_parameter_name := GetProcAddr('sqlite3_bind_parameter_name');
      sqlite3_bind_parameter_index := GetProcAddr('sqlite3_bind_parameter_index');
      sqlite3_clear_bindings := GetProcAddr('sqlite3_clear_bindings');
      sqlite3_column_count := GetProcAddr('sqlite3_column_count');
      sqlite3_column_name := GetProcAddr('sqlite3_column_name');
      sqlite3_column_name16 := GetProcAddr('sqlite3_column_name16');
      sqlite3_column_database_name := GetProcAddr('sqlite3_column_database_name');
      sqlite3_column_database_name16 := GetProcAddr('sqlite3_column_database_name16');
      sqlite3_column_table_name := GetProcAddr('sqlite3_column_table_name');
      sqlite3_column_table_name16 := GetProcAddr('sqlite3_column_table_name16');
      sqlite3_column_origin_name := GetProcAddr('sqlite3_column_origin_name');
      sqlite3_column_origin_name16 := GetProcAddr('sqlite3_column_origin_name16');
      sqlite3_column_decltype := GetProcAddr('sqlite3_column_decltype');
      sqlite3_column_decltype16 := GetProcAddr('sqlite3_column_decltype16');
      sqlite3_step := GetProcAddr('sqlite3_step');
      sqlite3_data_count := GetProcAddr('sqlite3_data_count');
      sqlite3_column_blob := GetProcAddr('sqlite3_column_blob');
      sqlite3_column_bytes := GetProcAddr('sqlite3_column_bytes');
      sqlite3_column_bytes16 := GetProcAddr('sqlite3_column_bytes16');
      sqlite3_column_double := GetProcAddr('sqlite3_column_double');
      sqlite3_column_int := GetProcAddr('sqlite3_column_int');
      sqlite3_column_int64 := GetProcAddr('sqlite3_column_int64');
      sqlite3_column_text := GetProcAddr('sqlite3_column_text');
      sqlite3_column_text16 := GetProcAddr('sqlite3_column_text16');
      sqlite3_column_type := GetProcAddr('sqlite3_column_type');
      sqlite3_column_value := GetProcAddr('sqlite3_column_value');
      sqlite3_finalize := GetProcAddr('sqlite3_finalize');
      sqlite3_reset := GetProcAddr('sqlite3_reset');
      sqlite3_create_function := GetProcAddr('sqlite3_create_function');
      sqlite3_create_function16 := GetProcAddr('sqlite3_create_function16');
      sqlite3_value_blob := GetProcAddr('sqlite3_value_blob');
      sqlite3_value_bytes := GetProcAddr('sqlite3_value_bytes');
      sqlite3_value_bytes16 := GetProcAddr('sqlite3_value_bytes16');
      sqlite3_value_double := GetProcAddr('sqlite3_value_double');
      sqlite3_value_int := GetProcAddr('sqlite3_value_int');
      sqlite3_value_int64 := GetProcAddr('sqlite3_value_int64');
      sqlite3_value_text := GetProcAddr('sqlite3_value_text');
      sqlite3_value_text16 := GetProcAddr('sqlite3_value_text16');
      sqlite3_value_text16le := GetProcAddr('sqlite3_value_text16le');
      sqlite3_value_text16be := GetProcAddr('sqlite3_value_text16be');
      sqlite3_value_type := GetProcAddr('sqlite3_value_type');
      sqlite3_value_numeric_type := GetProcAddr('sqlite3_value_numeric_type');
      sqlite3_aggregate_context := GetProcAddr('sqlite3_aggregate_context');
      sqlite3_user_data := GetProcAddr('sqlite3_user_data');
      sqlite3_context_db_handle := GetProcAddr('sqlite3_context_db_handle');
      sqlite3_get_auxdata := GetProcAddr('sqlite3_get_auxdata');
      sqlite3_set_auxdata := GetProcAddr('sqlite3_set_auxdata');
      sqlite3_result_blob := GetProcAddr('sqlite3_result_blob');
      sqlite3_result_double := GetProcAddr('sqlite3_result_double');
      sqlite3_result_error := GetProcAddr('sqlite3_result_error');
      sqlite3_result_error16 := GetProcAddr('sqlite3_result_error16');
      sqlite3_result_error_toobig := GetProcAddr('sqlite3_result_error_toobig');
      sqlite3_result_error_nomem := GetProcAddr('sqlite3_result_error_nomem');
      sqlite3_result_error_code := GetProcAddr('sqlite3_result_error_code');
      sqlite3_result_int := GetProcAddr('sqlite3_result_int');
      sqlite3_result_int64 := GetProcAddr('sqlite3_result_int64');
      sqlite3_result_null := GetProcAddr('sqlite3_result_null');
      sqlite3_result_text := GetProcAddr('sqlite3_result_text');
      sqlite3_result_text16 := GetProcAddr('sqlite3_result_text16');
      sqlite3_result_text16le := GetProcAddr('sqlite3_result_text16le');
      sqlite3_result_text16be := GetProcAddr('sqlite3_result_text16be');
      sqlite3_result_value := GetProcAddr('sqlite3_result_value');
      sqlite3_result_zeroblob := GetProcAddr('sqlite3_result_zeroblob');
      sqlite3_create_collation := GetProcAddr('sqlite3_create_collation');
      sqlite3_create_collation_v2 := GetProcAddr('sqlite3_create_collation_v2');
      sqlite3_create_collation16 := GetProcAddr('sqlite3_create_collation16');
      sqlite3_collation_needed := GetProcAddr('sqlite3_collation_needed');
      sqlite3_collation_needed16 := GetProcAddr('sqlite3_collation_needed16');
      sqlite3_sleep := GetProcAddr('sqlite3_sleep');
      sqlite3_get_autocommit := GetProcAddr('sqlite3_get_autocommit');
      sqlite3_db_handle := GetProcAddr('sqlite3_db_handle');
      sqlite3_next_stmt := GetProcAddr('sqlite3_next_stmt');
      sqlite3_commit_hook := GetProcAddr('sqlite3_commit_hook');
      sqlite3_rollback_hook := GetProcAddr('sqlite3_rollback_hook');
      sqlite3_update_hook := GetProcAddr('sqlite3_update_hook');
      sqlite3_enable_shared_cache := GetProcAddr('sqlite3_enable_shared_cache');
      sqlite3_release_memory := GetProcAddr('sqlite3_release_memory');
      sqlite3_soft_heap_limit64 := GetProcAddr('sqlite3_soft_heap_limit64');
      sqlite3_table_column_metadata := GetProcAddr('sqlite3_table_column_metadata');
      sqlite3_load_extension := GetProcAddr('sqlite3_load_extension');
      sqlite3_enable_load_extension := GetProcAddr('sqlite3_enable_load_extension');
      sqlite3_auto_extension := GetProcAddr('sqlite3_auto_extension');
      sqlite3_reset_auto_extension := GetProcAddr('sqlite3_reset_auto_extension');
      sqlite3_create_module := GetProcAddr('sqlite3_create_module');
      sqlite3_create_module_v2 := GetProcAddr('sqlite3_create_module_v2');
      sqlite3_declare_vtab := GetProcAddr('sqlite3_declare_vtab');
      sqlite3_overload_function := GetProcAddr('sqlite3_overload_function');
      sqlite3_blob_open := GetProcAddr('sqlite3_blob_open');
      sqlite3_blob_close := GetProcAddr('sqlite3_blob_close');
      sqlite3_blob_bytes := GetProcAddr('sqlite3_blob_bytes');
      sqlite3_blob_read := GetProcAddr('sqlite3_blob_read');
      sqlite3_blob_write := GetProcAddr('sqlite3_blob_write');
      sqlite3_vfs_find := GetProcAddr('sqlite3_vfs_find');
      sqlite3_vfs_register := GetProcAddr('sqlite3_vfs_register');
      sqlite3_vfs_unregister := GetProcAddr('sqlite3_vfs_unregister');
      sqlite3_mutex_alloc := GetProcAddr('sqlite3_mutex_alloc');
      sqlite3_mutex_free := GetProcAddr('sqlite3_mutex_free');
      sqlite3_mutex_enter := GetProcAddr('sqlite3_mutex_enter');
      sqlite3_mutex_try := GetProcAddr('sqlite3_mutex_try');
      sqlite3_mutex_leave := GetProcAddr('sqlite3_mutex_leave');
      sqlite3_db_mutex := GetProcAddr('sqlite3_db_mutex');
      sqlite3_file_control := GetProcAddr('sqlite3_file_control');
      sqlite3_test_control := GetProcAddr('sqlite3_test_control');
      sqlite3_status := GetProcAddr('sqlite3_status');
      sqlite3_db_status := GetProcAddr('sqlite3_db_status');
      sqlite3_stmt_status := GetProcAddr('sqlite3_stmt_status');
      sqlite3_backup_init := GetProcAddr('sqlite3_backup_init');
      sqlite3_backup_step := GetProcAddr('sqlite3_backup_step');
      sqlite3_backup_finish := GetProcAddr('sqlite3_backup_finish');
      sqlite3_backup_remaining := GetProcAddr('sqlite3_backup_remaining');
      sqlite3_backup_pagecount := GetProcAddr('sqlite3_backup_pagecount');
      sqlite3_strnicmp := GetProcAddr('sqlite3_strnicmp');
    except
      GIsLoadError := True;
      LibSQLite3Unload;
      raise;
    end;

    if AInitialize then begin
       // The sqlite3_initialize() routine initializes the SQLite library.
       // The sqlite3_shutdown() routine deallocates any resources that were allocated by sqlite3_initialize().
       // These routines are designed to aid in process initialization and shutdown on embedded systems.
       // Workstation applications using SQLite normally do not need to invoke either of these routines.

       // A call to sqlite3_initialize() is an "effective" call if it is the first time sqlite3_initialize()
       // is invoked during the lifetime of the process, or if it is the first time sqlite3_initialize()
       // is invoked following a call to sqlite3_shutdown(). Only an effective call of sqlite3_initialize()
       // does any initialization. All other calls are harmless no-ops.

      if sqlite3_initialize <> SQLITE_OK then begin
        GIsLoadError := True;
        LibSQLite3Unload;
        raise Exception.Create('SQLite3: initialize error!');
      end;
    end;

    Result := GIsInitialized;
  finally
    GLock.Release;
  end;
end;

procedure LibSQLite3Unload;
begin
  GLock.Acquire;
  try
    if not GIsInitialized then begin
      Exit;
    end;
    GIsInitialized := False;

    // A call to sqlite3_shutdown() is an "effective" call if it is the first call to sqlite3_shutdown()
    // since the last sqlite3_initialize(). Only an effective call to sqlite3_shutdown() does any deinitialization.
    // All other valid calls to sqlite3_shutdown() are harmless no-ops.
    if Addr(sqlite3_shutdown) <> nil then begin
      sqlite3_shutdown;
    end;

    if GHandle > 0 then begin
      FreeLibrary(GHandle);
      GHandle := 0;
    end;

    sqlite3_libversion := nil;
    sqlite3_sourceid := nil;
    sqlite3_libversion_number := nil;
    sqlite3_threadsafe := nil;
    sqlite3_close := nil;
    sqlite3_close_v2 := nil;
    sqlite3_exec := nil;
    sqlite3_initialize := nil;
    sqlite3_shutdown := nil;
    sqlite3_os_init := nil;
    sqlite3_os_end := nil;
    sqlite3_config := nil;
    sqlite3_db_config := nil;
    sqlite3_extended_result_codes := nil;
    sqlite3_last_insert_rowid := nil;
    sqlite3_changes := nil;
    sqlite3_total_changes := nil;
    sqlite3_interrupt := nil;
    sqlite3_complete := nil;
    sqlite3_complete16 := nil;
    sqlite3_busy_handler := nil;
    sqlite3_busy_timeout := nil;
    sqlite3_get_table := nil;
    sqlite3_free_table := nil;
    sqlite3_mprintf := nil;
    sqlite3_vmprintf := nil;
    sqlite3_snprintf := nil;
    sqlite3_malloc := nil;
    sqlite3_realloc := nil;
    sqlite3_free := nil;
    sqlite3_memory_used := nil;
    sqlite3_memory_highwater := nil;
    sqlite3_randomness := nil;
    sqlite3_set_authorizer := nil;
    sqlite3_trace := nil;
    sqlite3_profile := nil;
    sqlite3_progress_handler := nil;
    sqlite3_open := nil;
    sqlite3_open16 := nil;
    sqlite3_open_v2 := nil;
    sqlite3_errcode := nil;
    sqlite3_extended_errcode := nil;
    sqlite3_errmsg := nil;
    sqlite3_errmsg16 := nil;
    sqlite3_limit := nil;
    sqlite3_prepare := nil;
    sqlite3_prepare_v2 := nil;
    sqlite3_prepare16 := nil;
    sqlite3_prepare16_v2 := nil;
    sqlite3_sql := nil;
    sqlite3_bind_blob := nil;
    sqlite3_bind_double := nil;
    sqlite3_bind_int := nil;
    sqlite3_bind_int64 := nil;
    sqlite3_bind_null := nil;
    sqlite3_bind_text := nil;
    sqlite3_bind_text16 := nil;
    sqlite3_bind_value := nil;
    sqlite3_bind_zeroblob := nil;
    sqlite3_bind_parameter_count := nil;
    sqlite3_bind_parameter_name := nil;
    sqlite3_bind_parameter_index := nil;
    sqlite3_clear_bindings := nil;
    sqlite3_column_count := nil;
    sqlite3_column_name := nil;
    sqlite3_column_name16 := nil;
    sqlite3_column_database_name := nil;
    sqlite3_column_database_name16 := nil;
    sqlite3_column_table_name := nil;
    sqlite3_column_table_name16 := nil;
    sqlite3_column_origin_name := nil;
    sqlite3_column_origin_name16 := nil;
    sqlite3_column_decltype := nil;
    sqlite3_column_decltype16 := nil;
    sqlite3_step := nil;
    sqlite3_data_count := nil;
    sqlite3_column_blob := nil;
    sqlite3_column_bytes := nil;
    sqlite3_column_bytes16 := nil;
    sqlite3_column_double := nil;
    sqlite3_column_int := nil;
    sqlite3_column_int64 := nil;
    sqlite3_column_text := nil;
    sqlite3_column_text16 := nil;
    sqlite3_column_type := nil;
    sqlite3_column_value := nil;
    sqlite3_finalize := nil;
    sqlite3_reset := nil;
    sqlite3_create_function := nil;
    sqlite3_create_function16 := nil;
    sqlite3_value_blob := nil;
    sqlite3_value_bytes := nil;
    sqlite3_value_bytes16 := nil;
    sqlite3_value_double := nil;
    sqlite3_value_int := nil;
    sqlite3_value_int64 := nil;
    sqlite3_value_text := nil;
    sqlite3_value_text16 := nil;
    sqlite3_value_text16le := nil;
    sqlite3_value_text16be := nil;
    sqlite3_value_type := nil;
    sqlite3_value_numeric_type := nil;
    sqlite3_aggregate_context := nil;
    sqlite3_user_data := nil;
    sqlite3_context_db_handle := nil;
    sqlite3_get_auxdata := nil;
    sqlite3_set_auxdata := nil;
    sqlite3_result_blob := nil;
    sqlite3_result_double := nil;
    sqlite3_result_error := nil;
    sqlite3_result_error16 := nil;
    sqlite3_result_error_toobig := nil;
    sqlite3_result_error_nomem := nil;
    sqlite3_result_error_code := nil;
    sqlite3_result_int := nil;
    sqlite3_result_int64 := nil;
    sqlite3_result_null := nil;
    sqlite3_result_text := nil;
    sqlite3_result_text16 := nil;
    sqlite3_result_text16le := nil;
    sqlite3_result_text16be := nil;
    sqlite3_result_value := nil;
    sqlite3_result_zeroblob := nil;
    sqlite3_create_collation := nil;
    sqlite3_create_collation_v2 := nil;
    sqlite3_create_collation16 := nil;
    sqlite3_collation_needed := nil;
    sqlite3_collation_needed16 := nil;
    sqlite3_sleep := nil;
    sqlite3_get_autocommit := nil;
    sqlite3_db_handle := nil;
    sqlite3_next_stmt := nil;
    sqlite3_commit_hook := nil;
    sqlite3_rollback_hook := nil;
    sqlite3_update_hook := nil;
    sqlite3_enable_shared_cache := nil;
    sqlite3_release_memory := nil;
    sqlite3_soft_heap_limit64 := nil;
    sqlite3_table_column_metadata := nil;
    sqlite3_load_extension := nil;
    sqlite3_enable_load_extension := nil;
    sqlite3_auto_extension := nil;
    sqlite3_reset_auto_extension := nil;
    sqlite3_create_module := nil;
    sqlite3_create_module_v2 := nil;
    sqlite3_declare_vtab := nil;
    sqlite3_overload_function := nil;
    sqlite3_blob_open := nil;
    sqlite3_blob_close := nil;
    sqlite3_blob_bytes := nil;
    sqlite3_blob_read := nil;
    sqlite3_blob_write := nil;
    sqlite3_vfs_find := nil;
    sqlite3_vfs_register := nil;
    sqlite3_vfs_unregister := nil;
    sqlite3_mutex_alloc := nil;
    sqlite3_mutex_free := nil;
    sqlite3_mutex_enter := nil;
    sqlite3_mutex_try := nil;
    sqlite3_mutex_leave := nil;
    sqlite3_db_mutex := nil;
    sqlite3_file_control := nil;
    sqlite3_test_control := nil;
    sqlite3_status := nil;
    sqlite3_db_status := nil;
    sqlite3_stmt_status := nil;
    sqlite3_backup_init := nil;
    sqlite3_backup_step := nil;
    sqlite3_backup_finish := nil;
    sqlite3_backup_remaining := nil;
    sqlite3_backup_pagecount := nil;
    sqlite3_strnicmp := nil;
  finally
    GLock.Release;
  end;
end;

initialization
  GLock := TCriticalSection.Create;

finalization
  LibSQLite3Unload;
  FreeAndNil(GLock);

end.

