{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit t_ODBC;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

const
  c_ODBC_VERSION = $0351;

type
  // data types
  SqlChar = Byte;
  PSqlChar = ^SqlChar;
  SqlSChar = ShortInt;
  SqlDate = Byte;
  SqlDecimal = Byte;
  SqlDouble = Double;
  SqlFloat = Double;
  SqlInteger = LongInt;
  PSqlInteger = ^SqlInteger;
  SqlNumeric = Byte;
  SqlPointer = Pointer;
  PSqlPointer = ^SqlPointer;
  SqlReal = Single;
  SqlSmallint = SmallInt;
  PSqlSmallint = ^SqlSmallint;
  SqlUSmallint = Word;
  PSqlUSmallint = ^SqlUSmallint;
  SqlTime = Byte;
  SqlTimestamp = Byte;
  SqlVarchar = Byte;
  SqlHWnd = Longword;

  // function return type
  SqlReturn = SqlSmallint;
  Retcode = SmallInt;

  // SQL Handle types
  SqlHandle = Pointer;
  PSqlHandle = ^SqlHandle;
  SqlHEnv = Pointer;
  PSqlHEnv = ^SqlHEnv;
  SqlHDbc = Pointer;
  PSqlHDbc = ^SqlHDbc;
  SqlHStmt = Pointer;
  PSqlHStmt = ^SqlHStmt;
  SqlHDesc = Pointer;
  PSqlHDesc = ^SqlHDesc;

  HEnv = Pointer;
  HDbc = Pointer;
  HStmt = Pointer;

  // SQL portable types
  UChar = Byte;
  SChar = ShortInt;
  SDword = LongInt;
  SWord = SmallInt;
  UDword = LongWord;
  UWord = Word;
  PUWord = ^UWord;
  SqlUInteger = UDword;
  PSqlUInteger = ^SqlUInteger;
  SLong = LongInt;
  SShort = SmallInt;
  ULong = LongWord;
  UShort = Word;
  SDouble = Double;
  LDouble = Double;
  SFloat = Single;
  Ptr = Pointer;

  SqlWChar = WideChar;
  PSqlWChar = ^SqlWChar;
{$IFDEF UNICODE}
  SqlTChar = SqlWChar;
{$ELSE}
  SqlTChar = SqlChar;
{$ENDIF}

const
  // special length/indicator values
  SQL_NULL_DATA = (-1);
  SQL_DATA_AT_EXEC = (-2);

  // return values from functions
  SQL_SUCCESS = 0;
  SQL_SUCCESS_WITH_INFO = 1;
  SQL_NO_DATA = 100;
  SQL_ERROR = (-1);
  SQL_INVALID_HANDLE = (-2);
  SQL_STILL_EXECUTING = 2;
  SQL_NEED_DATA = 99;

  // flags for null-terminated string
  SQL_NTS = (-3);
  SQL_NTSL = (-3);  

  // maximum message length
  SQL_MAX_MESSAGE_LENGTH = 512;

  // date/time length constants
  SQL_DATE_LEN = 10;
  SQL_TIME_LEN = 8; // add P+1 if precision is nonzero
  SQL_TIMESTAMP_LEN = 19; // add P+1 if precision is nonzero

  // handle type identifiers
  SQL_HANDLE_ENV = 1;
  SQL_HANDLE_DBC = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

  // environment attribute
  SQL_ATTR_OUTPUT_NTS = 10001;

  // connection attributes
  SQL_ATTR_AUTO_IPD = 10001;
  SQL_ATTR_METADATA_ID = 10014;

  // statement attributes
  SQL_ATTR_APP_ROW_DESC = 10010;
  SQL_ATTR_APP_PARAM_DESC = 10011;
  SQL_ATTR_IMP_ROW_DESC = 10012;
  SQL_ATTR_IMP_PARAM_DESC = 10013;
  SQL_ATTR_CURSOR_SCROLLABLE = (-1);
  SQL_ATTR_CURSOR_SENSITIVITY = (-2);

  // SQL_ATTR_CURSOR_SCROLLABLE values
  SQL_NONSCROLLABLE = 0;
  SQL_SCROLLABLE = 1;

  // identifiers of fields in the SQL descriptor
  SQL_DESC_COUNT = 1001;
  SQL_DESC_TYPE = 1002;
  SQL_DESC_LENGTH = 1003;
  SQL_DESC_OCTET_LENGTH_PTR = 1004;
  SQL_DESC_PRECISION = 1005;
  SQL_DESC_SCALE = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE = 1008;
  SQL_DESC_INDICATOR_PTR = 1009;
  SQL_DESC_DATA_PTR = 1010;
  SQL_DESC_NAME = 1011;
  SQL_DESC_UNNAMED = 1012;
  SQL_DESC_OCTET_LENGTH = 1013;
  SQL_DESC_ALLOC_TYPE = 1099;

  // identifiers of fields in the diagnostics area*
  SQL_DIAG_RETURNCODE = 1;
  SQL_DIAG_NUMBER = 2;
  SQL_DIAG_ROW_COUNT = 3;
  SQL_DIAG_SQLSTATE = 4;
  SQL_DIAG_NATIVE = 5;
  SQL_DIAG_MESSAGE_TEXT = 6;
  SQL_DIAG_DYNAMIC_FUNCTION = 7;
  SQL_DIAG_CLASS_ORIGIN = 8;
  SQL_DIAG_SUBCLASS_ORIGIN = 9;
  SQL_DIAG_CONNECTION_NAME = 10;
  SQL_DIAG_SERVER_NAME = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

  // dynamic function codes
  SQL_DIAG_ALTER_DOMAIN = 3;
  SQL_DIAG_ALTER_TABLE = 4;
  SQL_DIAG_CALL = 7;
  SQL_DIAG_CREATE_ASSERTION = 6;
  SQL_DIAG_CREATE_CHARACTER_SET = 8;
  SQL_DIAG_CREATE_COLLATION = 10;
  SQL_DIAG_CREATE_DOMAIN = 23;
  SQL_DIAG_CREATE_INDEX = (-1);
  SQL_DIAG_CREATE_SCHEMA = 64;
  SQL_DIAG_CREATE_TABLE = 77;
  SQL_DIAG_CREATE_TRANSLATION = 79;
  SQL_DIAG_CREATE_VIEW = 84;
  SQL_DIAG_DELETE_WHERE = 19;
  SQL_DIAG_DROP_ASSERTION = 24;
  SQL_DIAG_DROP_CHARACTER_SET = 25;
  SQL_DIAG_DROP_COLLATION = 26;
  SQL_DIAG_DROP_DOMAIN = 27;
  SQL_DIAG_DROP_INDEX = (-2);
  SQL_DIAG_DROP_SCHEMA = 31;
  SQL_DIAG_DROP_TABLE = 32;
  SQL_DIAG_DROP_TRANSLATION = 33;
  SQL_DIAG_DROP_VIEW = 36;
  SQL_DIAG_DYNAMIC_DELETE_CURSOR = 38;
  SQL_DIAG_DYNAMIC_UPDATE_CURSOR = 81;
  SQL_DIAG_GRANT = 48;
  SQL_DIAG_INSERT = 50;
  SQL_DIAG_REVOKE = 59;
  SQL_DIAG_SELECT_CURSOR = 85;
  SQL_DIAG_UNKNOWN_STATEMENT = 0;
  SQL_DIAG_UPDATE_WHERE = 82;

  // SQL data type codes
  SQL_UNKNOWN_TYPE = 0;
  SQL_CHAR = 1;
  SQL_NUMERIC = 2;
  SQL_DECIMAL = 3;
  SQL_INTEGER = 4;
  SQL_SMALLINT = 5;
  SQL_FLOAT = 6;
  SQL_REAL = 7;
  SQL_DOUBLE = 8;
  SQL_DATETIME = 9;
  SQL_VARCHAR = 12;

  // One-parameter shortcuts for date/time data types
  SQL_TYPE_DATE = 91;
  SQL_TYPE_TIME = 92;
  SQL_TYPE_TIMESTAMP = 93;

  // Statement attribute values for cursor sensitivity
  SQL_UNSPECIFIED = 0;
  SQL_INSENSITIVE = 1;
  SQL_SENSITIVE = 2;

  // GetTypeInfo() request for all data types
  SQL_ALL_TYPES = 0;

  // Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData()
  SQL_DEFAULT = 99;

  // SQLGetData() code indicating that the application row descriptor
  // specifies the data type
  SQL_ARD_TYPE = (-99);

  // SQL date/time type subcodes
  SQL_CODE_DATE = 1;
  SQL_CODE_TIME = 2;
  SQL_CODE_TIMESTAMP = 3;

  // CLI option values
  SQL_FALSE = 0;
  SQL_TRUE = 1;

  // values of NULLABLE field in descriptor
  SQL_NO_NULLS = 0;
  SQL_NULLABLE = 1;

  // Value returned by SQLGetTypeInfo() to denote that it is
  // not known whether or not a data type supports null values.
  SQL_NULLABLE_UNKNOWN = 2;

  // Values returned by SQLGetTypeInfo() to show WHERE clause supported
  SQL_PRED_NONE = 0;

  SQL_PRED_CHAR = 1;
  SQL_PRED_BASIC = 2;

  // values of UNNAMED field in descriptor
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

  // values of ALLOC_TYPE field in descriptor
  SQL_DESC_ALLOC_AUTO = 1;
  SQL_DESC_ALLOC_USER = 2;

  // FreeStmt() options
  SQL_CLOSE = 0;
  SQL_DROP = 1;
  SQL_UNBIND = 2;
  SQL_RESET_PARAMS = 3;

  // Codes used for FetchOrientation in SQLFetchScroll(), and in SQLDataSources()
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;

  // Other codes used for FetchOrientation in SQLFetchScroll()
  SQL_FETCH_LAST = 3;
  SQL_FETCH_PRIOR = 4;
  SQL_FETCH_ABSOLUTE = 5;
  SQL_FETCH_RELATIVE = 6;

  // SQLEndTran() options
  SQL_COMMIT = 0;
  SQL_ROLLBACK = 1;

  // null handles returned by SQLAllocHandle()
  SQL_NULL_HENV = SqlHandle(0);
  SQL_NULL_HDBC = SqlHandle(0);
  SQL_NULL_HSTMT = SqlHandle(0);
  SQL_NULL_HDESC = SqlHandle(0);

  // null handle used in place of parent handle when allocating HENV
  SQL_NULL_HANDLE = SqlHandle(0);

  // Values that may appear in the result set of SQLSpecialColumns()
  SQL_SCOPE_CURROW = 0;
  SQL_SCOPE_TRANSACTION = 1;
  SQL_SCOPE_SESSION = 2;
  SQL_PC_UNKNOWN = 0;
  SQL_PC_NON_PSEUDO = 1;
  SQL_PC_PSEUDO = 2;

  // Reserved value for the IdentifierType argument of SQLSpecialColumns()
  SQL_ROW_IDENTIFIER = 1;

  // Reserved values for UNIQUE argument of SQLStatistics()
  SQL_INDEX_UNIQUE = 0;
  SQL_INDEX_ALL = 1;

  // Values that may appear in the result set of SQLStatistics()
  SQL_INDEX_CLUSTERED = 1;
  SQL_INDEX_HASHED = 2;
  SQL_INDEX_OTHER = 3;

  // SQLGetFunctions() values to identify ODBC APIs
  SQL_API_SQLALLOCCONNECT = 1;
  SQL_API_SQLALLOCENV = 2;
  SQL_API_SQLALLOCHANDLE = 1001;
  SQL_API_SQLALLOCSTMT = 3;
  SQL_API_SQLBINDCOL = 4;
  SQL_API_SQLBINDPARAM = 1002;
  SQL_API_SQLCANCEL = 5;
  SQL_API_SQLCLOSECURSOR = 1003;
  SQL_API_SQLCOLATTRIBUTE = 6;
  SQL_API_SQLCOLUMNS = 40;
  SQL_API_SQLCONNECT = 7;
  SQL_API_SQLCOPYDESC = 1004;
  SQL_API_SQLDATASOURCES = 57;
  SQL_API_SQLDESCRIBECOL = 8;
  SQL_API_SQLDISCONNECT = 9;
  SQL_API_SQLENDTRAN = 1005;
  SQL_API_SQLERROR = 10;
  SQL_API_SQLEXECDIRECT = 11;
  SQL_API_SQLEXECUTE = 12;
  SQL_API_SQLFETCH = 13;
  SQL_API_SQLFETCHSCROLL = 1021;
  SQL_API_SQLFREECONNECT = 14;
  SQL_API_SQLFREEENV = 15;
  SQL_API_SQLFREEHANDLE = 1006;
  SQL_API_SQLFREESTMT = 16;
  SQL_API_SQLGETCONNECTATTR = 1007;
  SQL_API_SQLGETCONNECTOPTION = 42;
  SQL_API_SQLGETCURSORNAME = 17;
  SQL_API_SQLGETDATA = 43;
  SQL_API_SQLGETDESCFIELD = 1008;
  SQL_API_SQLGETDESCREC = 1009;
  SQL_API_SQLGETDIAGFIELD = 1010;
  SQL_API_SQLGETDIAGREC = 1011;
  SQL_API_SQLGETENVATTR = 1012;
  SQL_API_SQLGETFUNCTIONS = 44;
  SQL_API_SQLGETINFO = 45;
  SQL_API_SQLGETSTMTATTR = 1014;
  SQL_API_SQLGETSTMTOPTION = 46;
  SQL_API_SQLGETTYPEINFO = 47;
  SQL_API_SQLNUMRESULTCOLS = 18;
  SQL_API_SQLPARAMDATA = 48;
  SQL_API_SQLPREPARE = 19;
  SQL_API_SQLPUTDATA = 49;
  SQL_API_SQLROWCOUNT = 20;
  SQL_API_SQLSETCONNECTATTR = 1016;
  SQL_API_SQLSETCONNECTOPTION = 50;
  SQL_API_SQLSETCURSORNAME = 21;
  SQL_API_SQLSETDESCFIELD = 1017;
  SQL_API_SQLSETDESCREC = 1018;
  SQL_API_SQLSETENVATTR = 1019;
  SQL_API_SQLSETPARAM = 22;
  SQL_API_SQLSETSTMTATTR = 1020;
  SQL_API_SQLSETSTMTOPTION = 51;
  SQL_API_SQLSPECIALCOLUMNS = 52;
  SQL_API_SQLSTATISTICS = 53;
  SQL_API_SQLTABLES = 54;
  SQL_API_SQLTRANSACT = 23;

  // Information requested by SQLGetInfo()
  SQL_MAX_DRIVER_CONNECTIONS = 0;
  SQL_MAXIMUM_DRIVER_CONNECTIONS = SQL_MAX_DRIVER_CONNECTIONS;
  SQL_MAX_CONCURRENT_ACTIVITIES = 1;
  SQL_MAXIMUM_CONCURRENT_ACTIVITIES = SQL_MAX_CONCURRENT_ACTIVITIES;
  SQL_DATA_SOURCE_NAME = 2;
  SQL_FETCH_DIRECTION = 8;
  SQL_SERVER_NAME = 13;
  SQL_SEARCH_PATTERN_ESCAPE = 14;
  SQL_DBMS_NAME = 17;
  SQL_DBMS_VER = 18;
  SQL_ACCESSIBLE_TABLES = 19;
  SQL_ACCESSIBLE_PROCEDURES = 20;
  SQL_CURSOR_COMMIT_BEHAVIOR = 23;
  SQL_DATA_SOURCE_READ_ONLY = 25;
  SQL_DEFAULT_TXN_ISOLATION = 26;
  SQL_IDENTIFIER_CASE = 28;
  SQL_IDENTIFIER_QUOTE_CHAR = 29;
  SQL_MAX_COLUMN_NAME_LEN = 30;
  SQL_MAXIMUM_COLUMN_NAME_LENGTH = SQL_MAX_COLUMN_NAME_LEN;
  SQL_MAX_CURSOR_NAME_LEN = 31;
  SQL_MAXIMUM_CURSOR_NAME_LENGTH = SQL_MAX_CURSOR_NAME_LEN;
  SQL_MAX_SCHEMA_NAME_LEN = 32;
  SQL_MAXIMUM_SCHEMA_NAME_LENGTH = SQL_MAX_SCHEMA_NAME_LEN;
  SQL_MAX_CATALOG_NAME_LEN = 34;
  SQL_MAXIMUM_CATALOG_NAME_LENGTH = SQL_MAX_CATALOG_NAME_LEN;
  SQL_MAX_TABLE_NAME_LEN = 35;
  SQL_SCROLL_CONCURRENCY = 43;
  SQL_TXN_CAPABLE = 46;
  SQL_TRANSACTION_CAPABLE = SQL_TXN_CAPABLE;
  SQL_USER_NAME = 47;
  SQL_TXN_ISOLATION_OPTION = 72;
  SQL_TRANSACTION_ISOLATION_OPTION = SQL_TXN_ISOLATION_OPTION;
  SQL_INTEGRITY = 73;
  SQL_GETDATA_EXTENSIONS = 81;
  SQL_NULL_COLLATION = 85;
  SQL_ALTER_TABLE = 86;
  SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
  SQL_SPECIAL_CHARACTERS = 94;
  SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
  SQL_MAXIMUM_COLUMNS_IN_GROUP_BY = SQL_MAX_COLUMNS_IN_GROUP_BY;
  SQL_MAX_COLUMNS_IN_INDEX = 98;
  SQL_MAXIMUM_COLUMNS_IN_INDEX = SQL_MAX_COLUMNS_IN_INDEX;
  SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
  SQL_MAXIMUM_COLUMNS_IN_ORDER_BY = SQL_MAX_COLUMNS_IN_ORDER_BY;
  SQL_MAX_COLUMNS_IN_SELECT = 100;
  SQL_MAXIMUM_COLUMNS_IN_SELECT = SQL_MAX_COLUMNS_IN_SELECT;
  SQL_MAX_COLUMNS_IN_TABLE = 101;
  SQL_MAX_INDEX_SIZE = 102;
  SQL_MAXIMUM_INDEX_SIZE = SQL_MAX_INDEX_SIZE;
  SQL_MAX_ROW_SIZE = 104;
  SQL_MAXIMUM_ROW_SIZE = SQL_MAX_ROW_SIZE;
  SQL_MAX_STATEMENT_LEN = 105;
  SQL_MAXIMUM_STATEMENT_LENGTH = SQL_MAX_STATEMENT_LEN;
  SQL_MAX_TABLES_IN_SELECT = 106;
  SQL_MAXIMUM_TABLES_IN_SELECT = SQL_MAX_TABLES_IN_SELECT;
  SQL_MAX_USER_NAME_LEN = 107;
  SQL_MAXIMUM_USER_NAME_LENGTH = SQL_MAX_USER_NAME_LEN;
  SQL_OJ_CAPABILITIES = 115;
  SQL_OUTER_JOIN_CAPABILITIES = SQL_OJ_CAPABILITIES;
  SQL_XOPEN_CLI_YEAR = 10000;
  SQL_CURSOR_SENSITIVITY = 10001;
  SQL_DESCRIBE_PARAMETER = 10002;
  SQL_CATALOG_NAME = 10003;
  SQL_COLLATION_SEQ = 10004;
  SQL_MAX_IDENTIFIER_LEN = 10005;
  SQL_MAXIMUM_IDENTIFIER_LENGTH = SQL_MAX_IDENTIFIER_LEN;

  // SQL_ALTER_TABLE bitmasks
  SQL_AT_ADD_COLUMN = $00000001;
  SQL_AT_DROP_COLUMN = $00000002;
  SQL_AT_ADD_CONSTRAINT = $00000008;

  // The following bitmasks are ODBC extensions and defined in sqlext.h
  SQL_AT_COLUMN_SINGLE = $00000020;
  SQL_AT_ADD_COLUMN_DEFAULT = $00000040;
  SQL_AT_ADD_COLUMN_COLLATION = $00000080;
  SQL_AT_SET_COLUMN_DEFAULT = $00000100;
  SQL_AT_DROP_COLUMN_DEFAULT = $00000200;
  SQL_AT_DROP_COLUMN_CASCADE = $00000400;
  SQL_AT_DROP_COLUMN_RESTRICT = $00000800;
  SQL_AT_ADD_TABLE_CONSTRAINT = $00001000;
  SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE = $00002000;
  SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT = $00004000;
  SQL_AT_CONSTRAINT_NAME_DEFINITION = $00008000;
  SQL_AT_CONSTRAINT_INITIALLY_DEFERRED = $00010000;
  SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE = $00020000;
  SQL_AT_CONSTRAINT_DEFERRABLE = $00040000;
  SQL_AT_CONSTRAINT_NON_DEFERRABLE = $00080000;

  // SQL_ASYNC_MODE values
  SQL_AM_NONE = 0;
  SQL_AM_CONNECTION = 1;
  SQL_AM_STATEMENT = 2;

  // SQL_CURSOR_COMMIT_BEHAVIOR values
  SQL_CB_DELETE = 0;
  SQL_CB_CLOSE = 1;
  SQL_CB_PRESERVE = 2;

  // SQL_FETCH_DIRECTION bitmasks
  SQL_FD_FETCH_NEXT = $00000001;
  SQL_FD_FETCH_FIRST = $00000002;
  SQL_FD_FETCH_LAST = $00000004;
  SQL_FD_FETCH_PRIOR = $00000008;
  SQL_FD_FETCH_ABSOLUTE = $00000010;
  SQL_FD_FETCH_RELATIVE = $00000020;

  // SQL_GETDATA_EXTENSIONS bitmasks
  SQL_GD_ANY_COLUMN = $00000001;
  SQL_GD_ANY_ORDER = $00000002;

  // SQL_IDENTIFIER_CASE values
  SQL_IC_UPPER = 1;
  SQL_IC_LOWER = 2;
  SQL_IC_SENSITIVE = 3;
  SQL_IC_MIXED = 4;

  // SQL_OJ_CAPABILITIES bitmasks
  // NB: this means 'outer join', not what you may be thinking
  SQL_OJ_LEFT = $00000001;
  SQL_OJ_RIGHT = $00000002;
  SQL_OJ_FULL = $00000004;
  SQL_OJ_NESTED = $00000008;
  SQL_OJ_NOT_ORDERED = $00000010;
  SQL_OJ_INNER = $00000020;
  SQL_OJ_ALL_COMPARISON_OPS = $00000040;

  // SQL_SCROLL_CONCURRENCY bitmasks
  SQL_SCCO_READ_ONLY = $00000001;
  SQL_SCCO_LOCK = $00000002;
  SQL_SCCO_OPT_ROWVER = $00000004;
  SQL_SCCO_OPT_VALUES = $00000008;

  // SQL_TXN_CAPABLE values
  SQL_TC_NONE = 0;
  SQL_TC_DML = 1;
  SQL_TC_ALL = 2;
  SQL_TC_DDL_COMMIT = 3;
  SQL_TC_DDL_IGNORE = 4;

  // SQL_TXN_ISOLATION_OPTION bitmasks
  SQL_TXN_READ_UNCOMMITTED = $00000001;
  SQL_TRANSACTION_READ_UNCOMMITTED = SQL_TXN_READ_UNCOMMITTED;
  SQL_TXN_READ_COMMITTED = $00000002;
  SQL_TRANSACTION_READ_COMMITTED = SQL_TXN_READ_COMMITTED;
  SQL_TXN_REPEATABLE_READ = $00000004;
  SQL_TRANSACTION_REPEATABLE_READ = SQL_TXN_REPEATABLE_READ;
  SQL_TXN_SERIALIZABLE = $00000008;
  SQL_TRANSACTION_SERIALIZABLE = SQL_TXN_SERIALIZABLE;

  // SQL_NULL_COLLATION values
  SQL_NC_HIGH = 0;
  SQL_NC_LOW = 1;

  SQL_WCHAR = (-8);
  SQL_WVARCHAR = (-9);
  SQL_WLONGVARCHAR = (-10);
  SQL_C_WCHAR = SQL_WCHAR;

  // generally useful constants
  SQL_SPEC_MAJOR = 3; // Major version of specification
  SQL_SPEC_MINOR = 51; // Minor version of specification
  SQL_SPEC_STRING = '03.51'; // String constant for version

  SQL_SQLSTATE_SIZE = 5; // size of SQLSTATE
  SQL_MAX_DSN_LENGTH = 32; // maximum data source name size

  SQL_MAX_OPTION_STRING_LENGTH = 256;

  // return code SQL_NO_DATA_FOUND is the same as SQL_NO_DATA*
  SQL_NO_DATA_FOUND = SQL_NO_DATA;

  // an env handle type
  SQL_HANDLE_SENV = 5;

  // env attribute
  SQL_ATTR_ODBC_VERSION = 200;
  SQL_ATTR_CONNECTION_POOLING = 201;
  SQL_ATTR_CP_MATCH = 202;

  // values for SQL_ATTR_CONNECTION_POOLING
  SQL_CP_OFF = ULong(0);
  SQL_CP_ONE_PER_DRIVER = ULong(1);
  SQL_CP_ONE_PER_HENV = ULong(2);
  SQL_CP_DEFAULT = SQL_CP_OFF;

  // values for SQL_ATTR_CP_MATCH
  SQL_CP_STRICT_MATCH = ULong(0);
  SQL_CP_RELAXED_MATCH = ULong(1);
  SQL_CP_MATCH_DEFAULT = SQL_CP_STRICT_MATCH;

  // values for SQL_ATTR_ODBC_VERSION
  SQL_OV_ODBC2 = ULong(2);
  SQL_OV_ODBC3 = ULong(3);

  // connection attributes
  SQL_ACCESS_MODE = 101;
  SQL_AUTOCOMMIT = 102;
  SQL_LOGIN_TIMEOUT = 103;
  SQL_OPT_TRACE = 104;
  SQL_OPT_TRACEFILE = 105;
  SQL_TRANSLATE_DLL = 106;
  SQL_TRANSLATE_OPTION = 107;
  SQL_TXN_ISOLATION = 108;
  SQL_CURRENT_QUALIFIER = 109;
  SQL_ODBC_CURSORS = 110;
  SQL_QUIET_MODE = 111;
  SQL_PACKET_SIZE = 112;

  // connection attributes with new names
  SQL_ATTR_ACCESS_MODE = SQL_ACCESS_MODE;
  SQL_ATTR_AUTOCOMMIT = SQL_AUTOCOMMIT;
  SQL_ATTR_CONNECTION_TIMEOUT = 113;
  SQL_ATTR_CURRENT_CATALOG = SQL_CURRENT_QUALIFIER;
  SQL_ATTR_DISCONNECT_BEHAVIOR = 114;
  SQL_ATTR_ENLIST_IN_DTC = 1207;
  SQL_ATTR_ENLIST_IN_XA = 1208;
  SQL_ATTR_LOGIN_TIMEOUT = SQL_LOGIN_TIMEOUT;
  SQL_ATTR_ODBC_CURSORS = SQL_ODBC_CURSORS;
  SQL_ATTR_PACKET_SIZE = SQL_PACKET_SIZE;
  SQL_ATTR_QUIET_MODE = SQL_QUIET_MODE;
  SQL_ATTR_TRACE = SQL_OPT_TRACE;
  SQL_ATTR_TRACEFILE = SQL_OPT_TRACEFILE;
  SQL_ATTR_TRANSLATE_LIB = SQL_TRANSLATE_DLL;
  SQL_ATTR_TRANSLATE_OPTION = SQL_TRANSLATE_OPTION;
  SQL_ATTR_TXN_ISOLATION = SQL_TXN_ISOLATION;
  SQL_ATTR_CONNECTION_DEAD = 1209; // GetConnectAttr only

  { ODBC Driver Manager sets this connection attribute to a unicode driver
    (which supports SQLConnectW) when the application is an ANSI application
    (which calls SQLConnect, SQLDriverConnect, or SQLBrowseConnect).
    This is SetConnectAttr only and application does not set this attribute
    This attribute was introduced because some unicode driver's some APIs may
    need to behave differently on ANSI or Unicode applications. A unicode
    driver, which has same behavior for both ANSI or Unicode applications,
    should return SQL_ERROR when the driver manager sets this connection
    attribute. When a unicode driver returns SQL_SUCCESS on this attribute,
    the driver manager treates ANSI and Unicode connections differently in
    connection pooling. }
  SQL_ATTR_ANSI_APP = 115;

  // SQL_CONNECT_OPT_DRVR_START is not meaningful for 3.0 driver
  SQL_CONNECT_OPT_DRVR_START = 1000;
  SQL_CONN_OPT_MAX = SQL_PACKET_SIZE;
  SQL_CONN_OPT_MIN = SQL_ACCESS_MODE;

  // SQL_ACCESS_MODE options
  SQL_MODE_READ_WRITE = ULong(0);
  SQL_MODE_READ_ONLY = ULong(1);
  SQL_MODE_DEFAULT = SQL_MODE_READ_WRITE;

  // SQL_AUTOCOMMIT options
  SQL_AUTOCOMMIT_OFF = ULong(0);
  SQL_AUTOCOMMIT_ON = ULong(1);
  SQL_AUTOCOMMIT_DEFAULT = SQL_AUTOCOMMIT_ON;

  // SQL_LOGIN_TIMEOUT options
  SQL_LOGIN_TIMEOUT_DEFAULT = ULong(15);

  // SQL_OPT_TRACE options
  SQL_OPT_TRACE_OFF = ULong(0);
  SQL_OPT_TRACE_ON = ULong(1);
  SQL_OPT_TRACE_DEFAULT = SQL_OPT_TRACE_OFF;
  SQL_OPT_TRACE_FILE_DEFAULT = '\\SQL.LOG';

  // SQL_ODBC_CURSORS options
  SQL_CUR_USE_IF_NEEDED = ULong(0);
  SQL_CUR_USE_ODBC = ULong(1);
  SQL_CUR_USE_DRIVER = ULong(2);
  SQL_CUR_DEFAULT = SQL_CUR_USE_DRIVER;

  // values for SQL_ATTR_DISCONNECT_BEHAVIOR
  SQL_DB_RETURN_TO_POOL = ULong(0);
  SQL_DB_DISCONNECT = ULong(1);
  SQL_DB_DEFAULT = SQL_DB_RETURN_TO_POOL;

  // values for SQL_ATTR_ENLIST_IN_DTC
  SQL_DTC_DONE = 0;

  // values for SQL_ATTR_CONNECTION_DEAD
  SQL_CD_TRUE = 1; // Connection is closed/dead
  SQL_CD_FALSE = 0; // Connection is open/available

  // values for SQL_ATTR_ANSI_APP
  SQL_AA_TRUE = 1; // the application is an ANSI app
  SQL_AA_FALSE = 0; // the application is a Unicode app

  // statement attributes
  SQL_QUERY_TIMEOUT = 0;
  SQL_MAX_ROWS = 1;
  SQL_NOSCAN = 2;
  SQL_MAX_LENGTH = 3;
  SQL_ASYNC_ENABLE = 4; // same as SQL_ATTR_ASYNC_ENABLE
  SQL_BIND_TYPE = 5;
  SQL_CURSOR_TYPE = 6;
  SQL_CONCURRENCY = 7;
  SQL_KEYSET_SIZE = 8;
  SQL_ROWSET_SIZE = 9;
  SQL_SIMULATE_CURSOR = 10;
  SQL_RETRIEVE_DATA = 11;
  SQL_USE_BOOKMARKS = 12;
  SQL_GET_BOOKMARK = 13; // GetStmtOption Only
  SQL_ROW_NUMBER = 14; // GetStmtOption Only

  SQL_ATTR_ASYNC_ENABLE = 4;
  SQL_ATTR_CONCURRENCY = SQL_CONCURRENCY;
  SQL_ATTR_CURSOR_TYPE = SQL_CURSOR_TYPE;
  SQL_ATTR_ENABLE_AUTO_IPD = 15;
  SQL_ATTR_FETCH_BOOKMARK_PTR = 16;
  SQL_ATTR_KEYSET_SIZE = SQL_KEYSET_SIZE;
  SQL_ATTR_MAX_LENGTH = SQL_MAX_LENGTH;
  SQL_ATTR_MAX_ROWS = SQL_MAX_ROWS;
  SQL_ATTR_NOSCAN = SQL_NOSCAN;
  SQL_ATTR_PARAM_BIND_OFFSET_PTR = 17;
  SQL_ATTR_PARAM_BIND_TYPE = 18;
  SQL_ATTR_PARAM_OPERATION_PTR = 19;
  SQL_ATTR_PARAM_STATUS_PTR = 20;
  SQL_ATTR_PARAMS_PROCESSED_PTR = 21;
  SQL_ATTR_PARAMSET_SIZE = 22;
  SQL_ATTR_QUERY_TIMEOUT = SQL_QUERY_TIMEOUT;
  SQL_ATTR_RETRIEVE_DATA = SQL_RETRIEVE_DATA;
  SQL_ATTR_ROW_BIND_OFFSET_PTR = 23;
  SQL_ATTR_ROW_BIND_TYPE = SQL_BIND_TYPE;
  SQL_ATTR_ROW_NUMBER = SQL_ROW_NUMBER; // GetStmtAttr
  SQL_ATTR_ROW_OPERATION_PTR = 24;
  SQL_ATTR_ROW_STATUS_PTR = 25;
  SQL_ATTR_ROWS_FETCHED_PTR = 26;
  SQL_ATTR_ROW_ARRAY_SIZE = 27;
  SQL_ATTR_SIMULATE_CURSOR = SQL_SIMULATE_CURSOR;
  SQL_ATTR_USE_BOOKMARKS = SQL_USE_BOOKMARKS;

  SQL_STMT_OPT_MAX = SQL_ROW_NUMBER;
  SQL_STMT_OPT_MIN = SQL_QUERY_TIMEOUT;


  //=====================================
  // This block moved to here from below because of dependent decarations

  // SQLColAttributes defines
  SQL_COLUMN_COUNT = 0;
  SQL_COLUMN_NAME = 1;
  SQL_COLUMN_TYPE = 2;
  SQL_COLUMN_LENGTH = 3;
  SQL_COLUMN_PRECISION = 4;
  SQL_COLUMN_SCALE = 5;
  SQL_COLUMN_DISPLAY_SIZE = 6;
  SQL_COLUMN_NULLABLE = 7;
  SQL_COLUMN_UNSIGNED = 8;
  SQL_COLUMN_MONEY = 9;
  SQL_COLUMN_UPDATABLE = 10;
  SQL_COLUMN_AUTO_INCREMENT = 11;
  SQL_COLUMN_CASE_SENSITIVE = 12;
  SQL_COLUMN_SEARCHABLE = 13;
  SQL_COLUMN_TYPE_NAME = 14;
  SQL_COLUMN_TABLE_NAME = 15;
  SQL_COLUMN_OWNER_NAME = 16;
  SQL_COLUMN_QUALIFIER_NAME = 17;
  SQL_COLUMN_LABEL = 18;

  SQL_COLATT_OPT_MAX = SQL_COLUMN_LABEL;
  SQL_COLATT_OPT_MIN = SQL_COLUMN_COUNT;

  // SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE
  SQL_ATTR_READONLY = 0;
  SQL_ATTR_WRITE = 1;
  SQL_ATTR_READWRITE_UNKNOWN = 2;

  // SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE
  // These are also used by SQLGetInfo
  SQL_UNSEARCHABLE = 0;
  SQL_LIKE_ONLY = 1;
  SQL_ALL_EXCEPT_LIKE = 2;
  SQL_SEARCHABLE = 3;
  SQL_PRED_SEARCHABLE = SQL_SEARCHABLE;

  // Special return values for SQLGetData
  SQL_NO_TOTAL = (-4);
  // End of move
  //=====================================

  // New defines for SEARCHABLE column in SQLGetTypeInfo
  SQL_COL_PRED_CHAR = SQL_LIKE_ONLY;
  SQL_COL_PRED_BASIC = SQL_ALL_EXCEPT_LIKE;

  // whether an attribute is a pointer or not
  SQL_IS_POINTER = (-4);

  SQL_IS_UINTEGER = (-5);
  SQL_IS_INTEGER = (-6);
  SQL_IS_USMALLINT = (-7);
  SQL_IS_SMALLINT = (-8);

  // the value of SQL_ATTR_PARAM_BIND_TYPE
  SQL_PARAM_BIND_BY_COLUMN = ULong(0);
  SQL_PARAM_BIND_TYPE_DEFAULT = SQL_PARAM_BIND_BY_COLUMN;

  // SQL_QUERY_TIMEOUT options
  SQL_QUERY_TIMEOUT_DEFAULT = ULong(0);

  // SQL_MAX_ROWS options
  SQL_MAX_ROWS_DEFAULT = ULong(0);

  // SQL_NOSCAN options
  SQL_NOSCAN_OFF = ULong(0); // 1.0 FALSE
  SQL_NOSCAN_ON = ULong(1); // 1.0 TRUE
  SQL_NOSCAN_DEFAULT = SQL_NOSCAN_OFF;

  // SQL_MAX_LENGTH options
  SQL_MAX_LENGTH_DEFAULT = ULong(0);

  // values for SQL_ATTR_ASYNC_ENABLE
  SQL_ASYNC_ENABLE_OFF = ULong(0);
  SQL_ASYNC_ENABLE_ON = ULong(1);
  SQL_ASYNC_ENABLE_DEFAULT = SQL_ASYNC_ENABLE_OFF;

  // SQL_BIND_TYPE options
  SQL_BIND_BY_COLUMN = ULong(0);
  SQL_BIND_TYPE_DEFAULT = SQL_BIND_BY_COLUMN; // Default value

  // SQL_CONCURRENCY options
  SQL_CONCUR_READ_ONLY = 1;
  SQL_CONCUR_LOCK = 2;
  SQL_CONCUR_ROWVER = 3;
  SQL_CONCUR_VALUES = 4;
  SQL_CONCUR_DEFAULT = SQL_CONCUR_READ_ONLY; // Default value

  // SQL_CURSOR_TYPE options
  SQL_CURSOR_FORWARD_ONLY = ULong(0);
  SQL_CURSOR_KEYSET_DRIVEN = ULong(1);
  SQL_CURSOR_DYNAMIC = ULong(2);
  SQL_CURSOR_STATIC = ULong(3);
  SQL_CURSOR_TYPE_DEFAULT = SQL_CURSOR_FORWARD_ONLY; // Default value

  // SQL_ROWSET_SIZE options
  SQL_ROWSET_SIZE_DEFAULT = ULong(1);

  // SQL_KEYSET_SIZE options
  SQL_KEYSET_SIZE_DEFAULT = ULong(0);

  // SQL_SIMULATE_CURSOR options
  SQL_SC_NON_UNIQUE = ULong(0);
  SQL_SC_TRY_UNIQUE = ULong(1);
  SQL_SC_UNIQUE = ULong(2);

  // SQL_RETRIEVE_DATA options
  SQL_RD_OFF = ULong(0);
  SQL_RD_ON = ULong(1);
  SQL_RD_DEFAULT = SQL_RD_ON;

  // SQL_USE_BOOKMARKS options
  SQL_UB_OFF = ULong(0);
  SQL_UB_ON = ULong(1);
  SQL_UB_DEFAULT = SQL_UB_OFF;

  // New values for SQL_USE_BOOKMARKS attribute
  SQL_UB_FIXED = SQL_UB_ON;
  SQL_UB_VARIABLE = ULong(2);

  // extended descriptor field
  SQL_DESC_ARRAY_SIZE = 20;
  SQL_DESC_ARRAY_STATUS_PTR = 21;
  SQL_DESC_AUTO_UNIQUE_VALUE = SQL_COLUMN_AUTO_INCREMENT;
  SQL_DESC_BASE_COLUMN_NAME = 22;
  SQL_DESC_BASE_TABLE_NAME = 23;
  SQL_DESC_BIND_OFFSET_PTR = 24;
  SQL_DESC_BIND_TYPE = 25;
  SQL_DESC_CASE_SENSITIVE = SQL_COLUMN_CASE_SENSITIVE;
  SQL_DESC_CATALOG_NAME = SQL_COLUMN_QUALIFIER_NAME;
  SQL_DESC_CONCISE_TYPE = SQL_COLUMN_TYPE;
  SQL_DESC_DATETIME_INTERVAL_PRECISION = 26;
  SQL_DESC_DISPLAY_SIZE = SQL_COLUMN_DISPLAY_SIZE;
  SQL_DESC_FIXED_PREC_SCALE = SQL_COLUMN_MONEY;
  SQL_DESC_LABEL = SQL_COLUMN_LABEL;
  SQL_DESC_LITERAL_PREFIX = 27;
  SQL_DESC_LITERAL_SUFFIX = 28;
  SQL_DESC_LOCAL_TYPE_NAME = 29;
  SQL_DESC_MAXIMUM_SCALE = 30;
  SQL_DESC_MINIMUM_SCALE = 31;
  SQL_DESC_NUM_PREC_RADIX = 32;
  SQL_DESC_PARAMETER_TYPE = 33;
  SQL_DESC_ROWS_PROCESSED_PTR = 34;
  SQL_DESC_ROWVER = 35;
  SQL_DESC_SCHEMA_NAME = SQL_COLUMN_OWNER_NAME;
  SQL_DESC_SEARCHABLE = SQL_COLUMN_SEARCHABLE;
  SQL_DESC_TYPE_NAME = SQL_COLUMN_TYPE_NAME;
  SQL_DESC_TABLE_NAME = SQL_COLUMN_TABLE_NAME;
  SQL_DESC_UNSIGNED = SQL_COLUMN_UNSIGNED;
  SQL_DESC_UPDATABLE = SQL_COLUMN_UPDATABLE;

  // defines for diagnostics fields
  SQL_DIAG_CURSOR_ROW_COUNT = (-1249);
  SQL_DIAG_ROW_NUMBER = (-1248);
  SQL_DIAG_COLUMN_NUMBER = (-1247);

  // SQL extended datatypes
  SQL_DATE = 9;
  SQL_INTERVAL = 10;
  SQL_TIME = 10;
  SQL_TIMESTAMP = 11;
  SQL_LONGVARCHAR = (-1);
  SQL_BINARY = (-2);
  SQL_VARBINARY = (-3);
  SQL_LONGVARBINARY = (-4);
  SQL_BIGINT = (-5);
  SQL_TINYINT = (-6);
  SQL_BIT = (-7);
  SQL_GUID = (-11);

  // interval code
  SQL_CODE_YEAR = 1;
  SQL_CODE_MONTH = 2;
  SQL_CODE_DAY = 3;
  SQL_CODE_HOUR = 4;
  SQL_CODE_MINUTE = 5;
  SQL_CODE_SECOND = 6;
  SQL_CODE_YEAR_TO_MONTH = 7;
  SQL_CODE_DAY_TO_HOUR = 8;
  SQL_CODE_DAY_TO_MINUTE = 9;
  SQL_CODE_DAY_TO_SECOND = 10;
  SQL_CODE_HOUR_TO_MINUTE = 11;
  SQL_CODE_HOUR_TO_SECOND = 12;
  SQL_CODE_MINUTE_TO_SECOND = 13;

  SQL_INTERVAL_YEAR = (100 + SQL_CODE_YEAR);
  SQL_INTERVAL_MONTH = (100 + SQL_CODE_MONTH);
  SQL_INTERVAL_DAY = (100 + SQL_CODE_DAY);
  SQL_INTERVAL_HOUR = (100 + SQL_CODE_HOUR);
  SQL_INTERVAL_MINUTE = (100 + SQL_CODE_MINUTE);
  SQL_INTERVAL_SECOND = (100 + SQL_CODE_SECOND);
  SQL_INTERVAL_YEAR_TO_MONTH = (100 + SQL_CODE_YEAR_TO_MONTH);
  SQL_INTERVAL_DAY_TO_HOUR = (100 + SQL_CODE_DAY_TO_HOUR);
  SQL_INTERVAL_DAY_TO_MINUTE = (100 + SQL_CODE_DAY_TO_MINUTE);
  SQL_INTERVAL_DAY_TO_SECOND = (100 + SQL_CODE_DAY_TO_SECOND);
  SQL_INTERVAL_HOUR_TO_MINUTE = (100 + SQL_CODE_HOUR_TO_MINUTE);
  SQL_INTERVAL_HOUR_TO_SECOND = (100 + SQL_CODE_HOUR_TO_SECOND);
  SQL_INTERVAL_MINUTE_TO_SECOND = (100 + SQL_CODE_MINUTE_TO_SECOND);

  SQL_UNICODE = SQL_WCHAR;
  SQL_UNICODE_VARCHAR = SQL_WVARCHAR;
  SQL_UNICODE_LONGVARCHAR = SQL_WLONGVARCHAR;
  SQL_UNICODE_CHAR = SQL_WCHAR;

  // C datatype to SQL datatype mapping SQL types
  // -------------------
  SQL_C_CHAR = SQL_CHAR; // CHAR, VARCHAR, DECIMAL, NUMERIC
  SQL_C_LONG = SQL_INTEGER; // INTEGER
  SQL_C_SHORT = SQL_SMALLINT; // SMALLINT
  SQL_C_FLOAT = SQL_REAL; // REAL
  SQL_C_DOUBLE = SQL_DOUBLE; // FLOAT, DOUBLE
  SQL_C_NUMERIC = SQL_NUMERIC;
  SQL_C_DEFAULT = 99;
  SQL_SIGNED_OFFSET = (-20);
  SQL_UNSIGNED_OFFSET = (-22);

  // C datatype to SQL datatype mapping
  SQL_C_DATE = SQL_DATE;
  SQL_C_TIME = SQL_TIME;
  SQL_C_TIMESTAMP = SQL_TIMESTAMP;
  SQL_C_TYPE_DATE = SQL_TYPE_DATE;
  SQL_C_TYPE_TIME = SQL_TYPE_TIME;
  SQL_C_TYPE_TIMESTAMP = SQL_TYPE_TIMESTAMP;
  SQL_C_INTERVAL_YEAR = SQL_INTERVAL_YEAR;
  SQL_C_INTERVAL_MONTH = SQL_INTERVAL_MONTH;
  SQL_C_INTERVAL_DAY = SQL_INTERVAL_DAY;
  SQL_C_INTERVAL_HOUR = SQL_INTERVAL_HOUR;
  SQL_C_INTERVAL_MINUTE = SQL_INTERVAL_MINUTE;
  SQL_C_INTERVAL_SECOND = SQL_INTERVAL_SECOND;
  SQL_C_INTERVAL_YEAR_TO_MONTH = SQL_INTERVAL_YEAR_TO_MONTH;
  SQL_C_INTERVAL_DAY_TO_HOUR = SQL_INTERVAL_DAY_TO_HOUR;
  SQL_C_INTERVAL_DAY_TO_MINUTE = SQL_INTERVAL_DAY_TO_MINUTE;
  SQL_C_INTERVAL_DAY_TO_SECOND = SQL_INTERVAL_DAY_TO_SECOND;
  SQL_C_INTERVAL_HOUR_TO_MINUTE = SQL_INTERVAL_HOUR_TO_MINUTE;
  SQL_C_INTERVAL_HOUR_TO_SECOND = SQL_INTERVAL_HOUR_TO_SECOND;
  SQL_C_INTERVAL_MINUTE_TO_SECOND = SQL_INTERVAL_MINUTE_TO_SECOND;
  SQL_C_BINARY = SQL_BINARY;
  SQL_C_BIT = SQL_BIT;
  SQL_C_SBIGINT = (SQL_BIGINT + SQL_SIGNED_OFFSET); // SIGNED BIGINT
  SQL_C_UBIGINT = (SQL_BIGINT + SQL_UNSIGNED_OFFSET); // UNSIGNED BIGINT
  SQL_C_TINYINT = SQL_TINYINT;
  SQL_C_SLONG = (SQL_C_LONG + SQL_SIGNED_OFFSET); // SIGNED INTEGER
  SQL_C_SSHORT = (SQL_C_SHORT + SQL_SIGNED_OFFSET); // SIGNED SMALLINT
  SQL_C_STINYINT = (SQL_TINYINT + SQL_SIGNED_OFFSET); // SIGNED TINYINT
  SQL_C_ULONG = (SQL_C_LONG + SQL_UNSIGNED_OFFSET); // UNSIGNED INTEGER
  SQL_C_USHORT = (SQL_C_SHORT + SQL_UNSIGNED_OFFSET); // UNSIGNED SMALLINT
  SQL_C_UTINYINT = (SQL_TINYINT + SQL_UNSIGNED_OFFSET); // UNSIGNED TINYINT
  SQL_C_BOOKMARK = SQL_C_ULONG; // BOOKMARK
  SQL_C_GUID = SQL_GUID;

  SQL_TYPE_NULL = 0;
  SQL_C_VARBOOKMARK = SQL_C_BINARY;

  // define for SQL_DIAG_ROW_NUMBER and SQL_DIAG_COLUMN_NUMBER
  SQL_NO_ROW_NUMBER = (-1);
  SQL_NO_COLUMN_NUMBER = (-1);
  SQL_ROW_NUMBER_UNKNOWN = (-2);
  SQL_COLUMN_NUMBER_UNKNOWN = (-2);

  // SQLBindParameter extensions
  SQL_DEFAULT_PARAM = (-5);
  SQL_IGNORE = (-6);
  SQL_COLUMN_IGNORE = SQL_IGNORE;
  SQL_LEN_DATA_AT_EXEC_OFFSET = (-100);




type
  TSQLAllocHandle = function(HandleType: SqlSmallint;
                             InputHandle: SqlHandle;
                             pOutputHandle: PSqlHandle): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLSetEnvAttr = function(EnvironmentHandle: SqlHEnv;
                            Attribute: SqlInteger;
                            ValuePtr: Pointer;
                            StringLength: SqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLConnectA = function(ConnectionHandle: SqlHDbc;
                          ServerName: PAnsiChar;
                          NameLength1: SqlSmallint;
                          UserName: PAnsiChar;
                          NameLength2: SqlSmallint;
                          Authentication: PAnsiChar;
                          NameLength3: SqlSmallint): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;

  TSQLConnectW = function(ConnectionHandle: SqlHDbc;
                          ServerName: PWideChar;
                          NameLength1: SqlSmallint;
                          UserName: PWideChar;
                          NameLength2: SqlSmallint;
                          Authentication: PWideChar;
                          NameLength3: SqlSmallint): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;
  

  TSQLBindParameter = function(HStmt: SqlHStmt;
                               ipar: SqlUSmallint;
                               fParamType: SqlSmallint;
                               fCType: SqlSmallint;
                               fSqlType: SqlSmallint;
                               cbColDef: SqlUInteger;
                               ibScale: SqlSmallint;
                               rgbValue: SqlPointer;
                               cbValueMax: SqlInteger;
                               pcbValue: PSqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLExecDirectA = function(StatementHandle: SqlHStmt;
                             StatementText: PAnsiChar;
                             TextLength: SqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;
  

  TSQLExecDirectW = function(StatementHandle: SqlHStmt;
                             StatementText: PWideChar;
                             TextLength: SqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLPrepareA = function(StatementHandle: SqlHStmt;
                          StatementText: PAnsiChar;
                          TextLength: SqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;

  TSQLPrepareW = function(StatementHandle: SqlHStmt;
                          StatementText: PWideChar;
                          TextLength: SqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLExecute = function(StatementHandle: SqlHStmt): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;
  

  TSQLBindCol = function(StatementHandle: SqlHStmt;
                         ColumnNumber: SqlUSmallint;
                         TargetType: SqlSmallint;
                         TargetValue: SqlPointer;
                         BufferLength: SqlInteger;
                         StrLen_or_Ind: PSqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;

  TSQLFetch = function(StatementHandle: SqlHStmt): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLCloseCursor = function(StatementHandle: SqlHStmt): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLFreeHandle = function(HandleType: SqlSmallint;
                            Handle: SqlHandle): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLDisconnect = function(ConnectionHandle: SqlHDbc): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLSetConnectAttrA = function(ConnectionHandle: SqlHDbc;
                                 Attribute: SqlInteger;
                                 ValuePtr: SqlPointer;
                                 StringLength: SqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;


  TSQLSetConnectAttrW = function(ConnectionHandle: SqlHDbc;
                                 Attribute: SqlInteger;
                                 ValuePtr: SqlPointer;
                                 StringLength: SqlInteger): SqlReturn;
{$IFDEF MSWINDOWS}
  stdcall
{$ELSE}
  cdecl
{$ENDIF}
;




// MACRO: test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO
function SQL_SUCCEEDED(const rc: SqlReturn): Boolean;
  
implementation

function SQL_SUCCEEDED(const rc: SqlReturn): Boolean;
begin
  Result := (rc and (not 1)) = 0; // OdbcApi
end;

end.