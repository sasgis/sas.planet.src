unit i_TileStorageSQLiteHolder;

interface

uses
  i_ContentTypeInfo,
  i_MapVersionInfo;

type
  TSetSQLiteExecProc = procedure (const ASQLStatement: AnsiString) of object;

  ITileStorageSQLiteHolder = interface
    ['{BCDBCE62-2C15-4296-AB26-E385BE4D4BC3}']
    procedure LogError(
      const ACmd: AnsiChar;
      const AMsg: String;
      const ARaiseError: Boolean
    );

    // execute statements (set params or make tables)
    procedure ExecMakeSession(const AExecProc: TSetSQLiteExecProc);
    procedure ExecForNewTable(const AExecProc: TSetSQLiteExecProc);
    procedure ExecEstablished(const AExecProc: TSetSQLiteExecProc);

    // contenttypes
    function GetContentTypeToDB(const AContentType: IContentTypeInfoBasic): AnsiString;
    function GetContentTypeInfo(const AContentTypeFromDB: AnsiString): IContentTypeInfoBasic;

    // get version
    function GetVersionInfo(const AVersionStr: String): IMapVersionInfo;
  end;

implementation

end.
