unit i_TileStorageSQLiteErrorLogger;

interface

type
  IErrorLoggerToFile = interface
    ['{9865A8A7-51CB-453E-83F3-D216B941180D}']
    // log string as is
    procedure LogString(const AValue: String);
    // close file
    procedure Close;
  end;

implementation

end.
