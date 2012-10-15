unit i_RegionProcessProgressInfoDownload;

interface

uses
  Types,
  i_ConfigDataWriteProvider,
  i_LogSimple,
  i_LogSimpleProvider,
  i_RegionProcessProgressInfo;

type
  IRegionProcessProgressInfoDownload = interface(IProgressInfoBase)
    ['{D0458E91-C891-40BD-9162-0130252E97E0}']
    function GetTotalToProcess: Int64;
    property TotalToProcess: Int64 read GetTotalToProcess;

    function GetDownloaded: Int64;
    property Downloaded: Int64 read GetDownloaded;

    function GetProcessed: Int64;
    property Processed: Int64 read GetProcessed;

    function GetDownloadSize: UInt64;
    property DownloadSize: UInt64 read GetDownloadSize;

    function GetElapsedTime: TDateTime;
    property ElapsedTime: TDateTime read GetElapsedTime;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetLogProvider: ILogSimpleProvider;
    property LogProvider: ILogSimpleProvider read GetLogProvider; 

    procedure SaveState(const ASLSSection: IConfigDataWriteProvider);

    function GetIsPaused: Boolean;
    property IsPaused: Boolean read GetIsPaused;
    procedure Pause;
    procedure Resume;
  end;

  IRegionProcessProgressInfoDownloadInternal = interface
    ['{9D2A57FB-D127-44D0-98F9-3BBDBEAEDCD6}']
    function GetNeedPause: Boolean;
    procedure SetNeedPause(AValue: Boolean);
    property NeedPause: Boolean read GetNeedPause write SetNeedPause;

    procedure Finish;
    procedure SetPaused;
    procedure SetStarted;
    procedure AddManyProcessedTile(const ALastTile: TPoint; const ACnt: Cardinal);
    procedure AddProcessedTile(const ATile: TPoint);
    procedure AddDownloadedTile(const ATile: TPoint; const ASize: Cardinal);
    procedure AddNotNecessaryTile(const ATile: TPoint);
    procedure SetTotalToProcess(AValue: Int64);

    function GetLog: ILogSimple;
    property Log: ILogSimple read GetLog;
  end;


implementation

end.
