unit i_RegionProcessProgressInfo;

interface

uses
  i_ConfigDataWriteProvider;
  
type
  IProgressInfoBase = interface
    ['{B636F3D1-3F17-4BE5-8D8E-E161F945E42D}']
    function GetProcessedRatio: Double;
    procedure SetProcessedRatio(const AValue: Double);
    property ProcessedRatio: Double read GetProcessedRatio write SetProcessedRatio;

    function GetFinished: Boolean;
    property Finished: Boolean read GetFinished;

    procedure Finish;
  end;

  IRegionProcessProgressInfo = interface(IProgressInfoBase)
    ['{58559CEF-9233-4E25-87E0-F88E1A78C5AD}']
    function GetCaption: string;
    procedure SetCaption(const AValue: string);
    property Caption: string read GetCaption write SetCaption;

    function GetFirstLine: string;
    procedure SetFirstLine(const AValue: string);
    property FirstLine: string read GetFirstLine write SetFirstLine;

    function GetSecondLine: string;
    procedure SetSecondLine(const AValue: string);
    property SecondLine: string read GetSecondLine write SetSecondLine;
  end;

  IRegionProcessProgressInfoDownload = interface(IProgressInfoBase)
    ['{D0458E91-C891-40BD-9162-0130252E97E0}']
    function GetTotalToProcess: Int64;
    property TotalToProcess: Int64 read GetTotalToProcess;

    function GetDownloaded: Int64;
    property Downloaded: Int64 read GetDownloaded;

    function GetProcessed: Int64;
    property Processed: Int64 read GetProcessed;

    function GetDownloadSize: Double;
    property DownloadSize: Double read GetDownloadSize;

    function GetElapsedTime: TDateTime;
    property ElapsedTime: TDateTime read GetElapsedTime;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    procedure Pause;
    procedure Resume;
    procedure SaveState(const ASLSSection: IConfigDataWriteProvider);
  end;

implementation

end.
