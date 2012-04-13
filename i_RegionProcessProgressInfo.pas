unit i_RegionProcessProgressInfo;

interface

type
  IProgressInfo = interface
    ['{B636F3D1-3F17-4BE5-8D8E-E161F945E42D}']
    function GetProcessed: Double;
    procedure SetProcessed(const AValue: Double);
    property Processed: Double read GetProcessed write SetProcessed;
  end;
  
  IRegionProcessProgressInfo = interface(IProgressInfo)
    ['{58559CEF-9233-4E25-87E0-F88E1A78C5AD}']
    function GetFinished: Boolean;
    property Finished: Boolean read GetFinished;

    function GetCaption: string;
    procedure SetCaption(const AValue: string);
    property Caption: string read GetCaption write SetCaption;

    function GetFirstLine: string;
    procedure SetFirstLine(const AValue: string);
    property FirstLine: string read GetFirstLine write SetFirstLine;

    function GetSecondLine: string;
    procedure SetSecondLine(const AValue: string);
    property SecondLine: string read GetSecondLine write SetSecondLine;

    procedure Finish;
  end;

implementation

end.
