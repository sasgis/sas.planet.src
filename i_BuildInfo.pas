unit i_BuildInfo;

interface

type
  IBuildInfo = interface
    ['{541D6359-3C4F-44D5-9BC0-CA12CAE45220}']
    function GetVersion: string;
    function GetVersionDetaled: string; 
    function GetBuildDate: TDateTime;
    function GetBuildType: string;
    function GetBuildSrcInfo(out ARev: Integer; out ANode: string): Boolean;
    function GetBuildReqInfo(out ARev: Integer; out ANode: string): Boolean;
    function GetCompilerInfo: string;
    function GetDescription: string;
  end;

implementation

end.
