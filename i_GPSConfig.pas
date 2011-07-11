unit i_GPSConfig;

interface

uses
  i_GPSModuleByCOMPortConfig,
  i_ConfigDataElement;

type
  IGPSConfig = interface(IConfigDataElement)
    ['{336A93F1-8E9C-4704-8384-214018758354}']
    function GetGPSEnabled: Boolean;
    procedure SetGPSEnabled(const AValue: Boolean);
    property GPSEnabled: Boolean read GetGPSEnabled write SetGPSEnabled;

    function GetNoDataTimeOut: Integer;
    procedure SetNoDataTimeOut(const AValue: Integer);
    property NoDataTimeOut: Integer read GetNoDataTimeOut write SetNoDataTimeOut;

    function GetWriteLog: Boolean;
    procedure SetWriteLog(const AValue: Boolean);
    property WriteLog: Boolean read GetWriteLog write SetWriteLog;

    function GetLogPath: WideString;
    property LogPath: WideString read GetLogPath;

    function GetModuleConfig: IGPSModuleByCOMPortConfig;
    property ModuleConfig: IGPSModuleByCOMPortConfig read GetModuleConfig;
  end;

implementation

end.
