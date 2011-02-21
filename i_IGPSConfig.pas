unit i_IGPSConfig;

interface

uses
  i_IGPSModuleByCOMPortSettings,
  i_IConfigDataElement;

type
  IGPSConfig = interface(IConfigDataElement)
    ['{336A93F1-8E9C-4704-8384-214018758354}']
    function GetGPSEnabled: Boolean;
    procedure SetGPSEnabled(AValue: Boolean);
    property GPSEnabled: Boolean read GetGPSEnabled write SetGPSEnabled;

    function GetWriteLog: Boolean;
    procedure SetWriteLog(AValue: Boolean);
    property WriteLog: Boolean read GetWriteLog write SetWriteLog;

    function GetLogPath: WideString;
    property LogPath: WideString read GetLogPath;

    function GetModuleConfig: IGPSModuleByCOMPortConfig;
    property ModuleConfig: IGPSModuleByCOMPortConfig read GetModuleConfig;
  end;

implementation

end.
