unit i_MapSvcScanConfig;

interface

uses
  i_PathConfig,
  i_ConfigDataElement;

type
  IMapSvcScanConfig = interface(IConfigDataElement)
    ['{26AC48DD-6899-4F3D-99D9-AF39C71E99E8}']
    function GetPath: IPathConfig;
    property Path: IPathConfig read GetPath;

    function GetUseStorage: Boolean;
    procedure SetUseStorage(const AValue: Boolean);
    property UseStorage: Boolean read GetUseStorage write SetUseStorage;

    function GetShowOnlyNew: Boolean;
    procedure SetShowOnlyNew(const AValue: Boolean);
    property ShowOnlyNew: Boolean read GetShowOnlyNew write SetShowOnlyNew;

    function GetMakeOnlyNew: Boolean;
    procedure SetMakeOnlyNew(const AValue: Boolean);
    property MakeOnlyNew: Boolean read GetMakeOnlyNew write SetMakeOnlyNew;

    function GetOldAfterDays: Integer;
    procedure SetOldAfterDays(const AValue: Integer);
    property OldAfterDays: Integer read GetOldAfterDays write SetOldAfterDays;
  end;

implementation

end.
