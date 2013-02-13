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

    function GetDataDoorsState: Integer;
    procedure SetDataDoorsState(const AValue: Integer);
    property DataDoorsState: Integer read GetDataDoorsState write SetDataDoorsState;

    function GetKosmosnimkiState: Integer;
    procedure SetKosmosnimkiState(const AValue: Integer);
    property KosmosnimkiState: Integer read GetKosmosnimkiState write SetKosmosnimkiState;

    function GetRosCosmosState: Integer;
    procedure SetRosCosmosState(const AValue: Integer);
    property RosCosmosState: Integer read GetRosCosmosState write SetRosCosmosState;

    function GetRosCosmosUserName: String;
    procedure SetRosCosmosUserName(const AValue: String);
    property RosCosmosUserName: String read GetRosCosmosUserName write SetRosCosmosUserName;

    function GetRosCosmosPassword: String;
    procedure SetRosCosmosPassword(const AValue: String);
    property RosCosmosPassword: String read GetRosCosmosPassword write SetRosCosmosPassword;
  end;

implementation

end.
