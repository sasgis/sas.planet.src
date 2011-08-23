unit i_MapTypeGUIConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapTypeGUIConfigStatic = interface
    ['{CDB1236F-59D5-4B84-AF3D-764C1595E0AD}']
    function GetName: string;
    property Name: string read GetName;

    function GetSortIndex: Integer;
    property SortIndex: Integer read GetSortIndex;

    function GetSeparator: Boolean;
    property Separator: Boolean read GetSeparator;

    function GetParentSubMenu: string;
    property ParentSubMenu: string read GetParentSubMenu;

    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;
  end;

  IMapTypeGUIConfig = interface(IConfigDataElement)
    ['{E597028D-5B2B-4771-A68D-1F9BD4111EC1}']
    function GetName: string;
    procedure SetName(const AValue: string);
    property Name: string read GetName write SetName;

    function GetSortIndex: Integer;
    procedure SetSortIndex(const AValue: Integer);
    property SortIndex: Integer read GetSortIndex write SetSortIndex;

    function GetSeparator: Boolean;
    procedure SetSeparator(const AValue: Boolean);
    property Separator: Boolean read GetSeparator write SetSeparator;

    function GetParentSubMenu: string;
    procedure SetParentSubMenu(const AValue: string);
    property ParentSubMenu: string read GetParentSubMenu write SetParentSubMenu;

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function GetStatic: IMapTypeGUIConfigStatic;
  end;

implementation

end.
