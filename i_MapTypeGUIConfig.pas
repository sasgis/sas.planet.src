unit i_MapTypeGUIConfig;

interface

uses
  Graphics,
  Classes,
  i_StringConfigDataElement,
  i_ConfigDataElement;

type
  IMapTypeGUIConfigStatic = interface
    ['{CDB1236F-59D5-4B84-AF3D-764C1595E0AD}']
    function GetName: string;
    property Name: string read GetName;

    function GetSortIndex: Integer;
    property SortIndex: Integer read GetSortIndex;

    function GetHotKey: TShortCut;
    property HotKey: TShortCut read GetHotKey;

    function GetSeparator: Boolean;
    property Separator: Boolean read GetSeparator;

    function GetParentSubMenu: string;
    property ParentSubMenu: string read GetParentSubMenu;

    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetInfoUrl: string;
    property InfoUrl: string read GetInfoUrl;

    function GetBmp18: TBitmap;
    property Bmp18: TBitmap read GetBmp18;

    function GetBmp24: TBitmap;
    property Bmp24: TBitmap read GetBmp24;
  end;

  IMapTypeGUIConfig = interface(IConfigDataElement)
    ['{E597028D-5B2B-4771-A68D-1F9BD4111EC1}']
    function GetName: IStringConfigDataElement;
    property Name: IStringConfigDataElement read GetName;

    function GetSortIndex: Integer;
    procedure SetSortIndex(const AValue: Integer);
    property SortIndex: Integer read GetSortIndex write SetSortIndex;

    function GetHotKey: TShortCut;
    procedure SetHotKey(const AValue: TShortCut);
    property HotKey: TShortCut read GetHotKey write SetHotKey;

    function GetSeparator: Boolean;
    procedure SetSeparator(const AValue: Boolean);
    property Separator: Boolean read GetSeparator write SetSeparator;

    function GetParentSubMenu: IStringConfigDataElement;
    property ParentSubMenu: IStringConfigDataElement read GetParentSubMenu;

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function GetInfoUrl: IStringConfigDataElement;
    property InfoUrl: IStringConfigDataElement read GetInfoUrl;

    function GetBmp18: TBitmap;
    property Bmp18: TBitmap read GetBmp18;

    function GetBmp24: TBitmap;
    property Bmp24: TBitmap read GetBmp24;

    function GetStatic: IMapTypeGUIConfigStatic;
  end;

implementation

end.
