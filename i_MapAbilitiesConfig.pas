unit i_MapAbilitiesConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapAbilitiesConfigStatic = interface
  ['{89BC8688-41A7-4ADE-A911-E90BAC6B5689}']
    function GetIsLayer: Boolean;
    property IsLayer: Boolean read GetIsLayer;

    function GetIsShowOnSmMap: Boolean;
    property IsShowOnSmMap: Boolean read GetIsShowOnSmMap;

    function GetIsUseStick: Boolean;
    property IsUseStick: Boolean read GetIsUseStick;

    function GetIsUseGenPrevious: Boolean;
    property IsUseGenPrevious: Boolean read GetIsUseGenPrevious;

    function GetUseDownload: Boolean;
    property UseDownload: Boolean read GetUseDownload;
  end;

  IMapAbilitiesConfig = interface(IConfigDataElement)
  ['{6CF60AD7-0284-4252-AC55-2A2C1ABAF4FC}']
    function GetIsLayer: Boolean;
    property IsLayer: Boolean read GetIsLayer;

    function GetIsShowOnSmMap: Boolean;
    procedure SetIsShowOnSmMap(AValue: Boolean);
    property IsShowOnSmMap: Boolean read GetIsShowOnSmMap write SetIsShowOnSmMap;

    function GetIsUseStick: Boolean;
    procedure SetIsUseStick(AValue: Boolean);
    property IsUseStick: Boolean read GetIsUseStick write SetIsUseStick;

    function GetIsUseGenPrevious: Boolean;
    procedure SetIsUseGenPrevious(AValue: Boolean);
    property IsUseGenPrevious: Boolean read GetIsUseGenPrevious write SetIsUseGenPrevious;

    function GetUseDownload: Boolean;
    procedure SetUseDownload(AValue: Boolean);
    property UseDownload: Boolean read GetUseDownload write SetUseDownload;

    function GetStatic: IMapAbilitiesConfigStatic;
  end;

implementation

end.
