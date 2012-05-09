unit i_PathConfig;

interface

uses
  i_ConfigDataElement;

type
  IPathConfig = interface(IConfigDataElement)
    ['{0C315B68-7227-4AA0-981C-7CD14C4DA362}']
    function GetDefaultPath: string;
    property DefaultPath: string read GetDefaultPath;

    function GetBasePathConfig: IPathConfig;
    procedure SetBasePathConfig(const AValue: IPathConfig);
    property BasePathConfig: IPathConfig read GetBasePathConfig write SetBasePathConfig;

    function GetPath: string;
    procedure SetPath(const AValue: string);
    property Path: string read GetPath write SetPath;

    function GetIsRelative: Boolean;
    property IsRelative: Boolean read GetIsRelative;

    function GetFullPath: string;
    property FullPath: string read GetFullPath;
  end;

implementation

end.
