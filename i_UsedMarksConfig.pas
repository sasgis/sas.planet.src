unit i_UsedMarksConfig;

interface

uses
  i_ConfigDataElement;

type
  IUsedMarksConfigStatic = interface
  ['{77A54AD4-2B5B-48CE-BD5F-1F4B89763FF2}']
    function GetIsUseMarks: Boolean;
    property IsUseMarks: Boolean read GetIsUseMarks;
    function GetIgnoreCategoriesVisible: Boolean;
    property IgnoreCategoriesVisible: Boolean read GetIgnoreCategoriesVisible;
    function GetIgnoreMarksVisible: Boolean;
    property IgnoreMarksVisible: Boolean read GetIgnoreMarksVisible;
  end;

  IUsedMarksConfig = interface(IConfigDataElement)
    ['{5E07CEB8-C461-4994-A2CD-3A38269060F9}']
    function GetIsUseMarks: Boolean;
    procedure SetIsUseMarks(AValue: Boolean);
    property IsUseMarks: Boolean read GetIsUseMarks write SetIsUseMarks;

    function GetIgnoreCategoriesVisible: Boolean;
    procedure SetIgnoreCategoriesVisible(AValue: Boolean);
    property IgnoreCategoriesVisible: Boolean read GetIgnoreCategoriesVisible write SetIgnoreCategoriesVisible;

    function GetIgnoreMarksVisible: Boolean;
    procedure SetIgnoreMarksVisible(AValue: Boolean);
    property IgnoreMarksVisible: Boolean read GetIgnoreMarksVisible write SetIgnoreMarksVisible;

    function GetStatic: IUsedMarksConfigStatic;
  end;

implementation

end.
