unit i_MarkTemplate;

interface

uses
  GR32,
  i_MarkCategory,
  i_MarkPicture;

type
  IMarkTemplate = interface
    ['{2D6A0C13-754C-4BC1-9003-361CA28D311E}']
    function GetNewName: string;

    function GetCategory: IMarkCategory;
    property Category: IMarkCategory read GetCategory;
  end;

  IMarkTemplatePoint = interface(IMarkTemplate)
    ['{B36731B8-7D98-4D56-996F-E6B77AA6FAB3}']
    function GetTextColor: TColor32;
    property TextColor: TColor32 read GetTextColor;

    function GetTextBgColor: TColor32;
    property TextBgColor: TColor32 read GetTextBgColor;

    function GetFontSize: Integer;
    property FontSize: Integer read GetFontSize;

    function GetMarkerSize: Integer;
    property MarkerSize: Integer read GetMarkerSize;

    function GetPic: IMarkPicture;
    property Pic: IMarkPicture read GetPic;

    function IsSame(ATemplate: IMarkTemplatePoint): Boolean;
  end;

  IMarkTemplateLine = interface(IMarkTemplate)
    ['{BF4FF116-98E1-43C5-A7FD-DCE3BF26E8D4}']
    function GetColor: TColor32;
    property Color: TColor32 read GetColor;

    function GetLineWeight: Integer;
    property LineWeight: Integer read GetLineWeight;

    function IsSame(ATemplate: IMarkTemplateLine): Boolean;
  end;

  IMarkTemplatePoly = interface(IMarkTemplate)
    ['{81CB621A-112D-4914-B801-BBBAAE11C797}']
    function GetColor1: TColor32;
    property Color1: TColor32 read GetColor1;

    function GetFillColor: TColor32;
    property FillColor: TColor32 read GetFillColor;

    function GetScale1: Integer;
    property Scale1: Integer read GetScale1;

    function IsSame(ATemplate: IMarkTemplatePoly): Boolean;
  end;

implementation

end.
