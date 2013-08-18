unit i_AppearanceOfVectorItem;

interface

uses
  GR32,
  i_MarkPicture,
  i_Appearance;

type
  IAppearancePointCaption = interface(IAppearance)
    ['{0AC73A92-23B1-4D92-AE97-BEE965944424}']
    function GetTextColor: TColor32;
    property TextColor: TColor32 read GetTextColor;

    function GetTextBgColor: TColor32;
    property TextBgColor: TColor32 read GetTextBgColor;

    function GetFontSize: Integer;
    property FontSize: Integer read GetFontSize;
  end;

  IAppearancePointIcon = interface(IAppearance)
    ['{537841F9-C492-4E42-BD96-66A1E78C65DC}']
    function GetMarkerSize: Integer;
    property MarkerSize: Integer read GetMarkerSize;

    function GetPicName: string;
    property PicName: string read GetPicName;

    function GetPic: IMarkPicture;
    property Pic: IMarkPicture read GetPic;
  end;

  IAppearanceLine = interface(IAppearance)
    ['{97FA7D16-A05E-4041-8DCF-0295279CD941}']
    function GetLineColor: TColor32;
    property LineColor: TColor32 read GetLineColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;
  end;

  IAppearancePolygonBorder = interface(IAppearance)
    ['{B38E37EB-FBD9-46F6-9ED1-A716AE5D155A}']
    function GetLineColor: TColor32;
    property LineColor: TColor32 read GetLineColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;
  end;

  IAppearancePolygonFill = interface(IAppearance)
    ['{B38E37EB-FBD9-46F6-9ED1-A716AE5D155A}']
    function GetFillColor: TColor32;
    property FillColor: TColor32 read GetFillColor;
  end;

implementation

end.
