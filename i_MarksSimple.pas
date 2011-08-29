unit i_MarksSimple;

interface

uses
  ActiveX,
  GR32,
  t_GeoTypes,
  i_MarkCategory,
  i_MarkPicture;

type
  IMarkID = interface
    ['{A3FE0170-8D32-4777-A3EA-53D678875B7B}']
    function GetName: string;
    property Name: string read GetName;
  end;

  IMark = interface
    ['{52794019-3681-4C92-B50F-0853D5B070DE}']
    function GetName: string;
    property Name: string read GetName;
    function GetCategory: IMarkCategory;
    property Category: IMarkCategory read GetCategory;
    function GetDesc: string;
    property Desc: string read GetDesc;
    function GetLLRect: TDoubleRect;
    property LLRect: TDoubleRect read GetLLRect;

    function GetHintText: string;
    function GetInfoHTML: string;
    function IsNew: Boolean;
    function IsSameId(AMarkId: IMarkID): Boolean;
    function GetGoToLonLat: TDoublePoint;
  end;

  IMarkPoint = interface(IMark)
    ['{6E8C2BA9-4A1A-49A8-98FF-8F5BFCBDB00C}']
    function GetPoint: TDoublePoint;
    property Point: TDoublePoint read GetPoint;

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
  end;

  IMarkLine = interface(IMark)
    ['{3C400B96-90E1-4ADD-9AA2-56199AC1910F}']
    function GetPoints: TArrayOfDoublePoint;
    property Points: TArrayOfDoublePoint read GetPoints;

    function GetLineColor: TColor32;
    property LineColor: TColor32 read GetLineColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;
  end;

  IMarkPoly = interface(IMark)
    ['{5C66FCE6-F235-4E34-B32A-AB1DD5F0C5B1}']
    function GetPoints: TArrayOfDoublePoint;
    property Points: TArrayOfDoublePoint read GetPoints;

    function GetBorderColor: TColor32;
    property BorderColor: TColor32 read GetBorderColor;

    function GetFillColor: TColor32;
    property FillColor: TColor32 read GetFillColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;
  end;

  IMarksSubset = interface
    ['{D2DBC018-AAF5-44CB-A2B1-B5AC1C3341C5}']
    function GetSubsetByLonLatRect(ARect: TDoubleRect): IMarksSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;
  end;

implementation

end.
