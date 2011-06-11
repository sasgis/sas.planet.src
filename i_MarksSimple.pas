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

  IMarkFull = interface
    ['{3502C7E6-F974-4F73-B458-F9C3E979F554}']
    function GetName: string;
    property Name: string read GetName;
    function GetCategory: IMarkCategory;
    property Category: IMarkCategory read GetCategory;
    function GetDesc: string;
    property Desc: string read GetDesc;
    function GetLLRect: TDoubleRect;
    property LLRect: TDoubleRect read GetLLRect;
    function GetPoints: TArrayOfDoublePoint;
    property Points: TArrayOfDoublePoint read GetPoints;
    function GetColor1: TColor32;
    property Color1: TColor32 read GetColor1;
    function GetColor2: TColor32;
    property Color2: TColor32 read GetColor2;
    function GetScale1: Integer;
    property Scale1: Integer read GetScale1;
    function GetScale2: Integer;
    property Scale2: Integer read GetScale2;
    function GetPic: IMarkPicture;
    property Pic: IMarkPicture read GetPic;

    function IsEmpty: Boolean;
    function IsPoint: Boolean;
    function IsLine: Boolean;
    function IsPoly: Boolean;
    function IsNew: Boolean;
    function IsSameId(AMarkId: IMarkID): Boolean;

    function GetGoToLonLat: TDoublePoint;
  end;

  IMarksSubset = interface
    ['{D2DBC018-AAF5-44CB-A2B1-B5AC1C3341C5}']
    function GetSubsetByLonLatRect(ARect: TDoubleRect): IMarksSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;
  end;

implementation

end.
