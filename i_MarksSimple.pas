unit i_MarksSimple;

interface

uses
  GR32,
  t_GeoTypes,
  i_IMarkPicture;

type
  IMarkID = interface
    ['{A3FE0170-8D32-4777-A3EA-53D678875B7B}']
    function GetId: Integer;
    property Id: Integer read GetId;
    function GetName: string;
    property Name: string read GetName;
  end;

  IMarkFull = interface(IMarkID)
    ['{3502C7E6-F974-4F73-B458-F9C3E979F554}']
    function GetCategoryId: Integer;
    property CategoryId: Integer read GetCategoryId;
    function GetDesc: string;
    property Desc: string read GetDesc;
    function GetLLRect: TDoubleRect;
    property LLRect: TDoubleRect read GetLLRect;
    function GetPoints: TDoublePointArray;
    property Points: TDoublePointArray read GetPoints;
    function GetColor1: TColor32;
    property Color1: TColor32 read GetColor1;
    function GetColor2: TColor32;
    property Color2: TColor32 read GetColor2;
    function GetScale1: Integer;
    property Scale1: Integer read GetScale1;
    function GetScale2: Integer;
    property Scale2: Integer read GetScale2;
    function GetPicName: string;
    property PicName: string read GetPicName;
    function GetPic: IMarkPicture;
    property Pic: IMarkPicture read GetPic;

    function IsEmpty: Boolean;
    function IsPoint: Boolean;
    function IsLine: Boolean;
    function IsPoly: Boolean;
    function GetGoToLonLat: TDoublePoint;
  end;

  IMarkVisible = interface
  ['{2611AAA5-10DA-472B-B3EE-31EA27EDD6CD}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;


implementation

end.
