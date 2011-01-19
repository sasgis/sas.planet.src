unit i_MarksSimple;

interface

uses
  ActiveX,
  GR32,
  t_GeoTypes,
  i_IMarkPicture;

type
  IMarkVisible = interface
  ['{2611AAA5-10DA-472B-B3EE-31EA27EDD6CD}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IMarkID = interface
    ['{A3FE0170-8D32-4777-A3EA-53D678875B7B}']
    function GetId: Integer;
    property Id: Integer read GetId;
    function GetName: string;
    property Name: string read GetName;
  end;

  IMarkTemplatePoint = interface
    ['{6D769EB1-7D5C-4652-8DC7-BF07166EAB21}']
    function GetCategoryId: Integer;
    property CategoryId: Integer read GetCategoryId;
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
  end;

  IMarkTemplateLine = interface
    ['{99D4D68F-B3E5-4A5D-8E3B-F27D8C44C013}']
    function GetCategoryId: Integer;
    property CategoryId: Integer read GetCategoryId;
    function GetColor1: TColor32;
    property Color1: TColor32 read GetColor1;
    function GetScale1: Integer;
    property Scale1: Integer read GetScale1;
  end;

  IMarkTemplatePoly = interface
    function GetCategoryId: Integer;
    property CategoryId: Integer read GetCategoryId;
    function GetColor1: TColor32;
    property Color1: TColor32 read GetColor1;
    function GetColor2: TColor32;
    property Color2: TColor32 read GetColor2;
    function GetScale1: Integer;
    property Scale1: Integer read GetScale1;
  end;


  IMarkFull = interface
    ['{3502C7E6-F974-4F73-B458-F9C3E979F554}']
    function GetId: Integer;
    property Id: Integer read GetId;
    function GetName: string;
    property Name: string read GetName;
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

  IMarksSubset = interface
    ['{D2DBC018-AAF5-44CB-A2B1-B5AC1C3341C5}']
    function GetSubsetByLonLatRect(ARect: TDoubleRect): IMarksSubset;
    function GetEnum: IEnumUnknown;
  end;

implementation

end.
