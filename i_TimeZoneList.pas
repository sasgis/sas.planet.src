unit i_TimeZoneList;

interface

uses
  t_GeoTypes;

type
  ITimeZoneArea = interface
    function GetCount: Integer;
    property Count: Integer read GetCount;

    procedure GetPoints(APoints: PDoublePointArray);
  end;

  ITimeZoneAreaList = interface
    ['{CE489152-7867-483C-A342-FA591309E13E}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ITimeZoneArea;
    property Items[i: Integer]: ITimeZoneArea read GetItem;
  end;

  ITimeZone = interface
    ['{B5A9A413-854A-4B58-A5BE-6230B70B36F2}']
    function GetDiff: TDateTime;
    property Diff: TDateTime read GetDiff;

    function GetAreaList: ITimeZoneAreaList;
    property AreaList: ITimeZoneAreaList read GetAreaList;
  end;

  ITimeZoneList = interface
    ['{2BFC0ED5-86D8-40D7-BB54-A1C6B3D2897F}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(AIndex: Integer): ITimeZone;
    property Items[i: Integer]: ITimeZone read GetItem;
  end;

implementation

end.
