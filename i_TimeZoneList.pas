unit i_TimeZoneList;

interface

uses
  i_VectorItemLonLat;

type
  ITimeZone = interface
    ['{B5A9A413-854A-4B58-A5BE-6230B70B36F2}']
    function GetDiff: TDateTime;
    property Diff: TDateTime read GetDiff;

    function GetPolygon: ILonLatPolygon;
    property Polygon: ILonLatPolygon read GetPolygon;
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
