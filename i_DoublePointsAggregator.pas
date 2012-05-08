unit i_DoublePointsAggregator;

interface

uses
  t_GeoTypes;

type
  IDoublePointsAggregator = interface
    ['{2B653087-1769-4C76-A880-17A2E27BD282}']
    procedure Add(const APoint: TDoublePoint);
    procedure AddPoints(
      const APoints: PDoublePointArray;
      ACount: Integer
    );
    procedure Clear;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;

implementation

end.
