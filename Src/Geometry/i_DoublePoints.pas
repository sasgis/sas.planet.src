unit i_DoublePoints;

interface

uses
  t_GeoTypes;

type
  IDoublePoints = interface
    ['{972AC801-81B4-4440-8827-E3F26BC45DD2}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;
  end;


implementation

end.
