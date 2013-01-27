unit i_Point;

interface

uses
  Types;

type
  IPoint = interface
    ['{A7E0BE8C-7A87-47D2-9EA7-694D1EA2A700}']
    function GetX: Integer;
    property X: Integer read GetX;

    function GetY: Integer;
    property Y: Integer read GetY;

    function GetPoint: TPoint;
    property Point: TPoint read GetPoint;

    function IsSame(const AValue: IPoint): Boolean;
  end;

implementation

end.
