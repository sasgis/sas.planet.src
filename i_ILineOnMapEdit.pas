unit i_ILineOnMapEdit;

interface

uses
  t_GeoTypes,
  i_IConfigDataElement;

type
  ILineOnMapEdit = interface(IConfigDataElement)
  ['{76049798-151B-4E06-9EF9-3BE14451BCFF}']
    function GetCount: Integer;
    function GetActiveIndex: Integer;
    function GetPoints: TDoublePointArray;
    function GetPointIndexInLonLatRect(ARect: TDoubleRect): Integer;
    procedure Empty;
    procedure SetActiveIndex(AValue: Integer);
    procedure DeleteActivePoint;
    procedure InsertPoint(APoint: TDoublePoint);
    procedure MoveActivePoint(APoint: TDoublePoint);
    procedure SetPoints(AValue: TDoublePointArray);
  end;
implementation

end.
