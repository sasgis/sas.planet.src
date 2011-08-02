unit i_MouseState;

interface

uses
  Types,
  Classes,
  Controls;

type
  IMouseState = interface
    ['{10305260-821C-4867-9446-6EF3B6655AE5}']
    function GetCurentPos: TPoint;
    property CurentPos: TPoint read GetCurentPos;

    function GetPreviousPos: TPoint;
    property PreviousPos: TPoint read GetPreviousPos;

    function GetCurrentShift: TShiftState;
    property CurrentShift: TShiftState read GetCurrentShift;

    function GetLastDownShift(AButton: TMouseButton): TShiftState;
    function GetLastDownPos(AButton: TMouseButton): TPoint;
    function GetLastUpShift(AButton: TMouseButton): TShiftState;
    function GetLastUpPos(AButton: TMouseButton): TPoint;
  end;

implementation

end.
