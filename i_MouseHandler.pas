unit i_MouseHandler;

interface

uses
  Types,
  Classes,
  Controls;

type
  IMouseHandler = interface
    ['{81780134-ECB7-4D1A-A0B7-FE00D52C7D82}']
    procedure OnMouseMove(
      AShift: TShiftState;
      APos: TPoint
    );
    procedure OnMouseDown(
      AButton: TMouseButton;
      AShift: TShiftState;
      APos: TPoint
    );
    procedure OnMouseUp(
      AButton: TMouseButton;
      AShift: TShiftState;
      APos: TPoint
    );
  end;

implementation

end.
