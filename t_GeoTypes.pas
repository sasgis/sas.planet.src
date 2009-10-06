unit t_GeoTypes;

interface

uses
  Types;

type
  TExtendedPoint = record
   X, Y: Extended;
  end;

  TExtendedRect = packed record
    case Integer of
      0: (Left, Top: Extended; Reserved:Longint; Right, Bottom: Extended);
      1: (TopLeft, BottomRight: TExtendedPoint);
  end;


  TPointArray = array of TPoint;

  TExtendedPointArray = array of TExtendedPoint;

implementation

end.
 