unit i_SelectionRect;

interface

uses
  Classes,
  t_GeoTypes,
  i_ConfigDataElement;

type
  ISelectionRect = interface(IConfigDataElement)
    ['{E01E4A76-CE68-4841-AD0B-15E26E90C4E3}']
    function IsEmpty: Boolean;
    procedure SetNextPoint(ALonLat: TDoublePoint; Shift: TShiftState);
    procedure Reset;
    function GetRect: TDoubleRect;
  end;

implementation

end.
