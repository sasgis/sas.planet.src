unit i_WindowPositionConfig;

interface

uses
  Types,
  i_ConfigDataElement;

type
  IWindowPositionConfig = interface(IConfigDataElement)
    ['{BD5C5719-02CB-4364-A670-B1DD75A5BAEE}']
    function GetBoundsRect: TRect;
    property BoundsRect: TRect read GetBoundsRect;
    procedure SetWindowPosition(const ARect: TRect);
  end;

implementation

end.
