unit i_IMainWindowPosition;

interface

uses
  Types,
  i_IConfigDataElement;

type
  IMainWindowPosition = interface(IConfigDataElement)
    ['{BD5C5719-02CB-4364-A670-B1DD75A5BAEE}']
    function GetIsFullScreen: Boolean;
    function GetIsMaximized: Boolean;
    function GetBoundsRect: TRect;
    procedure SetFullScreen;
    procedure SetMaximized;
    procedure SetNormalWindow;
    procedure SetWindowPosition(ARect: TRect);
  end;

implementation

end.
