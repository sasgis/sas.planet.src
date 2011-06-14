unit i_MapZoomingConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapZoomingConfig = interface(IConfigDataElement)
    ['{A322104E-A247-4EB3-83F6-C897F64E764C}']
    // Фиксировать центр изменения масштаба под курсором мыши
    function GetZoomingAtMousePos: Boolean;
    procedure SetZoomingAtMousePos(AValue: Boolean);
    property ZoomingAtMousePos: Boolean read GetZoomingAtMousePos write SetZoomingAtMousePos;

    //Анимированный зум
    function GetAnimateZoom: Boolean;
    procedure SetAnimateZoom(AValue: Boolean);
    property AnimateZoom: Boolean read GetAnimateZoom write SetAnimateZoom;

    function GetAnimateZoomTime: Cardinal;
    procedure SetAnimateZoomTime(AValue: Cardinal);
    property AnimateZoomTime: Cardinal read GetAnimateZoomTime write SetAnimateZoomTime;
  end;

implementation

end.
