unit i_IPosChangeMessage;

interface

uses
  Types,
  i_ICoordConverter,
  UMapType;

type
  IPosChangeMessage = interface
    ['{D804ACF7-73BF-4FD3-9907-6FF1F1334D01}']
    function GetViewSize: TPoint; stdcall;
    function GetCoordConverter: ICoordConverter; stdcall;
    function GetMap: TMapType; stdcall;
    function GetZoom: Byte; stdcall;
    function GetMapPixel: TPoint; stdcall;
  end;


implementation

end.
 