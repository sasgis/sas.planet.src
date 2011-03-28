unit i_PosChangeMessage;

interface

uses
  Types,
  i_JclNotify,
  i_LocalCoordConverter;

type
  IPosChangeMessage = interface(IJclNotificationMessage)
    ['{D804ACF7-73BF-4FD3-9907-6FF1F1334D01}']
    function GetVisualCoordConverter: ILocalCoordConverter; stdcall;
  end;


implementation

end.
 