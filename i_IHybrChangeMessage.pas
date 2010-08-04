unit i_IHybrChangeMessage;

interface

uses
  UMapType;

type
  THybrChangeAction = (hcaSelect = 1, hcaUnselect = 0);

  IHybrChangeMessage = interface
    ['{D804ACF7-73BF-4FD3-9907-6FF1F1334D01}']
    function GetAction: THybrChangeAction; stdcall;
    function GetMap: TMapType; stdcall;
  end;

implementation

end.

