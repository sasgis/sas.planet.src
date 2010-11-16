unit i_IHybrChangeMessage;

interface

uses
  UMapType;

type
  THybrChangeAction = (hcaSelect = 1, hcaUnselect = 0);

  IHybrChangeMessage = interface
    ['{2E58DA01-A5A3-4D21-B759-9C78D1F3102C}']
    function GetAction: THybrChangeAction; stdcall;
    function GetMap: TMapType; stdcall;
  end;

implementation

end.
