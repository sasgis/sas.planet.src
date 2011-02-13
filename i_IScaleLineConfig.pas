unit i_IScaleLineConfig;

interface

uses
  i_IConfigDataElement;

type
  IScaleLineConfig = interface(IConfigDataElement)
    ['{D3B5B8D5-B389-4406-9881-9704030CDD1E}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
 end;

implementation

end.
