unit i_ScaleLineConfig;

interface

uses
  i_ConfigDataElement;

type
  IScaleLineConfig = interface(IConfigDataElement)
    ['{C8AAEDF7-D20D-4ECA-919A-76EF8E60EAE8}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
 end;

implementation

end.
