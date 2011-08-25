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

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);
    property BottomMargin: Integer read GetBottomMargin write SetBottomMargin;
 end;

implementation

end.
