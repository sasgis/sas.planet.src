unit i_StringListChangeable;

interface

uses
  i_StringListStatic,
  i_Changeable;

type
  IStringListChangeable = interface(IChangeable)
    ['{0D822DAC-1139-42D3-8F04-B9AF08A7108A}']
    function GetStatic: IStringListStatic;
  end;

implementation

end.
