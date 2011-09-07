unit i_StringConfigDataElement;

interface

uses
  i_ConfigDataElement;

type
  IStringConfigDataElement = interface(IConfigDataElement)
  ['{4B8219F2-335D-44BD-AE07-080863300840}']
    function GetValue: string;
    procedure SetValue(AValue: string);
    property Value: string read GetValue write SetValue;

    function GetDefaultValue: string;
  end;

implementation

end.
