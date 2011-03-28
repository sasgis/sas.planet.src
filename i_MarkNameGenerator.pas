unit i_MarkNameGenerator;

interface

uses
  i_ConfigDataElement;

type
  IMarkNameGenerator = interface(IConfigDataElement)
    ['{50F72618-AC98-40D2-B98B-E5EC3876D7B1}']
    function GetFormatString: string;
    procedure SetFormatString(AValue: string);
    property FormatString: string read GetFormatString write SetFormatString;

    function GetCounter: Integer;
    procedure SetCounter(AValue: Integer);
    property Counter: Integer read GetCounter write SetCounter;

    function GetNewName: string;
  end;

implementation

end.
