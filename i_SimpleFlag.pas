unit i_SimpleFlag;

interface

type
  ISimpleFlag = interface
    ['{840AE4C5-D624-4EC9-9854-CD772F8414D9}']
    procedure SetFlag;
    function CheckFlagAndReset: Boolean;
    function CheckFlag: Boolean;
  end;

  ICounter = interface
    ['{D331EC1E-05DD-4231-8F56-FD1B11AA06D1}']
    function Inc: Integer;
    function Dec: Integer;
    function GetValue: Integer;
    function CheckEqual(AValue: Integer): Boolean;
    procedure Reset;
  end;

implementation

end.
