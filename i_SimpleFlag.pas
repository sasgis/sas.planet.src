unit i_SimpleFlag;

interface

type
  ISimpleFlag = interface
    ['{840AE4C5-D624-4EC9-9854-CD772F8414D9}']
    procedure SetFlag;
    function CheckFlagAndReset: Boolean;
  end;

implementation

end.
