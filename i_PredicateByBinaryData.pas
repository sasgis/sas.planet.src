unit i_PredicateByBinaryData;

interface

uses
  i_BinaryData;

type
  IPredicateByBinaryData = interface
    ['{4D6A0041-AF42-4065-A096-0BFAB1833C69}']
    function Check(const AData: IBinaryData): Boolean;
  end;

implementation

end.
