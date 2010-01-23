unit i_ISimpleFactory;

interface

type
  ISimpleFactory = interface
    ['{AD906C40-4D3F-4CEC-82EE-CCEDF23AF9DE}']
    function CreateInstance: IUnknown;
  end;

implementation

end.
