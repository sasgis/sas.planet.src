unit i_VectorItemSubsetChangeable;

interface

uses
  i_VectorItemSubset,
  i_Changeable;

type
  IVectorItemSubsetChangeable = interface(IChangeable)
    ['{7327418C-F37C-450E-8AF5-2F28998060B4}']
    function GetStatic: IVectorItemSubset;
  end;

implementation

end.
