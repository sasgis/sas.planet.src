unit i_MapTypeSetChangeable;

interface

uses
  i_Changeable,
  i_MapTypeSet;

type
  IMapTypeSetChangeable = interface(IChangeable)
    ['{F6548515-4FB4-45F1-A742-B886BBCB1024}']
    function GetStatic: IMapTypeSet;
  end;

implementation

end.
