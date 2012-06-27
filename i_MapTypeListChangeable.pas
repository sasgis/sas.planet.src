unit i_MapTypeListChangeable;

interface

uses
  i_Changeable,
  i_MapTypes;

type
  IMapTypeListChangeable = interface(IChangeable)
    ['{A4D1F26D-73B0-428F-990F-CD348980FE33}']
    function GetList: IMapTypeListStatic;
    property List: IMapTypeListStatic read GetList;
  end;

implementation

end.
