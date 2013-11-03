unit i_TileMatrixChangeable;

interface

uses
  i_Changeable,
  i_TileMatrix;

type
  ITileMatrixChangeable = interface(IChangeable)
    ['{FF7F2366-ABF9-4E23-884B-BFDE797EE82E}']
    function GetStatic: ITileMatrix;
  end;

implementation

end.
