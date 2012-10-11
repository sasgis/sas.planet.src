unit i_PredicateByTileRequest;

interface

uses
  i_TileRequest;

type
  IPredicateByTileRequest = interface
    ['{A1D2D694-2115-41C8-AA29-B950AEA7C1D6}']
    function Check(const ATileRequest: ITileRequest): Boolean;
  end;

implementation

end.
