unit i_PredicateByTileInfo;

interface

uses
  Types,
  i_TileInfoBasic;

type
  IPredicateByTileInfo = interface
    ['{9A3F53A1-7EBB-47D4-B081-064A093EBA0D}']
    function Check(const ATileInfo: ITileInfoBasic; AZoom: Byte; const ATile: TPoint): Boolean; overload;
    function Check(const ATileInfo: TTileInfo): Boolean; overload;
  end;

implementation

end.
