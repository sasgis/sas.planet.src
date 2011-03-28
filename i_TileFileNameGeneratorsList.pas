unit i_TileFileNameGeneratorsList;

interface

uses
  i_TileFileNameGenerator;

type
  ITileFileNameGeneratorsList = interface
    ['{790C3C21-648B-43B5-ABD4-DDC0A3D910B7}']
    function GetGenerator(CacheType: Byte): ITileFileNameGenerator;
  end;

implementation

end.
