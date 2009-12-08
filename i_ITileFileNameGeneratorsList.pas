unit i_ITileFileNameGeneratorsList;

interface

uses
  i_ITileFileNameGenerator;

type
  ITileFileNameGeneratorsList = interface
    function GetGenerator(CacheType: Byte): ITileFileNameGenerator;
  end;
implementation

end.
