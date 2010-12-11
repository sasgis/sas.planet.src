unit i_IPathDetalizeProvider;

interface

uses
  t_GeoTypes;

type
  IPathDetalizeProvider = interface
    ['{93696D0E-A464-4136-8CCE-E70BF48CA918}']
    function GetPath(ASource: TDoublePointArray; var AComment: string): TDoublePointArray;
  end;

implementation

end.
