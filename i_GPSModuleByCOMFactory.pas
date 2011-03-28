unit i_GPSModuleByCOMFactory;

interface


uses
  i_GPSModuleByCOM;

type
  IGPSModuleByCOMFactory = interface
    ['{BB5959FC-DC79-4CDD-AB35-E6BDB417966F}']
    function CreateGPSModule: IGPSModuleByCOM;
  end;

implementation

end.
