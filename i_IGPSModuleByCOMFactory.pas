unit i_IGPSModuleByCOMFactory;

interface


uses
  i_IGPSModuleByCOMPortSettings,
  i_IGPSModuleByCOM;

type
  IGPSModuleByCOMFactory = interface
    ['{BB5959FC-DC79-4CDD-AB35-E6BDB417966F}']
    function CreateGPSModule: IGPSModuleByCOM;
  end;

implementation

end.
