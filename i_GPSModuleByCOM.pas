unit i_GPSModuleByCOM;

interface

uses
  i_GPSModuleByCOMPortSettings,
  i_GPSModule;

type
  IGPSModuleByCOM = interface(IGPSModule)
    ['{EFB18F84-3019-44D2-9525-A12B3D97B14B}']
    procedure Connect(AConfig: IGPSModuleByCOMPortSettings); safecall;
    procedure Disconnect; safecall;

    function GetIsReadyToConnect: Boolean; safecall;
    property IsReadyToConnect: Boolean read GetIsReadyToConnect;
  end;

implementation

end.
