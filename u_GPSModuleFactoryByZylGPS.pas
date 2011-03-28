unit u_GPSModuleFactoryByZylGPS;

interface

uses
  i_GPSModuleByCOM,
  i_GPSModuleByCOMFactory;

type
  TGPSModuleFactoryByZylGPS = class(TInterfacedObject, IGPSModuleByCOMFactory)
  protected
    function CreateGPSModule: IGPSModuleByCOM;
  end;

implementation

uses
  u_GPSModuleByZylGPS;

{ TGPSModuleFactoryByZylGPS }

function TGPSModuleFactoryByZylGPS.CreateGPSModule: IGPSModuleByCOM;
begin
  Result := TGPSModuleByZylGPS.Create;
end;

end.
