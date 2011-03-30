unit i_SensorListGenerator;

interface

uses
  i_GUIDList;

type
  ISensorListGenerator = interface
    ['{28A94C0B-4831-4CD1-9467-424F41F7F28B}']
    function CreateSensorsList: IGUIDInterfaceList;
  end;

implementation

end.
