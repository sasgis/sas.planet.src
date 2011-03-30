unit i_SensorViewListGenerator;

interface

uses
  i_GUIDList;

type
  ISensorViewListGenerator = interface
    ['{886AABDC-90D7-4F6F-BCBF-E7AFBABA545B}']
    function CreateSensorViewList(ASensorList: IGUIDInterfaceList): IGUIDInterfaceList;
  end;

implementation

end.
