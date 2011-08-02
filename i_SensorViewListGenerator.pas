unit i_SensorViewListGenerator;

interface

uses
  Classes,
  i_GUIDList,
  i_SensorList;

type
  ISensorViewListGenerator = interface
    ['{886AABDC-90D7-4F6F-BCBF-E7AFBABA545B}']
    function CreateSensorViewList(ASensorList: ISensorList): IGUIDInterfaceList;
  end;

implementation

end.
