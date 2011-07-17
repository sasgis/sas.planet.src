unit i_SensorList;

interface

uses
  ActiveX,
  i_ConfigDataElement,
  i_Sensor;

type
  ISensorListEntity = interface(IConfigDataElement)
    ['{26BB13C7-C30E-472D-874E-997122427990}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetCaption: string;
    property Caption: string read GetCaption;

    function GetDescription: string;
    property Description: string read GetDescription;

    function GetMenuItemName: string;
    property MenuItemName: string read GetMenuItemName;

    function GetSensorTypeIID: TGUID;
    property SensorTypeIID: TGUID read GetSensorTypeIID;

    function GetSensor: ISensor;
  end;

  ISensorList = interface(IConfigDataElement)
    ['{69F7AA17-D6B4-4F49-891E-72AEA4DC053F}']
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): ISensorListEntity;
  end;

implementation

end.
