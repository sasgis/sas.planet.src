unit i_ActiveMapsConfig;

interface

uses
  i_MapTypes,
  i_IConfigDataElement;

type
  IActiveMapSingle = interface(IConfigDataElement)
    ['{12F47503-E574-4F4F-A30C-7304D38410C7}']
    function GetMapType: IMapType;
    function GetIsActive: Boolean;
  end;

  IActiveMap = interface(IConfigDataElement)
    ['{6BAD8743-D50B-4342-9A68-DA5FBDDFDB04}']
    function GetSelectedGUID: TGUID;
    function GetMapSingle(AMapGUID: TGUID): IActiveMapSingle;
    function GetMapsList: IMapTypeList;
  end;

  IActiveMapsSet = interface(IConfigDataElement)
    ['{09F8FEE4-984C-4D1F-A240-BD8FF3333F85}']
    function IsGUIDSelected(AMapGUID: TGUID): Boolean;
    function GetMapSingle(AMapGUID: TGUID): IActiveMapSingle;
    function GetSelectedMapsList: IMapTypeList;
    function GetMapsList: IMapTypeList;
  end;

  IMainActiveMap = interface(IConfigDataElement)
    procedure SelectMainByGUID(AMapGUID: TGUID);
    function GetActiveMap: IActiveMap;
    function GetMapsSet: IActiveMapsSet;
  end;

  IActivMapWithLayers = interface(IMainActiveMap)
    ['{92B95280-7FD6-402A-8260-3FD83ED6BE36}']
    procedure SelectLayerByGUID(AMapGUID: TGUID);
    procedure UnSelectLayerByGUID(AMapGUID: TGUID);

    function GetLayers: IActiveMapsSet;
    function GetAllActiveMapsSet: IActiveMapsSet;
  end;

  IMainMapsConfig = interface(IActivMapWithLayers)
    ['{8A8A42A5-9252-4E85-812C-6A5EEEF98443}']
    function GetSelectedMapType: IMapType;
    function GetBitmapLayersSet: IActiveMapsSet;
    function GetKmlLayersSet: IActiveMapsSet;
  end;
  
implementation

end.
 