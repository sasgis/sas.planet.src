unit i_ActiveMapsConfig;

interface

uses
  i_MapTypes,
  i_ConfigDataElement;

type
  IActiveMapSingle = interface(IConfigDataElement)
    ['{12F47503-E574-4F4F-A30C-7304D38410C7}']
    function GetMapType: IMapType;
    function GetIsActive: Boolean;
  end;

  IActiveMap = interface(IConfigDataElement)
    ['{6BAD8743-D50B-4342-9A68-DA5FBDDFDB04}']
    function GetSelectedGUID: TGUID;
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
    function GetMapsSet: IMapTypeSet;
  end;

  IActiveMapsSet = interface(IConfigDataElement)
    ['{09F8FEE4-984C-4D1F-A240-BD8FF3333F85}']
    function IsGUIDSelected(const AMapGUID: TGUID): Boolean;
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
    function GetSelectedMapsSet: IMapTypeSet;
    function GetMapsSet: IMapTypeSet;
  end;

  IMainActiveMap = interface(IConfigDataElement)
    procedure SelectMainByGUID(const AMapGUID: TGUID);
    function GetActiveMap: IActiveMap;
    function GetActiveMapsSet: IActiveMapsSet;
  end;

  IActivMapWithLayers = interface(IMainActiveMap)
    ['{92B95280-7FD6-402A-8260-3FD83ED6BE36}']
    procedure SelectLayerByGUID(const AMapGUID: TGUID);
    procedure UnSelectLayerByGUID(const AMapGUID: TGUID);

    function GetActiveLayersSet: IActiveMapsSet;
    function GetAllActiveMapsSet: IActiveMapsSet;
  end;

  IMainMapsConfig = interface(IActivMapWithLayers)
    ['{8A8A42A5-9252-4E85-812C-6A5EEEF98443}']
    function GetSelectedMapType: IMapType;
    function GetActiveBitmapLayersSet: IActiveMapsSet;
    function GetActiveKmlLayersSet: IActiveMapsSet;
  end;
  
implementation

end.
 