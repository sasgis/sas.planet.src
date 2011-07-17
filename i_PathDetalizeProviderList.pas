unit i_PathDetalizeProviderList;

interface

uses
  ActiveX,
  i_JclNotify,
  i_ConfigDataElement,
  i_PathDetalizeProvider;

type
  IPathDetalizeProviderListEntity = interface(IConfigDataElement)
    ['{343F27D6-4DDE-46D8-8F58-4BA220C1733E}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetCaption: string;
    property Caption: string read GetCaption;

    function GetDescription: string;
    property Description: string read GetDescription;

    function GetMenuItemName: string;
    property MenuItemName: string read GetMenuItemName;

    function GetProvider: IPathDetalizeProvider;
  end;

  IPathDetalizeProviderList = interface(IConfigDataElement)
    ['{73A94DEE-3216-402E-9A22-90E84A215CEC}']
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): IPathDetalizeProviderListEntity;
  end;


implementation

end.
