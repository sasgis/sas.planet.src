unit i_TileStorageTypeList;

interface

uses
  ActiveX,
  i_ConfigDataElement,
  i_TileStorageTypeConfig,
  i_TileStorageType;

type
  ITileStorageTypeList = interface(IConfigDataElement)
    ['{42BD0720-3B8A-4F19-8208-C6E4105377DE}']
    function GetDefault: ITileStorageType;
    procedure SetDefaultByGUID(AGUID: TGUID);

    function Get(AGUID: TGUID): ITileStorageType;
    function GetCanUseAsDefault(AGUID: TGUID): Boolean;
    function GetEnum: IEnumGUID;
  end;

implementation

end.
