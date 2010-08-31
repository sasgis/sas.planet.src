unit i_MapTypeIconsList;

interface

uses
  ActiveX,
  ImgList;

type
  IMapTypeIconsListItem = interface
    ['{2B83FF06-3D47-4346-AA8A-BD542A339A15}']
    function GetIconIndex: Integer;
  end;

  IMapTypeIconsList = interface
    ['{A0F83B1A-1B09-46F2-BA4E-9F97A6F95ABE}']
    function GetImageList: TCustomImageList;
    function GetMapTypeByGUID(AGUID: TGUID): IMapTypeIconsListItem;
    function GetIterator: IEnumGUID;
  end;

implementation

end.
