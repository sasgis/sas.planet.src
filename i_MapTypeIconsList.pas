unit i_MapTypeIconsList;

interface

uses
  ActiveX,
  ImgList;

type
  IMapTypeIconsList = interface
    ['{A0F83B1A-1B09-46F2-BA4E-9F97A6F95ABE}']
    function GetImageList: TCustomImageList;
    function GetIconIndexByGUID(AGUID: TGUID): Integer;
    function GetIterator: IEnumGUID;
  end;

implementation

end.
