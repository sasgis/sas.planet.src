unit i_TileStorageInfo;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_TileStorageTypeInfo;

type
  ITileStorageInfo = interface
    function GetTypeInfo: ITileStorageTypeInfo;
    function GetMainContentType: IContentTypeInfoBasic;
    function GetAllowDifferentContentTypes: Boolean;

    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetIsReadOnly: boolean;
    function GetCoordConverter: ICoordConverter;
  end;

implementation

end.
