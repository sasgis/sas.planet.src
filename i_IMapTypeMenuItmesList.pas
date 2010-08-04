unit i_IMapTypeMenuItmesList;

interface

uses
  ActiveX,
  i_IMapTypeMenuItem;

type
  IMapTypeMenuItmesList = interface
    ['{325D1BDB-BC26-4926-BA9D-8CDB2E7B8C16}']
    function GetMapTypeItemByGUID(AGUID: TGUID): IMapTypeMenuItem;
    function GetIterator: IEnumGUID;
  end;

implementation

end.
 