unit i_MapVersionFactoryList;

interface

uses
  i_MapVersionFactory;

type
  IMapVersionFactoryList = interface
    ['{6C02F059-8D6F-4561-AF73-678321CFBCBE}']
    function GetSimpleVersionFactory: IMapVersionFactory;
    function GetGEVersionFactory: IMapVersionFactory;
  end;

implementation

end.