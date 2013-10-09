unit i_MapVersionFactory;

interface

uses
  i_MapVersionInfo;

type
  IMapVersionFactory = interface
    ['{4E03F54E-C11D-443C-BF0E-D9A2B0D1299C}']
    function CreateByStoreString(const AValue: string; const AShowPrevVersion: Boolean = False): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo; const AShowPrevVersion: Boolean = False): IMapVersionInfo;

    function IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
  end;

implementation

end.
