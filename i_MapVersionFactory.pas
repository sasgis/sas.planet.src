unit i_MapVersionFactory;

interface

uses
  i_Changeable,
  i_MapVersionInfo;

type
  IMapVersionFactory = interface
    ['{4E03F54E-C11D-443C-BF0E-D9A2B0D1299C}']
    function CreateByStoreString(const AValue: string): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo): IMapVersionInfo;

    function IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
  end;

  IMapVersionFactoryChangeable = interface(IChangeable)
    ['{098EFB27-7733-41EC-88E5-9101B9729FB2}']
    function GetStatic: IMapVersionFactory;
  end;

  IMapVersionFactoryChangeableInternal = interface(IMapVersionFactoryChangeable)
    procedure SetFactory(const AValue: IMapVersionFactory);
  end;

implementation

end.
