unit i_InternalDomainInfoProvider;

interface

uses
  Classes;

type
  IInternalDomainInfoProvider = interface
    ['{CD84B08E-E84B-4688-9D9A-A9A34F29139D}']
    function LoadStreamByFilePath(AFilePath: string; AStream: TStream; out AContentType: string): Boolean;
  end;

  IInternalDomainInfoProviderList = interface
    ['{C2FE2C8D-C9F3-48F7-AB3B-37119722D118}']
    function GetByName(AName: string): IInternalDomainInfoProvider;
  end;

implementation

end.
