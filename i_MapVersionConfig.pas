unit i_MapVersionConfig;

interface

uses
  i_ConfigDataElement,
  i_MapVersionInfo;
  
type
  IMapVersionConfig = interface(IConfigDataElement)
    ['{0D710534-C49F-43BC-8092-A0F5ABB5E107}']
    function GetVersion: Variant;
    procedure SetVersion(const AValue: Variant);
    property Version: Variant read GetVersion write SetVersion;

    function GetStatic: IMapVersionInfo;
  end;

implementation

end.
