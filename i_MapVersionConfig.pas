unit i_MapVersionConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapVersionConfigStatic = interface
    ['{CC157D46-11DA-4035-963B-2F0BEAEA265A}']
    function GetVersion: Variant;
    property Version: Variant read GetVersion;
  end;

  IMapVersionConfig = interface(IConfigDataElement)
    ['{0D710534-C49F-43BC-8092-A0F5ABB5E107}']
    function GetVersion: Variant;
    procedure SetVersion(const AValue: Variant);
    property Version: Variant read GetVersion write SetVersion;

    function GetStatic: IMapVersionConfigStatic;
  end;

implementation

end.
