unit i_ITileStorageTypeConfig;

interface

uses
  i_ConfigDataElement;

type
  ITileStorageTypeConfig = interface(IConfigDataElement)
    ['{A955F0F6-D1AF-467E-A389-052F65572E45}']
    function GetBasePath: string;
    procedure SetBasePath(AValue: string);
    property BasePath: string read GetBasePath write SetBasePath;
  end;

implementation

end.
