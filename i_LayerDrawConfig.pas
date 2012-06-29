unit i_LayerDrawConfig;

interface

uses
  i_ConfigDataElement;

type
  ILayerDrawConfig = interface(IConfigDataElement)
    ['{A2A25FFE-CD8D-4728-9D31-F8417D6564DE}']
    function GetLayerZOrder: Integer;
    procedure SetLayerZOrder(AValue: Integer);
    property LayerZOrder: Integer read GetLayerZOrder write SetLayerZOrder;
  end;

implementation

end.
