unit i_GotoLayerConfig;

interface

uses
  i_ConfigDataElement;

type
  IGotoLayerConfig = interface(IConfigDataElement)
    ['{95096F2B-D3BF-46CC-9826-1D1D973EEC1F}']
    function GetShowTickCount: Cardinal;
    procedure SetShowTickCount(AValue: Cardinal);
    property ShowTickCount: Cardinal read GetShowTickCount write SetShowTickCount;
 end;

implementation

end.
