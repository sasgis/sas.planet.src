unit i_IMainMemCacheConfig;

interface

uses
  i_IConfigDataElement;
  
type
  IMainMemCacheConfig = interface(IConfigDataElement)
    ['{E60F1967-FE2D-4C3C-81D1-D99B50A0F21F}']
    function GetMaxSize: Integer;
    procedure SetMaxSize(AValue: Integer);
    property MaxSize: Integer read GetMaxSize write SetMaxSize;
  end;

implementation

end.
