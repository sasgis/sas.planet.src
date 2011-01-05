unit i_IImageResamplerConfig;

interface

uses
  i_IConfigDataElement,
  i_IImageResamplerFactory;

type
  IImageResamplerConfig = interface(IConfigDataElement)
    ['{6E7E207D-F684-41E4-93C9-EBEE584F4510}']
    function GetList: IImageResamplerFactoryList;

    function GetActiveIndex: Integer;
    procedure SetActiveIndex(AValue: Integer);
    property ActiveIndex: Integer read GetActiveIndex write SetActiveIndex;

    function GetActiveFactory: IImageResamplerFactory;
  end;

implementation

end.
