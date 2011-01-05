unit i_IImageResamplerFactory;

interface

uses
  GR32;

type
  IImageResamplerFactory = interface
    ['{4829EE36-667A-4A25-8CE0-1DAFDDC9B3D9}']
    function CreateResampler: TCustomResampler;
  end;

  IImageResamplerFactoryList = interface
    ['{CC888F5D-5DDA-427F-8127-93B0F1BD8CA5}']
    function Count: Integer;

    function Get(AIndex: Integer): IImageResamplerFactory;
    property Items[Index: Integer]: IImageResamplerFactory read Get; default;

    function GetCaption(AIndex: Integer): string;
    property Captions[Index: Integer]: string read GetCaption;
  end;

implementation

end.
