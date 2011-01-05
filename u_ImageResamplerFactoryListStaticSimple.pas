unit u_ImageResamplerFactoryListStaticSimple;

interface

uses
  u_ImageResamplerFactoryListStatic;

type
  TImageResamplerFactoryListStaticSimple = class(TImageResamplerFactoryListStatic)
  public
    constructor Create;
  end;

implementation

uses
  GR32_Resamplers,
  u_ImageResamplerFactory;

{ TImageResamplerFactoryListStaticSimple }

constructor TImageResamplerFactoryListStaticSimple.Create;
begin
  inherited;
  Add(TImageResamplerFactoryNil.Create, 'Neares');
  Add(TImageResamplerFactoryLinear.Create, 'Linear');
  Add(TImageResamplerFactoryKernel.Create(nil), 'Box');
  Add(TImageResamplerFactoryKernel.Create(TCosineKernel), 'Cosine');
  Add(TImageResamplerFactoryKernel.Create(TSplineKernel), 'Spline');
  Add(TImageResamplerFactoryKernel.Create(TCubicKernel), 'Cubic');
  Add(TImageResamplerFactoryKernel.Create(TMitchellKernel), 'Mitchell');
  Add(TImageResamplerFactoryKernel.Create(TAlbrechtKernel), 'Albrecht');
  Add(TImageResamplerFactoryKernel.Create(TLanczosKernel), 'Lanczos');
  Add(TImageResamplerFactoryKernel.Create(TGaussianKernel), 'Gaussian');
  Add(TImageResamplerFactoryKernel.Create(TBlackmanKernel), 'Blackman');
  Add(TImageResamplerFactoryKernel.Create(THannKernel), 'Hann');
  Add(TImageResamplerFactoryKernel.Create(THammingKernel), 'Hamming');
  Add(TImageResamplerFactoryKernel.Create(TSinshKernel), 'Sinsh');
  Add(TImageResamplerFactoryKernel.Create(THermiteKernel), 'Hermite');
end;

end.
