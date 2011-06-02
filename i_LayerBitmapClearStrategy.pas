unit i_LayerBitmapClearStrategy;

interface

uses
  GR32,
  i_LocalCoordConverter;

type
  ILayerBitmapClearStrategy =  interface
    ['{F2E51CCC-D584-4D88-98E7-0057F3825F63}']
    procedure Clear(ABitmap: TCustomBitmap32);
  end;

  ILayerBitmapClearStrategyFactory = interface
    ['{9F14B47C-2D9C-4974-B78E-E3E3E6B74725}']
    function GetStrategy(
      ASourceConverter, ATargetConverter: ILocalCoordConverter;
      ASourceBitmap: TCustomBitmap32;
      APrevStrategy: ILayerBitmapClearStrategy
    ): ILayerBitmapClearStrategy;
  end;

implementation

end.
