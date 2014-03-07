unit i_LocalCoordConverterFactory;

interface

uses
  Types,
  t_GeoTypes,
  i_ProjectionInfo,
  i_LocalCoordConverter;

type
  ILocalCoordConverterFactory = interface
    ['{7CD24832-CA4B-4C8D-AE33-C7B647695629}']
    function CreateNoScaleIntDelta(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TPoint
    ): ILocalCoordConverter;
    function CreateNoScale(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
    function CreateScaled(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapScale: Double;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
  end;

implementation

end.
