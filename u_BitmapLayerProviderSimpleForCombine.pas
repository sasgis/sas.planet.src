unit u_BitmapLayerProviderSimpleForCombine;

interface

uses
  GR32,
  i_OperationNotifier,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  i_BitmapPostProcessingConfig;

type
  TBitmapLayerProviderSimpleForCombine = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FRecolorConfig: IBitmapPostProcessingConfigStatic;
    FSourceProvider: IBitmapLayerProvider;
    FMarksImageProvider: IBitmapLayerProvider;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ARecolorConfig: IBitmapPostProcessingConfigStatic;
      const ASourceProvider: IBitmapLayerProvider;
      const AMarksImageProvider: IBitmapLayerProvider
    );
  end;

implementation

uses
  GR32_Resamplers,
  u_Bitmap32Static;

{ TBitmapLayerProviderSimpleForCombine }

constructor TBitmapLayerProviderSimpleForCombine.Create(
  const ARecolorConfig: IBitmapPostProcessingConfigStatic;
  const ASourceProvider: IBitmapLayerProvider;
  const AMarksImageProvider: IBitmapLayerProvider
);
begin
  inherited Create;
  FSourceProvider := ASourceProvider;
  FMarksImageProvider := AMarksImageProvider;
  FRecolorConfig := ARecolorConfig;
end;

function TBitmapLayerProviderSimpleForCombine.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VLayer: IBitmap32Static;
  VBitmap: TCustomBitmap32;
begin
  Result := FSourceProvider.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
  if Result <> nil then begin
    if FRecolorConfig <> nil then begin
      Result := FRecolorConfig.Process(Result);
    end;
  end;
  if FMarksImageProvider <> nil then begin
    VLayer := FMarksImageProvider.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
  end;
  if Result <> nil then begin
    if VLayer <> nil then begin
      VBitmap := TCustomBitmap32.Create;
      try
        VBitmap.Assign(Result.Bitmap);
        BlockTransfer(
          VBitmap,
          0, 0,
          VBitmap.ClipRect,
          VLayer.Bitmap,
          VLayer.Bitmap.BoundsRect,
          dmBlend
        );
        Result := TBitmap32Static.CreateWithOwn(VBitmap);
        VBitmap := nil;
      finally
        VBitmap.Free;
      end;
    end;
  end else begin
    Result := VLayer;
  end;
end;

end.
