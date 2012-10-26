unit u_BitmapLayerProviderWithBgColor;

interface

uses
  GR32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_BitmapLayerProvider;

type
  TBitmapLayerProviderWithBGColor = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FSourceProvider: IBitmapLayerProvider;
    FBackGroundColor: TColor32;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      ABackGroundColor: TColor32;
      const ASourceProvider: IBitmapLayerProvider
    );
  end;

implementation

uses
  Types,
  GR32_Resamplers,
  u_BitmapFunc,
  u_Bitmap32Static;

{ TBitmapLayerProviderWithBGColor }

constructor TBitmapLayerProviderWithBGColor.Create(
  ABackGroundColor: TColor32;
  const ASourceProvider: IBitmapLayerProvider
);
begin
  inherited Create;
  FSourceProvider := ASourceProvider;
  FBackGroundColor := ABackGroundColor;
  Assert(FSourceProvider <> nil);
end;

function TBitmapLayerProviderWithBGColor.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VTileSize: TPoint;
  VTargetBmp: TCustomBitmap32;
begin
  Result :=
    FSourceProvider.GetBitmapRect(
      AOperationID,
      ACancelNotifier,
      ALocalConverter
    );
  if Result <> nil then begin
    VTargetBmp := TCustomBitmap32.Create;
    try
      VTileSize := ALocalConverter.GetLocalRectSize;
      VTargetBmp.SetSize(VTileSize.X, VTileSize.Y);
      VTargetBmp.Clear(FBackGroundColor);
      BlockTransferFull(
        VTargetBmp,
        0,
        0,
        Result,
        dmBlend
      );
      Result := TBitmap32Static.CreateWithOwn(VTargetBmp);
      VTargetBmp := nil;
    finally
      VTargetBmp.Free;
    end;
  end;
end;

end.
