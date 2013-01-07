unit u_BitmapLayerProviderWithBgColor;

interface

uses
  GR32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderWithBGColor = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmapFactory: IBitmap32StaticFactory;
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
      const ABitmapFactory: IBitmap32StaticFactory;
      const ASourceProvider: IBitmapLayerProvider
    );
  end;

implementation

uses
  Types,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderWithBGColor }

constructor TBitmapLayerProviderWithBGColor.Create(
  ABackGroundColor: TColor32;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ASourceProvider: IBitmapLayerProvider
);
begin
  inherited Create;
  FSourceProvider := ASourceProvider;
  FBackGroundColor := ABackGroundColor;
  FBitmapFactory := ABitmapFactory;
  Assert(FSourceProvider <> nil);
end;

function TBitmapLayerProviderWithBGColor.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VTileSize: TPoint;
  VTargetBmp: TBitmap32ByStaticBitmap;
begin
  Result :=
    FSourceProvider.GetBitmapRect(
      AOperationID,
      ACancelNotifier,
      ALocalConverter
    );
  if Result <> nil then begin
    VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
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
      Result := VTargetBmp.BitmapStatic;
    finally
      VTargetBmp.Free;
    end;
  end;
end;

end.
