unit u_BitmapLayerProviderWithBGColor;

interface

uses
  GR32,
  i_OperationNotifier,
  i_LocalCoordConverter,
  i_BitmapLayerProvider;

type
  TBitmapLayerProviderWithBGColor = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FSourceProvider: IBitmapLayerProvider;
    FBackGroundColor: TColor32;
    FTempBitmap: TCustomBitmap32;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
  public
    constructor Create(
      ABackGroundColor: TColor32;
      ASourceProvider: IBitmapLayerProvider
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers;

{ TBitmapLayerProviderWithBGColor }

constructor TBitmapLayerProviderWithBGColor.Create(ABackGroundColor: TColor32;
  ASourceProvider: IBitmapLayerProvider);
begin
  FSourceProvider := ASourceProvider;
  FBackGroundColor := ABackGroundColor;
  Assert(FSourceProvider <> nil);

  FTempBitmap := TCustomBitmap32.Create;
end;

destructor TBitmapLayerProviderWithBGColor.Destroy;
begin
  FreeAndNil(FTempBitmap);
  inherited;
end;

function TBitmapLayerProviderWithBGColor.GetBitmapRect(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VTileSize: TPoint;
begin
  VTileSize := ALocalConverter.GetLocalRectSize;
  ATargetBmp.SetSize(VTileSize.X, VTileSize.Y);
  ATargetBmp.Clear(FBackGroundColor);
  Result :=
    FSourceProvider.GetBitmapRect(
      AOperationID,
      ACancelNotifier,
      FTempBitmap,
      ALocalConverter
    );
  if Result then begin
    BlockTransfer(
      ATargetBmp,
      0,
      0,
      ATargetBmp.ClipRect,
      FTempBitmap,
      FTempBitmap.BoundsRect,
      dmBlend
    );
  end;
end;

end.
