unit u_BitmapLayerProviderCombineTwoProviders;

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
  TBitmapLayerProviderCombineTwoProviders = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FProvider1: IBitmapLayerProvider;
    FProvider2: IBitmapLayerProvider;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      const AProvider1: IBitmapLayerProvider;
      const AProvider2: IBitmapLayerProvider
    );
  end;

implementation

uses
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderMapWithLayer }

constructor TBitmapLayerProviderCombineTwoProviders.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  const AProvider1: IBitmapLayerProvider;
  const AProvider2: IBitmapLayerProvider
);
begin
  Assert(Assigned(ABitmapFactory));
  Assert(Assigned(AProvider1));
  Assert(Assigned(AProvider2));
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FProvider1 := AProvider1;
  FProvider2 := AProvider2;
end;

function TBitmapLayerProviderCombineTwoProviders.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VLayer: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := nil;
  VLayer := nil;
  Result :=
    FProvider1.GetBitmapRect(
      AOperationID,
      ACancelNotifier,
      ALocalConverter
    );
  VLayer :=
    FProvider2.GetBitmapRect(
      AOperationID,
      ACancelNotifier,
      ALocalConverter
    );

  if Result <> nil then begin
    if VLayer <> nil then begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
      try
        AssignStaticToBitmap32(VBitmap, Result);
        BlockTransferFull(
          VBitmap,
          0, 0,
          VLayer,
          dmBlend
        );
        Result := VBitmap.BitmapStatic;
      finally
        VBitmap.Free;
      end;
    end;
  end else begin
    Result := VLayer;
  end;
end;

end.
