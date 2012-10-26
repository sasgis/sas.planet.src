unit u_BitmapLayerProviderComplex;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_BitmapLayerProvider;

type
  TBitmapLayerProviderComplex = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FProviderFrist: IBitmapLayerProvider;
    FProviderSecond: IBitmapLayerProvider;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const AProviderFrist: IBitmapLayerProvider;
      const AProviderSecond: IBitmapLayerProvider
    );
  end;

implementation

uses
  Types,
  GR32,
  GR32_Resamplers,
  u_BitmapFunc,
  u_Bitmap32Static;

{ TBitmapLayerProviderComplex }

constructor TBitmapLayerProviderComplex.Create(const AProviderFrist,
  AProviderSecond: IBitmapLayerProvider);
begin
  Assert(AProviderFrist <> nil);
  Assert(AProviderSecond <> nil);
  inherited Create;
  FProviderFrist := AProviderFrist;
  FProviderSecond := AProviderSecond;
end;

function TBitmapLayerProviderComplex.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VResultFirst: IBitmap32Static;
  VResultSecond: IBitmap32Static;
  VBitmap: TCustomBitmap32;
begin
  VResultFirst := FProviderFrist.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
  VResultSecond := FProviderSecond.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
  if VResultFirst = nil then begin
    Result := VResultSecond;
  end else begin
    if VResultSecond = nil then begin
      Result := VResultFirst;
    end else begin
      VBitmap := TCustomBitmap32.Create;
      try
        AssignStaticToBitmap32(VBitmap, VResultFirst);
        BlockTransferFull(
          VBitmap,
          0, 0,
          VResultSecond,
          dmBlend,
          cmMerge
        );
        Result := TBitmap32Static.CreateWithOwn(VBitmap);
        VBitmap := nil;
      finally
        VBitmap.Free;
      end;
    end;
  end;
end;

end.
