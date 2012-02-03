unit u_BitmapLayerProviderInPolygon;

interface

uses
  GR32,
  i_OperationNotifier,
  i_LocalCoordConverter,
  i_VectorItemProjected,
  i_BitmapLayerProvider;

type
  TBitmapLayerProviderInPolygon = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FSourceProvider: IBitmapLayerProvider;
    FPolyProjected: IProjectedPolygon;
    FLine: IProjectedPolygonLine;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
  public
    constructor Create(
      APolyProjected: IProjectedPolygon;
      ASourceProvider: IBitmapLayerProvider
    );
  end;

implementation

{ TBitmapLayerProviderInPolygon }

constructor TBitmapLayerProviderInPolygon.Create(
  APolyProjected: IProjectedPolygon;
  ASourceProvider: IBitmapLayerProvider
);
begin
  FSourceProvider := ASourceProvider;
  FPolyProjected := APolyProjected;
  Assert(FSourceProvider <> nil);
  Assert(FPolyProjected <> nil);
  Assert(FPolyProjected.Count > 0);
  FLine := FPolyProjected.Item[0];
end;

function TBitmapLayerProviderInPolygon.GetBitmapRect(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter
): Boolean;
begin
  if FLine.IsRectIntersectPolygon(ALocalConverter.GetRectInMapPixelFloat) then begin
    Result :=
      FSourceProvider.GetBitmapRect(
        AOperationID,
        ACancelNotifier,
        ATargetBmp,
        ALocalConverter
      );
  end else begin
    Result := False;
  end;
end;

end.
