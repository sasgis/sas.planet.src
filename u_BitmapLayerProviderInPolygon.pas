unit u_BitmapLayerProviderInPolygon;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_VectorItemProjected,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderInPolygon = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FSourceProvider: IBitmapLayerProvider;
    FPolyProjected: IProjectedPolygon;
    FLine: IProjectedPolygonLine;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const APolyProjected: IProjectedPolygon;
      const ASourceProvider: IBitmapLayerProvider
    );
  end;

implementation

{ TBitmapLayerProviderInPolygon }

constructor TBitmapLayerProviderInPolygon.Create(
  const APolyProjected: IProjectedPolygon;
  const ASourceProvider: IBitmapLayerProvider
);
begin
  inherited Create;
  FSourceProvider := ASourceProvider;
  FPolyProjected := APolyProjected;
  Assert(FSourceProvider <> nil);
  Assert(FPolyProjected <> nil);
  Assert(FPolyProjected.Count > 0);
  FLine := FPolyProjected.Item[0];
end;

function TBitmapLayerProviderInPolygon.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
begin
  if FLine.IsRectIntersectPolygon(ALocalConverter.GetRectInMapPixelFloat) then begin
    Result :=
      FSourceProvider.GetBitmapRect(
        AOperationID,
        ACancelNotifier,
        ALocalConverter
      );
  end else begin
    Result := nil;
  end;
end;

end.
