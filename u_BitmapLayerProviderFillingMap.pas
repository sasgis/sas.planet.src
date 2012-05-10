unit u_BitmapLayerProviderFillingMap;

interface

uses
  Types,
  i_OperationNotifier,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_MapTypes,
  i_BitmapLayerProvider,
  i_FillingMapColorer,
  u_MapType;

type
  TBitmapLayerProviderFillingMap = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FMapType: IMapType;
    FSourceZoom: Byte;
    FColorer: IFillingMapColorer;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const AMapType: IMapType;
      ASourceZoom: Byte;
      const AColorer: IFillingMapColorer
    );
  end;

implementation

{ TBitmapLayerProviderFillingMap }

constructor TBitmapLayerProviderFillingMap.Create(
  const AMapType: IMapType;
  ASourceZoom: Byte;
  const AColorer: IFillingMapColorer
);
begin
  inherited Create;
  FMapType := AMapType;
  FSourceZoom := ASourceZoom;
  FColorer := AColorer;
  Assert(FMapType <> nil);
  Assert(FMapType.MapType <> nil);
  Assert(FColorer <> nil);
end;

function TBitmapLayerProviderFillingMap.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: IOperationNotifier;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
begin
  if ALocalConverter.Zoom > FSourceZoom then begin
    Result := nil;
  end else begin
    Result :=
      FMapType.MapType.GetFillingMapBitmap(
        AOperationID,
        ACancelNotifier,
        ALocalConverter,
        FSourceZoom,
        FColorer
      );
  end;
end;

end.
