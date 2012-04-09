unit u_BitmapLayerProviderFillingMap;

interface

uses
  Types,
  i_OperationNotifier,
  i_CoordConverter,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_MapTypes,
  i_BitmapLayerProvider,
  i_FillingMapColorer,
  i_TileError,
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
      ACancelNotifier: IOperationNotifier;
      ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      AMapType: IMapType;
      ASourceZoom: Byte;
      AColorer: IFillingMapColorer
    );
  end;

implementation

{ TBitmapLayerProviderFillingMap }

constructor TBitmapLayerProviderFillingMap.Create(
  AMapType: IMapType;
  ASourceZoom: Byte;
  AColorer: IFillingMapColorer
);
begin
  FMapType := AMapType;
  FSourceZoom := ASourceZoom;
  FColorer := AColorer;
  Assert(FMapType <> nil);
  Assert(FMapType.MapType <> nil);
  Assert(FColorer <> nil);
end;

function TBitmapLayerProviderFillingMap.GetBitmapRect(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  ALocalConverter: ILocalCoordConverter
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
      )
  end;
end;

end.
