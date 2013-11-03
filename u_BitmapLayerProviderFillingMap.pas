unit u_BitmapLayerProviderFillingMap;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_MapTypes,
  i_MapVersionInfo,
  i_BitmapLayerProvider,
  i_FillingMapColorer,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderFillingMap = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FMapType: IMapType;
    FVersion: IMapVersionInfo;
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
    FColorer: IFillingMapColorer;

    function GetActualZoom(
      const ALocalConverter: ILocalCoordConverter
    ): Byte;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const AMapType: IMapType;
      const AVersion: IMapVersionInfo;
      AUseRelativeZoom: Boolean;
      AZoom: Integer;
      const AColorer: IFillingMapColorer
    );
  end;

implementation

{ TBitmapLayerProviderFillingMap }

constructor TBitmapLayerProviderFillingMap.Create(
  const AMapType: IMapType;
  const AVersion: IMapVersionInfo;
  AUseRelativeZoom: Boolean;
  AZoom: Integer;
  const AColorer: IFillingMapColorer
);
begin
  inherited Create;
  FMapType := AMapType;
  FVersion := AVersion;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FColorer := AColorer;
  Assert(FMapType <> nil);
  Assert(FMapType.MapType <> nil);
  Assert(FColorer <> nil);
end;

function TBitmapLayerProviderFillingMap.GetActualZoom(
  const ALocalConverter: ILocalCoordConverter
): Byte;
var
  VZoom: Integer;
begin
  VZoom := FZoom;
  if FUseRelativeZoom then begin
    VZoom := VZoom + ALocalConverter.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    ALocalConverter.GetGeoConverter.CheckZoom(Result);
  end;
end;

function TBitmapLayerProviderFillingMap.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VSourceZoom: Byte;
begin
  VSourceZoom := GetActualZoom(ALocalConverter);
  if ALocalConverter.Zoom > VSourceZoom then begin
    Result := nil;
  end else begin
    Result :=
      FMapType.MapType.GetFillingMapBitmap(
        AOperationID,
        ACancelNotifier,
        ALocalConverter,
        VSourceZoom,
        FVersion,
        FColorer
      );
  end;
end;

end.
