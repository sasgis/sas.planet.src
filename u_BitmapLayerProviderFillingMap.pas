unit u_BitmapLayerProviderFillingMap;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_TileStorage,
  i_MapVersionInfo,
  i_BitmapLayerProvider,
  i_FillingMapColorer,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderFillingMap = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FStorage: ITileStorage;
    FVersion: IMapVersionInfo;
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
    FColorer: IFillingMapColorer;

    function GetActualZoom(
      const ALocalConverter: ILocalCoordConverter
    ): Byte;
    function GetFillingMapBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter;
      ASourceZoom: byte;
      const AVersion: IMapVersionInfo;
      const AColorer: IFillingMapColorer
    ): IBitmap32Static;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      const AStorage: ITileStorage;
      const AVersion: IMapVersionInfo;
      AUseRelativeZoom: Boolean;
      AZoom: Integer;
      const AColorer: IFillingMapColorer
    );
  end;

implementation

uses
  GR32,
  t_GeoTypes,
  i_CoordConverter,
  i_TileIterator,
  i_TileInfoBasic,
  u_GeoFun,
  u_TileIteratorByRect,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderFillingMap }

constructor TBitmapLayerProviderFillingMap.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  const AStorage: ITileStorage;
  const AVersion: IMapVersionInfo;
  AUseRelativeZoom: Boolean;
  AZoom: Integer;
  const AColorer: IFillingMapColorer
);
begin
  Assert(Assigned(ABitmapFactory));
  Assert(Assigned(AStorage));
  Assert(Assigned(AVersion));
  Assert(Assigned(AColorer));
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FStorage := AStorage;
  FVersion := AVersion;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FColorer := AColorer;
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
      GetFillingMapBitmap(
        AOperationID,
        ACancelNotifier,
        ALocalConverter,
        VSourceZoom,
        FVersion,
        FColorer
      );
  end;
end;

function TBitmapLayerProviderFillingMap.GetFillingMapBitmap(
  AOperationID: Integer; const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter; ASourceZoom: byte;
  const AVersion: IMapVersionInfo;
  const AColorer: IFillingMapColorer): IBitmap32Static;
var
  VBitmap: TBitmap32ByStaticBitmap;
  VSize: TPoint;
  VTargetMapPixelRect: TDoubleRect;
  VSourceTileRect: TRect;
  VSourceRelativeRect: TDoubleRect;
  VSourceConverter: ICoordConverter;
  VTargetConverter: ICoordConverter;
  VSameSourceAndTarget: Boolean;
  VTargetZoom: Byte;
  VLonLatRect: TDoubleRect;
  VIterator: ITileIterator;
  VRelativeRectOfTile: TDoubleRect;
  VLonLatRectOfTile: TDoubleRect;
  VSolidDrow: Boolean;
  VTileRectInfo: ITileRectInfo;
  VEnumTileInfo: IEnumTileInfo;
  VTileInfo: TTileInfo;
  VMapPixelRectOfTile: TDoubleRect;
  VLocalPixelRectOfTile: TRect;
  VTileColor: TColor32;
begin
  VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
  try
    VSize := ALocalConverter.GetLocalRectSize;
    VBitmap.SetSize(VSize.X, VSize.Y);
    VBitmap.Clear(0);

    VSourceConverter := FStorage.CoordConverter;
    VTargetConverter := ALocalConverter.GeoConverter;
    VTargetZoom := ALocalConverter.Zoom;

    VTargetMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
    VTargetConverter.CheckPixelRectFloat(VTargetMapPixelRect, VTargetZoom);

    VSameSourceAndTarget := VSourceConverter.IsSameConverter(VTargetConverter);
    if VSameSourceAndTarget then begin
      VSourceRelativeRect := VSourceConverter.PixelRectFloat2RelativeRect(VTargetMapPixelRect, VTargetZoom);
    end else begin
      VLonLatRect := VTargetConverter.PixelRectFloat2LonLatRect(VTargetMapPixelRect, VTargetZoom);
      VSourceConverter.CheckLonLatRect(VLonLatRect);
      VSourceRelativeRect := VSourceConverter.LonLatRect2RelativeRect(VLonLatRect);
    end;
    VSourceTileRect :=
      RectFromDoubleRect(
        VSourceConverter.RelativeRect2TileRectFloat(VSourceRelativeRect, ASourceZoom),
        rrOutside
      );
    VSolidDrow :=
      (VSize.X <= (VSourceTileRect.Right - VSourceTileRect.Left) * 2) or
      (VSize.Y <= (VSourceTileRect.Bottom - VSourceTileRect.Top) * 2);
    VTileRectInfo := FStorage.GetTileRectInfo(VSourceTileRect, ASourceZoom, AVersion);
    if VTileRectInfo <> nil then begin
      VIterator := TTileIteratorByRect.Create(VSourceTileRect);
      VEnumTileInfo := VTileRectInfo.GetEnum(VIterator);
      while VEnumTileInfo.Next(VTileInfo) do begin
        VTileColor := AColorer.GetColor(VTileInfo);
        if VTileColor <> 0 then begin
          if VSameSourceAndTarget then begin
            VRelativeRectOfTile := VSourceConverter.TilePos2RelativeRect(VTileInfo.FTile, ASourceZoom);
          end else begin
            VLonLatRectOfTile := VSourceConverter.TilePos2LonLatRect(VTileInfo.FTile, ASourceZoom);
            VTargetConverter.CheckLonLatRect(VLonLatRectOfTile);
            VRelativeRectOfTile := VTargetConverter.LonLatRect2RelativeRect(VLonLatRectOfTile);
          end;
          VMapPixelRectOfTile := VTargetConverter.RelativeRect2PixelRectFloat(VRelativeRectOfTile, VTargetZoom);
          VLocalPixelRectOfTile := RectFromDoubleRect(ALocalConverter.MapRectFloat2LocalRectFloat(VMapPixelRectOfTile), rrToTopLeft);
          if not VSolidDrow then begin
            Dec(VLocalPixelRectOfTile.Right);
            Dec(VLocalPixelRectOfTile.Bottom);
          end;
          VBitmap.FillRectS(VLocalPixelRectOfTile, VTileColor);
        end;
      end;
    end;
    Result := VBitmap.BitmapStatic;
  finally
    VBitmap.Free;
  end;
end;

end.
