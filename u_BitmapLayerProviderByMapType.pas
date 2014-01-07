unit u_BitmapLayerProviderByMapType;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_TileObjCache,
  i_BitmapLayerProvider,
  i_TileError,
  i_MapVersionInfo,
  u_MapType,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByMapType = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FErrorLogger: ITileErrorLogger;
    FMapType: TMapType;
    FVersion: IMapVersionInfo;
    FCache: ITileObjCacheBitmap;
    FUsePrevZoom: Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const AErrorLogger: ITileErrorLogger;
      AMapType: TMapType;
      const AVersion: IMapVersionInfo;
      const ACache: ITileObjCacheBitmap;
      AUsePrevZoom: Boolean
    );
  end;

implementation

uses
  Types,
  SysUtils,
  i_CoordConverter,
  u_TileErrorInfo;

{ TBitmapLayerProviderByMapType }

constructor TBitmapLayerProviderByMapType.Create(
  const AErrorLogger: ITileErrorLogger;
  AMapType: TMapType;
  const AVersion: IMapVersionInfo;
  const ACache: ITileObjCacheBitmap;
  AUsePrevZoom: Boolean
);
begin
  Assert(Assigned(AMapType));
  Assert(Assigned(AVersion));
  inherited Create;
  FErrorLogger := AErrorLogger;
  FMapType := AMapType;
  FVersion := AVersion;
  FCache := ACache;
  FUsePrevZoom := AUsePrevZoom;
end;

function TBitmapLayerProviderByMapType.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VTile: TPoint;
  Vzoom: byte;
  VCoordConverter: ICoordConverter;
  VPixelRect: TRect;
  VError: ITileErrorInfo;
begin
  Vzoom := ALocalConverter.Zoom;
  VCoordConverter := ALocalConverter.GeoConverter;
  VPixelRect := ALocalConverter.GetRectInMapPixel;
  VTile := VCoordConverter.PixelRect2TileRect(VPixelRect, Vzoom).TopLeft;
  Assert(EqualRect(VPixelRect, VCoordConverter.TilePos2PixelRect(VTile, Vzoom)));

  try
    Result :=
      FMapType.LoadTileUni(
        VTile,
        Vzoom,
        FVersion,
        VCoordConverter,
        FUsePrevZoom,
        True,
        Assigned(FErrorLogger),
        FCache
      );
  except
    on E: Exception do begin
      if FErrorLogger <> nil then begin
        VError :=
          TTileErrorInfo.Create(
            FMapType.Zmp.GUID,
            Vzoom,
            VTile,
            E.Message
          );
        FErrorLogger.LogError(VError);
      end else begin
        raise;
      end;
    end;
    else if FErrorLogger <> nil then begin
        VError :=
          TTileErrorInfo.Create(
            FMapType.Zmp.GUID,
            VZoom,
            VTile,
            'Unexpected read tile error'
          );
        FErrorLogger.LogError(VError);
      end else begin
        raise;
      end;
  end;
end;

end.
