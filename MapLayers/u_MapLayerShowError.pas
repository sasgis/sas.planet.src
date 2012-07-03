unit u_MapLayerShowError;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_Notifier, 
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_TileError,
  i_SimpleFlag,
  i_BitmapMarker,
  i_TileErrorLogProviedrStuped,
  u_MapType,
  u_MapLayerBasic;

type
  TTileErrorInfoLayer = class(TMapLayerBasicNoBitmap)
  private
    FLogProvider: ITileErrorLogProviedrStuped;
    FTimerNoifier: INotifier;
    FNeedUpdateFlag: ISimpleFlag;

    FHideAfterTime: Cardinal;
    FErrorInfo: ITileErrorInfo;
    FFixedLonLat: TDoublePoint;
    FMarker: IBitmapMarker;

    procedure OnTimer;
    procedure OnErrorRecive;
    function CreateMarkerByError(const AErrorInfo: ITileErrorInfo): IBitmapMarker;
    procedure ShowError(const AErrorInfo: ITileErrorInfo);
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure DoHide; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const ALogProvider: ITileErrorLogProviedrStuped;
      const ATimerNoifier: INotifier
    );
  end;

implementation

uses
  Types,
  GR32_Resamplers,
  i_CoordConverter,
  i_Bitmap32Static,
  u_NotifyEventListener,
  u_SimpleFlagWithInterlock,
  u_BitmapMarker,
  u_Bitmap32Static,
  u_GeoFun;


{ TTileErrorInfoLayer }

constructor TTileErrorInfoLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const ALogProvider: ITileErrorLogProviedrStuped;
  const ATimerNoifier: INotifier
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FLogProvider := ALogProvider;
  FTimerNoifier := ATimerNoifier;
  FErrorInfo := nil;
  FNeedUpdateFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnErrorRecive),
    FLogProvider.GetNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    FTimerNoifier
  );
end;

function TTileErrorInfoLayer.CreateMarkerByError(
  const AErrorInfo: ITileErrorInfo
): IBitmapMarker;
var
  VText: string;
  VSize: TPoint;
  VMapNameSize: TSize;
  VMessageSize: TSize;
  VBitmap: TBitmap32;
  VBitmapStatic: IBitmap32Static;
begin
  inherited;
  Result := nil;
  if AErrorInfo <> nil then begin
    VBitmap := TBitmap32.Create;
    try
      VBitmap.CombineMode := cmMerge;
      if AErrorInfo.MapType <> nil then begin
        VText := AErrorInfo.MapType.GUIConfig.Name.Value;
        VMapNameSize := VBitmap.TextExtent(VText);
        VSize.X := VMapNameSize.cx;
        VSize.Y := VMapNameSize.cy + 20;
        VMessageSize := VBitmap.TextExtent(AErrorInfo.ErrorText);
        if VSize.X < VMessageSize.cx then begin
          VSize.X := VMessageSize.cx;
        end;
        Inc(VSize.Y, VMessageSize.cy + 20);
        Inc(VSize.X, 20);
        VBitmap.SetSize(VSize.X, VSize.Y);
        VBitmap.Clear(0);

        VBitmap.RenderText((VSize.X - VMapNameSize.cx) div 2, 10, VText, 0, clBlack32);
        VBitmap.RenderText((VSize.X - VMessageSize.cx) div 2, 30 + VMapNameSize.cy, AErrorInfo.ErrorText, 0, clBlack32);
      end else begin
        VMessageSize := VBitmap.TextExtent(AErrorInfo.ErrorText);
        VSize.X := VMessageSize.cx + 20;
        VSize.Y := VMessageSize.cy + 20;

        VBitmap.SetSize(VSize.X, VSize.Y);
        VBitmap.Clear(0);

        VBitmap.RenderText((VSize.X - VMessageSize.cx) div 2, 10, AErrorInfo.ErrorText, 0, clBlack32);
      end;
      VBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
      VBitmap := nil;
    finally
      VBitmap.Free;
    end;
    Result := TBitmapMarker.Create(VBitmapStatic, DoublePoint(VSize.X / 2, VSize.Y / 2));
  end;
end;

procedure TTileErrorInfoLayer.DoHide;
begin
  inherited;
  FHideAfterTime := 0;
  FErrorInfo := nil;
  FMarker := nil;
end;

procedure TTileErrorInfoLayer.OnErrorRecive;
begin
  FNeedUpdateFlag.SetFlag;
end;

procedure TTileErrorInfoLayer.OnTimer;
var
  VCurrTime: Cardinal;
  VNeedHide: Boolean;
begin
  if FNeedUpdateFlag.CheckFlagAndReset then begin
    ShowError(FLogProvider.GetLastErrorInfo);
  end else begin
    VNeedHide := True;
    if FHideAfterTime <> 0 then begin
      if FErrorInfo <> nil then begin
        VCurrTime := GetTickCount;
        if (VCurrTime < FHideAfterTime) then begin
          VNeedHide := False;
        end;
      end;
    end;
    if VNeedHide then begin
      ShowError(nil);
    end;
  end;
end;

procedure TTileErrorInfoLayer.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VMarker: IBitmapMarker;
  VFixedOnView: TDoublePoint;
  VTargetPointFloat: TDoublePoint;
  VTargetPoint: TPoint;
begin
  if FErrorInfo <> nil then begin
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(FFixedLonLat);
    if PixelPointInRect(VFixedOnView, DoubleRect(ALocalConverter.GetLocalRect)) then begin
      VMarker := FMarker;
      if VMarker = nil then begin
        VMarker := CreateMarkerByError(FErrorInfo);
      end;
      FMarker := VMarker;
      if VMarker <> nil then begin
        VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(FFixedLonLat);
        VTargetPointFloat :=
          DoublePoint(
            VFixedOnView.X - VMarker.AnchorPoint.X,
            VFixedOnView.Y - VMarker.AnchorPoint.Y
          );
        VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);
        if PtInRect(ALocalConverter.GetLocalRect, VTargetPoint) then begin
          BlockTransfer(
            ABuffer,
            VTargetPoint.X, VTargetPoint.Y,
            ABuffer.ClipRect,
            VMarker.Bitmap,
            VMarker.Bitmap.BoundsRect,
            dmBlend,
            cmBlend
          );
        end;
      end;
    end;
  end;
end;

procedure TTileErrorInfoLayer.ShowError(const AErrorInfo: ITileErrorInfo);
var
  VConverter: ICoordConverter;
  VMapType: TMapType;
  VZoom: Byte;
  VTile: TPoint;
begin
  ViewUpdateLock;
  try
    FErrorInfo := AErrorInfo;
    if FErrorInfo <> nil then begin
      VMapType := FErrorInfo.MapType;
      VConverter := VMapType.GeoConvert;
      FHideAfterTime := GetTickCount + 10000;
      VZoom := FErrorInfo.Zoom;
      VTile := FErrorInfo.Tile;
      VConverter.CheckTilePosStrict(VTile, VZoom, True);
      FFixedLonLat := VConverter.PixelPosFloat2LonLat(RectCenter(VConverter.TilePos2PixelRect(VTile, VZoom)), VZoom);
      FMarker := nil;
      SetNeedRedraw;
      Show;
    end else begin
      FHideAfterTime := 0;
      FMarker := nil;
      Hide;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

end.


