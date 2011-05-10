unit u_MapLayerShowError;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  i_JclNotify,
  i_LocalCoordConverter,
  i_ViewPortState,
  i_TileError,
  i_TileErrorLogProviedrStuped,
  u_MapType,
  u_MapLayerBasic;

type
  TTileErrorInfoLayer = class(TMapLayerFixedWithBitmap)
  private
    FLogProvider: ITileErrorLogProviedrStuped;
    FTimerNoifier: IJclNotifier;
    FNeedUpdateCounter: Integer;

    FHideAfterTime: Cardinal;
    FErrorInfo: ITileErrorInfo;

    procedure RenderText(AMapType: TMapType; AText: string);
    procedure OnTimer(Sender: TObject);
    procedure OnErrorRecive(Sender: TObject);
  protected
    function GetVisibleForNewPos(ANewVisualCoordConverter: ILocalCoordConverter): Boolean; override;
    procedure DoHide; override;
    procedure DoRedraw; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ALogProvider: ITileErrorLogProviedrStuped;
      ATimerNoifier: IJclNotifier
    );
    procedure ShowError(AErrorInfo: ITileErrorInfo);
  end;

implementation

uses
  Graphics,
  Types,
  i_CoordConverter,
  u_GeoFun;


{ TTileErrorInfoLayer }

constructor TTileErrorInfoLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ALogProvider: ITileErrorLogProviedrStuped;
  ATimerNoifier: IJclNotifier
);
var
  VBitmapSize: TPoint;
begin
  inherited Create(AParentMap, AViewPortState);
  FLogProvider := ALogProvider;
  FTimerNoifier := ATimerNoifier;
  FErrorInfo := nil;
  VBitmapSize.X := 256;
  VBitmapSize.Y := 100;
  FFixedOnBitmap.X := VBitmapSize.X / 2;
  FFixedOnBitmap.Y := VBitmapSize.Y / 2;
  FLayer.Bitmap.SetSize(VBitmapSize.X, VBitmapSize.Y);
  DoUpdateLayerSize(VBitmapSize);
end;

procedure TTileErrorInfoLayer.DoHide;
begin
  inherited;
  FHideAfterTime := 0;
  FErrorInfo := nil;
end;

procedure TTileErrorInfoLayer.DoRedraw;
var
  VTextWidth: integer;
  VSize: TPoint;
  VErrorInfo: ITileErrorInfo;
begin
  inherited;
  VErrorInfo := FErrorInfo;
  if VErrorInfo <> nil then begin
    VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
    FLayer.Bitmap.Clear(clBlack);
    if VErrorInfo.MapType <> nil then begin
      VTextWidth := FLayer.Bitmap.TextWidth(VErrorInfo.MapType.name);
      FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, VSize.Y div 4, VErrorInfo.MapType.name, 0, clBlack32);

      VTextWidth := FLayer.Bitmap.TextWidth(VErrorInfo.ErrorText);
      FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, (VSize.Y div 4) * 3, VErrorInfo.ErrorText, 0, clBlack32);
    end else begin
      VTextWidth := FLayer.Bitmap.TextWidth(VErrorInfo.ErrorText);
      FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, (VSize.Y div 2), VErrorInfo.ErrorText, 0, clBlack32);
    end;
  end;
end;

function TTileErrorInfoLayer.GetVisibleForNewPos(
  ANewVisualCoordConverter: ILocalCoordConverter): Boolean;
var
  VCurrTime: Cardinal;
  VErrorInfo: ITileErrorInfo;
begin
  Result := False;
  if FHideAfterTime <> 0 then begin
    VErrorInfo := FErrorInfo;
    if VErrorInfo <> nil then begin
      VCurrTime := GetTickCount;
      if (VCurrTime < FHideAfterTime) then begin
        if VErrorInfo.Zoom = LayerCoordConverter.GetZoom then begin
          Result := True;
        end;
      end;
    end;
  end;
end;

procedure TTileErrorInfoLayer.OnErrorRecive(Sender: TObject);
begin
  InterlockedIncrement(FNeedUpdateCounter);
end;

procedure TTileErrorInfoLayer.OnTimer(Sender: TObject);
var
  VCounter: Integer;
  VErrorInfo: ITileErrorInfo;
  VCurrTime: Cardinal;
begin
  VCounter := InterlockedExchange(FNeedUpdateCounter, 0);
  if VCounter >= 0 then begin
    VErrorInfo := FLogProvider.GetLastErrorInfo;
    ShowError(VErrorInfo);
  end else begin
    SetVisible(GetVisibleForNewPos(LayerCoordConverter));
  end;
end;

procedure TTileErrorInfoLayer.RenderText(AMapType: TMapType; AText: string);
var
  VTextWidth: integer;
  VSize: TPoint;
begin
  VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  FLayer.Bitmap.Clear(0);
  if AMapType <> nil then begin
    VTextWidth := FLayer.Bitmap.TextWidth(AMapType.name);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, VSize.Y div 4, AMapType.name, 0, clBlack32);

    VTextWidth := FLayer.Bitmap.TextWidth(AText);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, (VSize.Y div 4) * 3, AText, 0, clBlack32);
  end else begin
    VTextWidth := FLayer.Bitmap.TextWidth(AText);
    FLayer.Bitmap.RenderText((VSize.X - VTextWidth) div 2, (VSize.Y div 2), AText, 0, clBlack32);
  end;
end;

procedure TTileErrorInfoLayer.ShowError(AErrorInfo: ITileErrorInfo);
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
      SetNeedRedraw;
      SetNeedUpdateLocation;
      Show;
    end else begin
      Hide;
    end;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

end.
