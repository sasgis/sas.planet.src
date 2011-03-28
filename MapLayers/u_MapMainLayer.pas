unit u_MapMainLayer;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_ViewPortState,
  u_MapLayerShowError,
  u_MapType,
  u_MapLayerBasic;

type
  TMapMainLayer = class(TMapLayerBasic)
  private
    FErrorShowLayer: TTileErrorInfoLayer;
    FMapsConfig: IMainMapsConfig;
    FMainMap: IMapType;
    FLayersList: IMapTypeList;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  protected
    procedure DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
    procedure DoRedraw; override;
    procedure OnMainMapChange(Sender: TObject);
    procedure OnLayerSetChange(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; AMapsConfig: IMainMapsConfig);
    procedure StartThreads; override;
    property ErrorShowLayer: TTileErrorInfoLayer read FErrorShowLayer write FErrorShowLayer;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_TileIterator,
  i_BitmapPostProcessingConfig,
  u_ResStrings,
  u_TileIteratorByRect,
  u_NotifyEventListener,
  u_GlobalState;

{ TMapMainLayer }

constructor TMapMainLayer.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState; AMapsConfig: IMainMapsConfig);
begin
  inherited Create(AParentMap, AViewPortState);
  FMapsConfig := AMapsConfig;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnMainMapChange),
    FMapsConfig.GetActiveMap.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnLayerSetChange),
    FMapsConfig.GetBitmapLayersSet.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    GState.ViewConfig.GetChangeNotifier
  );
end;

procedure TMapMainLayer.DoRedraw;
var
  i: Cardinal;
  VMapType: TMapType;
  VGUID: TGUID;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VHybrList: IMapTypeList;
begin
  inherited;
  FLayer.Bitmap.Clear(0);
  if FMainMap <> nil then begin
    DrawMap(FMainMap.MapType, dmOpaque);
  end;

  VHybrList := FLayersList;
  if VHybrList <> nil then begin
    VEnum := VHybrList.GetIterator;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VItem := VHybrList.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      if VMapType.IsBitmapTiles then begin
        DrawMap(VMapType, dmBlend);
      end;
    end;
  end;
end;

procedure TMapMainLayer.DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
var
  VZoom: Byte;
  VBmp: TCustomBitmap32;

  { Прямоугольник пикселей растра в координатах основного конвертера }
  VBitmapOnMapPixelRect: TRect;
  { Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    основного конвертера }
  VTileSourceRect: TRect;
  { Текущий тайл в кооординатах основного конвертера }
  VTile: TPoint;
  { Прямоугольник пикслов текущего тайла в кооординатах основного конвертера }
  VCurrTilePixelRect: TRect;
  { Прямоугольник тайла подлежащий отображению на текущий растр }
  VTilePixelsToDraw: TRect;
  { Прямоугольник пикселов в которые будет скопирован текущий тайл }
  VCurrTileOnBitmapRect: TRect;

  VGeoConvert: ICoordConverter;
  VUsePre: Boolean;
  VBitmapConverter: ILocalCoordConverter;
  VTileIterator: ITileIterator;
  VRecolorConfig: IBitmapPostProcessingConfigStatic;
begin
  if AMapType.asLayer then begin
    VUsePre := FUsePrevZoomAtLayer;
  end else begin
    VUsePre := FUsePrevZoomAtMap;
  end;
  VRecolorConfig := GState.BitmapPostProcessingConfig.GetStatic;

  VBitmapConverter := BitmapCoordConverter;
  VGeoConvert := VBitmapConverter.GetGeoConverter;
  VZoom := VBitmapConverter.GetZoom;

  VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;
  VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

  VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);
  VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;

  VBmp := TCustomBitmap32.Create;
  try
    while VTileIterator.Next(VTile) do begin
        VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

        VTilePixelsToDraw.TopLeft := Point(0, 0);
        VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
        VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

        if VCurrTilePixelRect.Left < VBitmapOnMapPixelRect.Left then begin
          VTilePixelsToDraw.Left := VBitmapOnMapPixelRect.Left - VCurrTilePixelRect.Left;
          VCurrTilePixelRect.Left := VBitmapOnMapPixelRect.Left;
        end;

        if VCurrTilePixelRect.Top < VBitmapOnMapPixelRect.Top then begin
          VTilePixelsToDraw.Top := VBitmapOnMapPixelRect.Top - VCurrTilePixelRect.Top;
          VCurrTilePixelRect.Top := VBitmapOnMapPixelRect.Top;
        end;

        if VCurrTilePixelRect.Right > VBitmapOnMapPixelRect.Right then begin
          VTilePixelsToDraw.Right := VTilePixelsToDraw.Right - (VCurrTilePixelRect.Right - VBitmapOnMapPixelRect.Right);
          VCurrTilePixelRect.Right := VBitmapOnMapPixelRect.Right;
        end;

        if VCurrTilePixelRect.Bottom > VBitmapOnMapPixelRect.Bottom then begin
          VTilePixelsToDraw.Bottom := VTilePixelsToDraw.Bottom - (VCurrTilePixelRect.Bottom - VBitmapOnMapPixelRect.Bottom);
          VCurrTilePixelRect.Bottom := VBitmapOnMapPixelRect.Bottom;
        end;
        VCurrTileOnBitmapRect.TopLeft := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
        VCurrTileOnBitmapRect.BottomRight := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);
        try
          if AMapType.LoadTileUni(VBmp, VTile, VZoom, true, VGeoConvert, VUsePre, True, False) then begin
            VRecolorConfig.ProcessBitmap(VBmp);
            FLayer.Bitmap.Lock;
            try
              VBmp.DrawMode := ADrawMode;
              Assert(VCurrTileOnBitmapRect.Right - VCurrTileOnBitmapRect.Left = VTilePixelsToDraw.Right - VTilePixelsToDraw.Left);
              Assert(VCurrTileOnBitmapRect.Bottom - VCurrTileOnBitmapRect.Top = VTilePixelsToDraw.Bottom - VTilePixelsToDraw.Top);
              FLayer.Bitmap.Draw(VCurrTileOnBitmapRect, VTilePixelsToDraw, Vbmp);
            finally
              FLayer.Bitmap.UnLock;
            end;
          end;
        except
          FErrorShowLayer.ShowError(VTile, VZoom, AMapType, SAS_ERR_BadFile);
        end;
    end;
  finally
    VBmp.Free;
  end;
end;

procedure TMapMainLayer.OnConfigChange(Sender: TObject);
begin
  GState.ViewConfig.LockRead;
  try
    FUsePrevZoomAtMap := GState.ViewConfig.UsePrevZoomAtMap;
    FUsePrevZoomAtLayer := GState.ViewConfig.UsePrevZoomAtLayer;
  finally
    GState.ViewConfig.UnlockRead;
  end;
  Redraw;
end;

procedure TMapMainLayer.OnLayerSetChange(Sender: TObject);
begin
  FLayersList := FMapsConfig.GetBitmapLayersSet.GetSelectedMapsList;
  Redraw;
end;

procedure TMapMainLayer.OnMainMapChange(Sender: TObject);
begin
  FMainMap := FMapsConfig.GetSelectedMapType;
  Redraw;
end;

procedure TMapMainLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
  OnMainMapChange(nil);
  OnLayerSetChange(nil);
  Visible := True;
end;

end.


