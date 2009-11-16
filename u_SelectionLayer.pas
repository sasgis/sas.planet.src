unit u_SelectionLayer;

interface

uses
  u_MapLayerBasic;

type
  TSelectionLayer = class(TMapLayerBasic)
  protected

  public
    procedure Redraw; override;

  end;

implementation

uses
  Types,
  Graphics,
  GR32,
  t_GeoTypes,
  u_GlobalState,
  uMapType,
  Unit1;

{ TSelectionLayer }

procedure TSelectionLayer.Redraw;
var
    VSelectedLonLat: TExtendedRect;
    VZoomCurr: Byte;
    VSelectedPixels: TRect;
    VBitmapPixels: TRect;
begin
  inherited;
  VSelectedLonLat.TopLeft := rect_arr[0];
  VSelectedLonLat.BottomRight := rect_arr[1];
  VZoomCurr := GState.zoom_size - 1;
  VSelectedPixels := sat_map_both.GeoConvert.LonLatRect2PixelRect(VSelectedLonLat, VZoomCurr);
  VBitmapPixels.TopLeft := MapPixel2BitmapPixel(VSelectedPixels.TopLeft);
  VBitmapPixels.BottomRight := MapPixel2BitmapPixel(VSelectedPixels.BottomRight);

  FLayer.Bitmap.Clear(clBlack);
  FLayer.Bitmap.FrameRectS(
    VBitmapPixels.Left, VBitmapPixels.Top,
    VBitmapPixels.Right, VBitmapPixels.Bottom,
    SetAlpha(clBlue32,150)
  );
  FLayer.Bitmap.FrameRectS(
    VBitmapPixels.Left - 1, VBitmapPixels.Top - 1,
    VBitmapPixels.Right + 1, VBitmapPixels.Bottom + 1,
    SetAlpha(clBlue32,150)
  );
end;

end.
 