unit u_MapMarksLayer;

interface

uses
  GR32,
  GR32_Image,
  
  t_GeoTypes,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TMapMarksLayer = class(TMapLayerBasic)
  protected
    FLLRect: TDoubleRect;
    FTempBmp: TCustomBitmap32;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  Graphics,
  Classes,
  SysUtils,
  GR32_Resamplers,
  t_CommonTypes,
  u_GlobalState,
  i_IBitmapLayerProvider,
  u_WindowLayerBasic;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayer.Bitmap.Font.Name := 'Tahoma';
  FLayer.Bitmap.Font.Style := [];
  FTempBmp := TCustomBitmap32.Create;
  FTempBmp.DrawMode := dmBlend;
  FTempBmp.Resampler := TLinearResampler.Create;
end;

destructor TMapMarksLayer.Destroy;
begin
  FreeAndNil(FTempBmp);
  inherited;
end;

procedure TMapMarksLayer.DoRedraw;
var
  VBitmapSize: TPoint;
  VRect: TRect;
  VProv: IBitmapLayerProvider;
begin
  inherited;
  if (GState.show_point <> mshNone) then begin
    VProv := GState.MarksBitmapProvider;
    FLayer.Bitmap.DrawMode:=dmBlend;
    FLayer.Bitmap.CombineMode:=cmMerge;
    FLayer.Bitmap.Clear(clBlack);
    VBitmapSize := GetBitmapSizeInPixel;
    VRect.TopLeft := BitmapPixel2MapPixel(Point(0, 0));
    VRect.BottomRight := BitmapPixel2MapPixel(VBitmapSize);
    VProv.GetBitmapRect(FLayer.Bitmap, FGeoConvert, VRect, FZoom);
  end;
end;

end.
