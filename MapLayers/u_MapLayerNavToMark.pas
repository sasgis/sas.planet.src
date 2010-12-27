unit u_MapLayerNavToMark;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_ILocalCoordConverter,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TNavToMarkLayer = class(TMapLayerFixedWithBitmap)
  protected
    FMarkPoint: TDoublePoint;
    FId: integer;
    FCrossDist: Double;
    FArrowBitmap: TCustomBitmap32;
    FCrossBitmap: TCustomBitmap32;
    FCurrDist: Double;
    FCurrAngle: Double;
    procedure PrepareMarker;
  protected
    procedure DoRedraw; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure StartNav(APoint: TDoublePoint; Aid: integer);
    function GetMarkLonLat: TDoublePoint;
    property ID: Integer read FId;
  end;

implementation

uses
  Graphics,
  SysUtils,
  Math,
  GR32_Transforms,
  GR32_Polygons,
  u_GlobalState,
  i_ICoordConverter,
  Ugeofun;

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
var
  VSize: TPoint;
begin
  inherited;
  FArrowBitmap := TCustomBitmap32.Create;
  FArrowBitmap.DrawMode:=dmBlend;
  FArrowBitmap.CombineMode:=cmMerge;
  FCrossBitmap := TCustomBitmap32.Create;
  FCrossBitmap.DrawMode:=dmBlend;
  FCrossBitmap.CombineMode:=cmMerge;
  PrepareMarker;
  FCrossDist := 100;
  VSize := Point(FArrowBitmap.Width, FArrowBitmap.Height);
  FLayer.Bitmap.SetSize(VSize.X, VSize.Y);
  FLayer.Bitmap.Clear(0);
  DoUpdateLayerSize(VSize);
end;

destructor TNavToMarkLayer.Destroy;
begin
  FreeAndNil(FArrowBitmap);
  FreeAndNil(FCrossBitmap);
  inherited;
end;

procedure TNavToMarkLayer.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
var
  VMarkMapPos: TDoublePoint;
  VScreenCenterMapPos: TDoublePoint;
  VDelta: TDoublePoint;
  VDeltaNormed: TDoublePoint;
  VZoom: Byte;
  VConverter: ICoordConverter;
begin
  VConverter := ANewVisualCoordConverter.GetGeoConverter;
  VZoom := ANewVisualCoordConverter.GetZoom;
  VScreenCenterMapPos := ANewVisualCoordConverter.GetCenterMapPixelFloat;
  VMarkMapPos := VConverter.LonLat2PixelPosFloat(FMarkPoint, VZoom);
  VDelta.X := VMarkMapPos.X - VScreenCenterMapPos.X;
  VDelta.Y := VMarkMapPos.Y - VScreenCenterMapPos.Y;
  FCurrDist := Sqrt(Sqr(VDelta.X) + Sqr(VDelta.Y));
  if FCurrDist < FCrossDist then begin
    FFixedLonLat := FMarkPoint;
    FCurrAngle := 0;
  end else begin
    VDeltaNormed.X := VDelta.X / FCurrDist * FCrossDist;
    VDeltaNormed.Y := VDelta.Y / FCurrDist * FCrossDist;
    VMarkMapPos.X := VScreenCenterMapPos.X + VDeltaNormed.X;
    VMarkMapPos.Y := VScreenCenterMapPos.Y + VDeltaNormed.Y;
    FFixedLonLat := VConverter.PixelPosFloat2LonLat(VMarkMapPos, VZoom);
    FCurrAngle := ArcSin(VDelta.X/FCurrDist) / Pi * 180;
    if VDelta.Y < 0 then begin
      FCurrAngle := 180 - FCurrAngle;
    end;
  end;
  inherited;
  Redraw;
end;

procedure TNavToMarkLayer.DoRedraw;
var
  T: TAffineTransformation;
  VSize: TPoint;
begin
  inherited;
  VSize := LayerSize;
  if FCurrDist > FCrossDist then begin
    T := TAffineTransformation.Create;
    try
      T.SrcRect := FloatRect(0, 0, VSize.X, VSize.Y);
      T.Clear;

      T.Translate(-VSize.X / 2, -VSize.Y / 2);
      T.Rotate(0, 0, FCurrAngle);
      T.Translate(VSize.X / 2, VSize.Y / 2);
      FLayer.Bitmap.Lock;
      try
        FLayer.Bitmap.Clear(0);
        Transform(FLayer.Bitmap, FArrowBitmap, T);
      finally
        FLayer.Bitmap.Unlock;
      end;
    finally
      FreeAndNil(T);
    end;
  end else begin
    FLayer.Bitmap.Lock;
    try
      FLayer.Bitmap.Clear(0);
      FCrossBitmap.DrawTo(FLayer.Bitmap);
    finally
      FLayer.Bitmap.Unlock;
    end;
  end;
end;

function TNavToMarkLayer.GetMarkLonLat: TDoublePoint;
begin
  Result := FMarkPoint;
end;

procedure TNavToMarkLayer.PrepareMarker;
var
  VSize: TPoint;
  Polygon: TPolygon32;
  VMarkSize: integer;
  VColor: TColor32;
  VRect: TRect;
begin
  VMarkSize := 20;
  VSize := Point(VMarkSize * 2, VMarkSize * 2);
  FFixedOnBitmap := DoublePoint(VMarkSize, VMarkSize);
  VColor := SetAlpha(Color32(GState.GPSpar.GPS_ArrowColor), 150);
  FArrowBitmap.SetSize(VSize.Y, VSize.Y);
  FArrowBitmap.Clear(0);
  Polygon := TPolygon32.Create;
  try
    Polygon.Antialiased := true;
    Polygon.AntialiasMode := am32times;
    Polygon.Add(FixedPoint(FFixedOnBitmap.X, FFixedOnBitmap.Y));
    Polygon.Add(FixedPoint(FFixedOnBitmap.X - VMarkSize / 5, FFixedOnBitmap.Y - VMarkSize));
    Polygon.Add(FixedPoint(FFixedOnBitmap.X + VMarkSize / 5, FFixedOnBitmap.Y - VMarkSize));
    Polygon.DrawFill(FArrowBitmap, VColor)
  finally
    FreeAndNil(Polygon);
  end;
  FCrossBitmap.SetSize(VSize.Y, VSize.Y);
  FCrossBitmap.Clear(0);
  VRect.Left := Trunc(FFixedOnBitmap.X - VMarkSize /10);
  VRect.Top := Trunc(FFixedOnBitmap.Y - VMarkSize /2);
  VRect.Right := Trunc(FFixedOnBitmap.X + VMarkSize /10);
  VRect.Bottom := VRect.Top + VMarkSize;
  FCrossBitmap.FillRectS(VRect, VColor);
  VRect.Left := Trunc(FFixedOnBitmap.X - VMarkSize /2);
  VRect.Top := Trunc(FFixedOnBitmap.Y - VMarkSize /10);
  VRect.Right := VRect.Left + VMarkSize;
  VRect.Bottom := Trunc(FFixedOnBitmap.Y + VMarkSize /10);
  FCrossBitmap.FillRectS(VRect, VColor);
end;

procedure TNavToMarkLayer.StartNav(APoint: TDoublePoint; Aid: integer);
begin
  FMarkPoint := APoint;
  FId := Aid;
  Redraw;
  Show;
end;

end.
