unit u_CalcLineLayer;

interface

uses
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_IViewPortState,
  i_ILocalCoordConverter,
  i_ICalcLineLayerConfig,
  u_MapLayerBasic;

type
  TCalcLineLayer = class(TMapLayerBasicFullView)
  private
    FConfig: ICalcLineLayerConfig;
    FPolygon: TDoublePointArray;
    FPolyActivePointIndex: integer;

    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
    function LonLatArrayToVisualFloatArray(ALocalConverter: ILocalCoordConverter; APolygon: TDoublePointArray): TDoublePointArray;
    procedure OnConfigChange(Sender: TObject);

    procedure DrawPolyPoint(
      ABuffer: TBitmap32;
      const ABitmapSize: TPoint;
      const APosOnBitmap: TDoublePoint;
      const ASize: Integer;
      const AFillColor: TColor32;
      const ARectColor: TColor32
    );
  protected
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; AConfig: ICalcLineLayerConfig);
    procedure DrawLineCalc(APathLonLat: TDoublePointArray; AActiveIndex: Integer);
    procedure DrawNothing;
  end;

implementation

uses
  Classes,
  Graphics,
  GR32_PolygonsEx,
  GR32_Layers,
  GR32_VectorUtils,
  i_ICoordConverter,
  i_IDatum,
  i_IValueToStringConverter,
  u_NotifyEventListener,
  u_GlobalState,
  UResStrings;

{ TCalcLineLayer }

constructor TCalcLineLayer.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState; AConfig: ICalcLineLayerConfig);
begin
  inherited Create(TPositionedLayer.Create(AParentMap.Layers), AViewPortState);
  FConfig := AConfig;
  LayerPositioned.OnPaint := PaintLayer;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

procedure TCalcLineLayer.DoRedraw;
begin
  inherited;
  LayerPositioned.Changed;
end;

procedure TCalcLineLayer.DrawLineCalc(APathLonLat: TDoublePointArray;
  AActiveIndex: Integer);
begin
  if Length(APathLonLat) > 0 then begin
    FPolygon := Copy(APathLonLat);
    FPolyActivePointIndex := AActiveIndex;
    Redraw;
    Show;
  end else begin
    Hide;
  end;
end;

procedure TCalcLineLayer.DrawNothing;
begin
  Hide;
end;

procedure TCalcLineLayer.DrawPolyPoint(
  ABuffer: TBitmap32;
  const ABitmapSize: TPoint;
  const APosOnBitmap: TDoublePoint;
  const ASize: Integer;
  const AFillColor, ARectColor: TColor32
);
var
  VHalfSize: Double;
  VRect: TRect;
  VRectFloat: TDoubleRect;
begin
  VHalfSize := ASize / 2;
  VRectFloat.Left := APosOnBitmap.X - VHalfSize;
  VRectFloat.Top := APosOnBitmap.Y - VHalfSize;
  VRectFloat.Right := VRectFloat.Left + ASize;
  VRectFloat.Bottom := VRectFloat.Top + ASize;
  if
    (VRectFloat.Left > 0) and
    (VRectFloat.Top > 0) and
    (VRectFloat.Right < ABitmapSize.X) and
    (VRectFloat.Bottom < ABitmapSize.Y)
  then begin
    VRect.Left := Trunc(VRectFloat.Left);
    VRect.Top := Trunc(VRectFloat.Top);
    VRect.Right := Trunc(VRectFloat.Right);
    VRect.Bottom := Trunc(VRectFloat.Bottom);
    ABuffer.FillRectS(VRect, ARectColor);
    if AFillColor <> ARectColor then begin
      Inc(VRect.Left);
      Inc(VRect.Top);
      Dec(VRect.Right);
      Dec(VRect.Bottom);
      ABuffer.FillRectS(VRect, AFillColor);
    end;
  end;
end;

function TCalcLineLayer.LonLatArrayToVisualFloatArray(
  ALocalConverter: ILocalCoordConverter;
  APolygon: TDoublePointArray
): TDoublePointArray;
var
  i: Integer;
  VPointsCount: Integer;
  VLonLat: TDoublePoint;
  VGeoConvert: ICoordConverter;
begin
  VPointsCount := Length(APolygon);
  SetLength(Result, VPointsCount);

  VGeoConvert := ALocalConverter.GetGeoConverter;
  for i := 0 to VPointsCount - 1 do begin
    VLonLat := APolygon[i];
    VGeoConvert.CheckLonLatPos(VLonLat);
    Result[i] := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
  end;
end;

procedure TCalcLineLayer.OnConfigChange(Sender: TObject);
begin
  Redraw;
end;

procedure TCalcLineLayer.PaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  i, j, textW: integer;
  k1: TDoublePoint;
  len: Double;
  text: string;
  VBitmapSize: TPoint;
  VPointsOnBitmap: TDoublePointArray;
  VPointsCount: Integer;
  VFloatPoints: TArrayOfFloatPoint;
  VLocalConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
  VValueConverter: IValueToStringConverter;
  VDatum: IDatum;

  VLenShow: Boolean;
  VLineColor: TColor32;
  VLineWidth: integer;
  VPointFillColor: TColor32;
  VPointRectColor: TColor32;
  VPointFirstColor: TColor32;
  VPointActiveColor: TColor32;
  VPointSize: integer;
  VTextColor: TColor32;
  VTextBGColor: TColor32;
begin
  VPointsCount := Length(FPolygon);
  if VPointsCount > 0 then begin
    FConfig.LockRead;
    try
      VLenShow := FConfig.LenShow;
      VLineColor := FConfig.LineColor;
      VLineWidth := FConfig.LineWidth;
      VPointFillColor := FConfig.PointFillColor;
      VPointRectColor := FConfig.PointRectColor;
      VPointFirstColor := FConfig.PointFirstColor;
      VPointActiveColor := FConfig.PointActiveColor;
      VPointSize := FConfig.PointSize;
      VTextColor := FConfig.TextColor;
      VTextBGColor := FConfig.TextBGColor;
    finally
      FConfig.UnlockRead;
    end;

    VValueConverter := GState.ValueToStringConverterConfig.GetStaticConverter;
    VLocalConverter := FVisualCoordConverter;
    VGeoConvert := VLocalConverter.GetGeoConverter;
    VDatum := VGeoConvert.Datum;
    VPointsOnBitmap := LonLatArrayToVisualFloatArray(VLocalConverter, FPolygon);

    SetLength(VFloatPoints, VPointsCount);
    for i := 0 to VPointsCount - 1 do begin
      VFloatPoints[i] := FloatPoint(VPointsOnBitmap[i].X, VPointsOnBitmap[i].Y);
    end;
    PolylineFS(Buffer, VFloatPoints, VLineColor, True, VLineWidth, jsBevel);

    VBitmapSize := VLocalConverter.GetLocalRectSize;
    for i := 0 to VPointsCount - 2 do begin
      k1 := VPointsOnBitmap[i + 1];
      if ((k1.x > 0) and (k1.y > 0)) and ((k1.x < VBitmapSize.X) and (k1.y < VBitmapSize.Y)) then begin
        if i = VPointsCount - 2 then begin
          len := 0;
          for j := 0 to i do begin
            len := len + VDatum.CalcDist(FPolygon[j], FPolygon[j + 1]);
          end;
          text := SAS_STR_Whole + ': ' + VValueConverter.DistConvert(len);
          Buffer.Font.Size := 9;
          textW := Buffer.TextWidth(text) + 11;
          Buffer.FillRectS(
            Trunc(k1.x + 12),
            Trunc(k1.y),
            Trunc(k1.X + textW),
            Trunc(k1.y + 15),
            VTextBGColor
          );
          Buffer.RenderText(
            Trunc(k1.X + 15),
            Trunc(k1.y),
            text,
            3,
            VTextColor
          );
        end else begin
          if VLenShow then begin
            text := VValueConverter.DistConvert(VDatum.CalcDist(FPolygon[i], FPolygon[i + 1]));
            Buffer.Font.Size := 7;
            textW := Buffer.TextWidth(text) + 11;
            Buffer.FillRectS(
              Trunc(k1.x + 5),
              Trunc(k1.y + 5),
              Trunc(k1.X + textW),
              Trunc(k1.y + 16),
              VTextBGColor
            );
            Buffer.RenderText(
              Trunc(k1.X + 8),
              Trunc(k1.y + 5),
              text,
              0,
              VTextColor
            );
          end;
        end;
        DrawPolyPoint(Buffer, VBitmapSize, k1, VPointSize, VPointFillColor, VPointRectColor);
      end;
    end;
    k1 := VPointsOnBitmap[0];
    DrawPolyPoint(Buffer, VBitmapSize, k1, VPointSize, VPointFirstColor, VPointFirstColor);
    k1 := VPointsOnBitmap[FPolyActivePointIndex];
    DrawPolyPoint(Buffer, VBitmapSize, k1, VPointSize, VPointActiveColor, VPointActiveColor);
  end;
end;

end.
