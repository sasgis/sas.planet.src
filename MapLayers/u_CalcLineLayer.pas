unit u_CalcLineLayer;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  t_GeoTypes,
  i_IViewPortState,
  i_ILocalCoordConverter,
  i_IValueToStringConverter,
  i_ICalcLineLayerConfig,
  u_ClipPolygonByRect,
  u_MapLayerBasic;

type
  TCalcLineLayer = class(TMapLayerBasicFullView)
  private
    FConfig: ICalcLineLayerConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;

    FLenShow: Boolean;
    FLineColor: TColor32;
    FLineWidth: Integer;
    FPointFillColor: TColor32;
    FPointRectColor: TColor32;
    FPointFirstColor: TColor32;
    FPointActiveColor: TColor32;
    FPointSize: integer;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
    FValueConverter: IValueToStringConverter;


    FSourcePolygon: TDoublePointArray;
    FPolyActivePointIndex: integer;

    FDistArray: TDoubleDynArray;
    FBitmapSize: TPoint;
    FPointsOnBitmap: TDoublePointArray;
    FLinePolygon: TPolygon32;


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
    procedure DrawPointText(
      ABuffer: TBitmap32;
      const ABitmapSize: TPoint;
      const AText: string;
      const APosOnBitmap: TDoublePoint;
      const AFontSize: Integer;
      const ATextBGColor: TColor32;
      const ATextColor: TColor32
    );
    procedure PreparePolygon;
  protected
    procedure DoRedraw; override;
    procedure DoScaleChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: ICalcLineLayerConfig;
      AValueToStringConverterConfig: IValueToStringConverterConfig
    );
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure DrawLine(APathLonLat: TDoublePointArray; AActiveIndex: Integer);
    procedure DrawNothing;
  end;

implementation

uses
  SysUtils,
  GR32_Layers,
  i_ICoordConverter,
  i_IDatum,
  u_NotifyEventListener,
  UResStrings;

{ TCalcLineLayer }

constructor TCalcLineLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: ICalcLineLayerConfig;
  AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(TPositionedLayer.Create(AParentMap.Layers), AViewPortState);
  FConfig := AConfig;
  FValueToStringConverterConfig := AValueToStringConverterConfig;

  FLinePolygon := nil;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FValueToStringConverterConfig.GetChangeNotifier
  );
end;

destructor TCalcLineLayer.Destroy;
begin
  FreeAndNil(FLinePolygon);
  inherited;
end;

procedure TCalcLineLayer.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  Redraw;
end;

procedure TCalcLineLayer.DoRedraw;
begin
  inherited;
  PreparePolygon;
  LayerPositioned.Changed;
end;

procedure TCalcLineLayer.DoScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  Redraw;
end;

procedure TCalcLineLayer.DrawLine(APathLonLat: TDoublePointArray;
  AActiveIndex: Integer);
var
  VPointsCount: Integer;
begin
  FSourcePolygon := Copy(APathLonLat);
  FPolyActivePointIndex := AActiveIndex;

  VPointsCount := Length(FSourcePolygon);
  if VPointsCount > 0 then begin
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

procedure TCalcLineLayer.DrawPointText(
  ABuffer: TBitmap32;
  const ABitmapSize: TPoint;
  const AText: string;
  const APosOnBitmap: TDoublePoint;
  const AFontSize: Integer;
  const ATextBGColor: TColor32;
  const ATextColor: TColor32
);
var
  VTextSize: TSize;
  VRect: TRect;
begin
  if
    (APosOnBitmap.x > 0) and
    (APosOnBitmap.y > 0) and
    (APosOnBitmap.x < ABitmapSize.X) and
    (APosOnBitmap.y < ABitmapSize.Y)
  then begin
    ABuffer.Font.Size := AFontSize;
    VTextSize := ABuffer.TextExtent(AText);
    VRect.Left := Trunc(APosOnBitmap.x + 12);
    VRect.Top := Trunc(APosOnBitmap.Y);
    VRect.Right := VRect.Left + VTextSize.cx + 4;
    VRect.Bottom := VRect.Top + VTextSize.cy + 4;
    ABuffer.FillRectS(VRect, ATextBGColor );
    ABuffer.RenderText(VRect.Left + 2, VRect.Top + 2, AText, 3, ATextColor);
  end;
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
begin
  if
    (APosOnBitmap.x > 0) and
    (APosOnBitmap.y > 0) and
    (APosOnBitmap.x < ABitmapSize.X) and
    (APosOnBitmap.y < ABitmapSize.Y)
  then begin
    VHalfSize := ASize / 2;
    VRect.Left := Trunc(APosOnBitmap.X - VHalfSize);
    VRect.Top := Trunc(APosOnBitmap.Y - VHalfSize);
    VRect.Right := VRect.Left + ASize;
    VRect.Bottom := VRect.Top + ASize;
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
  FConfig.LockRead;
  try
    FLenShow := FConfig.LenShow;
    FLineColor := FConfig.LineColor;
    FLineWidth := FConfig.LineWidth;
    FPointFillColor := FConfig.PointFillColor;
    FPointRectColor := FConfig.PointRectColor;
    FPointFirstColor := FConfig.PointFirstColor;
    FPointActiveColor := FConfig.PointActiveColor;
    FPointSize := FConfig.PointSize;
    FTextColor := FConfig.TextColor;
    FTextBGColor := FConfig.TextBGColor;
  finally
    FConfig.UnlockRead;
  end;
  FValueConverter := FValueToStringConverterConfig.GetStaticConverter;

  Redraw;
end;

procedure TCalcLineLayer.PaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  VIndex: integer;
  VPosOnBitmap: TDoublePoint;
  text: string;
  VPointsCount: Integer;

begin
  VPointsCount := Length(FSourcePolygon);
  if VPointsCount > 0 then begin
    if FLinePolygon <> nil then begin
      FLinePolygon.DrawFill(Buffer, FLineColor);
    end;

    for VIndex := 1 to VPointsCount - 2 do begin
      VPosOnBitmap := FPointsOnBitmap[VIndex];
      if FLenShow then begin
        text := FValueConverter.DistConvert(FDistArray[VIndex] - FDistArray[VIndex - 1]);
        DrawPointText(Buffer, FBitmapSize, text, VPosOnBitmap, 7, FTextBGColor, FTextColor);
      end;
      DrawPolyPoint(Buffer, FBitmapSize, VPosOnBitmap, FPointSize, FPointFillColor, FPointRectColor);
    end;

    if VPointsCount > 1 then begin
      text := SAS_STR_Whole + ': ' + FValueConverter.DistConvert(FDistArray[VPointsCount - 1]);
      DrawPointText(Buffer, FBitmapSize, text, FPointsOnBitmap[VPointsCount - 1], 9, FTextBGColor, FTextColor);
    end;
    DrawPolyPoint(Buffer, FBitmapSize, FPointsOnBitmap[VPointsCount - 1], FPointSize, FPointFillColor, FPointRectColor);

    DrawPolyPoint(Buffer, FBitmapSize, FPointsOnBitmap[0], FPointSize, FPointFirstColor, FPointFirstColor);
    DrawPolyPoint(Buffer, FBitmapSize, FPointsOnBitmap[FPolyActivePointIndex], FPointSize, FPointActiveColor, FPointActiveColor);
  end;
end;

procedure TCalcLineLayer.PreparePolygon;
var
  VPointsCount: Integer;
  VLocalConverter: ILocalCoordConverter;
  VPolygon: TPolygon32;
  i: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VBitmapClip: IPolyClip;
  VPointsProcessedCount: Integer;
  VPointsOnBitmapPrepared: TDoublePointArray;
  VDatum: IDatum;
begin
  VPointsCount := Length(FSourcePolygon);
  if VPointsCount > 0 then begin
    VLocalConverter := FVisualCoordConverter;
    VBitmapClip := TPolyClipByRect.Create(VLocalConverter.GetLocalRect);

    VDatum := VLocalConverter.GetGeoConverter.Datum;
    SetLength(FDistArray, VPointsCount);
    FDistArray[0] := 0;
    for i := 1 to VPointsCount - 1 do begin
      FDistArray[i] := FDistArray[i - 1] + VDatum.CalcDist(FSourcePolygon[i - 1], FSourcePolygon[i]);
    end;

    FPointsOnBitmap := LonLatArrayToVisualFloatArray(VLocalConverter, FSourcePolygon);
    FBitmapSize := VLocalConverter.GetLocalRectSize;

    VPointsProcessedCount := VBitmapClip.Clip(FPointsOnBitmap, VPointsCount, VPointsOnBitmapPrepared);
    if VPointsProcessedCount > 0 then begin
      SetLength(VPathFixedPoints, VPointsProcessedCount);
      for i := 0 to VPointsProcessedCount - 1 do begin
        VPathFixedPoints[i] := FixedPoint(VPointsOnBitmapPrepared[i].X, VPointsOnBitmapPrepared[i].Y);
      end;
      VPolygon := TPolygon32.Create;
      try
        VPolygon.Antialiased := true;
        VPolygon.AntialiasMode := am4times;
        VPolygon.Closed := False;
        VPolygon.AddPoints(VPathFixedPoints[0], VPointsProcessedCount);
        with VPolygon.Outline do try
          FreeAndNil(FLinePolygon);
          FLinePolygon := Grow(Fixed(FLineWidth / 2), 0.5);
        finally
          free;
        end;
      finally
        VPolygon.Free;
      end;
    end;
  end;
end;

procedure TCalcLineLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
  LayerPositioned.OnPaint := PaintLayer;
end;

end.
