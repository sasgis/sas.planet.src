unit u_CalcLineLayer;

interface

uses
  Types,
  GR32,
  GR32_Polygons,
  GR32_Image,
  t_GeoTypes,
  i_ViewPortState,
  i_LocalCoordConverter,
  i_LineOnMapEdit,
  i_ValueToStringConverter,
  i_CalcLineLayerConfig,
  u_PolyLineLayerBase,
  u_MapLayerBasic;

type
  TCalcLineLayer = class(TPolyLineLayerBase)
  private
    FConfig: ICalcLineLayerConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;

    FLenShow: Boolean;
    FTextColor: TColor32;
    FTextBGColor: TColor32;
    FValueConverter: IValueToStringConverter;

    FDistArray: TDoubleDynArray;

    procedure DrawPointText(
      ABuffer: TBitmap32;
      const ABitmapSize: TPoint;
      const AText: string;
      const APosOnBitmap: TDoublePoint;
      const AFontSize: Integer;
      const ATextBGColor: TColor32;
      const ATextColor: TColor32
    );
  protected
    procedure DoConfigChange; override;
    procedure PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter); override;
    procedure PreparePolygon(ALocalConverter: ILocalCoordConverter); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ALineOnMapEdit: ILineOnMapEdit;
      AConfig: ICalcLineLayerConfig;
      AValueToStringConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  SysUtils,
  i_Datum,
  u_NotifyEventListener,
  u_ResStrings;

{ TCalcLineLayer }

constructor TCalcLineLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ALineOnMapEdit: ILineOnMapEdit;
  AConfig: ICalcLineLayerConfig;
  AValueToStringConverterConfig: IValueToStringConverterConfig
);
var
  VPolygon: TPolygon32;
begin
  VPolygon := TPolygon32.Create;
  VPolygon.Closed := False;
  VPolygon.Antialiased := true;
  VPolygon.AntialiasMode := am4times;
  inherited Create(AParentMap, AViewPortState, ALineOnMapEdit, AConfig, VPolygon, false);
  FConfig := AConfig;
  FValueToStringConverterConfig := AValueToStringConverterConfig;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FValueToStringConverterConfig.GetChangeNotifier
  );
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
    ABuffer.FillRectTS(VRect, ATextBGColor );
    ABuffer.RenderText(VRect.Left + 2, VRect.Top + 2, AText, 3, ATextColor);
  end;
end;

procedure TCalcLineLayer.DoConfigChange;
begin
  inherited;
  FLenShow := FConfig.LenShow;
  FTextColor := FConfig.TextColor;
  FTextBGColor := FConfig.TextBGColor;
  FValueConverter := FValueToStringConverterConfig.GetStatic;
end;

procedure TCalcLineLayer.PaintLayer(ABuffer: TBitmap32; ALocalConverter: ILocalCoordConverter);
var
  VIndex: integer;
  VPosOnBitmap: TDoublePoint;
  text: string;
  VPointsCount: Integer;

begin
  inherited;
  VPointsCount := Length(FDistArray);
  if VPointsCount > 0 then begin
    if FLenShow then begin
      for VIndex := 1 to VPointsCount - 2 do begin
        VPosOnBitmap := PointsOnBitmap[VIndex];
        text := FValueConverter.DistConvert(FDistArray[VIndex] - FDistArray[VIndex - 1]);
        DrawPointText(ABuffer, BitmapSize, text, VPosOnBitmap, 7, FTextBGColor, FTextColor);
      end;
    end;

    if VPointsCount > 1 then begin
      text := SAS_STR_Whole + ': ' + FValueConverter.DistConvert(FDistArray[VPointsCount - 1]);
      DrawPointText(ABuffer, BitmapSize, text, PointsOnBitmap[VPointsCount - 1], 9, FTextBGColor, FTextColor);
    end;
  end;
end;

procedure TCalcLineLayer.PreparePolygon(ALocalConverter: ILocalCoordConverter);
var
  VPointsCount: Integer;
  i: Integer;
  VDatum: IDatum;
begin
  inherited;
  VPointsCount := Length(SourcePolygon);
  if VPointsCount > 0 then begin
    VDatum := ALocalConverter.GetGeoConverter.Datum;
    SetLength(FDistArray, VPointsCount);
    FDistArray[0] := 0;
    for i := 1 to VPointsCount - 1 do begin
      FDistArray[i] := FDistArray[i - 1] + VDatum.CalcDist(SourcePolygon[i - 1], SourcePolygon[i]);
    end;
  end;
end;

end.
