unit u_CalcLineLayer;

interface

uses
  Types,
  Classes,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_ViewPortState,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LineOnMapEdit,
  i_ProjectionInfo,
  i_DoublePointsAggregator,
  i_ValueToStringConverter,
  i_PointCaptionsLayerConfig,
  u_MapLayerBasic;

type
  TCalcLineLayer = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IPointCaptionsLayerConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FLineOnMapEdit: IPathOnMapEdit;

    FTempBitmap: TBitmap32;
    FTempLastPointBitmap: TBitmap32;

    FLine: ILonLatPathWithSelected;
    FNeedUpdatePoints: Boolean;
    FProjection: IProjectionInfo;
    FProjectedPoints: IDoublePointsAggregator;
    FDistStrings: TStringList;
    FTextSizeArray: TArrayOfPoint;

    procedure DrawPointText(
      ABuffer: TBitmap32;
      const ABitmapSize: TPoint;
      const AText: string;
      const ATextSize: TSize;
      const APosOnBitmap: TDoublePoint;
      const AFontSize: Integer;
      const ATextBGColor: TColor32;
      const ATextColor: TColor32
    );
    procedure OnConfigChange;
    procedure OnLineChange;
  protected
    procedure ChangedSource;
    procedure PreparePoints(
      const AProjection: IProjectionInfo;
      out AProjectedPoints: IDoublePointsAggregator;
      out ADistStrings: TStringList;
      out ATextSizeArray: TArrayOfPoint
    );
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ALineOnMapEdit: IPathOnMapEdit;
      const AConfig: IPointCaptionsLayerConfig;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_Datum,
  i_CoordConverter,
  i_EnumDoublePoint,
  u_ListenerByEvent,
  u_GeoFun,
  u_DoublePointsAggregator,
  u_ResStrings;

{ TCalcLineLayer }

constructor TCalcLineLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AConfig: IPointCaptionsLayerConfig;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FConfig := AConfig;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FValueToStringConverterConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
  FTempBitmap := TBitmap32.Create;
  FTempBitmap.Font.Size := 7;
  FTempLastPointBitmap := TBitmap32.Create;
  FTempLastPointBitmap.Font.Size := 9;
end;

destructor TCalcLineLayer.Destroy;
begin
  FreeAndNil(FDistStrings);
  FreeAndNil(FTempBitmap);
  FreeAndNil(FTempLastPointBitmap);
  inherited;
end;

procedure TCalcLineLayer.ChangedSource;
begin
  FNeedUpdatePoints := True;
end;

procedure TCalcLineLayer.DrawPointText(
  ABuffer: TBitmap32;
  const ABitmapSize: TPoint;
  const AText: string;
  const ATextSize: TSize;
  const APosOnBitmap: TDoublePoint;
  const AFontSize: Integer;
  const ATextBGColor: TColor32;
  const ATextColor: TColor32
);
var
  VRect: TRect;
begin
  if (APosOnBitmap.x > 0) and
    (APosOnBitmap.y > 0) and
    (APosOnBitmap.x < ABitmapSize.X) and
    (APosOnBitmap.y < ABitmapSize.Y) then begin
    ABuffer.Font.Size := AFontSize;
    VRect.Left := Trunc(APosOnBitmap.x + 12);
    VRect.Top := Trunc(APosOnBitmap.Y);
    VRect.Right := VRect.Left + ATextSize.cx + 4;
    VRect.Bottom := VRect.Top + ATextSize.cy + 4;
    ABuffer.FillRectTS(VRect, ATextBGColor);
    ABuffer.RenderText(VRect.Left + 2, VRect.Top + 2, AText, 3, ATextColor);
  end;
end;

procedure TCalcLineLayer.OnConfigChange;
var
  VConfig: IPointCaptionsLayerConfigStatic;
begin
  ViewUpdateLock;
  try
    VConfig := FConfig.GetStatic;
    FTempBitmap.Font.Size := VConfig.FontSize;
    FTempLastPointBitmap.Font.Size := VConfig.LastPointFontSize;
    Visible := VConfig.Visible and (FLine <> nil) and (FLine.Count > 0);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TCalcLineLayer.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Path;
    if FLine.Count > 0 then begin
      SetNeedRedraw;
      Visible := FConfig.Visible;
    end else begin
      Hide;
    end;
    ChangedSource;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TCalcLineLayer.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VConfig: IPointCaptionsLayerConfigStatic;
  VProjection: IProjectionInfo;
  VPoints: IDoublePointsAggregator;
  VDistStrings: TStringList;
  VDistStringsNew: TStringList;
  VTextSizeArray: TArrayOfPoint;
  VNeedUpdatePoints: Boolean;
  VLocalRect: TRect;
  VBitmapSize: TPoint;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  VText: string;
  VTextSize: TSize;
  i: Integer;
begin
  inherited;
  VConfig := FConfig.GetStatic;
  VProjection := FProjection;
  VPoints := FProjectedPoints;
  VTextSizeArray := FTextSizeArray;
  VDistStrings := FDistStrings;
  VNeedUpdatePoints := FNeedUpdatePoints;
  if not VNeedUpdatePoints then begin
    if (VProjection = nil) or (VPoints = nil) then begin
      VNeedUpdatePoints := True;
    end else begin
      if not VProjection.GetIsSameProjectionInfo(ALocalConverter.ProjectionInfo) then begin
        VNeedUpdatePoints := True;
      end;
    end;
  end;
  if VNeedUpdatePoints then begin
    VProjection := ALocalConverter.ProjectionInfo;
    PreparePoints(VProjection, VPoints, VDistStringsNew, VTextSizeArray);
    FProjectedPoints := VPoints;
    FProjection := VProjection;
    FTextSizeArray := VTextSizeArray;
    FDistStrings := VDistStringsNew;
    FNeedUpdatePoints := False;
    VDistStrings.Free;
    VDistStrings := VDistStringsNew;
  end;

  if (VPoints = nil) or (FDistStrings = nil) or (FTextSizeArray = nil) then begin
    Exit;
  end;

  if VPoints.Count > 0 then begin
    VLocalRect := ALocalConverter.GetLocalRect;
    VBitmapSize.X := VLocalRect.Right - VLocalRect.Left;
    VBitmapSize.Y := VLocalRect.Bottom - VLocalRect.Top;
    if not VConfig.ShowLastPointOnly then begin
      for i := 0 to VPoints.Count - 2 do begin
        VText := VDistStrings[i];
        VTextSize.cx := VTextSizeArray[i].X;
        VTextSize.cy := VTextSizeArray[i].Y;
        VPosOnMap := VPoints.Points[i];
        VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
        DrawPointText(
          ABuffer,
          VBitmapSize,
          VText,
          VTextSize,
          VPosOnBitmap,
          VConfig.FontSize,
          VConfig.TextBGColor,
          VConfig.TextColor
        );
      end;
    end;
    i := VPoints.Count - 1;
    VText := VDistStrings[i];
    VTextSize.cx := VTextSizeArray[i].X;
    VTextSize.cy := VTextSizeArray[i].Y;
    VPosOnMap := VPoints.Points[i];
    VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
    DrawPointText(
      ABuffer,
      VBitmapSize,
      VText,
      VTextSize,
      VPosOnBitmap,
      VConfig.LastPointFontSize,
      VConfig.TextBGColor,
      VConfig.TextColor
    );
  end;
end;

procedure TCalcLineLayer.PreparePoints(
  const AProjection: IProjectionInfo;
  out AProjectedPoints: IDoublePointsAggregator;
  out ADistStrings: TStringList;
  out ATextSizeArray: TArrayOfPoint
);
var
  VLine: ILonLatPathWithSelected;
  VEnum: IEnumLonLatPoint;
  VConverter: ICoordConverter;
  VDatum: IDatum;
  VZoom: Byte;
  VCurrLonLat: TDoublePoint;
  VCurrIsEmpty: Boolean;
  VCurrProjected: TDoublePoint;
  VPrevLonLat: TDoublePoint;
  VPrevIsEmpty: Boolean;
  VPrevProjected: TDoublePoint;
  VDist: Double;
  VTotalDist: Double;
  VLonLat: TDoublePoint;
  VSkipPoint: Boolean;
  VText: string;
  VTextSize: TSize;
  VValueConverter: IValueToStringConverter;
  VStartAzimuth, VFinishAzimuth: Double;
  VAzimuth: string;
begin
  AProjectedPoints := nil;
  ADistStrings := nil;
  FTextSizeArray := nil;
  VLine := FLine;
  if VLine <> nil then begin
    VTotalDist := 0;
    VValueConverter := FValueToStringConverterConfig.GetStatic;
    VConverter := AProjection.GeoConverter;
    VZoom := AProjection.Zoom;
    VDatum := VConverter.Datum;
    ADistStrings := TStringList.Create;
    AProjectedPoints := TDoublePointsAggregator.Create;
    VEnum := VLine.GetEnum;
    if VEnum.Next(VPrevLonLat) then begin
      VSkipPoint := False;
      VPrevIsEmpty := False;
      VLonLat := VPrevLonLat;
      VConverter.CheckLonLatPos(VLonLat);
      VPrevProjected := VConverter.LonLat2PixelPosFloat(VLonLat, VZoom);
      while VEnum.Next(VCurrLonLat) do begin
        VCurrIsEmpty := PointIsEmpty(VCurrLonLat);
        if VCurrIsEmpty then begin
          VCurrProjected := VPrevProjected;
        end else begin
          if not VPrevIsEmpty then begin
            VDist := VDatum.CalcDist(VPrevLonLat, VCurrLonLat, VStartAzimuth, VFinishAzimuth);
            VTotalDist := VTotalDist + VDist;
          end;
          VLonLat := VCurrLonLat;
          VConverter.CheckLonLatPos(VLonLat);
          VCurrProjected := VConverter.LonLat2PixelPosFloat(VLonLat, VZoom);
          VSkipPoint :=
            (
            (abs(VPrevProjected.X - VCurrProjected.X) < 60) and
            (abs(VPrevProjected.Y - VCurrProjected.Y) < 15)
            );
          if not VSkipPoint then begin
            AProjectedPoints.Add(VCurrProjected);
            VText := VValueConverter.DistConvert(VTotalDist);
            ADistStrings.Add(VText);
            if Length(ATextSizeArray) < AProjectedPoints.Count then begin
              SetLength(ATextSizeArray, AProjectedPoints.Count);
            end;
            VTextSize := FTempBitmap.TextExtent(VText);
            ATextSizeArray[AProjectedPoints.Count - 1].X := VTextSize.cx;
            ATextSizeArray[AProjectedPoints.Count - 1].Y := VTextSize.cy;
            VPrevProjected := VCurrProjected;
          end;
        end;
        VPrevLonLat := VCurrLonLat;
        VPrevIsEmpty := VCurrIsEmpty;
      end;
      if VSkipPoint then begin
        AProjectedPoints.Add(VCurrProjected);
        VText := VValueConverter.DistConvert(VTotalDist);
        ADistStrings.Add(VText);
        if Length(ATextSizeArray) < AProjectedPoints.Count then begin
          SetLength(ATextSizeArray, AProjectedPoints.Count);
        end;
        VTextSize := FTempBitmap.TextExtent(VText);
        ATextSizeArray[AProjectedPoints.Count - 1].X := VTextSize.cx;
        ATextSizeArray[AProjectedPoints.Count - 1].Y := VTextSize.cy;
        VPrevProjected := VCurrProjected;
      end;
    end;
    if AProjectedPoints.Count > 0 then begin
      if FConfig.ShowAzimuth then begin
        VAzimuth := ' ' + SAS_STR_Azimuth + ': ' +
          FloatToStrF(VStartAzimuth, ffNumber, 12, 2) + #176; // #176 - degree symbol
      end else begin
        VAzimuth := '';
      end;
      VText := SAS_STR_Whole + ': ' + VValueConverter.DistConvert(VTotalDist) + VAzimuth;
      ADistStrings[AProjectedPoints.Count - 1] := VText;
      VTextSize := FTempLastPointBitmap.TextExtent(VText);
      ATextSizeArray[AProjectedPoints.Count - 1].X := VTextSize.cx;
      ATextSizeArray[AProjectedPoints.Count - 1].Y := VTextSize.cy;
    end;
  end;
end;

procedure TCalcLineLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
