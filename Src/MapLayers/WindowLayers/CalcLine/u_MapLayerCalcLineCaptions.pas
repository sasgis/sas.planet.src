{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MapLayerCalcLineCaptions;

interface

uses
  Types,
  Classes,
  SysUtils,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LineOnMapEdit,
  i_Datum,
  i_GeometryLonLat,
  i_Projection,
  i_DoublePointsAggregator,
  i_ValueToStringConverter,
  i_PointCaptionsLayerConfig,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerCalcLineCaptions = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IPointCaptionsLayerConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FLineOnMapEdit: IPathOnMapEdit;

    FTempBitmap: TBitmap32;
    FTempLastPointBitmap: TBitmap32;

    FLine: ILonLatPathWithSelected;
    FNeedUpdatePoints: Boolean;
    FProjection: IProjection;
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
      const AFontName: string;
      const ATextBGColor: TColor32;
      const ATextColor: TColor32
    );
    procedure OnConfigChange;
    procedure OnLineChange;
    procedure _PrepareSingleLine(
      const AValueConverter: IValueToStringConverter;
      const ALine: IGeometryLonLatSingleLine;
      const AProjection: IProjection;
      const AIsFinalize: Boolean;
      var ATextSizeArray: TArrayOfPoint;
      var AProjectedPoints: IDoublePointsAggregator;
      var ALastStartAzimuth: Double;
      var ATotalDist: Double;
      var ADistStrings: TStringList
    );
    procedure _PrepareGeometry(
      const AValueConverter: IValueToStringConverter;
      const ALine: IGeometryLonLatLine;
      const AProjection: IProjection;
      var ATextSizeArray: TArrayOfPoint;
      var AProjectedPoints: IDoublePointsAggregator;
      var ADistStrings: TStringList
    );
  protected
    procedure ChangedSource;
    procedure PreparePoints(
      const AProjection: IProjection;
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
      const AValueToStringConverter: IValueToStringConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_ProjectionType,
  u_ListenerByEvent,
  u_DoublePointsAggregator,
  u_ResStrings;

function AzimuthToString(const AAzimuth: Double): string; inline;
const
  cDegreeSymbol = #176;
begin
  Result := FloatToStrF(AAzimuth, ffNumber, 12, 2) + cDegreeSymbol;
end;

{ TMapLayerCalcLineCaptions }

constructor TMapLayerCalcLineCaptions.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AConfig: IPointCaptionsLayerConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable
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
  FValueToStringConverter := AValueToStringConverter;
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FValueToStringConverter.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
  FTempBitmap := TBitmap32.Create;
  FTempLastPointBitmap := TBitmap32.Create;
end;

destructor TMapLayerCalcLineCaptions.Destroy;
begin
  FreeAndNil(FDistStrings);
  FreeAndNil(FTempBitmap);
  FreeAndNil(FTempLastPointBitmap);
  inherited;
end;

procedure TMapLayerCalcLineCaptions.ChangedSource;
begin
  FNeedUpdatePoints := True;
end;

procedure TMapLayerCalcLineCaptions.DrawPointText(
  ABuffer: TBitmap32;
  const ABitmapSize: TPoint;
  const AText: string;
  const ATextSize: TSize;
  const APosOnBitmap: TDoublePoint;
  const AFontSize: Integer;
  const AFontName: string;
  const ATextBGColor: TColor32;
  const ATextColor: TColor32
);
var
  VRect: TRect;
begin
  if (APosOnBitmap.X > 0) and (APosOnBitmap.X < ABitmapSize.X) and
     (APosOnBitmap.Y > 0) and (APosOnBitmap.Y < ABitmapSize.Y)
  then begin
    VRect.Left := Trunc(APosOnBitmap.X + 12);
    VRect.Top := Trunc(APosOnBitmap.Y);
    VRect.Right := VRect.Left + ATextSize.cx + 4;
    VRect.Bottom := VRect.Top + ATextSize.cy + 4;
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(VRect);
    end else begin
      ABuffer.FillRectTS(VRect, ATextBGColor);
      ABuffer.Font.Size := AFontSize;
      ABuffer.Font.Name := AFontName;
      ABuffer.Font.Color := WinColor(ATextColor);
      ABuffer.Textout(VRect.Left + 2, VRect.Top + 2, AText);
    end;
  end;
end;

procedure TMapLayerCalcLineCaptions.OnConfigChange;
var
  VConfig: IPointCaptionsLayerConfigStatic;
begin
  ViewUpdateLock;
  try
    VConfig := FConfig.GetStatic;
    FTempBitmap.Font.Size := VConfig.FontSize;
    FTempBitmap.Font.Name := VConfig.FontName;
    FTempLastPointBitmap.Font.Size := VConfig.LastPointFontSize;
    FTempLastPointBitmap.Font.Name := VConfig.FontName;
    Visible := VConfig.Visible and Assigned(FLine);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerCalcLineCaptions.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Path;
    if Assigned(FLine) then begin
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

procedure TMapLayerCalcLineCaptions.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  I: Integer;
  VConfig: IPointCaptionsLayerConfigStatic;
  VProjection: IProjection;
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
      if not VProjection.IsSame(ALocalConverter.Projection) then begin
        VNeedUpdatePoints := True;
      end;
    end;
  end;
  if VNeedUpdatePoints then begin
    VProjection := ALocalConverter.Projection;
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
    if VConfig.ShowIntermediateDist or VConfig.ShowDistIncrement then begin
      for I := 0 to VPoints.Count - 2 do begin
        VText := VDistStrings[I];
        VTextSize.cx := VTextSizeArray[I].X;
        VTextSize.cy := VTextSizeArray[I].Y;
        VPosOnMap := VPoints.Points[I];
        VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
        DrawPointText(
          ABuffer,
          VBitmapSize,
          VText,
          VTextSize,
          VPosOnBitmap,
          VConfig.FontSize,
          VConfig.FontName,
          VConfig.TextBGColor,
          VConfig.TextColor
        );
      end;
    end;
    I := VPoints.Count - 1;
    VText := VDistStrings[I];
    VTextSize.cx := VTextSizeArray[I].X;
    VTextSize.cy := VTextSizeArray[I].Y;
    VPosOnMap := VPoints.Points[I];
    VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
    DrawPointText(
      ABuffer,
      VBitmapSize,
      VText,
      VTextSize,
      VPosOnBitmap,
      VConfig.LastPointFontSize,
      VConfig.FontName,
      VConfig.TextBGColor,
      VConfig.TextColor
    );
  end;
end;

procedure TMapLayerCalcLineCaptions._PrepareSingleLine(
  const AValueConverter: IValueToStringConverter;
  const ALine: IGeometryLonLatSingleLine;
  const AProjection: IProjection;
  const AIsFinalize: Boolean;
  var ATextSizeArray: TArrayOfPoint;
  var AProjectedPoints: IDoublePointsAggregator;
  var ALastStartAzimuth, ATotalDist: Double;
  var ADistStrings: TStringList
);
type
  TTextItemInfo = record
    Enabled: Boolean;
    Text: string;
  end;
var
  I: Integer;
  VCount: Integer;
  VPoints: PDoublePointArray;
  VIsLastPoint: Boolean;
  VFinishAzimuth: Double;
  VCurrLonLat: TDoublePoint;
  VCurrProjected: TDoublePoint;
  VDist: Double;
  VDistSkipped: Double;
  VLonLat: TDoublePoint;
  VPrevLonLat: TDoublePoint;
  VPrevProjected: TDoublePoint;
  VText: string;
  VTextSize: TSize;
  VProjectionType: IProjectionType;
  VDatum: IDatum;
  VBitmap: array[Boolean] of TBitmap32;
  VTextItems: array [0..2] of TTextItemInfo;
begin
  VCount := ALine.Count;
  VPoints := ALine.Points;

  if VCount <= 1 then begin
    Exit;
  end;

  if Length(ATextSizeArray) < AProjectedPoints.Count + VCount then begin
    SetLength(ATextSizeArray, AProjectedPoints.Count + VCount);
  end;

  VProjectionType := AProjection.ProjectionType;
  VDatum := VProjectionType.Datum;
  VPrevLonLat := VPoints[0];
  VLonLat := VPrevLonLat;
  VProjectionType.ValidateLonLatPos(VLonLat);
  VPrevProjected := AProjection.LonLat2PixelPosFloat(VLonLat);
  VDistSkipped := 0;

  VTextItems[0].Enabled := FConfig.ShowIntermediateDist;
  VTextItems[1].Enabled := FConfig.ShowDistIncrement;
  VTextItems[2].Enabled := FConfig.ShowAzimuth;

  VBitmap[False] := FTempBitmap;
  VBitmap[True] := FTempLastPointBitmap;

  for I := 1 to VCount - 1 do begin
    VCurrLonLat := VPoints[I];

    VDist := VDatum.CalcDist(VPrevLonLat, VCurrLonLat, ALastStartAzimuth, VFinishAzimuth);
    ATotalDist := ATotalDist + VDist;

    VPrevLonLat := VCurrLonLat;
    VLonLat := VCurrLonLat;
    VProjectionType.ValidateLonLatPos(VLonLat);
    VCurrProjected := AProjection.LonLat2PixelPosFloat(VLonLat);

    VIsLastPoint := (I = VCount - 1);

    if not VIsLastPoint then begin
      if (Abs(VPrevProjected.X - VCurrProjected.X) < 60) and
         (Abs(VPrevProjected.Y - VCurrProjected.Y) < 15)
      then begin
        VDistSkipped := VDistSkipped + VDist;
        Continue; // skip nearest points
      end else begin
        VPrevProjected := VCurrProjected;
      end;
    end;

    AProjectedPoints.Add(VCurrProjected);

    VIsLastPoint := VIsLastPoint and AIsFinalize;

    if VTextItems[0].Enabled or VIsLastPoint then begin
      if VIsLastPoint then begin
        VTextItems[0].Text := SAS_STR_Whole + ': ' + AValueConverter.DistConvert(ATotalDist);
      end else begin
        VTextItems[0].Text := AValueConverter.DistConvert(ATotalDist);
      end;
    end else begin
      VTextItems[0].Text := '';
    end;

    if VTextItems[1].Enabled and ((I > 1) or (VTextItems[0].Text = '')) then begin
      if VTextItems[0].Text <> '' then begin
        VTextItems[1].Text := ' (+' + AValueConverter.DistConvert(VDistSkipped + VDist) + ')';
      end else begin
        VTextItems[1].Text := '+' + AValueConverter.DistConvert(VDistSkipped + VDist);
      end;
    end else begin
      VTextItems[1].Text := '';
    end;
    VDistSkipped := 0;

    if VTextItems[2].Enabled and ((VTextItems[0].Text <> '') or (VTextItems[1].Text <> '')) then begin
      if VIsLastPoint then begin
        VTextItems[2].Text := '; ' + SAS_STR_Azimuth + ': ' + AzimuthToString(ALastStartAzimuth);
      end else begin
        VTextItems[2].Text := '; ' + AzimuthToString(ALastStartAzimuth);
      end;
    end else begin
      VTextItems[2].Text := '';
    end;

    VText := VTextItems[0].Text + VTextItems[1].Text + VTextItems[2].Text;
    ADistStrings.Add(VText);

    VTextSize := VBitmap[VIsLastPoint].TextExtent(VText);
    ATextSizeArray[AProjectedPoints.Count - 1].X := VTextSize.cx;
    ATextSizeArray[AProjectedPoints.Count - 1].Y := VTextSize.cy;
  end;
end;

procedure TMapLayerCalcLineCaptions._PrepareGeometry(
  const AValueConverter: IValueToStringConverter;
  const ALine: IGeometryLonLatLine;
  const AProjection: IProjection;
  var ATextSizeArray: TArrayOfPoint;
  var AProjectedPoints: IDoublePointsAggregator;
  var ADistStrings: TStringList
);
var
  I: Integer;
  VTotalDist: Double;
  VLastStartAzimuth: Double;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  VTotalDist := 0;
  VLastStartAzimuth := 0;
  if Supports(ALine, IGeometryLonLatSingleLine, VSingleLine) then begin
    _PrepareSingleLine(
      AValueConverter,
      VSingleLine,
      AProjection,
      True,
      ATextSizeArray,
      AProjectedPoints,
      VLastStartAzimuth,
      VTotalDist,
      ADistStrings
    );
  end else if Supports(ALine, IGeometryLonLatMultiLine, VMultiLine) then begin
    for I := 0 to VMultiLine.Count - 1 do begin
      _PrepareSingleLine(
        AValueConverter,
        VMultiLine.Item[I],
        AProjection,
        (I = VMultiLine.Count - 1),
        ATextSizeArray,
        AProjectedPoints,
        VLastStartAzimuth,
        VTotalDist,
        ADistStrings
      );
    end;
  end else begin
    Assert(False);
  end;
end;

procedure TMapLayerCalcLineCaptions.PreparePoints(
  const AProjection: IProjection;
  out AProjectedPoints: IDoublePointsAggregator;
  out ADistStrings: TStringList;
  out ATextSizeArray: TArrayOfPoint
);
var
  VLine: ILonLatPathWithSelected;
  VValueConverter: IValueToStringConverter;
begin
  AProjectedPoints := nil;
  ADistStrings := nil;
  FTextSizeArray := nil;
  VLine := FLine;
  if VLine <> nil then begin
    VValueConverter := FValueToStringConverter.GetStatic;
    ADistStrings := TStringList.Create;
    AProjectedPoints := TDoublePointsAggregator.Create;
    _PrepareGeometry(
      VValueConverter,
      VLine.Geometry,
      AProjection,
      ATextSizeArray,
      AProjectedPoints,
      ADistStrings
    );
    SetLength(ATextSizeArray, AProjectedPoints.Count);
  end;
end;

procedure TMapLayerCalcLineCaptions.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
