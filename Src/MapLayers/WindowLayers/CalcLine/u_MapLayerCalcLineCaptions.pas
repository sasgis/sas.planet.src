{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MapLayerCalcLineCaptions;

interface

uses
  Types,
  Classes,
  SysUtils,
  Contnrs,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_GeoCalc,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LineOnMapEdit,
  i_GeometryLonLat,
  i_Projection,
  i_DoublePointsAggregator,
  i_ValueToStringConverter,
  i_PointCaptionsLayerConfig,
  i_MainFormState,
  u_MapCaptionDrawable,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerCalcLineCaptions = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FConfig: IPointCaptionsLayerConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FLineOnMapEdit: IPathOnMapEdit;

    FGeoCalc: IGeoCalc;
    FGeoCalcChangeable: IGeoCalcChangeable;

    FLine: ILonLatPathWithSelected;
    FNeedUpdatePoints: Boolean;
    FProjection: IProjection;
    FPoints: IDoublePointsAggregator;
    FPointsText: TStringDynArray;

    FIsValid: Boolean;
    FRect: TRect;
    FPos: TArrayOfPoint;
    FCaptions: TObjectList;
    FPointsCount: Integer;

    function GetCaption(
      const AIndex: Integer;
      const ACreateIfNotExists: Boolean
    ): TMapCaptionDrawable;

    procedure OnConfigChange;
    procedure OnLineChange;

    procedure _PrepareSingleLine(
      const AValueConverter: IValueToStringConverter;
      const ALine: IGeometryLonLatSingleLine;
      const AProjection: IProjection;
      const AIsFinalize: Boolean;
      var AProjectedPoints: IDoublePointsAggregator;
      var ALastStartAzimuth: Double;
      var ATotalDist: Double;
      var APointsText: TStringDynArray
    );
    procedure _PrepareGeometry(
      const AValueConverter: IValueToStringConverter;
      const ALine: IGeometryLonLatLine;
      const AProjection: IProjection;
      var AProjectedPoints: IDoublePointsAggregator;
      var APointsText: TStringDynArray
    );
    procedure PreparePoints(
      const AProjection: IProjection;
      out AProjectedPoints: IDoublePointsAggregator;
      var APointsText: TStringDynArray
    );
  protected
    procedure InvalidateLayer(const ALocalConverter: ILocalCoordConverter); override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AMainFormState: IMainFormState;
      const ALineOnMapEdit: IPathOnMapEdit;
      const AConfig: IPointCaptionsLayerConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AGeoCalc: IGeoCalcChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  i_Datum,
  i_ProjectionType,
  u_GeoFunc,
  u_ListenerByEvent,
  u_DoublePointsAggregator,
  u_ResStrings;

function AzimuthToString(const AAzimuth: Double): string; inline;
const
  cDegreeSymbol = #176;
begin
  Result := FloatToStrF(AAzimuth, ffNumber, 15, 2) + cDegreeSymbol;
end;

{ TMapLayerCalcLineCaptions }

constructor TMapLayerCalcLineCaptions.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AConfig: IPointCaptionsLayerConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AGeoCalc: IGeoCalcChangeable
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );

  FMainFormState := AMainFormState;
  FConfig := AConfig;
  FValueToStringConverter := AValueToStringConverter;
  FLineOnMapEdit := ALineOnMapEdit;
  FGeoCalcChangeable := AGeoCalc;

  FCaptions := TObjectList.Create(True);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FValueToStringConverter.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FGeoCalcChangeable.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.ChangeNotifier
  );
end;

destructor TMapLayerCalcLineCaptions.Destroy;
begin
  FreeAndNil(FCaptions);
  inherited Destroy;
end;

function TMapLayerCalcLineCaptions.GetCaption(
  const AIndex: Integer;
  const ACreateIfNotExists: Boolean
): TMapCaptionDrawable;
begin
  if FCaptions.Count > AIndex then begin
    Result := TMapCaptionDrawable(FCaptions.Items[AIndex]);
  end else
  if ACreateIfNotExists then begin
    Result := TMapCaptionDrawable.Create;
    FCaptions.Add(Result);
  end else begin
    raise Exception.CreateFmt(
      Self.ClassName + ': Caption index out of range (%d of %d)', [AIndex, FCaptions.Count]
    );
  end;
end;

procedure TMapLayerCalcLineCaptions.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FNeedUpdatePoints := True;
    FGeoCalc := FGeoCalcChangeable.GetStatic;
    Visible := FConfig.Visible and Assigned(FLine);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerCalcLineCaptions.OnLineChange;
begin
  ViewUpdateLock;
  try
    FNeedUpdatePoints := True;
    FLine := FLineOnMapEdit.Path;
    Visible := FConfig.Visible and Assigned(FLine);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerCalcLineCaptions.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);
var
  I: Integer;
  VConfig: IPointCaptionsLayerConfigStatic;
  VNeedUpdatePoints: Boolean;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  VCaption: TMapCaptionDrawable;
  VIndex: Integer;
  VLastPointIndex: Integer;
  VRect: TRect;
  VLocalRect: TRect;
  VFontSize: Integer;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  if not Visible then begin
    Exit;
  end;

  VNeedUpdatePoints :=
    FNeedUpdatePoints or
    (FPoints = nil) or
    (FPointsText = nil) or
    (FProjection = nil) or
    not FProjection.IsSame(ALocalConverter.Projection);

  if VNeedUpdatePoints then begin
    FProjection := ALocalConverter.Projection;
    PreparePoints(FProjection, FPoints, FPointsText);
    FNeedUpdatePoints := False;
  end;

  if (FPoints <> nil) and (FPoints.Count > 0) then begin
    Assert(Length(FPointsText) >= FPoints.Count);

    VConfig := FConfig.GetStatic;
    VLocalRect := ALocalConverter.GetLocalRect;
    FRect := GR32.MakeRect(0, 0, 0, 0);

    if Length(FPos) < FPoints.Count then begin
      SetLength(FPos, FPoints.Count);
    end;

    VIndex := 0;
    VLastPointIndex := FPoints.Count - 1;

    for I := 0 to FPoints.Count - 1 do begin

      if FPointsText[I] = '' then begin
        // there is no caption for the current point
        Continue;
      end;

      VPosOnMap := FPoints.Points[I];
      VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
      FPos[VIndex] := PointFromDoublePoint(VPosOnBitmap, prToTopLeft);

      if not GR32.PtInRect(VLocalRect, FPos[VIndex]) then begin
        // current point is out of the screen
        Continue;
      end;

      if I = VLastPointIndex then begin
        VFontSize := VConfig.LastPointFontSize;
      end else begin
        VFontSize := VConfig.FontSize;
      end;

      VCaption := GetCaption(VIndex, True);

      VCaption.SetText(
        FPointsText[I],
        VConfig.TextBGColor,
        VConfig.FontName,
        VFontSize,
        VConfig.TextColor
      );

      VRect := VCaption.GetBoundsForPosition(FPos[VIndex]);

      if GR32.IsRectEmpty(VRect) or not GR32.IntersectRect(VRect, VRect, VLocalRect) then begin
        Continue;
      end;

      if not GR32.IsRectEmpty(FRect) then begin
        GR32.UnionRect(FRect, FRect, VRect);
      end else begin
        FRect := VRect;
      end;

      Inc(VIndex);
    end;

    FPointsCount := VIndex;

    FIsValid := (FPointsCount > 0) and not GR32.IsRectEmpty(FRect);
    if not FIsValid then begin
      Exit;
    end;

    // draw
    if FMainFormState.IsMapMoving then begin
      DoInvalidateFull;
    end else begin
      DoInvalidateRect(FRect);
    end;
  end;
end;

procedure TMapLayerCalcLineCaptions.PaintLayer(ABuffer: TBitmap32);
var
  I: Integer;
  VCaption: TMapCaptionDrawable;
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      Assert(FCaptions.Count >= FPointsCount);
      Assert(Length(FPos) >= FPointsCount);

      ABuffer.BeginUpdate;
      try
        for I := 0 to FPointsCount - 1 do begin
          VCaption := GetCaption(I, False);
          VCaption.DrawToBitmap(ABuffer, FPos[I]);
        end;
      finally
        ABuffer.EndUpdate;
      end;
    end;
  end;
end;

procedure TMapLayerCalcLineCaptions._PrepareSingleLine(
  const AValueConverter: IValueToStringConverter;
  const ALine: IGeometryLonLatSingleLine;
  const AProjection: IProjection;
  const AIsFinalize: Boolean;
  var AProjectedPoints: IDoublePointsAggregator;
  var ALastStartAzimuth, ATotalDist: Double;
  var APointsText: TStringDynArray
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
  VProjectionType: IProjectionType;
  VDatum: IDatum;
  VTextItems: array [0..2] of TTextItemInfo;
  VPointIndex: Integer;
begin
  VCount := ALine.Count;
  VPoints := ALine.Points;

  if VCount <= 1 then begin
    Exit;
  end;

  if Length(APointsText) < AProjectedPoints.Count + VCount then begin
    SetLength(APointsText, AProjectedPoints.Count + VCount);
  end;

  VPointIndex := AProjectedPoints.Count;

  VDatum := FGeoCalc.Datum;

  VPrevLonLat := VPoints[0];
  VLonLat := VPrevLonLat;
  VProjectionType := AProjection.ProjectionType;
  VProjectionType.ValidateLonLatPos(VLonLat);
  VPrevProjected := AProjection.LonLat2PixelPosFloat(VLonLat);
  VDistSkipped := 0;

  VTextItems[0].Enabled := FConfig.ShowIntermediateDist;
  VTextItems[1].Enabled := FConfig.ShowDistIncrement;
  VTextItems[2].Enabled := FConfig.ShowAzimuth;

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
        VTextItems[0].Text := Format(SAS_STR_Whole, [AValueConverter.DistConvert(ATotalDist)]);
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
        VTextItems[2].Text := '; ' + Format(SAS_STR_Azimuth, [AzimuthToString(ALastStartAzimuth)]);
      end else begin
        VTextItems[2].Text := '; ' + AzimuthToString(ALastStartAzimuth);
      end;
    end else begin
      VTextItems[2].Text := '';
    end;

    VText := VTextItems[0].Text + VTextItems[1].Text + VTextItems[2].Text;

    APointsText[VPointIndex] := VText;
    Inc(VPointIndex);
  end;
end;

procedure TMapLayerCalcLineCaptions._PrepareGeometry(
  const AValueConverter: IValueToStringConverter;
  const ALine: IGeometryLonLatLine;
  const AProjection: IProjection;
  var AProjectedPoints: IDoublePointsAggregator;
  var APointsText: TStringDynArray
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
      AProjectedPoints,
      VLastStartAzimuth,
      VTotalDist,
      APointsText
    );
  end else
  if Supports(ALine, IGeometryLonLatMultiLine, VMultiLine) then begin
    for I := 0 to VMultiLine.Count - 1 do begin
      _PrepareSingleLine(
        AValueConverter,
        VMultiLine.Item[I],
        AProjection,
        (I = VMultiLine.Count - 1),
        AProjectedPoints,
        VLastStartAzimuth,
        VTotalDist,
        APointsText
      );
    end;
  end else begin
    Assert(False);
  end;
end;

procedure TMapLayerCalcLineCaptions.PreparePoints(
  const AProjection: IProjection;
  out AProjectedPoints: IDoublePointsAggregator;
  var APointsText: TStringDynArray
);
var
  VLine: ILonLatPathWithSelected;
  VValueConverter: IValueToStringConverter;
begin
  VLine := FLine;
  if VLine <> nil then begin
    VValueConverter := FValueToStringConverter.GetStatic;
    AProjectedPoints := TDoublePointsAggregator.Create;
    _PrepareGeometry(
      VValueConverter,
      VLine.Geometry,
      AProjection,
      AProjectedPoints,
      APointsText
    );
  end else begin
    AProjectedPoints := nil;
    APointsText := nil;
  end;
end;

procedure TMapLayerCalcLineCaptions.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
