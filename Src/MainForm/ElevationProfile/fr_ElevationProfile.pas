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

unit fr_ElevationProfile;

interface

{$IFDEF DEBUG}
  {.$DEFINE ENABLE_DIAGNOSTICS}
  {.$DEFINE SHOW_ELEV_TO_DIST_SCALE_INFO}
{$ENDIF}

{$IF CompilerVersion > 23.0}
  {$DEFINE HAS_TEE_DRAW_STYLE}
  {$DEFINE HAS_TEE_ZOOM_OPT}
{$IFEND}

uses
  {$IFDEF ENABLE_DIAGNOSTICS}
  Windows,
  Diagnostics,
  {$ENDIF}
  Types,
  SysUtils,
  Classes,
  Math,
  Controls,
  Graphics,
  Forms,
  Menus,
  ExtCtrls,
  StdCtrls,
  TBXDkPanels,
  TeeGDIPlus,
  TeEngine,
  TeeProcs,
  Chart,
  Series,
  t_GeoTypes,
  i_Datum,
  i_GeometryLonLat,
  i_LanguageManager,
  i_MapViewGoto,
  i_Listener,
  i_ElevationProfileConfig,
  u_CommonFormAndFrameParents;

type
  TProfileMinMaxAvgRec = record
    Min: Double;
    Max: Double;
    Avg: Double;
  end;

  TProfileSlopeRec = record
    Gain: Double;
    Loss: Double;

    GainPoints: Integer;
    LossPoints: Integer;
  end;

  TProfileInfoRec = record
    Dist: Double;
    Seconds: Int64;

    Elev: TProfileMinMaxAvgRec;
    ElevAscent: Double;
    ElevDescent: Double;

    MaxSlope: TProfileSlopeRec;
    AvgSlope: TProfileSlopeRec;

    Speed: TProfileMinMaxAvgRec;

    PointsCount: Integer;
  end;

  TProfilePointInfoRec = record
    IsValid: Boolean;

    Elev: Double;
    Dist: Double;
    Speed: Double;
    Slope: Double;

    MouseX: Integer;
    MouseY: Integer;

    LonLat: TDoublePoint;
    IsLonLatValid: Boolean;

    IsMapCenterd: Boolean;
  end;

  TOnCloseEvent = procedure() of object;
  TOnRefreshEvent = procedure() of object;

  TfrElevationProfile = class(TFrame)
    pnlTop: TPanel;
    lblInfo: TLabel;
    btnClose: TTBXButton;
    pmMain: TPopupMenu;
    mniShowSpeed: TMenuItem;
    mniShowElevation: TMenuItem;
    mniCenterMap: TMenuItem;
    mniResetZoom: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    chtProfile: TChart;
    pnlPointInfo: TPanel;
    lblPointInfo: TLabel;
    pnlPointLine: TPanel;
    mniFilterData: TMenuItem;
    mniKeepAspectRatio: TMenuItem;
    mniZoomWithMouseWheel: TMenuItem;
    mniScaleElevToDist: TMenuItem;
    mniElevationSource: TMenuItem;
    mniTrackData: TMenuItem;
    mniDEMData: TMenuItem;
    mniN3: TMenuItem;
    N4: TMenuItem;
    mniStatistics: TMenuItem;
    mniTrackName: TMenuItem;
    mniDistance: TMenuItem;
    mniDuration: TMenuItem;
    mniElevationMinAvgMax: TMenuItem;
    mniAscentDescent: TMenuItem;
    mniMaxSlope: TMenuItem;
    mniAverageSlope: TMenuItem;
    mniSpeedMinAvgMax: TMenuItem;
    procedure btnCloseClick(Sender: TObject);
    procedure mniShowSpeedClick(Sender: TObject);
    procedure mniResetZoomClick(Sender: TObject);
    procedure mniShowElevationClick(Sender: TObject);
    procedure mniCenterMapClick(Sender: TObject);
    procedure chtProfileAfterDraw(Sender: TObject);
    procedure chtProfileMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chtProfileResize(Sender: TObject);
    procedure chtProfileZoom(Sender: TObject);
    procedure chtProfileScroll(Sender: TObject);
    procedure chtProfileContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);

    procedure chtProfileUndoZoom(Sender: TObject);
    procedure mniFilterDataClick(Sender: TObject);
    procedure mniKeepAspectRatioClick(Sender: TObject);
    procedure mniZoomWithMouseWheelClick(Sender: TObject);
    procedure mniScaleElevToDistClick(Sender: TObject);
    procedure mniTrackDataClick(Sender: TObject);
    procedure mniDEMDataClick(Sender: TObject);
    procedure mniStatisticsItemClick(Sender: TObject);
  private
    FDatum: IDatum;
    FMapGoTo: IMapViewGoto;
    FOnClose: TOnCloseEvent;
    FOnRefresh: TOnRefreshEvent;

    FConfig: IElevationProfileConfig;
    FConfigStatic: IElevationProfileConfigStatic;
    FConfigChangeListener: IListener;

    FLines: TArrayOfGeometryLonLatSingleLine;
    FDist: array of Double;
    FIsDistInMeters: Boolean;

    FInfo: TProfileInfoRec;
    FPointInfo: TProfilePointInfoRec;

    FElevationSeries: TAreaSeries;
    FSpeedSeries: TAreaSeries;

    FAxisValuesFormatDef: string;
    FFormatSettings: TFormatSettings;

    FTrackName: string;
  private
    procedure SetupChart;

    procedure ShowSeries;
    procedure FillSeriesWithLineData(const ALine: IGeometryLonLatSingleLine);
    procedure ResetAxesMinMax;
    procedure ScaleElevToDistAsOneToOne;
    procedure BeginUpdateSeries;
    procedure EndUpdateSeries;

    procedure ShowInfo;

    procedure ShowPointInfo;
    procedure HidePointInfo;
    procedure UpdatePointInfo(const AMouseX, AMouseY: Integer; const ADist: PDouble = nil);

    procedure OnConfigChange;

    procedure UpdateUI;

    class procedure InitInfo(out AInfo: TProfileInfoRec); static;
    class procedure ResetInfo(out AInfo: TProfileInfoRec); static; inline;
  protected
    procedure RefreshTranslation; override;
  public
    procedure ShowProfile(
      const ADatum: IDatum;
      const ALines: TArrayOfGeometryLonLatSingleLine;
      const AName: string
    );
    procedure SetLocation(const ALonLat: TDoublePoint);
    procedure SetFocusOnChart;
    procedure Clear;
  public
    constructor Create(
      const AParent: TWinControl;
      const AOnClose: TOnCloseEvent;
      const AOnRefresh: TOnRefreshEvent;
      const AConfig: IElevationProfileConfig;
      const ALanguageManager: ILanguageManager;
      const AMapGoTo: IMapViewGoto
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  DateUtils,
  gnugettext,
  i_EnumDoublePoint,
  u_ListenerByEvent,
  u_GeoFunc,
  u_ResStrings;

resourcestring
  rsElevationProfileDistFmt = 'Distance: %.2f %s';
  rsElevationProfileTimeFmt = 'Time: %s';
  rsElevationProfileElevFmt = 'Elevation: %d, %d, %d m';
  rsElevationProfileElevAscentFmt = 'Ascent: %d m';
  rsElevationProfileElevDescentFmt = 'Descent: %d m';
  rsElevationProfileElevMaxSlopeFmt = 'Max Slope: %.1f%%, %.1f%%';
  rsElevationProfileElevAvgSlopeFmt = 'Avg Slope: %.1f%%, %.1f%%';
  rsElevationProfileSpeedFmt = 'Speed: %.2f, %.2f, %.2f km/h';

const
  CElevationSeriesColor = clRed;
  CSpeedSeriesColor = clBlue;
  CSeriesTransparency = 75;

const
  CFilterElevWindow = 3;
  CFilterSpeedWindow = 5;

{$R *.dfm}

{ TfrElevationProfile }

constructor TfrElevationProfile.Create(
  const AParent: TWinControl;
  const AOnClose: TOnCloseEvent;
  const AOnRefresh: TOnRefreshEvent;
  const AConfig: IElevationProfileConfig;
  const ALanguageManager: ILanguageManager;
  const AMapGoTo: IMapViewGoto
);
begin
  Assert(AConfig <> nil);

  inherited Create(ALanguageManager);

  FOnClose := AOnClose;
  FOnRefresh := AOnRefresh;
  FConfig := AConfig;
  FMapGoTo := AMapGoTo;

  Self.Parent := AParent;

  SetupChart;

  HidePointInfo;
  ResetInfo(FInfo);

  FFormatSettings.DecimalSeparator := '.';

  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigChangeListener);
  OnConfigChange;

  UpdateUI;
end;

destructor TfrElevationProfile.Destroy;
begin
  if FConfigChangeListener <> nil then begin
    FConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeListener := nil;
  end;

  inherited Destroy;
end;

procedure TfrElevationProfile.UpdateUI;
const
  CMsgCtxt = 'ElevationProfile';
begin
  mniDistance.Caption := pgettext(CMsgCtxt, 'Distance');
end;

procedure TfrElevationProfile.RefreshTranslation;
begin
  inherited;

  if Visible then begin
    UpdateUI;
    ShowInfo;
    ShowPointInfo;
  end;
end;

procedure TfrElevationProfile.OnConfigChange;
begin
  FConfigStatic := FConfig.GetStatic;

  mniShowSpeed.Checked := FConfigStatic.ShowSpeed;
  mniShowElevation.Checked := FConfigStatic.ShowElevation;
  mniTrackData.Checked := FConfigStatic.ElevationSource = esTrackMetadata;
  mniDEMData.Checked := FConfigStatic.ElevationSource = esTerrainProvider;
  mniFilterData.Checked := FConfigStatic.UseDataFiltering;
  mniCenterMap.Checked := FConfigStatic.CenterMap;

  mniTrackName.Checked := epsTrackName in FConfigStatic.StatInfoSet;
  mniDistance.Checked := epsDist in FConfigStatic.StatInfoSet;
  mniDuration.Checked := epsTime in FConfigStatic.StatInfoSet;
  mniElevationMinAvgMax.Checked := epsElevMinAvgMax in FConfigStatic.StatInfoSet;
  mniAscentDescent.Checked := epsElevAscentDescent in FConfigStatic.StatInfoSet;
  mniMaxSlope.Checked := epsElevSlopeMaxGainLoss in FConfigStatic.StatInfoSet;
  mniAverageSlope.Checked := epsElevSlopeAvgGainLoss in FConfigStatic.StatInfoSet;
  mniSpeedMinAvgMax.Checked := epsSpeedMinAvgMax in FConfigStatic.StatInfoSet;

  {$IFDEF HAS_TEE_ZOOM_OPT}
  mniZoomWithMouseWheel.Checked := FConfigStatic.ZoomWithMouseWheel;
  mniKeepAspectRatio.Checked := FConfigStatic.KeepAspectRatio;

  chtProfile.Zoom.KeepAspectRatio := FConfigStatic.KeepAspectRatio;

  if FConfigStatic.ZoomWithMouseWheel then begin
    chtProfile.Panning.MouseWheel := pmwNone;
    chtProfile.Zoom.MouseWheel := pmwNormal;
  end else begin
    chtProfile.Panning.MouseWheel := pmwNormal;
    chtProfile.Zoom.MouseWheel := pmwNone;
  end;
  {$ELSE}
  mniZoomWithMouseWheel.Visible := False;
  mniKeepAspectRatio.Visible := False;
  {$ENDIF}

  FElevationSeries.Visible := FConfigStatic.ShowElevation;
  FSpeedSeries.Visible := FConfigStatic.ShowSpeed;
end;

procedure TfrElevationProfile.SetupChart;

  function _CreateAreaSeries(
    const AVertAxis: TVertAxis;
    const AColor: TColor
  ): TAreaSeries;
  begin
    Result := chtProfile.AddSeries(TAreaSeries) as TAreaSeries;

    with Result do begin
      AreaLinesPen.Visible := False;
      DrawArea := True;
      {$IFDEF HAS_TEE_DRAW_STYLE}
      DrawStyle := dsAll;
      {$ENDIF}
      LinePen.Visible := False;
      Pointer.Visible := False;
      Transparency := CSeriesTransparency;
      VertAxis := AVertAxis;
      Color := AColor;
    end;
  end;

  procedure _SetupAxis(
    const AChartAxis: TChartAxis;
    const AGridColor: TColor = clBlack;
    const AValuesUnit: string = ''
  );
  var
    VAlpha: Cardinal;
  begin
    VAlpha := Round(255 * 0.9) shl 24;
    with AChartAxis do begin
      Axis.Visible := True;
      Axis.Width := 1;
      Axis.Color := Cardinal(clBlack) or VAlpha;
      Grid.Color := Cardinal(AGridColor) or VAlpha;
      if AValuesUnit <> '' then begin
        AxisValuesFormat := AxisValuesFormat + ' ' + AValuesUnit;
      end;
    end;
  end;

begin
  with TTeeGDIPlus.Create(chtProfile) do begin
    Active := True;
    Antialias := False;
    AntiAliasText := gpfNormal;
    TeePanel := chtProfile;
  end;

  FAxisValuesFormatDef := chtProfile.BottomAxis.AxisValuesFormat;

  _SetupAxis(chtProfile.LeftAxis, CElevationSeriesColor, SAS_UNITS_m);
  _SetupAxis(chtProfile.RightAxis, CSpeedSeriesColor, SAS_UNITS_kmperh);
  _SetupAxis(chtProfile.TopAxis);
  _SetupAxis(chtProfile.BottomAxis);

  FSpeedSeries := _CreateAreaSeries(aRightAxis, CSpeedSeriesColor);
  FElevationSeries := _CreateAreaSeries(aLeftAxis, CElevationSeriesColor);
end;

// Public API

procedure TfrElevationProfile.ShowProfile(
  const ADatum: IDatum;
  const ALines: TArrayOfGeometryLonLatSingleLine;
  const AName: string
);
begin
  FDatum := ADatum;
  FLines := ALines;
  FTrackName := AName;

  HidePointInfo;
  ShowSeries;
  ShowInfo;
end;

procedure TfrElevationProfile.SetFocusOnChart;
begin
  if Visible then begin
    chtProfile.SetFocus;
  end;
end;

procedure TfrElevationProfile.Clear;
begin
  FDatum := nil;
  FLines := nil;
  FDist := nil;

  FSpeedSeries.Clear;
  FElevationSeries.Clear;
end;

// Chart events

procedure TfrElevationProfile.chtProfileContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  if PtInRect(chtProfile.ChartRect, MousePos) then begin
    // Disable popup menu inside chart rect
    Handled := True;
  end;
end;

procedure TfrElevationProfile.chtProfileAfterDraw(Sender: TObject);
begin
  if not FPointInfo.IsValid then begin
    HidePointInfo;
  end;
end;

procedure TfrElevationProfile.chtProfileMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and PtInRect(chtProfile.ChartRect, Point(X, Y)) then begin
    UpdatePointInfo(X, Y);
    ShowPointInfo;
  end;
end;

procedure TfrElevationProfile.chtProfileResize(Sender: TObject);
begin
  FPointInfo.IsValid := False;
end;

procedure TfrElevationProfile.chtProfileScroll(Sender: TObject);
begin
  FPointInfo.IsValid := False;
end;

procedure TfrElevationProfile.chtProfileUndoZoom(Sender: TObject);
begin
  FPointInfo.IsValid := False;
end;

procedure TfrElevationProfile.chtProfileZoom(Sender: TObject);
begin
  FPointInfo.IsValid := False;
end;

// PopupMenu Actions

procedure TfrElevationProfile.mniShowSpeedClick(Sender: TObject);
begin
  FConfig.ShowSpeed := mniShowSpeed.Checked;

  ShowInfo;
  ShowPointInfo;
end;

procedure TfrElevationProfile.mniTrackDataClick(Sender: TObject);
begin
  mniTrackData.Checked := True;
  FConfig.ElevationSource := esTrackMetadata;
  FOnRefresh;
end;

procedure TfrElevationProfile.mniStatisticsItemClick(Sender: TObject);
var
  VItem: TMenuItem;
  VStat: TElevationProfileStatInfoSet;
  VValue: TElevationProfileStatInfo;
begin
  if Sender is TMenuItem then begin
    VItem := Sender as TMenuItem;
    VStat := FConfig.StatInfoSet;
    VValue := TElevationProfileStatInfo(VItem.Tag);
    if VItem.Checked then begin
      Exclude(VStat, VValue);
    end else begin
      Include(VStat, VValue);
    end;
    FConfig.StatInfoSet := VStat;
    ShowInfo;
  end;
end;

procedure TfrElevationProfile.mniDEMDataClick(Sender: TObject);
begin
  mniDEMData.Checked := True;
  FConfig.ElevationSource := esTerrainProvider;
  FOnRefresh;
end;

procedure TfrElevationProfile.mniZoomWithMouseWheelClick(Sender: TObject);
begin
  FConfig.ZoomWithMouseWheel := mniZoomWithMouseWheel.Checked;
end;

procedure TfrElevationProfile.mniScaleElevToDistClick(Sender: TObject);
begin
  ScaleElevToDistAsOneToOne;
end;

procedure TfrElevationProfile.mniShowElevationClick(Sender: TObject);
begin
  FConfig.ShowElevation := mniShowElevation.Checked;

  ShowInfo;
  ShowPointInfo;
end;

procedure TfrElevationProfile.mniCenterMapClick(Sender: TObject);
begin
  FConfig.CenterMap := mniCenterMap.Checked;
end;

procedure TfrElevationProfile.mniFilterDataClick(Sender: TObject);
begin
  FConfig.UseDataFiltering := mniFilterData.Checked;

  ShowSeries;
  ShowInfo;
  ShowPointInfo;
end;

procedure TfrElevationProfile.mniKeepAspectRatioClick(Sender: TObject);
begin
  FConfig.KeepAspectRatio := mniKeepAspectRatio.Checked;
end;

procedure TfrElevationProfile.mniResetZoomClick(Sender: TObject);
begin
  chtProfile.UndoZoom;
  ResetAxesMinMax;
end;

// Chart series

procedure TfrElevationProfile.ShowSeries;
const
  CMaxDistInMeters = 9999;
var
  I: Integer;
  VCount: Integer;
begin
  // reset zoom
  chtProfile.UndoZoom;

  InitInfo(FInfo);

  VCount := 0;
  for I := 0 to Length(FLines) - 1 do begin
    Inc(VCount, FLines[I].Count);
  end;
  SetLength(FDist, VCount);

  BeginUpdateSeries;
  try
    FSpeedSeries.Clear;
    FElevationSeries.Clear;

    for I := 0 to Length(FLines) - 1 do begin
      FillSeriesWithLineData(FLines[I]);
    end;

    FIsDistInMeters := FInfo.Dist <= CMaxDistInMeters;

    if not FIsDistInMeters then begin
      for I := 0 to Length(FDist) - 1 do begin
        FDist[I] := FDist[I] / 1000;
        FElevationSeries.XValue[I] := FDist[I];
        FSpeedSeries.XValue[I] := FDist[I];
      end;
      chtProfile.BottomAxis.AxisValuesFormat := FAxisValuesFormatDef + ' ' + SAS_UNITS_km;
    end else begin
      chtProfile.BottomAxis.AxisValuesFormat := FAxisValuesFormatDef + ' ' + SAS_UNITS_m;
    end;

    ResetAxesMinMax;

    FElevationSeries.Visible := FConfigStatic.ShowElevation;
    FSpeedSeries.Visible := FConfigStatic.ShowSpeed and (FInfo.Seconds > 0);
    mniShowSpeed.Enabled := FInfo.Seconds > 0;

    if not FElevationSeries.Visible then begin
      if chtProfile.MarginLeft < 8 then begin
        chtProfile.MarginLeft := 8;
      end;
    end;
  finally
    EndUpdateSeries;
  end;

  if FInfo.PointsCount > 0 then begin
    // calc average values
    FInfo.Elev.Avg := FInfo.Elev.Avg / FInfo.PointsCount;
    FInfo.Speed.Avg := FInfo.Speed.Avg / FInfo.PointsCount;

    if FInfo.AvgSlope.GainPoints > 0 then begin
      FInfo.AvgSlope.Gain := FInfo.AvgSlope.Gain / FInfo.AvgSlope.GainPoints;
    end;
    if FInfo.AvgSlope.LossPoints > 0 then begin
      FInfo.AvgSlope.Loss := FInfo.AvgSlope.Loss / FInfo.AvgSlope.LossPoints;
    end;
  end else begin
    ResetInfo(FInfo);
  end;
end;

procedure TfrElevationProfile.ResetAxesMinMax;
const
  CMin = 0.99;
  CMax = 1.01;
begin
  chtProfile.Axes.Bottom.SetMinMax(
    FElevationSeries.MinXValue,
    FElevationSeries.MaxXValue
  );

  chtProfile.Axes.Left.SetMinMax(
    FElevationSeries.MinYValue * CMin,
    FElevationSeries.MaxYValue * CMax
  );

  chtProfile.Axes.Right.SetMinMax(
    FSpeedSeries.MinYValue * CMin,
    FSpeedSeries.MaxYValue * CMax
  );
end;

procedure TfrElevationProfile.FillSeriesWithLineData(
  const ALine: IGeometryLonLatSingleLine
);

  procedure Filter(
    const AValues: TChartValueList;
    const AStartIndex: Integer;
    const AWindow: Integer
  );
  var
    I: Integer;
    VCount: Integer;
    VHalf: Integer;
    VAcc: Double;
    VTmp: array of Double;
  begin
    VCount := AValues.Count - AStartIndex;

    if (VCount < AWindow) or (AWindow < 2) then begin
      Exit;
    end;

    SetLength(VTmp, VCount);

    VAcc := 0;
    VHalf := AWindow div 2;

    for I := 0 to AWindow - 1 do begin
      VAcc := VAcc + AValues[AStartIndex + I];
    end;
    for I := 0 to VHalf do begin
      VTmp[I] := VAcc / AWindow;
    end;
    for I := VHalf + 1 to VCount - 1 - VHalf do begin
      VAcc := VAcc + AValues[I + VHalf] - AValues[I - (VHalf + 1)];
      VTmp[I] := VAcc / AWindow;
    end;
    for I := VCount - VHalf to VCount - 1 do begin
      VTmp[I] := VAcc / AWindow;
    end;

    for I := 0 to VCount - 1 do begin
      AValues[AStartIndex + I] := VTmp[I];
    end;
  end;

  procedure CalcMinMaxAvg(
    const AValues: TChartValueList;
    const AStartIndex: Integer;
    var ARec: TProfileMinMaxAvgRec
  );
  var
    I: Integer;
    VCurr: Double;
  begin
    for I := AStartIndex to AValues.Count - 1 do begin
      VCurr := AValues[I];
      if VCurr < ARec.Min then begin
        ARec.Min := VCurr;
      end;
      if VCurr > ARec.Max then begin
        ARec.Max := VCurr;
      end;
      ARec.Avg := ARec.Avg + VCurr; // accumulate only
    end;
  end;

  procedure CalcElevAscentDescent(
    const AValues: TChartValueList;
    const AStartIndex: Integer
  );
  var
    I: Integer;
    VCurr: Double;
    VPrev: Double;
  begin
    if AValues.Count < 2 then begin
      Exit;
    end;
    VPrev := AValues[AStartIndex];
    for I := AStartIndex + 1 to AValues.Count - 1 do begin
      VCurr := AValues[I];
      if VCurr > VPrev then begin
        FInfo.ElevAscent := FInfo.ElevAscent + (VCurr - VPrev);
      end else
      if VCurr < VPrev then begin
        FInfo.ElevDescent := FInfo.ElevDescent + (VPrev - VCurr);
      end;
      VPrev := VCurr;
    end;
  end;

  procedure CalcSlope(
    const AValues: TChartValueList;
    const AStartIndex: Integer
  );
  var
    I: Integer;
    VCurr: Double;
    VPrev: Double;
    VDist: Double;
    VSlope: Double;
  begin
    if AValues.Count < 2 then begin
      Exit;
    end;
    VPrev := AValues[AStartIndex];
    for I := AStartIndex + 1 to AValues.Count - 1 do begin
      VCurr := AValues[I];
      VDist := FDist[I] - FDist[I-1];
      if VDist > 0 then begin
        VSlope := 100 * (VCurr - VPrev) / VDist;
        if VCurr > VPrev then begin
          // Max Gain
          if VSlope > FInfo.MaxSlope.Gain then begin
            FInfo.MaxSlope.Gain := VSlope;
          end;
          // Avg Gain
          FInfo.AvgSlope.Gain := FInfo.AvgSlope.Gain + VSlope; // accumulate only
          Inc(FInfo.AvgSlope.GainPoints);
        end else
        if VCurr < VPrev then begin
          // Max Loss
          if VSlope < FInfo.MaxSlope.Loss then begin
            FInfo.MaxSlope.Loss := VSlope;
          end;
          // Avg Loss
          FInfo.AvgSlope.Loss := FInfo.AvgSlope.Loss + VSlope; // accumulate only
          Inc(FInfo.AvgSlope.LossPoints);
        end else begin
          Inc(FInfo.AvgSlope.GainPoints);
          Inc(FInfo.AvgSlope.LossPoints);
        end;
      end;
      VPrev := VCurr;
    end;
  end;

var
  VEnum: IEnumLonLatPoint;
  VPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VMeta: TDoublePointsMetaItem;
  VDist: Double;
  VElev: Double;
  VSpeed: Double;
  VSeconds: Int64;
  VPrevTimeIndex: Integer;
  VPrevTimeValue: TDateTime;
  VIsPrevOk: Boolean;
  VStartIndex: Integer;
begin
  VIsPrevOk := False;

  VPrevTimeIndex := -1;
  VPrevTimeValue := 0;

  VStartIndex := FInfo.PointsCount;

  VEnum := ALine.GetEnum;

  while VEnum.Next(VPoint, VMeta) do begin

    // distance
    if VIsPrevOk then begin
      FInfo.Dist := FInfo.Dist + FDatum.CalcDist(VPrevPoint, VPoint);
    end;
    FDist[FInfo.PointsCount] := FInfo.Dist;

    // elevation
    if VMeta.IsElevationOk then begin
      VElev := VMeta.Elevation;
    end else begin
      VElev := 0;
    end;

    // speed
    VSpeed := 0;
    VSeconds := 0;

    if VMeta.IsTimeStampOk then begin
      if VPrevTimeIndex >= 0 then begin
        VDist := FDist[FInfo.PointsCount] - FDist[VPrevTimeIndex];
        VSeconds := SecondsBetween(VMeta.TimeStamp, VPrevTimeValue);
        if VSeconds > 0 then begin
          VSpeed := (VDist / VSeconds) / 1000 * 3600; // km/h
        end;
      end;
      VPrevTimeIndex := FInfo.PointsCount;
      VPrevTimeValue := VMeta.TimeStamp;
    end;

    // time
    Inc(FInfo.Seconds, VSeconds);

    // add values
    FElevationSeries.AddXY(FInfo.Dist, VElev);
    FSpeedSeries.AddXY(FInfo.Dist, VSpeed);

    Inc(FInfo.PointsCount);

    // prepare to the next step
    VPrevPoint := VPoint;
    VIsPrevOk := True;
  end;

  if FConfigStatic.UseDataFiltering then begin
    Filter(FElevationSeries.YValues, VStartIndex, CFilterElevWindow);
    Filter(FSpeedSeries.YValues, VStartIndex, CFilterSpeedWindow);
  end;

  CalcMinMaxAvg(FElevationSeries.YValues, VStartIndex, FInfo.Elev);
  CalcMinMaxAvg(FSpeedSeries.YValues, VStartIndex, FInfo.Speed);

  CalcElevAscentDescent(FElevationSeries.YValues, VStartIndex);
  CalcSlope(FElevationSeries.YValues, VStartIndex);
end;

procedure TfrElevationProfile.ScaleElevToDistAsOneToOne;
var
  I: Integer;
  VElevSize, VDistSize: Integer;
  VElevScale, VDistScale: Double;
begin
  // it takes at least two iterations to make an accurate 1:1 scaling
  for I := 0 to 1 do begin

    chtProfile.UndoZoom;

    with chtProfile.Axes.Left do begin
      VElevSize := IAxisSize;
      VElevScale := CalcPosPoint(IStartPos) - CalcPosPoint(IStartPos + 1);
    end;

    with chtProfile.Axes.Bottom do begin
      VDistSize := IAxisSize;
      VDistScale := CalcPosPoint(IStartPos + 1) - CalcPosPoint(IStartPos);
    end;

    if not FIsDistInMeters then begin
      VDistScale := VDistScale * 1000;
    end;

    if VDistScale > VElevScale then begin
      chtProfile.Axes.Left.SetMinMax(0, VDistScale * VElevSize);
    end else begin
      if not FIsDistInMeters then begin
        VElevScale := VElevScale / 1000;
      end;
      chtProfile.Axes.Bottom.SetMinMax(0, VElevScale * VDistSize);
    end;

    Application.ProcessMessages;
  end;
end;

procedure TfrElevationProfile.BeginUpdateSeries;
var
  I: Integer;
begin
  chtProfile.AutoRepaint := False;
  for I := 0 to chtProfile.SeriesList.Count - 1 do begin
    chtProfile.Series[I].BeginUpdate;
  end;
end;

procedure TfrElevationProfile.EndUpdateSeries;
var
  I: Integer;
begin
  for I := 0 to chtProfile.SeriesList.Count - 1 do begin
    chtProfile.Series[I].EndUpdate;
  end;
  chtProfile.AutoRepaint := True;
  chtProfile.Repaint;
end;

// Info panel

procedure TfrElevationProfile.ShowInfo;

  procedure _AppendStr(var AInfo: string; const AStr: string);
  begin
    if AInfo <> '' then begin
      AInfo := AInfo + ' | ' + AStr;
    end else begin
      AInfo := AStr;
    end;
  end;

var
  VInfo: string;
  VDistVal: Double;
  VDistUnit: string;
  VStat: TElevationProfileStatInfoSet;
begin
  VStat := FConfigStatic.StatInfoSet;

  VInfo := '';

  // name
  if (epsTrackName in VStat) and (FTrackName <> '') then begin
    _AppendStr(VInfo, FTrackName);
  end;

  // distance
  if epsDist in VStat then begin
    if FInfo.Dist > 1000 then begin
      VDistVal := FInfo.Dist / 1000;
      VDistUnit := SAS_UNITS_km;
    end else begin
      VDistVal := FInfo.Dist;
      VDistUnit := SAS_UNITS_m;
    end;
    _AppendStr(VInfo, Format(rsElevationProfileDistFmt, [VDistVal, VDistUnit], FFormatSettings));
  end;

  // duration
  if (epsTime in VStat) and (FInfo.Seconds > 0) then begin
    _AppendStr(VInfo, Format(rsElevationProfileTimeFmt, [FormatDateTime('hh:nn:ss', FInfo.Seconds / SecsPerDay)]));
  end;

  // elevation
  if FConfigStatic.ShowElevation then begin
    // min, avg, max
    if epsElevMinAvgMax in VStat then begin
      _AppendStr(VInfo, Format(rsElevationProfileElevFmt, [Round(FInfo.Elev.Min), Round(FInfo.Elev.Avg), Round(FInfo.Elev.Max)], FFormatSettings));
    end;

    // ascent and descent
    if epsElevAscentDescent in VStat then begin
      _AppendStr(VInfo, Format(rsElevationProfileElevAscentFmt, [Round(FInfo.ElevAscent)]));
      _AppendStr(VInfo, Format(rsElevationProfileElevDescentFmt, [Round(FInfo.ElevDescent)]));
    end;

    // max slope
    if epsElevSlopeMaxGainLoss in VStat then begin
      _AppendStr(VInfo, Format(rsElevationProfileElevMaxSlopeFmt, [FInfo.MaxSlope.Gain, FInfo.MaxSlope.Loss], FFormatSettings));
    end;

    // avg slope
    if epsElevSlopeAvgGainLoss in VStat then begin
      _AppendStr(VInfo, Format(rsElevationProfileElevAvgSlopeFmt, [FInfo.AvgSlope.Gain, FInfo.AvgSlope.Loss], FFormatSettings));
    end;
  end;

  // speed min, avg, max
  if FConfigStatic.ShowSpeed and (FInfo.Seconds > 0) and (epsSpeedMinAvgMax in VStat) then begin
    _AppendStr(VInfo, Format(rsElevationProfileSpeedFmt, [FInfo.Speed.Min, FInfo.Speed.Avg, FInfo.Speed.Max], FFormatSettings));
  end;

  // show
  lblInfo.Caption := VInfo;
end;

procedure TfrElevationProfile.btnCloseClick(Sender: TObject);
begin
  FOnClose();
  HidePointInfo;
  Clear;
end;

// Chart point info

procedure TfrElevationProfile.ShowPointInfo;
var
  VLeft, VTop: Integer;
  VHint: string;
  {$IFDEF SHOW_ELEV_TO_DIST_SCALE_INFO}
  E, D: Double;
  {$ENDIF}
begin
  if not FPointInfo.IsValid then begin
    HidePointInfo;
    Exit;
  end;

  VHint := '';

  if FSpeedSeries.Visible then begin
    VHint := VHint + Format(_('Speed: %.2f km/h') + #13#10, [FPointInfo.Speed], FFormatSettings);
  end;

  if FElevationSeries.Visible then begin
    VHint := VHint + Format(_('Elevation: %.2f m') + #13#10, [FPointInfo.Elev], FFormatSettings);
    VHint := VHint + Format(_('Slope: %.1f%%') + #13#10, [FPointInfo.Slope], FFormatSettings);
  end;

  if FIsDistInMeters then begin
    VHint := VHint + Format(_('Distance: %.2f m'), [FPointInfo.Dist], FFormatSettings);
  end else begin
    VHint := VHint + Format(_('Distance: %.2f km'), [FPointInfo.Dist], FFormatSettings);
  end;

  // elevation to distance scale
  {$IFDEF SHOW_ELEV_TO_DIST_SCALE_INFO}
  with chtProfile.Axes.Left do begin
    E := CalcPosPoint(IStartPos) - CalcPosPoint(IStartPos + 1);
  end;
  with chtProfile.Axes.Bottom do begin
    D := CalcPosPoint(IStartPos + 1) - CalcPosPoint(IStartPos);
  end;
  if not FIsDistInMeters then begin
    D := D * 1000;
  end;
  if E > 0 then begin
    VHint := VHint + #13#10 + Format('Scale: 1:%f', [D/E], FFormatSettings);
  end;
  {$ENDIF}

  // hint
  lblPointInfo.Caption := VHint;

  pnlPointInfo.Width := lblPointInfo.Width + 10;
  pnlPointInfo.Height := lblPointInfo.Height + 10;


  VLeft := FPointInfo.MouseX + 15;
  if VLeft + pnlPointInfo.Width >= chtProfile.ChartRect.Right then begin
    VLeft := FPointInfo.MouseX - 15 - pnlPointInfo.Width;
  end;

  VTop := FPointInfo.MouseY + 15;
  if VTop + pnlPointInfo.Height >= chtProfile.ChartRect.Bottom then begin
    VTop := FPointInfo.MouseY - 15 - pnlPointInfo.Height;
  end;

  pnlPointInfo.Left := VLeft;
  pnlPointInfo.Top := VTop;

  pnlPointInfo.Visible := True;

  // line
  pnlPointLine.Height := chtProfile.ChartRect.Bottom - chtProfile.ChartRect.Top;

  pnlPointLine.Left := FPointInfo.MouseX;
  pnlPointLine.Top := chtProfile.ChartRect.Top;

  pnlPointLine.Visible := True;

  // point
  if FPointInfo.IsLonLatValid then begin
    if not FPointInfo.IsMapCenterd and FConfigStatic.CenterMap then begin
      FMapGoTo.GotoLonLat(FPointInfo.LonLat, True);
    end else begin
      FMapGoTo.ShowMarker(FPointInfo.LonLat);
    end;
  end else begin
    FMapGoTo.HideMarker;
  end;
end;

procedure TfrElevationProfile.HidePointInfo;
begin
  FMapGoTo.HideMarker;

  pnlPointInfo.Visible := False;
  pnlPointLine.Visible := False;

  FPointInfo.IsValid := False;
end;

procedure TfrElevationProfile.UpdatePointInfo(const AMouseX, AMouseY: Integer; const ADist: PDouble);

  function FindNearestDist(
    const AValue: Double;
    out ALeft, ARight: Integer
  ): Boolean;
  var
    I: Integer;
    VLen: Integer;
  begin
    VLen := Length(FDist);

    if VLen = 0 then begin
      Result := False;
      Exit;
    end;

    Result := True;

    if AValue <= FDist[0] then begin
      ALeft := 0;
      ARight := ALeft;
      Exit;
    end;

    if AValue >= FDist[VLen-1] then begin
      ALeft := VLen-1;
      ARight := ALeft;
      Exit;
    end;

    ALeft := 0;
    ARight := VLen-1;

    // Binary search
    while ARight - ALeft > 1 do begin
      I := (ARight + ALeft) div 2;
      if AValue <= FDist[I] then begin
        ARight := I;
      end else begin
        ALeft := I;
      end;
    end;
  end;

  function InterpolateValue(
    const AValues: TChartValueList;
    const ADistValue: Double;
    const ALeft, ARight: Integer
  ): Double;
  begin
    if ALeft <> ARight then begin
      // Linear interpolation
      Result := AValues[ALeft] + (AValues[ARight] - AValues[ALeft]) *
        (ADistValue - FDist[ALeft]) / (FDist[ARight] - FDist[ALeft]);
    end else begin
      Result := AValues[ALeft];
    end;
  end;

  function FindPointLonLat(
    const ADistValue: Double;
    const ALeft, ARight: Integer;
    out APoint: TDoublePoint
  ): Boolean;
  var
    I: Integer;
    VLeft, VRight: Integer;
    VLeftIndex, VRightIndex: Integer;
    VLeftPoint, VRightPoint: TDoublePoint;
    VInitialBearing, VFinalBearing: Double;
    VDist: Double;
  begin
    VLeft := ALeft;
    VLeftIndex := 0;
    for I := 0 to Length(FLines) - 1 do begin
      if VLeft >= FLines[I].Count then begin
        Dec(VLeft, FLines[I].Count);
        Inc(VLeftIndex);
      end else begin
        Break;
      end;
    end;

    VRight := ARight;
    VRightIndex := 0;
    for I := 0 to Length(FLines) - 1 do begin
      if VRight >= FLines[I].Count then begin
        Dec(VRight, FLines[I].Count);
        Inc(VRightIndex);
      end else begin
        Break;
      end;
    end;

    if VLeftIndex <> VRightIndex then begin
      Result := False;
      Exit;
    end;

    if VLeft = VRight then begin
      APoint := FLines[VLeftIndex].Points[VLeft];
      Result := True;
      Exit;
    end;

    VLeftPoint := FLines[VLeftIndex].Points[VLeft];
    VRightPoint := FLines[VRightIndex].Points[VRight];

    // find initial bearing
    FDatum.CalcDist(VLeftPoint, VRightPoint, VInitialBearing, VFinalBearing);

    VDist := ADistValue - FDist[ALeft];
    if not FIsDistInMeters then begin
      VDist := VDist * 1000;
    end;

    // find target point
    APoint := FDatum.CalcFinishPosition(VLeftPoint, VInitialBearing, VDist);

    Result := True;
  end;

var
  VLeft, VRight: Integer;
  VDist, VCursorDist: Double;
begin
  if ADist <> nil then begin
    VCursorDist := ADist^;
  end else begin
    VCursorDist := FElevationSeries.XScreenToValue(AMouseX);
  end;

  FPointInfo.IsValid := FindNearestDist(VCursorDist, VLeft, VRight);

  if not FPointInfo.IsValid then begin
    HidePointInfo;
    Exit;
  end;

  FPointInfo.MouseX := AMouseX;
  FPointInfo.MouseY := AMouseY;

  FPointInfo.Elev := InterpolateValue(FElevationSeries.YValues, VCursorDist, VLeft, VRight);
  FPointInfo.Speed := InterpolateValue(FSpeedSeries.YValues, VCursorDist, VLeft, VRight);

  FPointInfo.Dist := VCursorDist;
  FPointInfo.IsLonLatValid := FindPointLonLat(VCursorDist, VLeft, VRight, FPointInfo.LonLat);

  // slope
  if VLeft <> VRight then begin
    VDist := FElevationSeries.XValues[VRight] - FElevationSeries.XValues[VLeft];
    if not FIsDistInMeters then begin
      VDist := VDist * 1000;
    end;
    FPointInfo.Slope := 100 * (FElevationSeries.YValues[VRight] - FElevationSeries.YValues[VLeft]) / VDist;
  end else begin
    FPointInfo.Slope := 0;
  end;

  FPointInfo.IsMapCenterd := ADist <> nil;
end;

// Utility functions

class procedure TfrElevationProfile.InitInfo(out AInfo: TProfileInfoRec);
const
  CEmptyMinMaxAvgRec: TProfileMinMaxAvgRec = (
    Min: MaxInt; Max: -MaxInt; Avg: 0;
  );
begin
  ResetInfo(AInfo);
  AInfo.Elev := CEmptyMinMaxAvgRec;
  AInfo.Speed := CEmptyMinMaxAvgRec;
end;

class procedure TfrElevationProfile.ResetInfo(out AInfo: TProfileInfoRec);
begin
  FillChar(AInfo, SizeOf(TProfileInfoRec), 0);
end;

procedure TfrElevationProfile.SetLocation(const ALonLat: TDoublePoint);

  function FindNearestPoint(
    const APoints: PDoublePointArray;
    const ACount: Integer;
    out ADist: Double;
    out ANearestPoint: TDoublePoint;
    out ANearestIndex: Integer
  ): Boolean;
  const
    cEpsilon = 0.000001;
  var
    I: Integer;
    VDist: Double;
    VCurrPoint: PDoublePoint;
    VPrevPoint: PDoublePoint;
    VVectorW: TDoublePoint;
    VVectorV: TDoublePoint;
    C1: Double;
    C2: Double;
    B: Double;
    VVectorDist: TDoublePoint;
  begin
    Result := False;
    ADist := NaN;
    if ACount > 1 then begin
      VPrevPoint := @APoints[0];
      for I := 1 to ACount - 1 do begin
        VCurrPoint := @APoints[I];
        VVectorW.X := ALonLat.X - VPrevPoint.X;
        VVectorW.Y := ALonLat.Y - VPrevPoint.Y;
        VVectorV.X := VCurrPoint.X - VPrevPoint.X;
        VVectorV.Y := VCurrPoint.Y - VPrevPoint.Y;
        C1 := VVectorW.X * VVectorV.X + VVectorW.Y * VVectorV.Y;
        if C1 > 0 then begin
          C2 := VVectorV.X * VVectorV.X + VVectorV.Y * VVectorV.Y;
          if C2 > C1 then begin
            B := C1 / C2;
            VVectorDist.X := VVectorW.X - B * VVectorV.X;
            VVectorDist.Y := VVectorW.Y - B * VVectorV.Y;
            VDist := (VVectorDist.X * VVectorDist.X + VVectorDist.Y * VVectorDist.Y);
            if VDist < cEpsilon then begin
              if (not Result) or (ADist > VDist) then begin
                Result := True;
                ADist := VDist;
                ANearestIndex := I - 1;
                ANearestPoint := VPrevPoint^;
              end;
            end;
          end;
        end;
        VPrevPoint := VCurrPoint;
      end;
    end;
  end;

  function FindNearestPointMulti(
    out APoint: TDoublePoint;
    out AIndex: Integer
  ): Boolean;
  var
    I: Integer;
    VCount: Integer;
    VDist: Double;
    VDistMin: Double;
    {$IFDEF ENABLE_DIAGNOSTICS}
    VStopWatch: TStopwatch;
    {$ENDIF}
  begin
    Result := False;
    VCount := 0;
    VDistMin := NaN;
    {$IFDEF ENABLE_DIAGNOSTICS}
    VStopWatch := TStopwatch.StartNew;
    {$ENDIF}
    for I := 0 to Length(FLines) - 1 do begin
      if FindNearestPoint(FLines[I].Points, FLines[I].Count, VDist, APoint, AIndex) then begin
        if Result then begin
          if VDist < VDistMin then begin
            VDistMin := VDist;
            Inc(AIndex, VCount);
          end;
        end else begin
          Result := True;
          VDistMin := VDist;
          Inc(AIndex, VCount);
        end;
      end;
      Inc(VCount, FLines[I].Count);
    end;
    {$IFDEF ENABLE_DIAGNOSTICS}
    VStopWatch.Stop;
    OutputDebugString(PChar(
      Self.ClassName + '.FindNearestPointMulti: ' + VStopWatch.ElapsedMilliseconds.ToString + ' ms'
    ));
    {$ENDIF}
  end;

var
  VXPos: Integer;
  VIndex: Integer;
  VDist: Double;
  VNearestPoint: TDoublePoint;
begin
  if PointIsEmpty(ALonLat) then begin
    Exit;
  end;

  if FindNearestPointMulti(VNearestPoint, VIndex) then begin
    VDist := FDatum.CalcDist(VNearestPoint, ALonLat);
    if not FIsDistInMeters then begin
      VDist := VDist / 1000;
    end;
    VDist := FDist[VIndex] + VDist;

    VXPos := FElevationSeries.CalcXPosValue(VDist);
    UpdatePointInfo(VXPos, 10, @VDist);
    ShowPointInfo;
  end;
end;

end.
