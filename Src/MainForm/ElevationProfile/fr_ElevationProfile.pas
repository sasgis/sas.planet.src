{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

{$IF CompilerVersion > 19.0}
  {$DEFINE HAS_TEE_GDI_PLUS}
{$IFEND}

{$IF CompilerVersion > 23.0}
  {$DEFINE HAS_TEE_DRAW_STYLE}
{$IFEND}

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Graphics,
  Forms,
  Menus,
  ExtCtrls,
  StdCtrls,
  TBXDkPanels,
  {.$IFDEF HAS_TEE_GDI_PLUS}
  TeeGDIPlus,
  {.$ENDIF}
  TeEngine,
  TeeProcs,
  Chart,
  Series,
  t_GeoTypes,
  i_Datum,
  i_GeometryLonLat,
  i_LanguageManager,
  i_MapViewGoto,
  u_CommonFormAndFrameParents;

type
  TProfileMinMaxAvgRec = record
    Min: Double;
    Max: Double;
    Avg: Double;
  end;

  TProfileInfoRec = record
    Dist: Double;
    Seconds: Int64;

    Elev: TProfileMinMaxAvgRec;
    ElevAscent: Double;
    ElevDescent: Double;

    Speed: TProfileMinMaxAvgRec;

    PointsCount: Integer;
  end;

  TProfilePointInfoRec = record
    IsValid: Boolean;

    Elev: Double;
    Dist: Double;
    Speed: Double;

    MouseX: Integer;
    MouseY: Integer;

    LonLat: TDoublePoint;
    IsLonLatValid: Boolean;
  end;

  TOnCloseEvent = procedure() of object;

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
  private
    FDatum: IDatum;
    FMapGoTo: IMapViewGoto;
    FOnClose: TOnCloseEvent;

    FShowSpeed: Boolean;
    FShowElevation: Boolean;
    FFilterData: Boolean;
    FCenterMap: Boolean;

    FLine: array of IGeometryLonLatSingleLine;
    FDist: array of Double;
    FIsDistInMeters: Boolean;

    FInfo: TProfileInfoRec;
    FPointInfo: TProfilePointInfoRec;

    FElevationSeries: TAreaSeries;
    FSpeedSeries: TAreaSeries;

    FAxisValuesFormatDef: string;
    FFormatSettings: TFormatSettings;
  private
    procedure SetupChart;

    procedure ShowSeries;
    procedure FillSeriesWithLineData(const ALine: IGeometryLonLatSingleLine);
    procedure BeginUpdateSeries;
    procedure EndUpdateSeries;

    procedure ShowInfo;

    procedure ShowPointInfo;
    procedure HidePointInfo;
    procedure UpdatePointInfo(const AMouseX, AMouseY: Integer);

    class procedure InitInfo(out AInfo: TProfileInfoRec); static;
    class procedure ResetInfo(out AInfo: TProfileInfoRec); static; inline;
  public
    procedure ShowProfile(
      const ALine: IGeometryLonLatLine
    );
    procedure Clear;
  public
    constructor Create(
      const AParent: TWinControl;
      const AOnClose: TOnCloseEvent;
      const ALanguageManager: ILanguageManager;
      const ADatum: IDatum;
      const AMapGoTo: IMapViewGoto
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  DateUtils,
  gnugettext,
  i_EnumDoublePoint,
  u_ResStrings;

resourcestring
  rsElevationProfileDistFmt = 'Distance: %.2f %s';
  rsElevationProfileElevFmt = 'Elevation: %d, %d, %d m';
  rsElevationProfileElevAscentFmt = 'Ascent: %d m';
  rsElevationProfileElevDescentFmt = 'Descent: %d m';
  rsElevationProfileSpeedFmt = 'Speed: %.2f, %.2f, %.2f km/h';
  rsElevationProfileTimeFmt = 'Time: %s';

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
  const ALanguageManager: ILanguageManager;
  const ADatum: IDatum;
  const AMapGoTo: IMapViewGoto
);
begin
  inherited Create(ALanguageManager);

  FOnClose := AOnClose;
  FDatum := ADatum;
  FMapGoTo := AMapGoTo;

  Self.Parent := AParent;

  FShowSpeed := False;
  FShowElevation := True;
  FFilterData := False;
  FCenterMap := True;

  mniShowSpeed.Checked := FShowSpeed;
  mniShowElevation.Checked := FShowElevation;
  mniFilterData.Checked := FFilterData;
  mniCenterMap.Checked := FCenterMap;

  SetupChart;

  HidePointInfo;
  ResetInfo(FInfo);

  FFormatSettings.DecimalSeparator := '.';
end;

destructor TfrElevationProfile.Destroy;
begin
  inherited Destroy;
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
  {$IFDEF HAS_TEE_GDI_PLUS}
  with TTeeGDIPlus.Create(chtProfile) do begin
    Active := True;
    Antialias := False;
    AntiAliasText := gpfNormal;
    TeePanel := chtProfile;
  end;
  {$ENDIF}

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
  const ALine: IGeometryLonLatLine
);
var
  I: Integer;
  VLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  HidePointInfo;

  if Supports(ALine, IGeometryLonLatSingleLine, VLine) then begin
    SetLength(FLine, 1);
    FLine[0] := VLine;
  end else
  if Supports(ALine, IGeometryLonLatMultiLine, VMultiLine) then begin
    SetLength(FLine, VMultiLine.Count);
    for I := 0 to VMultiLine.Count - 1 do begin
      FLine[I] := VMultiLine.Item[I];
    end;
  end else begin
    raise Exception.Create('Unexpected IGeometryLonLatLine type!');
  end;

  ShowSeries;

  ShowInfo;
end;

procedure TfrElevationProfile.Clear;
begin
  FLine := nil;
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
  FShowSpeed := mniShowSpeed.Checked;
  FSpeedSeries.Visible := FShowSpeed;

  ShowInfo;
  ShowPointInfo;
end;

procedure TfrElevationProfile.mniShowElevationClick(Sender: TObject);
begin
  FShowElevation := mniShowElevation.Checked;
  FElevationSeries.Visible := FShowElevation;

  ShowInfo;
  ShowPointInfo;
end;

procedure TfrElevationProfile.mniCenterMapClick(Sender: TObject);
begin
  FCenterMap := mniCenterMap.Checked;
end;

procedure TfrElevationProfile.mniFilterDataClick(Sender: TObject);
begin
  FFilterData := mniFilterData.Checked;

  ShowSeries;
  ShowInfo;
  ShowPointInfo;
end;

procedure TfrElevationProfile.mniResetZoomClick(Sender: TObject);
begin
  chtProfile.UndoZoom;
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
  for I := 0 to Length(FLine) - 1 do begin
    Inc(VCount, FLine[I].Count);
  end;
  SetLength(FDist, VCount);

  BeginUpdateSeries;
  try
    FSpeedSeries.Clear;
    FElevationSeries.Clear;

    for I := 0 to Length(FLine) - 1 do begin
      FillSeriesWithLineData(FLine[I]);
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

    FElevationSeries.Visible := FShowElevation;
    FSpeedSeries.Visible := FShowSpeed and (FInfo.Seconds > 0);
    mniShowSpeed.Enabled := FInfo.Seconds > 0;

    if not FElevationSeries.Visible then begin
      if chtProfile.MarginLeft < 8 then begin
        chtProfile.MarginLeft := 8;
      end;
    end;

    chtProfile.Axes.Left.SetMinMax(
      FElevationSeries.MinYValue * 0.99,
      FElevationSeries.MaxYValue * 1.01
    );
    chtProfile.Axes.Right.SetMinMax(
      FSpeedSeries.MinYValue * 0.99,
      FSpeedSeries.MaxYValue * 1.01
    );
  finally
    EndUpdateSeries;
  end;

  if FInfo.PointsCount > 0 then begin
    // calc average values
    FInfo.Elev.Avg := FInfo.Elev.Avg / FInfo.PointsCount;
    FInfo.Speed.Avg := FInfo.Speed.Avg / FInfo.PointsCount;
  end else begin
    ResetInfo(FInfo);
  end;
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
    VPrev := AValues[0];
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

  if FFilterData then begin
    Filter(FElevationSeries.YValues, VStartIndex, CFilterElevWindow);
    Filter(FSpeedSeries.YValues, VStartIndex, CFilterSpeedWindow);
  end;

  CalcMinMaxAvg(FElevationSeries.YValues, VStartIndex, FInfo.Elev);
  CalcMinMaxAvg(FSpeedSeries.YValues, VStartIndex, FInfo.Speed);

  CalcElevAscentDescent(FElevationSeries.YValues, VStartIndex);
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
const
  CSep = ' | ';
var
  VInfo: string;
  VDistVal: Double;
  VDistUnit: string;
begin
  // distance
  if FInfo.Dist > 1000 then begin
    VDistVal := FInfo.Dist / 1000;
    VDistUnit := SAS_UNITS_km;
  end else begin
    VDistVal := FInfo.Dist;
    VDistUnit := SAS_UNITS_m;
  end;

  VInfo := Format(rsElevationProfileDistFmt, [VDistVal, VDistUnit], FFormatSettings);

  // elevation
  if FShowElevation then begin
    VInfo := VInfo + CSep +
      Format(rsElevationProfileElevFmt, [Round(FInfo.Elev.Min),
        Round(FInfo.Elev.Avg), Round(FInfo.Elev.Max)], FFormatSettings
      ) + CSep +
      Format(rsElevationProfileElevAscentFmt, [Round(FInfo.ElevAscent)]) + CSep +
      Format(rsElevationProfileElevDescentFmt, [Round(FInfo.ElevDescent)]);
  end;

  // speed and time
  if FShowSpeed and (FInfo.Seconds > 0) then begin
    VInfo := VInfo + CSep +
      Format(rsElevationProfileSpeedFmt, [FInfo.Speed.Min, FInfo.Speed.Avg, FInfo.Speed.Max], FFormatSettings) + CSep +
      Format(rsElevationProfileTimeFmt, [FormatDateTime('hh:nn:ss', FInfo.Seconds / SecsPerDay)]);
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
  VSpeed, VElev, VDist: string;
begin
  if not FPointInfo.IsValid then begin
    HidePointInfo;
    Exit;
  end;

  VSpeed := '';
  VElev := '';

  if FSpeedSeries.Visible then begin
    VSpeed := Format(_('Speed: %.2f km/h') + #13#10, [FPointInfo.Speed], FFormatSettings);
  end;
  if FElevationSeries.Visible then begin
    VElev := Format(_('Elevation: %.2f m') + #13#10, [FPointInfo.Elev], FFormatSettings);
  end;

  if FIsDistInMeters then begin
    VDist := Format(_('Distance: %.2f m'), [FPointInfo.Dist], FFormatSettings);
  end else begin
    VDist := Format(_('Distance: %.2f km'), [FPointInfo.Dist], FFormatSettings);
  end;

  // hint
  lblPointInfo.Caption := VSpeed + VElev + VDist;

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
    if FCenterMap then begin
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

procedure TfrElevationProfile.UpdatePointInfo(const AMouseX, AMouseY: Integer);

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
    for I := 0 to Length(FLine) - 1 do begin
      if VLeft >= FLine[I].Count then begin
        Dec(VLeft, FLine[I].Count);
        Inc(VLeftIndex);
      end else begin
        Break;
      end;
    end;

    VRight := ARight;
    VRightIndex := 0;
    for I := 0 to Length(FLine) - 1 do begin
      if VRight >= FLine[I].Count then begin
        Dec(VRight, FLine[I].Count);
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
      APoint := FLine[VLeftIndex].Points[VLeft];
      Result := True;
      Exit;
    end;

    VLeftPoint := FLine[VLeftIndex].Points[VLeft];
    VRightPoint := FLine[VRightIndex].Points[VRight];

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
  VCursorDist, VCursorElev: Double;
begin
  FElevationSeries.GetCursorValues(VCursorDist, VCursorElev);
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

end.
