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
  TeeGDIPlus,
  TeEngine,
  TeeProcs,
  Chart,
  Series,
  i_Datum,
  i_GeometryLonLat,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TProfileStatisticsRec = record
    Dist: Double;
    Seconds: Int64;

    ElevMin: Double;
    ElevAvr: Double;
    ElevMax: Double;

    SpeedMin: Double;
    SpeedAvr: Double;
    SpeedMax: Double;

    PointsCount: Integer;
  end;

  TProfileMouseClickInfo = record
    IsValid: Boolean;

    X, Y: Integer;

    Elev: Double;
    Dist: Double;
    Speed: Double;
  end;

  TOnCloseEvent = procedure() of object;

  TfrElevationProfile = class(TFrame)
    pnlTop: TPanel;
    lblInfo: TLabel;
    btnClose: TTBXButton;
    pmMain: TPopupMenu;
    mniShowSpeed: TMenuItem;
    mniShowElevation: TMenuItem;
    N1: TMenuItem;
    mniResetZoom: TMenuItem;
    chtProfile: TChart;
    TeeGDIPlus1: TTeeGDIPlus;
    pnlPointInfo: TPanel;
    lblPointInfo: TLabel;
    pnlPointLine: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure mniShowSpeedClick(Sender: TObject);
    procedure mniResetZoomClick(Sender: TObject);
    procedure mniShowElevationClick(Sender: TObject);
    procedure chtProfileAfterDraw(Sender: TObject);
    procedure chtProfileMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chtProfileResize(Sender: TObject);
    procedure chtProfileZoom(Sender: TObject);
    procedure chtProfileScroll(Sender: TObject);
    procedure chtProfileContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    FDatum: IDatum;
    FOnClose: TOnCloseEvent;

    FShowSpeed: Boolean;
    FShowElevation: Boolean;

    FLine: array of IGeometryLonLatSingleLine;
    FDist: array of Double;
    FIsDistInMeters: Boolean;

    FPointInfo: TProfileMouseClickInfo;

    FElevationSeries: TAreaSeries;
    FSpeedSeries: TAreaSeries;

    FAxisValuesFormatDef: string;

    procedure SetupChart;

    procedure AddSingleLineSeries(
      const ALine: IGeometryLonLatSingleLine;
      var AStat: TProfileStatisticsRec
    );
    procedure ShowStat(const AStat: TProfileStatisticsRec);

    function ToKmPerHour(const AMetersPerSec: Double): Double; inline;
    function ToKmPerHourStr(const AMetersPerSec: Double): string; inline;

    procedure BeginUpdateSeries;
    procedure EndUpdateSeries;
    procedure ShowPointInfo;
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
      const ADatum: IDatum
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  DateUtils,
  gnugettext,
  t_GeoTypes,
  i_EnumDoublePoint,
  u_ResStrings;

resourcestring
  rsElevationProfileDistFmt = 'Distance: %s %s';
  rsElevationProfileElevFmt = 'Elevation: %d, %d, %d %s';
  rsElevationProfileSpeedFmt = 'Speed: %s, %s, %s %s';
  rsElevationProfileTimeFmt = 'Time: %s';

const
  CElevationSeriesColor = clRed;
  CSpeedSeriesColor = clBlue;

  CSeriesTransparency = 75;

  CEmptyStatRec: TProfileStatisticsRec = (
    Dist        : 0;
    Seconds     : 0;
    ElevMin     : MaxInt;
    ElevAvr     : 0;
    ElevMax     : -MaxInt;
    SpeedMin    : MaxInt;
    SpeedAvr    : 0;
    SpeedMax    : -MaxInt;
    PointsCount : 0;
  );

{$R *.dfm}

{ TfrElevationProfile }

constructor TfrElevationProfile.Create(
  const AParent: TWinControl;
  const AOnClose: TOnCloseEvent;
  const ALanguageManager: ILanguageManager;
  const ADatum: IDatum
);
begin
  inherited Create(ALanguageManager);

  FOnClose := AOnClose;
  FDatum := ADatum;

  Self.Parent := AParent;

  FShowSpeed := False;
  FShowElevation := True;

  mniShowSpeed.Checked := FShowSpeed;
  mniShowElevation.Checked := FShowElevation;

  FPointInfo.IsValid := False;

  SetupChart;
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
      DrawStyle := dsAll;
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
  FAxisValuesFormatDef := chtProfile.BottomAxis.AxisValuesFormat;

  _SetupAxis(chtProfile.LeftAxis, CElevationSeriesColor, SAS_UNITS_m);
  _SetupAxis(chtProfile.RightAxis, CSpeedSeriesColor, SAS_UNITS_kmperh);
  _SetupAxis(chtProfile.TopAxis);
  _SetupAxis(chtProfile.BottomAxis);

  FSpeedSeries := _CreateAreaSeries(aRightAxis, CSpeedSeriesColor);
  FElevationSeries := _CreateAreaSeries(aLeftAxis, CElevationSeriesColor);
end;

procedure TfrElevationProfile.Clear;
begin
  FLine := nil;
  FDist := nil;

  FSpeedSeries.Clear;
  FElevationSeries.Clear;
end;

procedure TfrElevationProfile.btnCloseClick(Sender: TObject);
begin
  FOnClose();
  Clear;
end;

procedure TfrElevationProfile.chtProfileContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  // Disable popup menu inside chart rect
  if PtInRect(chtProfile.ChartRect, MousePos) then begin
    Handled := True;
  end;
end;

procedure TfrElevationProfile.chtProfileAfterDraw(Sender: TObject);
begin
  if not FPointInfo.IsValid then begin
    pnlPointInfo.Visible := False;
    pnlPointLine.Visible := False;
  end;
end;

procedure TfrElevationProfile.chtProfileMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

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
      Result := AValues[ALeft] + (AValues[ARight] - AValues[ALeft]) *
        (ADistValue - FDist[ALeft]) / (FDist[ARight] - FDist[ALeft]);
    end else begin
      Result := AValues[ALeft];
    end;
  end;

var
  VLeft, VRight: Integer;
  VCursorDist, VCursorElev: Double;
begin
  if Button <> mbLeft then begin
    Exit;
  end;

  FElevationSeries.GetCursorValues(VCursorDist, VCursorElev);
  FPointInfo.IsValid := FindNearestDist(VCursorDist, VLeft, VRight);

  if not FPointInfo.IsValid then begin
    Exit;
  end;

  FPointInfo.X := X;
  FPointInfo.Y := Y;

  FPointInfo.Elev := InterpolateValue(FElevationSeries.YValues, VCursorDist, VLeft, VRight);
  FPointInfo.Speed := InterpolateValue(FSpeedSeries.YValues, VCursorDist, VLeft, VRight);

  FPointInfo.Dist := VCursorDist;

  ShowPointInfo;
end;

procedure TfrElevationProfile.ShowPointInfo;
var
  VSpeed, VElev, VDist: string;
begin
  // prepare
  VSpeed := '';
  VElev := '';

  if FSpeedSeries.Visible then begin
    VSpeed := Format(_('Speed: %.2f km/h') + #13#10, [FPointInfo.Speed]);
  end;
  if FElevationSeries.Visible then begin
    VElev := Format(_('Elevation: %.2f m') + #13#10, [FPointInfo.Elev]);
  end;

  if FIsDistInMeters then begin
    VDist := Format(_('Distance: %d m'), [Round(FPointInfo.Dist)]);
  end else begin
    VDist := Format(_('Distance: %.2f km'), [FPointInfo.Dist]);
  end;

  // caption
  lblPointInfo.Caption := VSpeed + VElev + VDist;

  pnlPointInfo.Width := lblPointInfo.Width + 10;
  pnlPointInfo.Height := lblPointInfo.Height + 10;

  pnlPointInfo.Left := FPointInfo.X + 10;
  pnlPointInfo.Top := FPointInfo.Y;

  pnlPointInfo.Visible := True;

  // line
  pnlPointLine.Height := chtProfile.ChartRect.Bottom - chtProfile.ChartRect.Top;

  pnlPointLine.Left := FPointInfo.X;
  pnlPointLine.Top := chtProfile.ChartRect.Top;

  pnlPointLine.Visible := True;
end;

procedure TfrElevationProfile.chtProfileResize(Sender: TObject);
begin
  FPointInfo.IsValid := False;
end;

procedure TfrElevationProfile.chtProfileScroll(Sender: TObject);
begin
  FPointInfo.IsValid := False;
end;

procedure TfrElevationProfile.chtProfileZoom(Sender: TObject);
begin
  FPointInfo.IsValid := False;
end;

procedure TfrElevationProfile.ShowProfile(
  const ALine: IGeometryLonLatLine
);
const
  CMaxDistInMeters = 9999;
var
  I: Integer;
  VCount: Integer;
  VStat: TProfileStatisticsRec;
  VLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  FPointInfo.IsValid := False;

  VStat := CEmptyStatRec;

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
      AddSingleLineSeries(FLine[I], VStat);
    end;

    FIsDistInMeters := VStat.Dist <= CMaxDistInMeters;

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
    FSpeedSeries.Visible := FShowSpeed and (VStat.Seconds > 0);

    if FElevationSeries.Visible then begin
      if chtProfile.MarginLeft < 8 then begin
        chtProfile.MarginLeft := 8;
      end;
    end;

    chtProfile.Axes.Left.SetMinMax(
      FElevationSeries.MinYValue * 0.99,
      FElevationSeries.MaxYValue * 1.01
    );
  finally
    EndUpdateSeries;
  end;

  if VStat.PointsCount > 0 then begin
    // calc average values
    VStat.ElevAvr := VStat.ElevAvr / VStat.PointsCount;
    VStat.SpeedAvr := VStat.SpeedAvr / VStat.PointsCount;
  end;

  // show statistics
  ShowStat(VStat);

  // reset zoom
  chtProfile.UndoZoom;
end;

procedure TfrElevationProfile.AddSingleLineSeries(
  const ALine: IGeometryLonLatSingleLine;
  var AStat: TProfileStatisticsRec
);
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
begin
  VPrevTimeIndex := -1;
  VPrevTimeValue := 0;

  VEnum := ALine.GetEnum;

  while VEnum.Next(VPoint, VMeta) do begin

    // Elevation
    if AStat.PointsCount > 0 then begin
      AStat.Dist := AStat.Dist + FDatum.CalcDist(VPrevPoint, VPoint);
    end;
    FDist[AStat.PointsCount] := AStat.Dist;

    if VMeta.IsElevationOk then begin
      VElev := VMeta.Elevation;
    end else begin
      VElev := 0;
    end;

    if VElev < AStat.ElevMin then begin
      AStat.ElevMin := VElev;
    end;
    if VElev > AStat.ElevMax then begin
      AStat.ElevMax := VElev;
    end;

    AStat.ElevAvr := AStat.ElevAvr + VElev; // accumulate only

    FElevationSeries.AddXY(AStat.Dist, VElev);

    // Speed
    VSpeed := 0;
    VSeconds := 0;

    if VMeta.IsTimeStampOk then begin
      if VPrevTimeIndex >= 0 then begin
        VDist := FDist[AStat.PointsCount] - FDist[VPrevTimeIndex];
        VSeconds := SecondsBetween(VMeta.TimeStamp, VPrevTimeValue);
        if VSeconds > 0 then begin
          VSpeed := VDist / VSeconds; // meters per second
        end;
      end;
      VPrevTimeIndex := AStat.PointsCount;
      VPrevTimeValue := VMeta.TimeStamp;
    end;

    if VSpeed < AStat.SpeedMin then begin
      AStat.SpeedMin := VSpeed;
    end;
    if VSpeed > AStat.SpeedMax then begin
      AStat.SpeedMax := VSpeed;
    end;

    AStat.SpeedAvr := AStat.SpeedAvr + VSpeed; // accumulate only
    Inc(AStat.Seconds, VSeconds);

    FSpeedSeries.AddXY(AStat.Dist, ToKmPerHour(VSpeed));

    // prepare next step
    Inc(AStat.PointsCount);
    VPrevPoint := VPoint;
  end;
end;

procedure TfrElevationProfile.ShowStat(const AStat: TProfileStatisticsRec);
const
  CSep = ',  ';
var
  VDistVal: string;
  VDistUnit: string;
begin
  if AStat.Dist > 1000 then begin
    VDistVal := Format('%.2f', [AStat.Dist / 1000]);
    VDistUnit := SAS_UNITS_km;
  end else begin
    VDistVal := IntToStr(Round(AStat.Dist));
    VDistUnit := SAS_UNITS_m;
  end;

  lblInfo.Caption :=
    Format(rsElevationProfileDistFmt, [VDistVal, VDistUnit]) + ',  ' +
    Format(rsElevationProfileElevFmt, [Round(AStat.ElevMin), Round(AStat.ElevAvr), Round(AStat.ElevMax), SAS_UNITS_m]);

  if AStat.Seconds > 0 then begin
    lblInfo.Caption := lblInfo.Caption + CSep +
      Format(rsElevationProfileSpeedFmt, [ToKmPerHourStr(AStat.SpeedMin),
        ToKmPerHourStr(AStat.SpeedAvr), ToKmPerHourStr(AStat.SpeedMax), SAS_UNITS_kmperh]
      ) + CSep +
      Format(
        rsElevationProfileTimeFmt, [FormatDateTime('hh:nn:ss', AStat.Seconds / SecsPerDay)]
      );
  end;
end;

function TfrElevationProfile.ToKmPerHour(const AMetersPerSec: Double): Double;
begin
  Result := AMetersPerSec / 1000 * 3600;
end;

function TfrElevationProfile.ToKmPerHourStr(const AMetersPerSec: Double): string;
begin
  Result := Format('%.2f', [ToKmPerHour(AMetersPerSec)]);
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

// PopupMenu Actions

procedure TfrElevationProfile.mniShowSpeedClick(Sender: TObject);
begin
  FShowSpeed := mniShowSpeed.Checked;
  FSpeedSeries.Visible := FShowSpeed;
end;

procedure TfrElevationProfile.mniShowElevationClick(Sender: TObject);
begin
  FShowElevation := mniShowElevation.Checked;
  FElevationSeries.Visible := FShowElevation;
end;

procedure TfrElevationProfile.mniResetZoomClick(Sender: TObject);
begin
  chtProfile.UndoZoom;
end;

end.
