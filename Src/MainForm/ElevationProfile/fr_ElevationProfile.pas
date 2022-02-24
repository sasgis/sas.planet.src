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
  SysUtils,
  Classes,
  Controls,
  Graphics,
  Forms,
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
  u_CommonFormAndFrameParents, Vcl.Menus;

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

  TOnCloseEvent = procedure() of object;

  TfrElevationProfile = class(TFrame)
    pnlTop: TPanel;
    chtProfile: TChart;
    lblInfo: TLabel;
    btnClose: TTBXButton;
    pmMain: TPopupMenu;
    mniShowSpeed: TMenuItem;
    mniShowElevation: TMenuItem;
    N1: TMenuItem;
    mniResetZoom: TMenuItem;
    procedure btnCloseClick(Sender: TObject);
    procedure mniShowSpeedClick(Sender: TObject);
    procedure mniResetZoomClick(Sender: TObject);
    procedure mniShowElevationClick(Sender: TObject);
  private
    FDatum: IDatum;
    FOnClose: TOnCloseEvent;

    FShowSpeed: Boolean;
    FShowElevation: Boolean;

    FLine: array of IGeometryLonLatSingleLine;
    FDist: array of Double;

    FLineElevationSeries: TAreaSeries;
    FLineSpeedSeries: TAreaSeries;

    function CreateAreaSeries(
      const AVertAxis: TVertAxis;
      const AColor: TColor;
      const ATransparency: Integer
    ): TAreaSeries;

    procedure AddSingleLineSeries(
      const ALine: IGeometryLonLatSingleLine;
      var AStat: TProfileStatisticsRec
    );
    procedure ShowStat(const AStat: TProfileStatisticsRec);

    function ToKmPerHour(const AMetersPerSec: Double): Double; inline;
    function ToKmPerHourStr(const AMetersPerSec: Double): string; inline;
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
  t_GeoTypes,
  i_EnumDoublePoint,
  u_ResStrings;

resourcestring
  rsElevationProfileDistFmt = 'Distance: %s %s';
  rsElevationProfileElevFmt = 'Elevation: %d, %d, %d %s';
  rsElevationProfileSpeedFmt = 'Speed: %s, %s, %s %s';
  rsElevationProfileTimeFmt = 'Time: %s';

const
  CSeriesTransparency = 75;

  CMaxDistMeters = 10000;

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

  FLineSpeedSeries := CreateAreaSeries(aRightAxis, clBlue, CSeriesTransparency);
  FLineElevationSeries := CreateAreaSeries(aLeftAxis, clRed, CSeriesTransparency);

  FShowSpeed := True;
  FShowElevation := True;

  mniShowSpeed.Checked := FShowSpeed;
  mniShowElevation.Checked := FShowElevation;
end;

destructor TfrElevationProfile.Destroy;
begin

  inherited;
end;

procedure TfrElevationProfile.Clear;
begin
  FLine := nil;
  FDist := nil;

  FLineSpeedSeries.Clear;
  FLineElevationSeries.Clear;
end;

procedure TfrElevationProfile.btnCloseClick(Sender: TObject);
begin
  FOnClose();
  Clear;
end;

function TfrElevationProfile.CreateAreaSeries(
  const AVertAxis: TVertAxis;
  const AColor: TColor;
  const ATransparency: Integer
): TAreaSeries;
begin
  Result := TAreaSeries.Create(chtProfile);

  with Result do begin
    AreaLinesPen.Visible := False;
    DrawArea := True;
    DrawStyle := dsAll;
    LinePen.Visible := False;
    Pointer.Visible := False;
    Transparency := ATransparency;
    ParentChart := chtProfile;
    VertAxis := AVertAxis;
    Color := AColor;
  end;
end;

procedure TfrElevationProfile.ShowProfile(
  const ALine: IGeometryLonLatLine
);
var
  I: Integer;
  VCount: Integer;
  VStat: TProfileStatisticsRec;
  VLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  Clear;

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

  for I := 0 to Length(FLine) - 1 do begin
    AddSingleLineSeries(FLine[I], VStat);
  end;

  if VStat.Dist > CMaxDistMeters then begin
    for I := 0 to Length(FDist) - 1 do begin
      FDist[I] := FDist[I] / 1000;
      FLineElevationSeries.XValue[I] := FDist[I];
      FLineSpeedSeries.XValue[I] := FDist[I];
    end;
  end;

  // calc average values
  VStat.ElevAvr := VStat.ElevAvr / VStat.PointsCount;
  VStat.SpeedAvr := VStat.SpeedAvr / VStat.PointsCount;

  // reset zoom
  chtProfile.UndoZoom;

  // show statistics
  ShowStat(VStat);

  FLineElevationSeries.Visible := FShowElevation;
  FLineSpeedSeries.Visible := FShowSpeed;
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

    FLineElevationSeries.AddXY(AStat.Dist, VElev);

    // Speed
    VSpeed := 0;
    VSeconds := 0;

    if VMeta.IsTimeStampOk then begin
      if VPrevTimeIndex >= 0 then begin
        VDist := FDist[AStat.PointsCount] - FDist[VPrevTimeIndex];
        VSeconds := SecondsBetween(VMeta.TimeStamp, VPrevTimeValue);
        if VSeconds > 0 then begin
          VSpeed := VDist / VSeconds; // m/s
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

    FLineSpeedSeries.AddXY(AStat.Dist, ToKmPerHour(VSpeed));

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

// PopupMenu Actions

procedure TfrElevationProfile.mniShowSpeedClick(Sender: TObject);
begin
  FShowSpeed := mniShowSpeed.Checked;
  FLineSpeedSeries.Visible := FShowSpeed;
end;

procedure TfrElevationProfile.mniShowElevationClick(Sender: TObject);
begin
  FShowElevation := mniShowElevation.Checked;
  FLineElevationSeries.Visible := FShowElevation;
end;

procedure TfrElevationProfile.mniResetZoomClick(Sender: TObject);
begin
  chtProfile.UndoZoom;
end;

end.
