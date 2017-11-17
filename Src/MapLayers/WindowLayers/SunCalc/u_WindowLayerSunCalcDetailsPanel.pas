{******************************************************************************}
{* SAS.Planet (SAS.œÎ‡ÌÂÚ‡)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_WindowLayerSunCalcDetailsPanel;

interface

uses
  Types,
  Controls,
  Classes,
  GR32,
  GR32_Image,
  GR32_Layers,
  t_SunCalcConfig,
  t_GeoTypes,
  i_PopUp,
  i_SunCalcConfig,
  i_SunCalcProvider,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  u_GeoFunc,
  u_WindowLayerWithBitmapBase;

type
  TRenderedTextRec = record
    Text: string;
    Bitmap: TBitmap32;
  end;

  TWindowLayerSunCalcDetailsPanel = class(TWindowLayerWithBitmapBase)
  private
    FFont: TSunCalcFontInfo;
    FColors: TSunCalcDetailsPanelColors;

    FMargins: TRect;
    FBorder: TRect;

    FHeight: Integer;
    FWidth: Integer;

    FLocation: TDoublePoint;
    FDateTime: TDateTime;
    FTzOffset: Extended;

    FPopUpMenu: IPopUp;
    FSunCalcConfig: ISunCalcConfig;
    FSunCalcProvider: ISunCalcProvider;
    FLocalCoordConverter: ILocalCoordConverterChangeable;

    FRowHeight: Integer;
    FRowsCount: Integer;

    FColWidth: TSunCalcDetailsPanelColsWidth;

    FIsDetailedView: Boolean;
    procedure OnMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure DrawRow(
      const ARowNum: Integer;
      const ATime: TDateTime;
      const AName: string
    );
    procedure OnPosChange;
    procedure OnSunCalcConfigChange;
    procedure OnSunCalcProviderChange;
  protected
    function GetNewBitmapSize: TPoint; override;
    function GetNewLayerLocation: TFloatRect; override;
    procedure DoUpdateBitmapDraw; override;
    procedure DoUpdateLayerVisibility; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const ALocalCoordConverter: ILocalCoordConverterChangeable;
      const ASunCalcConfig: ISunCalcConfig;
      const ASunCalcProvider: ISunCalcProvider;
      const ASunCalcPopUpMenu: IPopUp
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  DateUtils,
  SunCalc,
  i_LocalCoordConverter,
  u_ListenerByEvent,
  u_TimeZoneInfo;

resourcestring
  rsTime = 'Time';
  rsAz = 'Az.';
  rsAlt = 'Alt.';
  rsEvent = 'Event';

  rsAstroStart = 'Astro. twilight start';
  rsNauticalSatrt = 'Nautical twilight start';
  rsCivilStart = 'Civil twilight start';
  rsRise = 'Rise';
  rsGoldenHrEnd = 'Golden hour end';
  rsNoon = 'Noon';
  rsGoldenHrStart = 'Golden hour start';
  rsSet = 'Set';
  rsCivilEnd = 'Civil twilight end';
  rsNauticalEnd = 'Nautical twilight end';
  rsAstroEnd = 'Astro. twilight end';
  rsMidnight = 'Midnight';

{ TWindowLayerSunCalcDetailsPanel }

constructor TWindowLayerSunCalcDetailsPanel.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier:INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const ALocalCoordConverter: ILocalCoordConverterChangeable;
  const ASunCalcConfig: ISunCalcConfig;
  const ASunCalcProvider: ISunCalcProvider;
  const ASunCalcPopUpMenu: IPopUp
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TBitmapLayer.Create(AParentMap.Layers)
  );

  FPopUpMenu := ASunCalcPopUpMenu;
  FSunCalcConfig := ASunCalcConfig;
  FSunCalcProvider := ASunCalcProvider;
  FLocalCoordConverter := ALocalCoordConverter;

  Layer.OnMouseDown := OnMouseDown;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcProviderChange),
    FSunCalcProvider.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcConfigChange),
    FSunCalcConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalCoordConverter.ChangeNotifier
  );

  FFont.FontName := '';
  FFont.FontSize := 0;
  FFont.TextColor := 0;
  FFont.BgColor := 0;

  FMargins := Rect(0, FSunCalcConfig.DetailsPanelRowHight, 0, 0);
  FBorder := Rect(10, 5, 0, 0);

  FDateTime := 0;
  FLocation := CEmptyDoublePoint;
  FTzOffset := NaN;

  FIsDetailedView := False;
end;

destructor TWindowLayerSunCalcDetailsPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TWindowLayerSunCalcDetailsPanel.OnMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbRight then begin
    FPopUpMenu.PopUp;
  end;
end;

procedure TWindowLayerSunCalcDetailsPanel.DrawRow(
  const ARowNum: Integer;
  const ATime: TDateTime;
  const AName: string
);
var
  X, Y: Integer;
  VSunPos: TSunPos;
  VTime: string;
  VAzimuth: string;
  VAltitude: string;
  VLocalTime: TDateTime;
begin
  if ATime <> 0 then begin
    if FIsDetailedView then begin
      VSunPos := SunCalc.GetPosition(ATime, FLocation.Y, FLocation.X);
      VAzimuth := Format('%.0f∞', [RadToDeg(VSunPos.Azimuth + Pi)]);
      VAltitude := Format('%.1f∞', [RadToDeg(VSunPos.Altitude)]);
    end;
    VLocalTime := TTimeZoneInfo.UTCToTzLocalTime(ATime, FTzOffset);
    VTime := FormatDateTime('hh:mm', VLocalTime);
  end else begin
    VTime := '';
    VAzimuth := '';
    VAltitude := '';
  end;

  X := FBorder.Left;
  Y := FBorder.Top + ARowNum * FRowHeight;

  Layer.Bitmap.RenderText(X, Y, VTime, 0, FFont.TextColor);
  Inc(X, FColWidth[0]);

  if FIsDetailedView then begin
    Layer.Bitmap.RenderText(X, Y, VAzimuth, 0, FFont.TextColor);
  end;
  Inc(X, FColWidth[1]);

  if FIsDetailedView then begin
    Layer.Bitmap.RenderText(X, Y, VAltitude, 0, FFont.TextColor);
  end;
  Inc(X, FColWidth[2]);

  Layer.Bitmap.RenderText(X, Y, AName, 0, FFont.TextColor);
end;

procedure TWindowLayerSunCalcDetailsPanel.DoUpdateBitmapDraw;
var
  I: Integer;
  X, Y: Integer;
  VDay: TDateTime;
  VSunTimes: TSunCalcTimes;
begin
  inherited;

  Layer.Bitmap.BeginUpdate;
  try
    Layer.Bitmap.Clear(FColors.BgColor);

    Layer.Bitmap.Font.Name := FFont.FontName;
    Layer.Bitmap.Font.Size := FFont.FontSize;

    for I := 0 to FRowsCount - 1 do begin
      Layer.Bitmap.HorzLineS(0, FRowHeight * (I + 1), FWidth - 1, FColors.GridLinesColor);
    end;

    I := 0;
    X := FBorder.Left;
    Y := FBorder.Top;

    if FIsDetailedView then begin
      Layer.Bitmap.RenderText(X, Y, rsTime, 0, FFont.TextColor);
      Inc(X, FColWidth[0]);

      Layer.Bitmap.RenderText(X, Y, rsAz, 0, FFont.TextColor);
      Inc(X, FColWidth[1]);

      Layer.Bitmap.RenderText(X, Y, rsAlt, 0, FFont.TextColor);
      Inc(X, FColWidth[2]);

      Layer.Bitmap.RenderText(X, Y, rsEvent, 0, FFont.TextColor);

      Inc(I);
    end;

    VDay := StartOfTheDay(TTimeZoneInfo.UTCToTzLocalTime(FDateTime, FTzOffset));
    VSunTimes := SunCalc.GetTimes(VDay, FLocation.Y, FLocation.X);

    if FIsDetailedView then begin
      DrawRow(I, VSunTimes[nightEnd].Value, rsAstroStart);
      Inc(I);

      DrawRow(I, VSunTimes[nauticalDawn].Value, rsNauticalSatrt);
      Inc(I);
    end;

    DrawRow(I, VSunTimes[Dawn].Value, rsCivilStart);
    Inc(I);

    DrawRow(I, VSunTimes[sunrise].Value, rsRise);
    Inc(I);

    if FIsDetailedView then begin
      DrawRow(I, VSunTimes[goldenHourEnd].Value, rsGoldenHrEnd);
      Inc(I);
    end;

    DrawRow(I, VSunTimes[solarNoon].Value, rsNoon);
    Inc(I);

    if FIsDetailedView then begin
      DrawRow(I, VSunTimes[goldenHour].Value, rsGoldenHrStart);
      Inc(I);
    end;

    DrawRow(I, VSunTimes[sunset].Value, rsSet);
    Inc(I);

    DrawRow(I, VSunTimes[Dusk].Value, rsCivilEnd);
    Inc(I);

    if FIsDetailedView then begin
      DrawRow(I, VSunTimes[nauticalDusk].Value, rsNauticalEnd);
      Inc(I);

      DrawRow(I, VSunTimes[night].Value, rsAstroEnd);
      Inc(I);

      DrawRow(I, VSunTimes[nadir].Value, rsMidnight);
    end;
  finally
    Layer.Bitmap.EndUpdate;
    Layer.Bitmap.Changed;
  end;
end;

procedure TWindowLayerSunCalcDetailsPanel.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

function TWindowLayerSunCalcDetailsPanel.GetNewBitmapSize: TPoint;
begin
  Result.X := FWidth;
  Result.Y := FHeight;
end;

function TWindowLayerSunCalcDetailsPanel.GetNewLayerLocation: TFloatRect;
begin
  Result.Left := FLocalCoordConverter.GetStatic.GetLocalRectSize.X - FWidth;
  Result.Top := FSunCalcConfig.YearTimeLineHight + FSunCalcConfig.DayTimeLineHight + FMargins.Top;
  Result.Right := Result.Left + Layer.Bitmap.Width;
  Result.Bottom := Result.Top + Layer.Bitmap.Height;
end;

procedure TWindowLayerSunCalcDetailsPanel.OnSunCalcConfigChange;
var
  I: Integer;
  VColorSchema: ISunCalcColorSchema;
  VColorSchemaStatic: ISunCalcColorSchemaStatic;
begin
  ViewUpdateLock;
  try
    FSunCalcConfig.LockRead;
    try
      Visible := FSunCalcConfig.Visible;
      
      FIsDetailedView := FSunCalcConfig.IsDetailedView;

      FColWidth := FSunCalcConfig.DetailsPanelColsWidth;
      if not FIsDetailedView then begin
        FColWidth[1] := 0; // Az.
        FColWidth[2] := 0; // Alt.
      end;

      FWidth := 0;
      for I := 0 to Length(FColWidth) - 1 do begin
        Inc(FWidth, FColWidth[I]);
      end;

      if FIsDetailedView then begin
        FRowsCount := 12 + 1;
      end else begin
        FRowsCount := 5;
      end;
      FRowHeight := FSunCalcConfig.DetailsPanelRowHight;
      FHeight := FRowHeight * FRowsCount;

      VColorSchema := FSunCalcConfig.ColorSchemaList.GetActiveColorSchema;
      VColorSchemaStatic := VColorSchema.GetStatic;

      FFont := VColorSchemaStatic.DetailsPanelFont;
      FColors := VColorSchemaStatic.DetailsPanelColors;
    finally
      FSunCalcConfig.UnlockRead;
    end;

    SetNeedUpdateBitmapSize;
    SetNeedUpdateLayerLocation;
    SetNeedUpdateBitmapDraw;
    SetNeedUpdateLayerVisibility;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerSunCalcDetailsPanel.OnSunCalcProviderChange;
var
  VLocation: TDoublePoint;
  VDateTime: TDateTime;
  VTzOffset: Extended;
  VIsSameLocation: Boolean;
  VIsSameDate: Boolean;
  VIsSameTzOffset: Boolean;
begin
  ViewUpdateLock;
  try
    FSunCalcProvider.LockRead;
    try
      VLocation := FSunCalcProvider.Location;
      VDateTime := FSunCalcProvider.UTCDateTime;
    finally
      FSunCalcProvider.UnlockRead;
    end;

    if (VDateTime = 0) or PointIsEmpty(VLocation) then begin
      Exit;
    end;

    VIsSameLocation := DoublePointsEqual(VLocation, FLocation);
    VIsSameDate := SameDate(VDateTime, FDateTime);

    if IsNan(FTzOffset) or not VIsSameLocation or not VIsSameDate then begin
      if not FSunCalcProvider.GetTzOffset(VDateTime, VTzOffset) then begin
        VTzOffset := TTimeZoneInfo.GetSystemTzOffset(VDateTime);
      end;
      if IsNan(FTzOffset) then begin
        VIsSameTzOffset := False;
      end else begin
        VIsSameTzOffset := SameValue(VTzOffset, FTzOffset);
      end;
    end else begin
      VTzOffset := FTzOffset;
      VIsSameTzOffset := True;
    end;

    if not VIsSameLocation or not VIsSameDate or not VIsSameTzOffset then begin
      SetNeedUpdateBitmapDraw;
    end;

    FDateTime := VDateTime;
    FLocation := VLocation;
    FTzOffset := VTzOffset;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerSunCalcDetailsPanel.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateBitmapSize;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerSunCalcDetailsPanel.StartThreads;
begin
  inherited;
  OnSunCalcConfigChange;
  OnSunCalcProviderChange;
end;

end.
