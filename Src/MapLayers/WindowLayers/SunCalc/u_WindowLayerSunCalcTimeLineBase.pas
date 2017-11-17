{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
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

unit u_WindowLayerSunCalcTimeLineBase;

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
  i_MouseState,
  i_NotifierTime,
  i_MarkerDrawable,
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

  TWindowLayerSunCalcTimeLineBase = class(TWindowLayerWithBitmapBase)
  protected
    FFont: TSunCalcFontInfo;
    FMarkerCaptionFont: TSunCalcFontInfo;

    FMargins: TRect;
    FBorder: TRect;

    FHeight: Integer;
    FTimeLineTop: Integer;
    FTimeLineWidth: Integer;
    FTimeLineHeight: Integer;

    FMarker: IMarkerDrawable;

    FLocation: TDoublePoint;
    FDateTime: TDateTime;
    FTzOffset: Extended;
    FIsMarkerMoving: Boolean;

    FPopUpMenu: IPopUp;
    FSunCalcConfig: ISunCalcConfig;
    FSunCalcProvider: ISunCalcProvider;
    FLocalCoordConverter: ILocalCoordConverterChangeable;

    FRedrawOnDateChanged: Boolean;
    FRedrawOnDateTimeChanged: Boolean;
    FRedrawOnLocationChanged: Boolean;
    FRedrawOnTzOffsetChanged: Boolean;

    FMouseState: IMouseState;
    FIsShowMarkerCaption: Boolean;

    FMarkerCaptionText: string;
    FMarkerCaptionRect: TRect;
    FMarkerRenderedText: TBitmap32;

    FRenderedText: array of TRenderedTextRec;

    procedure ClearRenderedText;

    function IsMouseAboveMarker: Boolean; inline;

    function PosToUtcDateTime(const X: Integer): TDateTime; virtual; abstract;
    function UtcDateTimeToPosF(const ADateTime: TDateTime): Double; virtual; abstract;

    function GetMarkerPos: TDoublePoint; inline;
    procedure UpdateTimeLineWidth(const ABitmapWidth: Integer); inline;

    procedure OnPosChange;
    procedure OnTimerEvent;
    procedure OnSunCalcConfigChange; virtual; abstract;
    procedure OnSunCalcProviderChange;

    procedure OnMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure OnMouseUp(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure OnMouseMove(
      Sender: TObject;
      Shift: TShiftState;
      X, Y: Integer
    );

    procedure DrawMarker(const ACaptionText: string);
    procedure DrawScaleItems(const AVertLineColor: TColor32);
  protected
    function GetNewBitmapSize: TPoint; override;
    function GetNewLayerLocation: TFloatRect; override;
    procedure DoUpdateLayerVisibility; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AMouseState: IMouseState;
      const ATimerNoifier: INotifierTime;
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
  u_TimeZoneInfo,
  i_LocalCoordConverter,
  u_ListenerByEvent,
  u_ListenerTime;

{ TWindowLayerSunCalcTimeLineBase }

constructor TWindowLayerSunCalcTimeLineBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier:INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AMouseState: IMouseState;
  const ATimerNoifier: INotifierTime;
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

  FMouseState := AMouseState;
  FSunCalcConfig := ASunCalcConfig;
  FSunCalcProvider := ASunCalcProvider;
  FLocalCoordConverter := ALocalCoordConverter;
  FPopUpMenu := ASunCalcPopUpMenu;

  Layer.OnMouseDown := OnMouseDown;
  Layer.OnMouseUp := OnMouseUp;
  Layer.OnMouseMove := OnMouseMove;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcProviderChange),
    FSunCalcProvider.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcConfigChange),
    FSunCalcConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcConfigChange),
    FSunCalcConfig.ColorSchemaList.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalCoordConverter.ChangeNotifier
  );

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimerEvent, 100),
    ATimerNoifier
  );

  FFont.FontName := '';
  FFont.FontSize := 0;
  FFont.TextColor := 0;
  FFont.BgColor := 0;

  FMarkerCaptionFont.FontName := '';
  FMarkerCaptionFont.FontSize := 0;
  FMarkerCaptionFont.TextColor := 0;
  FMarkerCaptionFont.BgColor := 0;


  FMargins := Rect(0, 0, 0, 0);
  FBorder := Rect(0, 0, 0, 0);

  FHeight := 0;
  FTimeLineTop := 0;
  FTimeLineWidth := 0;
  FTimeLineHeight := 0;

  FDateTime := 0;
  FLocation := CEmptyDoublePoint;

  FMarker := nil;
  FIsMarkerMoving := False;

  FMarkerCaptionText := '';
  FMarkerRenderedText := nil;
  FIsShowMarkerCaption := False;

  FRedrawOnDateChanged := False;
  FRedrawOnDateTimeChanged := False;
  FRedrawOnLocationChanged := False;
  FRedrawOnTzOffsetChanged := False;
end;

destructor TWindowLayerSunCalcTimeLineBase.Destroy;
begin
  ClearRenderedText;
  FreeAndNil(FMarkerRenderedText);
  inherited Destroy;
end;

procedure TWindowLayerSunCalcTimeLineBase.DrawScaleItems(
  const AVertLineColor: TColor32
);
var
  I: Integer;
  VBitmap: TBitmap32;
  VTextSize: TSize;
  VStep: Integer;
  VItemsCount: Integer;
begin
  VItemsCount := Length(FRenderedText);

  if VItemsCount = 0 then begin
    Exit;
  end;

  VStep := FTimeLineWidth div VItemsCount;

  // Draw Vert lines
  for I := 0 to VItemsCount - 1 do begin
    Layer.Bitmap.VertLineS(
      FBorder.Left + I * VStep,
      FBorder.Top,
      FTimeLineTop - 1,
      AVertLineColor
    );
  end;

  // Draw Captions
  for I := 0 to VItemsCount - 1 do begin
    if FRenderedText[I].Bitmap = nil then begin
      FRenderedText[I].Bitmap := TBitmap32.Create;

      VBitmap := FRenderedText[I].Bitmap;

      if FFont.FontName <> '' then begin
        VBitmap.Font.Name := FFont.FontName;
      end;

      if FFont.FontSize > 0 then begin
        VBitmap.Font.Size := FFont.FontSize;
      end;

      VTextSize := VBitmap.TextExtent(FRenderedText[I].Text);
      VBitmap.SetSize(VTextSize.cx, VTextSize.cy);

      VBitmap.RenderText(0, 0, FRenderedText[I].Text, 0, FFont.TextColor);
      VBitmap.DrawMode := dmBlend;
    end;

    FRenderedText[I].Bitmap.DrawTo(
      Layer.Bitmap,
      FBorder.Left + I * VStep + 10,
      FBorder.Top
    );
  end;
end;

procedure TWindowLayerSunCalcTimeLineBase.DrawMarker(
  const ACaptionText: string
);
var
  VShift: Integer;
  VTextSize: TSize;
  VMarkerPos: TDoublePoint;
  VMarkerRect: TRect;
begin
  Assert(FMarker <> nil);

  VMarkerPos := GetMarkerPos;
  if PointIsEmpty(VMarkerPos) then begin
    Exit;
  end;

  // Draw Marker
  FMarker.DrawToBitmap(Layer.Bitmap, VMarkerPos);

  // Draw Caption
  if ACaptionText <> '' then begin
    if (ACaptionText <> FMarkerCaptionText) or (FMarkerRenderedText = nil) then begin
      if FMarkerRenderedText = nil then begin
        FMarkerRenderedText := TBitmap32.Create;

        if FMarkerCaptionFont.FontName <> '' then begin
          FMarkerRenderedText.Font.Name := FMarkerCaptionFont.FontName;
        end;

        if FMarkerCaptionFont.FontSize > 0 then begin
          FMarkerRenderedText.Font.Size := FMarkerCaptionFont.FontSize;
        end;
      end;

      VTextSize := FMarkerRenderedText.TextExtent(ACaptionText);
      VMarkerRect := FMarker.GetBoundsForPosition(VMarkerPos);

      FMarkerCaptionRect.Left := VMarkerRect.Right + 2;
      FMarkerCaptionRect.Top := VMarkerRect.Top - 1 - VTextSize.cy;
      FMarkerCaptionRect.Right := FMarkerCaptionRect.Left + VTextSize.cx + 8;
      FMarkerCaptionRect.Bottom := VMarkerRect.Top;

      FMarkerRenderedText.SetSize(
        FMarkerCaptionRect.Right - FMarkerCaptionRect.Left,
        FMarkerCaptionRect.Bottom - FMarkerCaptionRect.Top
      );

      FMarkerRenderedText.Clear(FMarkerCaptionFont.BgColor);
      FMarkerRenderedText.RenderText(4, 0, ACaptionText, 0, FMarkerCaptionFont.TextColor);

      FMarkerCaptionText := ACaptionText;

      if FMarkerCaptionRect.Right > Layer.Bitmap.Width - 1 then begin
        VShift := 2 + 2 +
          (FMarkerCaptionRect.Right - FMarkerCaptionRect.Left) +
          (VMarkerRect.Right - VMarkerRect.Left);

        Dec(FMarkerCaptionRect.Left, VShift);
        Dec(FMarkerCaptionRect.Right, VShift);
      end;
    end;

    FMarkerRenderedText.DrawTo(
      Layer.Bitmap,
      FMarkerCaptionRect.Left,
      FMarkerCaptionRect.Top
    );
  end;
end;

procedure TWindowLayerSunCalcTimeLineBase.ClearRenderedText;
var
  I: Integer;
begin
  for I := Low(FRenderedText) to High(FRenderedText) do begin
    FreeAndNil(FRenderedText[I].Bitmap);
  end;
end;

function TWindowLayerSunCalcTimeLineBase.GetMarkerPos: TDoublePoint;
begin
  if FDateTime <> 0 then begin
    Result.X := UtcDateTimeToPosF(FDateTime);
    Result.Y := FTimeLineTop + FTimeLineHeight / 2;
  end else begin
    Result := CEmptyDoublePoint;
  end;
end;

procedure TWindowLayerSunCalcTimeLineBase.UpdateTimeLineWidth(const ABitmapWidth: Integer);
begin
  FTimeLineWidth := ((ABitmapWidth - FBorder.Left - FBorder.Right) div 24) * 24;
end;

procedure TWindowLayerSunCalcTimeLineBase.OnMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  case Button of
    mbRight: begin
      FPopUpMenu.PopUp;
    end;

    mbLeft: begin
      FIsMarkerMoving := True;
      FSunCalcProvider.UTCDateTime := PosToUtcDateTime(X - FMargins.Left);
    end;
  end;
end;

procedure TWindowLayerSunCalcTimeLineBase.OnMouseUp(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  FIsMarkerMoving := False;
end;

procedure TWindowLayerSunCalcTimeLineBase.OnMouseMove(
  Sender: TObject;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if FIsMarkerMoving then begin
    FSunCalcProvider.UTCDateTime := PosToUtcDateTime(X - FMargins.Left);
  end;
end;

procedure TWindowLayerSunCalcTimeLineBase.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

function TWindowLayerSunCalcTimeLineBase.GetNewBitmapSize: TPoint;
begin
  Result.X := FLocalCoordConverter.GetStatic.GetLocalRectSize.X - FMargins.Left - FMargins.Right;
  Result.Y := FHeight;
  UpdateTimeLineWidth(Result.X);
end;

function TWindowLayerSunCalcTimeLineBase.GetNewLayerLocation: TFloatRect;
begin
  Result.Left := FMargins.Left;
  Result.Top := FMargins.Top;
  Result.Right := Result.Left + Layer.Bitmap.Width;
  Result.Bottom := Result.Top + Layer.Bitmap.Height;
end;

procedure TWindowLayerSunCalcTimeLineBase.OnSunCalcProviderChange;
var
  VLocation: TDoublePoint;
  VDateTime: TDateTime;
  VTzOffset: Extended;
  VIsSameLocation: Boolean;
  VIsSameDate: Boolean;
  VIsSameDateTime: Boolean;
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
    VIsSameDateTime := VIsSameDate and SameDateTime(VDateTime, FDateTime);

    if not VIsSameLocation or not VIsSameDate then begin
      if not FSunCalcProvider.GetTzOffset(VDateTime, VTzOffset) then begin
        VTzOffset := TTimeZoneInfo.GetSystemTzOffset(VDateTime);
      end;
      VIsSameTzOffset := SameValue(VTzOffset, FTzOffset);
    end else begin
      VTzOffset := FTzOffset;
      VIsSameTzOffset := True;
    end;

    if (not VIsSameLocation and FRedrawOnLocationChanged) or
       (not VIsSameDate and FRedrawOnDateChanged) or
       (not VIsSameDateTime and FRedrawOnDateTimeChanged) or
       (not VIsSameTzOffset and FRedrawOnTzOffsetChanged)
    then begin
      SetNeedUpdateBitmapDraw;
    end;

    FDateTime := VDateTime;
    FLocation := VLocation;
    FTzOffset := VTzOffset;
  finally
    ViewUpdateUnlock;
  end;
end;

function TWindowLayerSunCalcTimeLineBase.IsMouseAboveMarker: Boolean;
var
  VMousePos: TPoint;
  VMarkerPosAbsolute: TDoublePoint;
  VMarkerRect: TRect;
begin
  if FMarker = nil then begin
    Result := False;
    Exit;
  end;

  VMousePos := FMouseState.CurentPos;
  VMarkerPosAbsolute := GetMarkerPos;

  if PointIsEmpty(VMarkerPosAbsolute) then begin
    Result := False;
    Exit;
  end;

  VMarkerPosAbsolute.X := VMarkerPosAbsolute.X + FMargins.Left;
  VMarkerPosAbsolute.Y := VMarkerPosAbsolute.Y + FMargins.Top;

  VMarkerRect := FMarker.GetBoundsForPosition(VMarkerPosAbsolute);

  Result :=
    (VMousePos.X <= VMarkerRect.Right) and (VMousePos.X >= VMarkerRect.Left) and
    (VMousePos.Y >= VMarkerRect.Top) and (VMousePos.Y <= VMarkerRect.Bottom);
end;

procedure TWindowLayerSunCalcTimeLineBase.OnTimerEvent;
var
  VIsMouseAboveMarker: Boolean;
  VIsShowMarkerCaption: Boolean;
begin
  if not Visible or FIsMarkerMoving then begin
    Exit;
  end;

  VIsShowMarkerCaption := FIsShowMarkerCaption;
  VIsMouseAboveMarker := IsMouseAboveMarker;

  if VIsMouseAboveMarker and not VIsShowMarkerCaption then begin
    VIsShowMarkerCaption := True;
  end else if not VIsMouseAboveMarker and VIsShowMarkerCaption then begin
    if not FIsMarkerMoving then begin
      VIsShowMarkerCaption := False;
    end;
  end;

  if VIsShowMarkerCaption <> FIsShowMarkerCaption then begin
    ViewUpdateLock;
    try
      FIsShowMarkerCaption := VIsShowMarkerCaption;
      SetNeedUpdateBitmapDraw;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TWindowLayerSunCalcTimeLineBase.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateBitmapSize;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerSunCalcTimeLineBase.StartThreads;
begin
  inherited;
  OnSunCalcConfigChange;
  OnSunCalcProviderChange;
end;

end.
