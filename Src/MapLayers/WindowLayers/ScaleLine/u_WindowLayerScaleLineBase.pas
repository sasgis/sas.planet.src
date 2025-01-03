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

unit u_WindowLayerScaleLineBase;

interface

uses
  Types,
  Controls,
  Classes,
  SysUtils,
  GR32,
  GR32_Image,
  i_NotifierOperation,
  i_NotifierTime,
  i_SimpleFlag,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_ScaleLineConfig,
  i_PopUp,
  u_WindowLayerWithBitmapBase;

type
  TWindowLayerScaleLineBase = class(TWindowLayerWithBitmapBase)
  private
    FConfig: IScaleLineConfig;
    FView: ILocalCoordConverterChangeable;
    FLayerChangeFlag: ISimpleFlag;
    FTmpBitmap: TBitmap32;
    FPopupMenu: IPopUp;
    procedure OnConfigChange;
    procedure OnPosChange;
    procedure OnTimer;
    procedure OnMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
  protected
    class function ValueToStr(
      const AValue: Double;
      const ADigits: Integer;
      const AUnits: string
    ): string; inline;
    procedure DrawOutLinedText(
      const ABitmap: TBitmap32;
      const X, Y: Integer;
      const AText: string;
      const ATextColor: TColor32;
      const AOutLineColor: TColor32
    );
    function GetNiceLen(const ALen: Double): Double;
    function GetNewVisibility: Boolean; virtual; abstract;
    function GetNewLayerLocation: TFloatRect; override;
    procedure DoUpdateLayerVisibility; override;
    procedure StartThreads; override;
    property Config: IScaleLineConfig read FConfig;
    property View: ILocalCoordConverterChangeable read FView;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AGuiSyncronizedTimerNotifier: INotifierTime;
      const APopupMenu: IPopUp;
      const AConfig: IScaleLineConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  GR32_Layers,
  GR32_Resamplers,
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock,
  u_ListenerTime;

{ TWindowLayerScaleLineBase }

constructor TWindowLayerScaleLineBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AGuiSyncronizedTimerNotifier: INotifierTime;
  const APopupMenu: IPopUp;
  const AConfig: IScaleLineConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TBitmapLayer.Create(AParentMap.Layers)
  );

  FConfig := AConfig;
  FView := AView;
  FPopupMenu := APopupMenu;

  Layer.AlphaHit := True;
  Layer.OnMouseDown := OnMouseDown;

  FTmpBitmap := TBitmap32.Create;
  FLayerChangeFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FView.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 100),
    AGuiSyncronizedTimerNotifier
  );
end;

destructor TWindowLayerScaleLineBase.Destroy;
begin
  FreeAndNil(FTmpBitmap);
  inherited;
end;

function TWindowLayerScaleLineBase.GetNiceLen(const ALen: Double): Double;
const
  CNiceValues: array [0..54] of Double = (
    40000000,
    30000000,
    20000000,
    15000000,
    10000000,
    8000000,
    5000000,
    4000000,
    3000000,
    2000000,
    1500000,
    1000000,
    800000,
    500000,
    400000,
    300000,
    200000,
    150000,
    100000,
    80000,
    50000,
    40000,
    30000,
    20000,
    15000,
    10000,
    8000,
    5000,
    4000,
    3000,
    2000,
    1500,
    1000,
    800,
    500,
    400,
    300,
    200,
    150,
    100,
    80,
    50,
    40,
    30,
    20,
    15,
    10,
    8,
    6,
    4,
    3,
    2,
    1.5,
    1,
    0.5
  );
var
  I: Integer;
begin
  for I := 0 to Length(CNiceValues) - 1 do begin
    Result := CNiceValues[I];
    if ALen > Result then begin
      Break;
    end;
  end;
end;

procedure TWindowLayerScaleLineBase.OnConfigChange;
var
  VVisible: Boolean;
begin
  FTmpBitmap.Font.Name := FConfig.FontName;
  FTmpBitmap.Font.Size := FConfig.FontSize;

  ViewUpdateLock;
  try
    VVisible := GetNewVisibility;
    if VVisible <> Visible then begin
      Visible := VVisible;
    end;
    SetNeedUpdateBitmapSize;
    SetNeedUpdateBitmapDraw;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerScaleLineBase.OnPosChange;
begin
  FLayerChangeFlag.SetFlag;
end;

procedure TWindowLayerScaleLineBase.OnTimer;
var
  VVisible: Boolean;
begin
  if FLayerChangeFlag.CheckFlagAndReset then begin
    ViewUpdateLock;
    try
      VVisible := GetNewVisibility;
      if VVisible <> Visible then begin
        Visible := VVisible;
      end;
      SetNeedUpdateBitmapDraw;
      SetNeedUpdateLayerLocation;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TWindowLayerScaleLineBase.OnMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbRight then begin
    FPopupMenu.PopUp;
  end;
end;

procedure TWindowLayerScaleLineBase.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TWindowLayerScaleLineBase.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

const
  CBackGroundColor: TColor32 = 0;

function IsPixelForOutLine(
  const ABitmap: TBitmap32;
  const X, Y: Integer;
  const AOutLineColor: TColor32
): Boolean; inline;
var
  I, J: Integer;
  VPixel: TColor32;
begin
  Result := False;
  if ABitmap.Pixel[X, Y] <> CBackGroundColor then begin
    Exit;
  end;
  for I := -1 to 1 do begin
    for J := -1 to 1 do begin
      VPixel := ABitmap.Pixel[X + I, Y + J];
      if (VPixel <> CBackGroundColor) and (VPixel <> AOutLineColor) then begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TWindowLayerScaleLineBase.DrawOutLinedText(
  const ABitmap: TBitmap32;
  const X, Y: Integer;
  const AText: string;
  const ATextColor: TColor32;
  const AOutLineColor: TColor32
);
var
  I, J: Integer;
  VSize: TSize;
begin
  VSize := FTmpBitmap.TextExtent(AText);
  FTmpBitmap.SetSize(VSize.cx + 4, VSize.cy + 4);

  FTmpBitmap.Clear(CBackGroundColor);
  FTmpBitmap.RenderText(2, 2, AText, ATextColor, False);

  for I := 1 to FTmpBitmap.Width - 2 do begin
    for J := 1 to FTmpBitmap.Height - 2 do begin
      if IsPixelForOutLine(FTmpBitmap, I, J, AOutLineColor) then begin
        FTmpBitmap.Pixel[I, J] := AOutLineColor;
      end;
    end;
  end;

  BlockTransfer(
    ABitmap,
    X, Y,
    ABitmap.ClipRect,
    FTmpBitmap,
    FTmpBitmap.BoundsRect,
    dmOpaque
  );
end;

function TWindowLayerScaleLineBase.GetNewLayerLocation: TFloatRect;
var
  VSize: TPoint;
begin
  VSize := Types.Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
  Result.Left := 6;
  Result.Bottom := FView.GetStatic.GetLocalRect.Bottom - 6 - FConfig.BottomMargin;
  Result.Right := Result.Left + VSize.X;
  Result.Top := Result.Bottom - VSize.Y;
end;

class function TWindowLayerScaleLineBase.ValueToStr(
  const AValue: Double;
  const ADigits: Integer;
  const AUnits: string
): string;
begin
  Result := FloatToStrF(AValue, ffNumber, 15, ADigits) + ' ' + AUnits;
end;

end.
