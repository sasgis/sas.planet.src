{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_LayerScaleLine;

interface

uses
  Types,
  Controls,
  Classes,
  GR32,
  GR32_Image,
  i_NotifierOperation,
  i_NotifierTime,
  i_SimpleFlag,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_ScaleLineConfig,
  i_PopUp,
  u_WindowLayerWithPos;

type
  TLayerScaleLineBase = class(TWindowLayerWithBitmapBase)
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
    procedure DrawOutLinedText(
      X, Y: Integer;
      const Text: string;
      TextColor: TColor32;
      OutLineColor: TColor32;
      TargetBitmap: TBitmap32
    );
    function GetNiceLen(ALen: Double): Double;
    function GetNewVisibility: boolean; virtual; abstract;
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
      const ATimerNoifier: INotifierTime;
      const APopupMenu: IPopUp;
      const AConfig: IScaleLineConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Layers,
  GR32_Resamplers,
  i_CoordConverter,
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock,
  u_ListenerTime,
  u_ResStrings,
  u_GeoFun,
  t_GeoTypes;

{ TLayerScaleLine }

constructor TLayerScaleLineBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ATimerNoifier: INotifierTime;
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
    ATimerNoifier
  );

  Layer.AlphaHit := True;
  Layer.OnMouseDown := OnMouseDown;

  Layer.Bitmap.Font.Name := FConfig.FontName;
  Layer.Bitmap.Font.Size := FConfig.FontSize;

  FTmpBitmap := TBitmap32.Create;
  FTmpBitmap.Font := Layer.Bitmap.Font;
  FTmpBitmap.Font.Size := Layer.Bitmap.Font.Size;
end;

destructor TLayerScaleLineBase.Destroy;
begin
  FreeAndNil(FTmpBitmap);
  inherited;
end;

function TLayerScaleLineBase.GetNiceLen(ALen: Double): Double;
const
  CNiceValues: array [0..54] of Double =
    (
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
  i: Integer;
begin
  for i := 0 to Length(CNiceValues) - 1 do begin
    Result := CNiceValues[i];
    if ALen > Result then begin
      Break;
    end;
  end;
end;

procedure TLayerScaleLineBase.OnConfigChange;
begin
  FLayerChangeFlag.SetFlag;
end;

procedure TLayerScaleLineBase.OnPosChange;
begin
  FLayerChangeFlag.SetFlag;
end;

procedure TLayerScaleLineBase.OnTimer;
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

procedure TLayerScaleLineBase.OnMouseDown(
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

procedure TLayerScaleLineBase.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TLayerScaleLineBase.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

procedure TLayerScaleLineBase.DrawOutLinedText(
  X, Y: Integer;
  const Text: string;
  TextColor: TColor32;
  OutLineColor: TColor32;
  TargetBitmap: TBitmap32
);
var
  I, J: Integer;
begin
  FTmpBitmap.SetSize(FTmpBitmap.TextWidth(Text) + 4, FTmpBitmap.TextHeight(Text) + 4);
  FTmpBitmap.Clear(0);
  FTmpBitmap.RenderText(2, 2, Text, 0, TextColor);
  for I := 1 to FTmpBitmap.Width - 2 do begin
    for J := 1 to FTmpBitmap.Height - 2 do begin
      if (FTmpBitmap.Pixel[I, J] <> TextColor) and (FTmpBitmap.Pixel[I, J] <> OutLineColor) then begin
        if (FTmpBitmap.Pixel[I + 1, J] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J] = TextColor) or
          (FTmpBitmap.Pixel[I, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I, J - 1] = TextColor) or
          (FTmpBitmap.Pixel[I + 1, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I + 1, J - 1] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J - 1] = TextColor) then begin
          FTmpBitmap.Pixel[I, J] := OutLineColor;
        end;
      end;
    end;
  end;
  BlockTransfer(
    TargetBitmap,
    X, Y,
    TargetBitmap.ClipRect,
    FTmpBitmap.Bits,
    FTmpBitmap.Width,
    FTmpBitmap.Height,
    FTmpBitmap.BoundsRect,
    dmOpaque
  );
end;

function TLayerScaleLineBase.GetNewLayerLocation: TFloatRect;
var
  VSize: TPoint;
begin
  VSize := Types.Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
  Result.Left := 6;
  Result.Bottom := FView.GetStatic.GetLocalRect.Bottom - 6 - FConfig.BottomMargin;
  Result.Right := Result.Left + VSize.X;
  Result.Top := Result.Bottom - VSize.Y;
end;

end.
