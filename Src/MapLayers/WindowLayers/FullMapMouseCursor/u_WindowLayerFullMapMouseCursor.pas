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

unit u_WindowLayerFullMapMouseCursor;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_FullMapMouseCursorLayerConfig,
  i_MainFormState,
  i_MouseState,
  u_WindowLayerBasicBase;

type
  TWindowLayerFullMapMouseCursor = class(TWindowLayerBasicBase)
  private
    FLocalConverter: ILocalCoordConverterChangeable;
    FConfig: IFullMapMouseCursorLayerConfig;
    FMainFormState: IMainFormState;
    FMouseState: IMouseState;

    FPos: TPoint;
    FIsValid: Boolean;

    procedure OnConfigChange;
    procedure OnTimer;
  protected
    procedure InvalidateLayer; override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AMainFormState: IMainFormState;
      const AGuiSyncronizedTimerNotifier: INotifierTime;
      const AMouseState: IMouseState;
      const AConfig: IFullMapMouseCursorLayerConfig
    );
  end;

implementation

uses
  GR32_Layers,
  i_LocalCoordConverter,
  u_ListenerTime,
  u_ListenerByEvent;

{ TWindowLayerFullMapMouseCursor }

constructor TWindowLayerFullMapMouseCursor.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const AGuiSyncronizedTimerNotifier: INotifierTime;
  const AMouseState: IMouseState;
  const AConfig: IFullMapMouseCursorLayerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    TCustomLayer.Create(AParentMap.Layers)
  );
  FConfig := AConfig;
  FMainFormState := AMainFormState;
  FMouseState := AMouseState;
  FLocalConverter := APosition;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FMainFormState.ChangeNotifier
  );
  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 25),
    AGuiSyncronizedTimerNotifier
  );
end;

procedure TWindowLayerFullMapMouseCursor.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.Enabled and ((FMainFormState.State <> ao_movemap) or (FConfig.ShowAlways));
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerFullMapMouseCursor.OnTimer;
var
  VPos: TPoint;
begin
  if Visible then begin
    VPos := FMouseState.CurentPos;
    if not FIsValid or (VPos.X <> FPos.X) or (VPos.Y <> FPos.Y) then begin
      ViewUpdateLock;
      try
        SetNeedFullRepaintLayer;
      finally
        ViewUpdateUnlock;
      end;
    end;
  end;
end;

procedure TWindowLayerFullMapMouseCursor.InvalidateLayer;

  procedure InvalidateCross(const AViewRect: TRect);
  begin
    // Vertical
    DoInvalidateRect(MakeRect(FPos.X, AViewRect.Top, FPos.X + 1, AViewRect.Bottom));
    // Horizontal
    DoInvalidateRect(MakeRect(AViewRect.Left, FPos.Y, AViewRect.Right, FPos.Y + 1));
  end;

var
  VViewRect: TRect;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FLocalConverter.GetStatic;
  VViewRect := VLocalConverter.GetLocalRect;

  if FIsValid then begin
    FIsValid := False;
    InvalidateCross(VViewRect); // erase
  end;

  if Visible then begin
    FIsValid := True;
    FPos := FMouseState.CurentPos;
    InvalidateCross(VViewRect); // draw
  end;
end;

procedure TWindowLayerFullMapMouseCursor.PaintLayer(ABuffer: TBitmap32);
var
  VColor: TColor32;
begin
  if not FIsValid then begin
    Exit;
  end;

  ABuffer.BeginUpdate;
  try
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(MakeRect(FPos.X, 0, FPos.X + 1, ABuffer.Height));
      ABuffer.Changed(MakeRect(0, FPos.Y, ABuffer.Width, FPos.Y + 1));
    end else begin
      VColor := FConfig.LineColor;
      ABuffer.VertLineS(FPos.X, 0, ABuffer.Height, VColor);
      ABuffer.HorzLineS(0, FPos.Y, ABuffer.Width, VColor);
    end;
  finally
    ABuffer.EndUpdate;
  end;
end;

procedure TWindowLayerFullMapMouseCursor.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
