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

unit u_WindowLayerFullMapMouseCursor;

interface

uses
  GR32,
  GR32_Image,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
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

    FLastPos: TPoint;
    procedure OnConfigChange;
    procedure OnTimerEvent;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AMainFormState: IMainFormState;
      const ATimerNoifier: INotifierTime;
      const AMouseState: IMouseState;
      const AConfig: IFullMapMouseCursorLayerConfig
    );
  end;

implementation

uses
  GR32_Layers,
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
  const ATimerNoifier: INotifierTime;
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
    TListenerTimeCheck.Create(Self.OnTimerEvent, 25),
    ATimerNoifier
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

procedure TWindowLayerFullMapMouseCursor.OnTimerEvent;
var
  VPos: TPoint;
begin
  if Visible then begin
    VPos := FMouseState.CurentPos;
    if (VPos.X <> FLastPos.X) or (VPos.Y <> FLastPos.Y) then begin
      FLastPos := VPos;
      Layer.Changed;
    end;
  end;
end;

procedure TWindowLayerFullMapMouseCursor.PaintLayer(ABuffer: TBitmap32);
var
  VPos: TPoint;
  VColor: TColor32;
begin
  inherited;
  VPos := FLastPos;
  VColor := FConfig.LineColor;

  ABuffer.BeginUpdate;
  try
    ABuffer.VertLineS(VPos.X, 0, ABuffer.Height, VColor);
    ABuffer.HorzLineS(0, VPos.Y, ABuffer.Width, VColor);
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
