{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_FullMapMouseCursorLayer;

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
  u_WindowLayerWithPos;

type
  TFullMapMouseCursorLayer = class(TWindowLayerBasicBase)
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
      AParentMap: TImage32;
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

{ TFullMapMouseCursorLayer }

constructor TFullMapMouseCursorLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
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
    TListenerTimeCheck.Create(Self.OnTimerEvent, 10),
    ATimerNoifier
  );
end;

procedure TFullMapMouseCursorLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.Enabled and ((FMainFormState.State <> ao_movemap) or (FConfig.ShowAlways));
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TFullMapMouseCursorLayer.OnTimerEvent;
var
  VPos: TPoint;
  VRect: TRect;
  VViewRect: TRect;
  VLocalConverter: ILocalCoordConverter;
begin
  if Visible then begin
    VPos := FMouseState.CurentPos;
    VLocalConverter := FLocalConverter.GetStatic;
    VViewRect := VLocalConverter.GetLocalRect;
    if (VPos.X <> FLastPos.X) or (VPos.Y <> FLastPos.Y) then begin
      VRect.Left := VViewRect.Left;
      VRect.Top := FLastPos.Y - 1;
      VRect.Right := VViewRect.Right;
      VRect.Bottom := FLastPos.Y + 1;
      Layer.Changed(VRect);

      VRect.Left := FLastPos.X - 1;
      VRect.Top := VViewRect.Top;
      VRect.Right := FLastPos.X + 1;
      VRect.Bottom := VViewRect.Bottom;
      Layer.Changed(VRect);

      VRect.Left := VViewRect.Left;
      VRect.Top := VPos.Y - 1;
      VRect.Right := VViewRect.Right;
      VRect.Bottom := VPos.Y + 1;
      Layer.Changed(VRect);

      VRect.Left := VPos.X - 1;
      VRect.Top := VViewRect.Top;
      VRect.Right := VPos.X + 1;
      VRect.Bottom := VViewRect.Bottom;
      FLastPos := VPos;
      Layer.Changed(VRect);
    end;
  end;
end;

procedure TFullMapMouseCursorLayer.PaintLayer(ABuffer: TBitmap32);
var
  VPos: TPoint;
  VColor: TColor32;
begin
  inherited;
  VPos := FLastPos;
  VColor := FConfig.LineColor;
  ABuffer.VertLineS(VPos.X, 0, ABuffer.Height, VColor);
  ABuffer.HorzLineS(0, VPos.Y, ABuffer.Width, VColor);
end;

procedure TFullMapMouseCursorLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.


