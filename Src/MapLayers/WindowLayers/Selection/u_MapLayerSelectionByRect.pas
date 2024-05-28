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

unit u_MapLayerSelectionByRect;

interface

uses
  GR32,
  GR32_Image,
  i_MainFormState,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_SelectionRect,
  i_SelectionRectLayerConfig,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerSelectionByRect = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FConfig: ISelectionRectLayerConfig;
    FSelection: ISelectionRect;

    FFillColor: TColor32;
    FBorderColor: TColor32;

    FForceMapRedraw: Boolean;

    FIsValid: Boolean;
    FRect: TRect;
    FDrawRect: TRect;

    procedure OnSelectionChange;
    procedure OnConfigChange;
  protected
    procedure InvalidateLayer(const ALocalConverter: ILocalCoordConverter); override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AMainFormState: IMainFormState;
      const ASelection: ISelectionRect;
      const AConfig: ISelectionRectLayerConfig;
      const AForceMapRedraw: Boolean
    );
  end;


implementation

uses
  Types,
  Math,
  t_GeoTypes,
  i_Projection,
  u_ListenerByEvent,
  u_GeoFunc;

{ TSelectionRectLayer }

constructor TMapLayerSelectionByRect.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const ASelection: ISelectionRect;
  const AConfig: ISelectionRectLayerConfig;
  const AForceMapRedraw: Boolean
);
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(ASelection));

  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );

  FMainFormState := AMainFormState;
  FConfig := AConfig;
  FSelection := ASelection;
  FForceMapRedraw := AForceMapRedraw;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSelectionChange),
    FSelection.GetChangeNotifier
  );
end;

procedure TMapLayerSelectionByRect.OnConfigChange;
begin
  FConfig.LockRead;
  try
    FFillColor := FConfig.FillColor;
    FBorderColor := FConfig.BorderColor;
  finally
    FConfig.UnlockRead;
  end;
  if Visible then begin
    ViewUpdateLock;
    try
      SetNeedRedraw;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerSelectionByRect.OnSelectionChange;
begin
  ViewUpdateLock;
  try
    if FSelection.IsEmpty then begin
      Hide;
    end else begin
      Show;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerSelectionByRect.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);
var
  VSelectedLonLat: TDoubleRect;
  VSelectedPixels: TDoubleRect;
  VProjection: IProjection;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  if not Visible then begin
    Exit;
  end;

  FIsValid := True;

  VProjection := ALocalConverter.Projection;
  VSelectedLonLat := FSelection.GetRect;
  VProjection.ProjectionType.ValidateLonLatRect(VSelectedLonLat);
  VSelectedPixels := VProjection.LonLatRect2PixelRectFloat(VSelectedLonLat);

  FDrawRect :=
    RectFromDoubleRect(
      ALocalConverter.MapRectFloat2LocalRectFloat(VSelectedPixels),
      rrToTopLeft
    );

  FRect := FDrawRect;
  GR32.InflateRect(FRect, 1, 1);

  // draw
  if FForceMapRedraw or FMainFormState.IsMapMoving then begin
    DoInvalidateFull;
  end else begin
    DoInvalidateRect(FRect);
  end;
end;

procedure TMapLayerSelectionByRect.PaintLayer(ABuffer: TBitmap32);
begin
  if not FIsValid then begin
    Exit;
  end;

  if ABuffer.MeasuringMode then begin
    ABuffer.Changed(FRect);
    Exit;
  end;

  ABuffer.BeginUpdate;
  try
    ABuffer.FillRectTS(
      FDrawRect.Left,
      FDrawRect.Top,
      FDrawRect.Right,
      FDrawRect.Bottom,
      FFillColor
    );
    ABuffer.FrameRectTS(
      FDrawRect.Left,
      FDrawRect.Top,
      FDrawRect.Right,
      FDrawRect.Bottom,
      FBorderColor
    );
    ABuffer.FrameRectTS(
      FDrawRect.Left - 1,
      FDrawRect.Top - 1,
      FDrawRect.Right + 1,
      FDrawRect.Bottom + 1,
      FBorderColor
    );
  finally
    ABuffer.EndUpdate;
  end;
end;

procedure TMapLayerSelectionByRect.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
