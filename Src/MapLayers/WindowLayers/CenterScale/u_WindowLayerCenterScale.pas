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

unit u_WindowLayerCenterScale;

interface

uses
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes,
  i_NotifierOperation,
  i_MarkerDrawable,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_CenterScaleConfig,
  u_WindowLayerBasicBase;

type
  TWindowLayerCenterScale = class(TWindowLayerBasicBase)
  private
    FConfig: ICenterScaleConfig;
    FMarker: IMarkerDrawable;
    FMarkerChangeable: IMarkerDrawableChangeable;
    FPosition: ILocalCoordConverterChangeable;

    FLastFixedPoint: TDoublePoint;

    FRect: TRect;
    FIsValid: Boolean;

    procedure OnConfigChange;
    procedure OnMarkerChange;
    procedure OnPosChange;
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
      const AMarkerChangeable: IMarkerDrawableChangeable;
      const AConfig: ICenterScaleConfig
    );
  end;

implementation

uses
  Types,
  Math,
  u_ListenerByEvent,
  u_GeoFunc;

{ TWindowLayerCenterScale }

constructor TWindowLayerCenterScale.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AMarkerChangeable: IMarkerDrawableChangeable;
  const AConfig: ICenterScaleConfig
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
  FPosition := APosition;
  FMarkerChangeable := AMarkerChangeable;

  FMarker := FMarkerChangeable.GetStatic;
  FLastFixedPoint := CEmptyDoublePoint;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarkerChange),
    FMarkerChangeable.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );
end;

procedure TWindowLayerCenterScale.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.Visible;
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerCenterScale.OnMarkerChange;
begin
  FMarker := FMarkerChangeable.GetStatic;
  if Visible then begin
    ViewUpdateLock;
    try
      SetNeedFullRepaintLayer;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TWindowLayerCenterScale.OnPosChange;
var
  VNewFixedPoint: TDoublePoint;
begin
  if Visible then begin
    VNewFixedPoint := RectCenter(FPosition.GetStatic.GetLocalRect);
    if not DoublePointsEqual(VNewFixedPoint, FLastFixedPoint) then begin
      FLastFixedPoint := VNewFixedPoint;
      ViewUpdateLock;
      try
        SetNeedFullRepaintLayer;
      finally
        ViewUpdateUnlock;
      end;
    end;
  end;
end;

procedure TWindowLayerCenterScale.InvalidateLayer;
var
  VCenter: TDoublePoint;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  if Visible then begin
    FIsValid := True;
    VCenter := RectCenter(FPosition.GetStatic.GetLocalRect);
    FRect := FMarker.GetBoundsForPosition(VCenter);
    DoInvalidateRect(FRect); // draw
  end;
end;

procedure TWindowLayerCenterScale.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      FMarker.DrawToBitmap(ABuffer, RectCenter(FRect));
    end;
  end;
end;

procedure TWindowLayerCenterScale.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
