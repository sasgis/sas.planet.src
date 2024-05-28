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

unit u_MapLayerPointsSet;

interface

uses
  GR32,
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_Projection,
  i_MainFormState,
  i_MarkerDrawable,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  i_GeometryProjected,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerPointsSet = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FGeometryProjectedFactory: IGeometryProjectedFactory;
    FPointMarker: IMarkerDrawable;
    FPointMarkerChangeable: IMarkerDrawableChangeable;
    FSource: IGeometryLonLatMultiPointChangeable;

    FNeedUpdatePoints: Boolean;
    FProjection: IProjection;
    FLonLatPointsPrepared: IGeometryLonLatMultiPoint;
    FProjectedPoints: IGeometryProjectedMultiPoint;

    FIsValid: Boolean;
    FRect: TRect;
    FLocalConverter: ILocalCoordConverter;

    procedure OnMarkerChange;
    procedure OnSourceChange;
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
      const AGeometryProjectedFactory: IGeometryProjectedFactory;
      const ASource: IGeometryLonLatMultiPointChangeable;
      const APointMarkerChangeable: IMarkerDrawableChangeable
    );
  end;

implementation

uses
  Types,
  Math,
  t_GeoTypes,
  i_EnumDoublePoint,
  u_GeoFunc,
  u_ListenerByEvent;

{ TMapLayerPointsSet }

constructor TMapLayerPointsSet.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const AGeometryProjectedFactory: IGeometryProjectedFactory;
  const ASource: IGeometryLonLatMultiPointChangeable;
  const APointMarkerChangeable: IMarkerDrawableChangeable
);
begin
  Assert(Assigned(AGeometryProjectedFactory));
  Assert(Assigned(ASource));
  Assert(Assigned(APointMarkerChangeable));

  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );

  FMainFormState := AMainFormState;
  FGeometryProjectedFactory := AGeometryProjectedFactory;
  FSource := ASource;
  FPointMarkerChangeable := APointMarkerChangeable;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarkerChange),
    FPointMarkerChangeable.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSourceChange),
    FSource.ChangeNotifier
  );
end;

procedure TMapLayerPointsSet.OnMarkerChange;
begin
  FPointMarker := FPointMarkerChangeable.GetStatic;
  if Visible then begin
    ViewUpdateLock;
    try
      SetNeedRedraw;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerPointsSet.OnSourceChange;
begin
  ViewUpdateLock;
  try
    if Assigned(FSource.GetStatic) then begin
      Show;
    end else begin
      Hide;
    end;
    FNeedUpdatePoints := True;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerPointsSet.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);
var
  VProjection: IProjection;
  VNeedUpdatePoints: Boolean;
  VViewRect: TDoubleRect;
  VPoints: IGeometryLonLatMultiPoint;
  VProjectedPoints: IGeometryProjectedMultiPoint;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  if not Visible then begin
    Exit;
  end;

  FLocalConverter := ALocalConverter;

  VNeedUpdatePoints := FNeedUpdatePoints;
  if VNeedUpdatePoints then begin
    VPoints := FSource.GetStatic;
    if Assigned(VPoints) then begin
      VNeedUpdatePoints := not VPoints.IsSame(FLonLatPointsPrepared);
    end else begin
      VNeedUpdatePoints := Assigned(FLonLatPointsPrepared);
    end;
  end else begin
    VPoints := FLonLatPointsPrepared;
  end;

  if VNeedUpdatePoints then begin
    FLonLatPointsPrepared := VPoints;
  end;

  VProjection := FProjection;
  if not FLocalConverter.Projection.IsSame(VProjection) then begin
    VNeedUpdatePoints := True;
    VProjection := FLocalConverter.Projection;
    FProjection := VProjection;
  end;

  if VNeedUpdatePoints then begin
    if Assigned(VPoints) then begin
      FProjectedPoints := FGeometryProjectedFactory.CreateMultiPointByLonLat(VProjection, VPoints);
    end else begin
      FProjectedPoints := nil;
    end;
  end;
  VProjectedPoints := FProjectedPoints;

  if Assigned(VProjectedPoints) then begin
    VViewRect := FLocalConverter.GetRectInMapPixelFloat;
    FIsValid := IsIntersecProjectedRect(VViewRect, VProjectedPoints.Bounds);
    if FIsValid then begin
      FRect := RectFromDoubleRect(VProjectedPoints.Bounds, rrOutside);

      // draw
      if FMainFormState.IsMapMoving then begin
        DoInvalidateFull;
      end else begin
        DoInvalidateRect(FRect);
      end;
    end;
  end;
end;

procedure TMapLayerPointsSet.PaintLayer(ABuffer: TBitmap32);
var
  VViewRect: TDoubleRect;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  VEnum: IEnumProjectedPoint;
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      VEnum := FProjectedPoints.GetEnum;
      VViewRect := FLocalConverter.GetRectInMapPixelFloat;
      while VEnum.Next(VPosOnMap) do begin
        if PixelPointInRect(VPosOnMap, VViewRect) then begin
          VPosOnBitmap := FLocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
          FPointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
        end;
      end;
    end;
  end;
end;

procedure TMapLayerPointsSet.StartThreads;
begin
  inherited;
  OnMarkerChange;
end;

end.
