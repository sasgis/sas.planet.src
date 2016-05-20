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

unit u_MapLayerPointsSet;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_Projection,
  i_MarkerDrawable,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  i_GeometryProjected,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerPointsSet = class(TMapLayerBasicNoBitmap)
  private
    FGeometryProjectedFactory: IGeometryProjectedFactory;
    FPointMarker: IMarkerDrawableChangeable;
    FSource: IGeometryLonLatMultiPointChangeable;

    FNeedUpdatePoints: Boolean;
    FProjection: IProjection;
    FLonLatPointsPrepared: IGeometryLonLatMultiPoint;
    FProjectedPoints: IGeometryProjectedMultiPoint;
    procedure OnConfigChange;
  protected
    procedure ChangedSource;
    procedure OnSourceChange;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AGeometryProjectedFactory: IGeometryProjectedFactory;
      const ASource: IGeometryLonLatMultiPointChangeable;
      const APointMarker: IMarkerDrawableChangeable
    );
  end;

implementation

uses
  SysUtils,
  i_Listener,
  i_EnumDoublePoint,
  u_GeoFunc,
  u_ListenerByEvent;

{ TMapLayerPointsSet }

constructor TMapLayerPointsSet.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AGeometryProjectedFactory: IGeometryProjectedFactory;
  const ASource: IGeometryLonLatMultiPointChangeable;
  const APointMarker: IMarkerDrawableChangeable
);
var
  VListener: IListener;
begin
  Assert(Assigned(AGeometryProjectedFactory));
  Assert(Assigned(ASource));
  Assert(Assigned(APointMarker));
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FGeometryProjectedFactory := AGeometryProjectedFactory;
  FSource := ASource;
  FPointMarker := APointMarker;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FPointMarker.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSourceChange),
    FSource.ChangeNotifier
  );
end;

procedure TMapLayerPointsSet.ChangedSource;
begin
  FNeedUpdatePoints := True;
end;

procedure TMapLayerPointsSet.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerPointsSet.OnSourceChange;
begin
  ViewUpdateLock;
  try
    if Assigned(FSource.GetStatic) then begin
      SetNeedRedraw;
      Show;
    end else begin
      Hide;
    end;
    ChangedSource;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerPointsSet.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VProjection: IProjection;
  VNeedUpdatePoints: Boolean;
  VViewRect: TDoubleRect;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  VPointMarker: IMarkerDrawable;
  VPoints: IGeometryLonLatMultiPoint;
  VProjectedPoints: IGeometryProjectedMultiPoint;
  VEnum: IEnumProjectedPoint;
begin
  inherited;
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
  if not ALocalConverter.Projection.IsSame(VProjection) then begin
    VNeedUpdatePoints := True;
    VProjection := ALocalConverter.Projection;
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
    VViewRect := ALocalConverter.GetRectInMapPixelFloat;
    if IsIntersecProjectedRect(VViewRect, VProjectedPoints.Bounds) then begin
      VPointMarker := FPointMarker.GetStatic;
      VEnum := VProjectedPoints.GetEnum;
      while VEnum.Next(VPosOnMap) do begin
        if PixelPointInRect(VPosOnMap, VViewRect) then begin
          VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
          VPointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
        end;
      end;
    end;
  end;
end;

procedure TMapLayerPointsSet.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
