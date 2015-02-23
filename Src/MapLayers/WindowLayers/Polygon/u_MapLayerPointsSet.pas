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
  i_LineOnMapEdit,
  i_ProjectionInfo,
  i_MarkerDrawable,
  i_DoublePointsAggregator,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerPointsSetBase = class(TMapLayerBasicNoBitmap)
  private
    FFirstPointMarker: IMarkerDrawableChangeable;
    FActivePointMarker: IMarkerDrawableChangeable;
    FNormalPointMarker: IMarkerDrawableChangeable;

    FNeedUpdatePoints: Boolean;
    FProjection: IProjectionInfo;
    FProjectedPoints: IDoublePointsAggregator;
    FActivePointIndex: Integer;
    procedure OnConfigChange;
  protected
    procedure ChangedSource;
    procedure PreparePoints(
      const AProjection: IProjectionInfo;
      out AProjectedPoints: IDoublePointsAggregator;
      out AActivePointIndex: Integer
    ); virtual; abstract;
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AFirstPointMarker: IMarkerDrawableChangeable;
      const AActivePointMarker: IMarkerDrawableChangeable;
      const ANormalPointMarker: IMarkerDrawableChangeable
    );
  end;

  TMapLayerPointsSetByPathEdit = class(TMapLayerPointsSetBase)
  private
    FLineOnMapEdit: IPathOnMapEdit;
    FLine: ILonLatPathWithSelected;
    procedure OnLineChange;
  protected
    procedure PreparePoints(
      const AProjection: IProjectionInfo;
      out AProjectedPoints: IDoublePointsAggregator;
      out AActivePointIndex: Integer
    ); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ALineOnMapEdit: IPathOnMapEdit;
      const AFirstPointMarker: IMarkerDrawableChangeable;
      const AActivePointMarker: IMarkerDrawableChangeable;
      const ANormalPointMarker: IMarkerDrawableChangeable
    );
  end;

  TMapLayerPointsSetByPolygonEdit = class(TMapLayerPointsSetBase)
  private
    FLineOnMapEdit: IPolygonOnMapEdit;
    FLine: ILonLatPolygonWithSelected;
    procedure OnLineChange;
  protected
    procedure PreparePoints(
      const AProjection: IProjectionInfo;
      out AProjectedPoints: IDoublePointsAggregator;
      out AActivePointIndex: Integer
    ); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ALineOnMapEdit: IPolygonOnMapEdit;
      const AFirstPointMarker: IMarkerDrawableChangeable;
      const AActivePointMarker: IMarkerDrawableChangeable;
      const ANormalPointMarker: IMarkerDrawableChangeable
    );
  end;

implementation

uses
  SysUtils,
  i_Listener,
  i_CoordConverter,
  u_DoublePointsAggregator,
  u_ListenerByEvent;

{ TMapLayerPointsSetBase }

constructor TMapLayerPointsSetBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AFirstPointMarker: IMarkerDrawableChangeable;
  const AActivePointMarker: IMarkerDrawableChangeable;
  const ANormalPointMarker: IMarkerDrawableChangeable
);
var
  VListener: IListener;
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FFirstPointMarker := AFirstPointMarker;
  FActivePointMarker := AActivePointMarker;
  FNormalPointMarker := ANormalPointMarker;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FFirstPointMarker.GetChangeNotifier
  );
  LinksList.Add(
    VListener,
    FActivePointMarker.GetChangeNotifier
  );
  LinksList.Add(
    VListener,
    FNormalPointMarker.GetChangeNotifier
  );
end;

procedure TMapLayerPointsSetBase.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerPointsSetBase.ChangedSource;
begin
  FNeedUpdatePoints := True;
end;

procedure TMapLayerPointsSetBase.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VProjection: IProjectionInfo;
  VPoints: IDoublePointsAggregator;
  VActiveIndex: Integer;
  VNeedUpdatePoints: Boolean;
  VLocalRect: TRect;
  VBitmapSize: TPoint;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  i: Integer;
  VFirstPointMarker: IMarkerDrawable;
  VActivePointMarker: IMarkerDrawable;
  VNormalPointMarker: IMarkerDrawable;
begin
  inherited;
  VFirstPointMarker := FFirstPointMarker.GetStatic;
  VActivePointMarker := FActivePointMarker.GetStatic;
  VNormalPointMarker := FNormalPointMarker.GetStatic;

  VProjection := FProjection;
  VPoints := FProjectedPoints;
  VActiveIndex := FActivePointIndex;
  VNeedUpdatePoints := FNeedUpdatePoints;
  if not VNeedUpdatePoints then begin
    if (VProjection = nil) or (VPoints = nil) then begin
      VNeedUpdatePoints := True;
    end else begin
      if not ALocalConverter.ProjectionInfo.GetIsSameProjectionInfo(VProjection) then begin
        VNeedUpdatePoints := True;
      end;
    end;
  end;
  if VNeedUpdatePoints then begin
    VProjection := ALocalConverter.ProjectionInfo;
    PreparePoints(VProjection, VPoints, VActiveIndex);
    FProjectedPoints := VPoints;
    FProjection := VProjection;
    FActivePointIndex := VActiveIndex;
    FNeedUpdatePoints := False;
  end;

  if VPoints = nil then begin
    Exit;
  end;

  if VPoints.Count > 0 then begin
    VLocalRect := ALocalConverter.GetLocalRect;
    VBitmapSize.X := VLocalRect.Right - VLocalRect.Left;
    VBitmapSize.Y := VLocalRect.Bottom - VLocalRect.Top;
    VPosOnMap := VPoints.Points[0];
    VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
    if VActiveIndex = 0 then begin
      VActivePointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
    end else begin
      VFirstPointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
    end;
    for i := 1 to VPoints.Count - 1 do begin
      VPosOnMap := VPoints.Points[i];
      VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
      if VActiveIndex = i then begin
        VActivePointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
      end else begin
        VNormalPointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
      end;
    end;
  end;
end;

procedure TMapLayerPointsSetBase.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

{ TMapLayerPointsSetByPathEdit }

constructor TMapLayerPointsSetByPathEdit.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AFirstPointMarker: IMarkerDrawableChangeable;
  const AActivePointMarker: IMarkerDrawableChangeable;
  const ANormalPointMarker: IMarkerDrawableChangeable
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    AVectorGeometryProjectedFactory,
    AFirstPointMarker,
    AActivePointMarker,
    ANormalPointMarker
  );
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
end;

procedure TMapLayerPointsSetByPathEdit.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Path;
    if not FLine.Geometry.IsEmpty then begin
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

procedure TMapLayerPointsSetByPathEdit.PreparePoints(
  const AProjection: IProjectionInfo;
  out AProjectedPoints: IDoublePointsAggregator;
  out AActivePointIndex: Integer
);
var
  VLine: ILonLatPathWithSelected;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VLonLatPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VProjectedPoint: TDoublePoint;
  i, j: Integer;
  VSigleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
  VIndex: Integer;
  VSegmentIndex: Integer;
  VPointIndex: Integer;
begin
  AProjectedPoints := nil;
  AActivePointIndex := -1;
  VLine := FLine;
  if VLine <> nil then begin
    if not VLine.Geometry.IsEmpty then begin
      AProjectedPoints := TDoublePointsAggregator.Create;
      VConverter := AProjection.GeoConverter;
      VZoom := AProjection.Zoom;
      VSegmentIndex := VLine.GetSelectedSegmentIndex;
      VPointIndex := VLine.GetSelectedPointIndex;
      VIndex := 0;
      if Supports(VLine.Geometry, IGeometryLonLatSingleLine, VSigleLine) then begin
        i := 0;
        for j := 0 to VSigleLine.Count - 1 do begin
          VLonLatPoint := VSigleLine.Points[j];
          VConverter.ValidateLonLatPos(VLonLatPoint);
          VProjectedPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
          if (VSegmentIndex = i) and (VPointIndex = j) then begin
            AProjectedPoints.Add(VProjectedPoint);
            VPrevPoint := VProjectedPoint;
            AActivePointIndex := VIndex;
            Inc(VIndex);
          end else begin
            if VIndex = 0 then begin
              AProjectedPoints.Add(VProjectedPoint);
              VPrevPoint := VProjectedPoint;
              Inc(VIndex);
            end else begin
              if (abs(VProjectedPoint.X - VPrevPoint.X) > 1) or (abs(VProjectedPoint.Y - VPrevPoint.Y) > 1) then begin
                AProjectedPoints.Add(VProjectedPoint);
                VPrevPoint := VProjectedPoint;
                Inc(VIndex);
              end;
            end;
          end;
        end;
      end else if Supports(VLine.Geometry, IGeometryLonLatMultiLine, VMultiLine) then begin
        for i := 0 to VMultiLine.Count - 1 do begin
          VSigleLine := VMultiLine.Item[i];
          for j := 0 to VSigleLine.Count - 1 do begin
            VLonLatPoint := VSigleLine.Points[j];
            VConverter.ValidateLonLatPos(VLonLatPoint);
            VProjectedPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
            if (VSegmentIndex = i) and (VPointIndex = j) then begin
              AProjectedPoints.Add(VProjectedPoint);
              VPrevPoint := VProjectedPoint;
              AActivePointIndex := VIndex;
              Inc(VIndex);
            end else begin
              if VIndex = 0 then begin
                AProjectedPoints.Add(VProjectedPoint);
                VPrevPoint := VProjectedPoint;
                Inc(VIndex);
              end else begin
                if (abs(VProjectedPoint.X - VPrevPoint.X) > 1) or (abs(VProjectedPoint.Y - VPrevPoint.Y) > 1) then begin
                  AProjectedPoints.Add(VProjectedPoint);
                  VPrevPoint := VProjectedPoint;
                  Inc(VIndex);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TMapLayerPointsSetByPolygonEdit }

constructor TMapLayerPointsSetByPolygonEdit.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ALineOnMapEdit: IPolygonOnMapEdit;
  const AFirstPointMarker: IMarkerDrawableChangeable;
  const AActivePointMarker: IMarkerDrawableChangeable;
  const ANormalPointMarker: IMarkerDrawableChangeable
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    AVectorGeometryProjectedFactory,
    AFirstPointMarker,
    AActivePointMarker,
    ANormalPointMarker
  );
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
end;

procedure TMapLayerPointsSetByPolygonEdit.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Polygon;
    if not FLine.Geometry.IsEmpty then begin
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

procedure TMapLayerPointsSetByPolygonEdit.PreparePoints(
  const AProjection: IProjectionInfo;
  out AProjectedPoints: IDoublePointsAggregator;
  out AActivePointIndex: Integer
);
var
  VLine: ILonLatPolygonWithSelected;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VLonLatPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VProjectedPoint: TDoublePoint;
  i, j: Integer;
  VMultiLine: IGeometryLonLatMultiPolygon;
  VSigleLine: IGeometryLonLatSinglePolygon;
  VIndex: Integer;
  VSegmentIndex: Integer;
  VPointIndex: Integer;
begin
  AProjectedPoints := nil;
  AActivePointIndex := -1;
  VLine := FLine;
  if VLine <> nil then begin
    if not VLine.Geometry.IsEmpty then begin
      AProjectedPoints := TDoublePointsAggregator.Create;
      VConverter := AProjection.GeoConverter;
      VZoom := AProjection.Zoom;
      VSegmentIndex := VLine.GetSelectedSegmentIndex;
      VPointIndex := VLine.GetSelectedPointIndex;
      VIndex := 0;
      if Supports(VLine.Geometry, IGeometryLonLatSinglePolygon, VSigleLine) then begin
        i := 0;
        for j := 0 to VSigleLine.Count - 1 do begin
          VLonLatPoint := VSigleLine.Points[j];
          VConverter.ValidateLonLatPos(VLonLatPoint);
          VProjectedPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
          if (VSegmentIndex = i) and (VPointIndex = j) then begin
            AProjectedPoints.Add(VProjectedPoint);
            VPrevPoint := VProjectedPoint;
            AActivePointIndex := VIndex;
            Inc(VIndex);
          end else begin
            if VIndex = 0 then begin
              AProjectedPoints.Add(VProjectedPoint);
              VPrevPoint := VProjectedPoint;
              Inc(VIndex);
            end else begin
              if (abs(VProjectedPoint.X - VPrevPoint.X) > 1) or (abs(VProjectedPoint.Y - VPrevPoint.Y) > 1) then begin
                AProjectedPoints.Add(VProjectedPoint);
                VPrevPoint := VProjectedPoint;
                Inc(VIndex);
              end;
            end;
          end;
        end;
      end else if Supports(VLine.Geometry, IGeometryLonLatMultiPolygon, VMultiLine) then begin
        for i := 0 to VMultiLine.Count - 1 do begin
          VSigleLine := VMultiLine.Item[i];
          for j := 0 to VSigleLine.Count - 1 do begin
            VLonLatPoint := VSigleLine.Points[j];
            VConverter.ValidateLonLatPos(VLonLatPoint);
            VProjectedPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
            if (VSegmentIndex = i) and (VPointIndex = j) then begin
              AProjectedPoints.Add(VProjectedPoint);
              VPrevPoint := VProjectedPoint;
              AActivePointIndex := VIndex;
              Inc(VIndex);
            end else begin
              if VIndex = 0 then begin
                AProjectedPoints.Add(VProjectedPoint);
                VPrevPoint := VProjectedPoint;
                Inc(VIndex);
              end else begin
                if (abs(VProjectedPoint.X - VPrevPoint.X) > 1) or (abs(VProjectedPoint.Y - VPrevPoint.Y) > 1) then begin
                  AProjectedPoints.Add(VProjectedPoint);
                  VPrevPoint := VProjectedPoint;
                  Inc(VIndex);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
