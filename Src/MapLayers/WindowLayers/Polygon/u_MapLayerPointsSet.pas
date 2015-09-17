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
  i_Projection,
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
    FProjection: IProjection;
    FProjectedPoints: IDoublePointsAggregator;
    FActivePointIndex: Integer;
    procedure OnConfigChange;
  protected
    procedure ChangedSource;
    procedure PreparePoints(
      const AProjection: IProjection;
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
      const AProjection: IProjection;
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
      const AProjection: IProjection;
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
  VProjection: IProjection;
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
      if not ALocalConverter.Projection.IsSame(VProjection) then begin
        VNeedUpdatePoints := True;
      end;
    end;
  end;
  if VNeedUpdatePoints then begin
    VProjection := ALocalConverter.Projection;
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
    if Assigned(FLine) then begin
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

procedure PrepareSingleLine(
  const AProjection: IProjection;
  const AGeometry: IGeometryLonLatSingleLine;
  const ASourceActivePointIndex: Integer;
  const AProjectedPoints: IDoublePointsAggregator;
  var AIndex: Integer;
  var AActivePointIndex: Integer
);
var
  i: Integer;
  VLonLatPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VProjectedPoint: TDoublePoint;
begin
  if AProjectedPoints.Count > 0 then begin
    VPrevPoint := AProjectedPoints.Points[AProjectedPoints.Count - 1];
  end;
  for i := 0 to AGeometry.Count - 1 do begin
    VLonLatPoint := AGeometry.Points[i];
    AProjection.ProjectionType.ValidateLonLatPos(VLonLatPoint);
    VProjectedPoint := AProjection.LonLat2PixelPosFloat(VLonLatPoint);
    if (ASourceActivePointIndex = AIndex) then begin
      AActivePointIndex := AProjectedPoints.Count;
      AProjectedPoints.Add(VProjectedPoint);
      VPrevPoint := VProjectedPoint;
    end else begin
      if AProjectedPoints.Count = 0 then begin
        AProjectedPoints.Add(VProjectedPoint);
        VPrevPoint := VProjectedPoint;
      end else begin
        if (abs(VProjectedPoint.X - VPrevPoint.X) > 1) or (abs(VProjectedPoint.Y - VPrevPoint.Y) > 1) then begin
          AProjectedPoints.Add(VProjectedPoint);
          VPrevPoint := VProjectedPoint;
        end;
      end;
    end;
    Inc(AIndex);
  end;
  Inc(AIndex);
end;

procedure PrepareMultiLine(
  const AProjection: IProjection;
  const AGeometry: IGeometryLonLatMultiLine;
  const ASourceActivePointIndex: Integer;
  const AProjectedPoints: IDoublePointsAggregator;
  var AIndex: Integer;
  var AActivePointIndex: Integer
);
var
  i: Integer;
begin
  for i := 0 to AGeometry.Count - 1 do begin
    PrepareSingleLine(
      AProjection,
      AGeometry.Item[i],
      ASourceActivePointIndex,
      AProjectedPoints,
      AIndex,
      AActivePointIndex
    );
  end;
end;

procedure PrepareLine(
  const AProjection: IProjection;
  const AGeometry: IGeometryLonLatLine;
  const ASourceActivePointIndex: Integer;
  const AProjectedPoints: IDoublePointsAggregator;
  var AIndex: Integer;
  var AActivePointIndex: Integer
);
var
  VSigleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  if Supports(AGeometry, IGeometryLonLatSingleLine, VSigleLine) then begin
    PrepareSingleLine(
      AProjection,
      VSigleLine,
      ASourceActivePointIndex,
      AProjectedPoints,
      AIndex,
      AActivePointIndex
    );
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    PrepareMultiLine(
      AProjection,
      VMultiLine,
      ASourceActivePointIndex,
      AProjectedPoints,
      AIndex,
      AActivePointIndex
    );
  end;
end;

procedure PrepareContour(
  const AProjection: IProjection;
  const AGeometry: IGeometryLonLatContour;
  const ASourceActivePointIndex: Integer;
  const AProjectedPoints: IDoublePointsAggregator;
  var AIndex: Integer;
  var AActivePointIndex: Integer
);
var
  i: Integer;
  VLonLatPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VProjectedPoint: TDoublePoint;
begin
  if AProjectedPoints.Count > 0 then begin
    VPrevPoint := AProjectedPoints.Points[AProjectedPoints.Count - 1];
  end;
  for i := 0 to AGeometry.Count - 1 do begin
    VLonLatPoint := AGeometry.Points[i];
    AProjection.ProjectionType.ValidateLonLatPos(VLonLatPoint);
    VProjectedPoint := AProjection.LonLat2PixelPosFloat(VLonLatPoint);
    if (ASourceActivePointIndex = AIndex) then begin
      AActivePointIndex := AProjectedPoints.Count;
      AProjectedPoints.Add(VProjectedPoint);
      VPrevPoint := VProjectedPoint;
    end else begin
      if AProjectedPoints.Count = 0 then begin
        AProjectedPoints.Add(VProjectedPoint);
        VPrevPoint := VProjectedPoint;
      end else begin
        if (abs(VProjectedPoint.X - VPrevPoint.X) > 1) or (abs(VProjectedPoint.Y - VPrevPoint.Y) > 1) then begin
          AProjectedPoints.Add(VProjectedPoint);
          VPrevPoint := VProjectedPoint;
        end;
      end;
    end;
    Inc(AIndex);
  end;
  Inc(AIndex);
end;

procedure PrepareSinglePolygon(
  const AProjection: IProjection;
  const AGeometry: IGeometryLonLatSinglePolygon;
  const ASourceActivePointIndex: Integer;
  const AProjectedPoints: IDoublePointsAggregator;
  var AIndex: Integer;
  var AActivePointIndex: Integer
);
var
  i: Integer;
begin
  PrepareContour(
    AProjection,
    AGeometry.OuterBorder,
    ASourceActivePointIndex,
    AProjectedPoints,
    AIndex,
    AActivePointIndex
  );
  for i := 0 to AGeometry.HoleCount - 1 do begin
    PrepareContour(
      AProjection,
      AGeometry.HoleBorder[i],
      ASourceActivePointIndex,
      AProjectedPoints,
      AIndex,
      AActivePointIndex
    );
  end;
end;

procedure PrepareMultiPolygon(
  const AProjection: IProjection;
  const AGeometry: IGeometryLonLatMultiPolygon;
  const ASourceActivePointIndex: Integer;
  const AProjectedPoints: IDoublePointsAggregator;
  var AIndex: Integer;
  var AActivePointIndex: Integer
);
var
  i: Integer;
begin
  for i := 0 to AGeometry.Count - 1 do begin
    PrepareSinglePolygon(
      AProjection,
      AGeometry.Item[i],
      ASourceActivePointIndex,
      AProjectedPoints,
      AIndex,
      AActivePointIndex
    );
  end;
end;

procedure PreparePolygon(
  const AProjection: IProjection;
  const AGeometry: IGeometryLonLatPolygon;
  const ASourceActivePointIndex: Integer;
  const AProjectedPoints: IDoublePointsAggregator;
  var AIndex: Integer;
  var AActivePointIndex: Integer
);
var
  VSiglePolygon: IGeometryLonLatSinglePolygon;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
begin
  if Supports(AGeometry, IGeometryLonLatSinglePolygon, VSiglePolygon) then begin
    PrepareSinglePolygon(
      AProjection,
      VSiglePolygon,
      ASourceActivePointIndex,
      AProjectedPoints,
      AIndex,
      AActivePointIndex
    );
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    PrepareMultiPolygon(
      AProjection,
      VMultiPolygon,
      ASourceActivePointIndex,
      AProjectedPoints,
      AIndex,
      AActivePointIndex
    );
  end;
end;

procedure TMapLayerPointsSetByPathEdit.PreparePoints(
  const AProjection: IProjection;
  out AProjectedPoints: IDoublePointsAggregator;
  out AActivePointIndex: Integer
);
var
  VLine: ILonLatPathWithSelected;
  VIndex: Integer;
begin
  AProjectedPoints := nil;
  AActivePointIndex := -1;
  VLine := FLine;
  if Assigned(VLine) then begin
    AProjectedPoints := TDoublePointsAggregator.Create;
    VIndex := 0;
    PrepareLine(
      AProjection,
      VLine.Geometry,
      VLine.GetSelectedPointIndex,
      AProjectedPoints,
      VIndex,
      AActivePointIndex
    );
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
    if Assigned(FLine) then begin
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
  const AProjection: IProjection;
  out AProjectedPoints: IDoublePointsAggregator;
  out AActivePointIndex: Integer
);
var
  VLine: ILonLatPolygonWithSelected;
  VIndex: Integer;
begin
  AProjectedPoints := nil;
  AActivePointIndex := -1;
  VLine := FLine;
  if Assigned(VLine) then begin
    AProjectedPoints := TDoublePointsAggregator.Create;
    VIndex := 0;
    PreparePolygon(
      AProjection,
      VLine.Geometry,
      VLine.GetSelectedPointIndex,
      AProjectedPoints,
      VIndex,
      AActivePointIndex
    );
  end;
end;

end.
