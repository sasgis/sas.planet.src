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

unit u_MapLayerGPSMarkerRings;

interface

uses
  SysUtils,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_Datum,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_SimpleFlag,
  i_ProjectedDrawableElement,
  i_MarkerRingsConfig,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_GeometryProjectedFactory,
  i_GeometryLonLatFactory,
  i_GPSRecorder,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerGPSMarkerRings = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IMarkerRingsConfig;
    FGPSRecorder: IGPSRecorder;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FPolygonBuilder: IGeometryLonLatPolygonBuilder; 

    FGpsPosChangeFlag: ISimpleFlag;

    FGPSPosCS: IReadWriteSync;
    FGPSPosLonLat: TDoublePoint;
    FCirclesLonLat: IGeometryLonLatPolygon;
    FCirclesProjected: IProjectedDrawableElement;

    function GetLonLatCirclesByPoint(
      const APos: TDoublePoint;
      const ADatum: IDatum;
      const AConfig: IMarkerRingsConfigStatic
    ): IGeometryLonLatPolygon;
    function GetProjectedCirclesByLonLat(
      const ASource: IGeometryLonLatPolygon;
      const AProjection: IProjection
    ): IGeometryProjectedPolygon;
    procedure GPSReceiverReceive;
    procedure OnConfigChange;
    procedure OnTimer;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ATimerNoifier: INotifierTime;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AConfig: IMarkerRingsConfig;
      const AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  Math,
  GR32_Polygons,
  i_GPS,
  u_GeoFunc,
  u_Synchronizer,
  u_SimpleFlagWithInterlock,
  u_ListenerTime,
  u_ListenerByEvent,
  u_ProjectedDrawableElementByPolygon;

{ TMapLayerGPSMarkerRings }

constructor TMapLayerGPSMarkerRings.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ATimerNoifier: INotifierTime;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AConfig: IMarkerRingsConfig;
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FPolygonBuilder := FVectorGeometryLonLatFactory.MakePolygonBuilder;

  FGpsPosChangeFlag := TSimpleFlagWithInterlock.Create;
  FGPSPosCS := GSync.SyncVariable.Make(Self.ClassName);

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 500),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.GPSReceiverReceive),
    FGPSRecorder.GetChangeNotifier
  );
end;

function TMapLayerGPSMarkerRings.GetLonLatCirclesByPoint(
  const APos: TDoublePoint;
  const ADatum: IDatum;
  const AConfig: IMarkerRingsConfigStatic
): IGeometryLonLatPolygon;
var
  i: Integer;
  VDist: Double;
  VPolygon: IGeometryLonLatSinglePolygon;
  VStep: Double;
begin
  VStep := AConfig.StepDistance;
  for i := 1 to AConfig.Count do begin
    VDist := VStep * i;
    VPolygon :=
      FVectorGeometryLonLatFactory.CreateLonLatPolygonCircleByPoint(
        ADatum,
        APos,
        VDist
      );
    FPolygonBuilder.AddPolygon(VPolygon);
  end;
  Result := FPolygonBuilder.MakeStaticAndClear;
end;

function TMapLayerGPSMarkerRings.GetProjectedCirclesByLonLat(
  const ASource: IGeometryLonLatPolygon;
  const AProjection: IProjection
): IGeometryProjectedPolygon;
begin
  Result := FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(AProjection, ASource);
end;

procedure TMapLayerGPSMarkerRings.GPSReceiverReceive;
begin
  FGpsPosChangeFlag.SetFlag;
end;

procedure TMapLayerGPSMarkerRings.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FGPSPosCS.BeginWrite;
    try
      FCirclesLonLat := nil;
      FCirclesProjected := nil;
    finally
      FGPSPosCS.EndWrite;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGPSMarkerRings.OnTimer;
var
  VGPSPosition: IGPSPosition;
  VLonLat: TDoublePoint;
begin
  if FGpsPosChangeFlag.CheckFlagAndReset then begin
    ViewUpdateLock;
    try
      VGPSPosition := FGPSRecorder.CurrentPosition;
      if (not VGPSPosition.PositionOK) then begin
        // no position
        Hide;
      end else begin
        // ok
        VLonLat := VGPSPosition.LonLat;
        FGPSPosCS.BeginWrite;
        try
          if not DoublePointsEqual(FGPSPosLonLat, VLonLat) then begin
            FGPSPosLonLat := VLonLat;
            FCirclesLonLat := nil;
            FCirclesProjected := nil;
            SetNeedRedraw;
          end;
        finally
          FGPSPosCS.EndWrite;
        end;
        Show;
      end;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerGPSMarkerRings.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VLonLat: TDoublePoint;
  VConfig: IMarkerRingsConfigStatic;
  VCirclesLonLat: IGeometryLonLatPolygon;
  VCirclesProjected: IGeometryProjectedPolygon;
  VDrawable: IProjectedDrawableElement;
begin
  inherited;
  VConfig := FConfig.GetStatic;
  if VConfig.Count <= 0 then begin
    Exit;
  end;

  FGPSPosCS.BeginRead;
  try
    VLonLat := FGPSPosLonLat;
    VCirclesLonLat := FCirclesLonLat;
    VDrawable := FCirclesProjected;
  finally
    FGPSPosCS.EndRead;
  end;
  if VDrawable <> nil then begin
    if not VDrawable.Projection.IsSame(ALocalConverter.Projection) then begin
      VDrawable := nil;
    end;
  end;
  if VCirclesLonLat = nil then begin
    VCirclesLonLat := GetLonLatCirclesByPoint(VLonLat, ALocalConverter.Projection.ProjectionType.Datum, VConfig);
  end;
  if VCirclesLonLat = nil then begin
    Exit;
  end;
  FGPSPosCS.BeginWrite;
  try
    if DoublePointsEqual(VLonLat, FGPSPosLonLat) then begin
      FCirclesLonLat := VCirclesLonLat;
    end;
  finally
    FGPSPosCS.EndWrite
  end;
  if VDrawable = nil then begin
    VCirclesProjected := GetProjectedCirclesByLonLat(VCirclesLonLat, ALocalConverter.Projection);
    VDrawable :=
      TProjectedDrawableElementByPolygonSimpleEdge.Create(
        ALocalConverter.Projection,
        VCirclesProjected,
        amNone,
        clRed32
      );
  end;
  if VDrawable = nil then begin
    Exit;
  end;
  FGPSPosCS.BeginWrite;
  try
    if DoublePointsEqual(VLonLat, FGPSPosLonLat) then begin
      FCirclesProjected := VDrawable;
    end;
  finally
    FGPSPosCS.EndWrite
  end;
  VDrawable.Draw(ABuffer, ALocalConverter);
end;

end.
