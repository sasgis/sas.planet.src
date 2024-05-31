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

unit u_MapLayerGPSMarkerRings;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierTime,
  i_NotifierOperation,
  i_Datum,
  i_Projection,
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
  i_MainFormState,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerGPSMarkerRings = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FConfig: IMarkerRingsConfig;
    FGpsRecorder: IGPSRecorder;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FPolygonBuilder: IGeometryLonLatPolygonBuilder;

    FGpsPosChangeFlag: ISimpleFlag;
    FConfigStatic: IMarkerRingsConfigStatic;

    FLocalConverter: ILocalCoordConverter;

    FIsValid: Boolean;
    FRect: TRect;
    FLonLatPos: TDoublePoint;
    FCirclesLonLat: IGeometryLonLatPolygon;
    FCirclesDrawable: IProjectedDrawableElement;

    function GetLonLatCirclesByPoint(
      const APos: TDoublePoint;
      const ADatum: IDatum;
      const AConfig: IMarkerRingsConfigStatic
    ): IGeometryLonLatPolygon;
    function GetProjectedCirclesByLonLat(
      const ASource: IGeometryLonLatPolygon;
      const AProjection: IProjection
    ): IGeometryProjectedPolygon;

    procedure OnGpsPosChange;
    procedure OnConfigChange;
    procedure OnTimer;
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
      const ATimerNoifier: INotifierTime;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AConfig: IMarkerRingsConfig;
      const AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  Types,
  Math,
  i_GPS,
  u_GeoFunc,
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
  const AMainFormState: IMainFormState;
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

  FMainFormState := AMainFormState;
  FConfig := AConfig;
  FGpsRecorder := AGPSRecorder;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FPolygonBuilder := FVectorGeometryLonLatFactory.MakePolygonBuilder;

  FGpsPosChangeFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 200),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnGpsPosChange),
    FGpsRecorder.ChangeNotifier
  );
end;

function TMapLayerGPSMarkerRings.GetLonLatCirclesByPoint(
  const APos: TDoublePoint;
  const ADatum: IDatum;
  const AConfig: IMarkerRingsConfigStatic
): IGeometryLonLatPolygon;
var
  I: Integer;
  VDist: Double;
  VStep: Double;
  VPolygon: IGeometryLonLatSinglePolygon;
begin
  VStep := AConfig.StepDistance;
  for I := 1 to AConfig.Count do begin
    VDist := VStep * I;
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
  Result :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      AProjection,
      ASource
    );
end;

procedure TMapLayerGPSMarkerRings.OnGpsPosChange;
begin
  FGpsPosChangeFlag.SetFlag;
end;

procedure TMapLayerGPSMarkerRings.OnConfigChange;
begin
  FCirclesLonLat := nil;
  FCirclesDrawable := nil;
  FConfigStatic := FConfig.GetStatic;

  if Visible then begin
    ViewUpdateLock;
    try
      SetNeedRedraw;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerGPSMarkerRings.OnTimer;
var
  VLonLat: TDoublePoint;
  VGPSPosition: IGPSPosition;
begin
  if (FConfigStatic.Count > 0) and FGpsPosChangeFlag.CheckFlagAndReset then begin
    ViewUpdateLock;
    try
      VGPSPosition := FGpsRecorder.CurrentPosition;
      if not VGPSPosition.PositionOK then begin
        // no position
        Hide;
      end else begin
        // ok
        VLonLat := VGPSPosition.LonLat;
        if not DoublePointsEqual(FLonLatPos, VLonLat) then begin
          FLonLatPos := VLonLat;
          FCirclesLonLat := nil;
          FCirclesDrawable := nil;
          SetNeedRedraw;
        end;
        Show;
      end;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerGPSMarkerRings.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);
var
  VCirclesProjected: IGeometryProjectedPolygon;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  if not Visible then begin
    Exit;
  end;

  FLocalConverter := ALocalConverter;

  if FCirclesDrawable <> nil then begin
    if not FCirclesDrawable.Projection.IsSame(FLocalConverter.Projection) then begin
      FCirclesDrawable := nil;
    end;
  end;

  if FCirclesLonLat = nil then begin
    FCirclesLonLat :=
      GetLonLatCirclesByPoint(
        FLonLatPos,
        FLocalConverter.Projection.ProjectionType.Datum,
        FConfigStatic
      );
    if FCirclesLonLat = nil then begin
      Exit;
    end;
  end;

  if FCirclesDrawable = nil then begin
    VCirclesProjected :=
      GetProjectedCirclesByLonLat(
        FCirclesLonLat,
        FLocalConverter.Projection
      );
    FCirclesDrawable :=
      TProjectedDrawableElementByPolygonSimpleEdge.Create(
        FLocalConverter.Projection,
        VCirclesProjected,
        clRed32
      );
  end;

  if FCirclesDrawable <> nil then begin
    FRect := FCirclesDrawable.GetBounds(FLocalConverter);
    FIsValid := not GR32.IsRectEmpty(FRect);
    if not FIsValid then begin
      Exit;
    end;
    // draw
    if FMainFormState.IsMapMoving then begin
      DoInvalidateFull;
    end else begin
      DoInvalidateRect(FRect);
    end;
  end;
end;

procedure TMapLayerGPSMarkerRings.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      FCirclesDrawable.Draw(ABuffer, FLocalConverter);
    end;
  end;
end;

procedure TMapLayerGPSMarkerRings.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
