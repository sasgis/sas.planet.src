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

unit u_MapLayerCalcCircleCaptions;

interface

uses
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LineOnMapEdit,
  i_GeometryLonLat,
  i_Projection,
  i_ValueToStringConverter,
  i_PointCaptionsLayerConfig,
  i_MainFormState,
  u_MapCaptionDrawable,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerCalcCircleCaptions = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FConfig: IPointCaptionsLayerConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FCircleOnMapEdit: ICircleOnMapEdit;

    FLine: ILonLatPathWithSelected;

    FIsValid: Boolean;
    FRect: TRect;
    FPos: TPoint;

    FCaption: TMapCaptionDrawable;

    procedure OnConfigChange;
    procedure OnLineChange;
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
      const ACircleOnMapEdit: ICircleOnMapEdit;
      const AConfig: IPointCaptionsLayerConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  i_ProjectionType,
  u_GeoFunc,
  u_ListenerByEvent,  
  u_ResStrings;

{ TMapLayerCalcLineCaptions }

constructor TMapLayerCalcCircleCaptions.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const ACircleOnMapEdit: ICircleOnMapEdit;
  const AConfig: IPointCaptionsLayerConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable
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
  FValueToStringConverter := AValueToStringConverter;
  FCircleOnMapEdit := ACircleOnMapEdit;

  FCaption := TMapCaptionDrawable.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FValueToStringConverter.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FCircleOnMapEdit.ChangeNotifier
  );
end;

destructor TMapLayerCalcCircleCaptions.Destroy;
begin
  FreeAndNil(FCaption);
  inherited Destroy;
end;

procedure TMapLayerCalcCircleCaptions.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.Visible and Assigned(FLine);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerCalcCircleCaptions.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FCircleOnMapEdit.Path;
    if Assigned(FLine) then begin
      SetNeedRedraw;
      Visible := FConfig.Visible;
    end else begin
      Hide;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerCalcCircleCaptions.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);
var
  VConfig: IPointCaptionsLayerConfigStatic;
  VProjection: IProjection;
  VPoints: PDoublePointArray;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  VText: string;
  VLine: IGeometryLonLatSingleLine;
  VLonLat: TDoublePoint;
  VValueConverter: IValueToStringConverter;
  VLonLatPath: ILonLatPathWithSelected;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  VLonLatPath := FLine;

  FIsValid :=
    Visible and
    (VLonLatPath <> nil) and
    (VLonLatPath.Count > 1) and
    Supports(VLonLatPath.Geometry, IGeometryLonLatSingleLine, VLine);

  if FIsValid then begin
    VValueConverter := FValueToStringConverter.GetStatic;
    VText := Format(SAS_STR_Radius, [VValueConverter.DistConvert(FCircleOnMapEdit.Radius)]);

    VConfig := FConfig.GetStatic;

    FCaption.SetText(
      VText,
      VConfig.TextBGColor,
      VConfig.FontName,
      VConfig.LastPointFontSize,
      VConfig.TextColor
    );

    VPoints := VLine.Points;
    Inc(VPoints);
    VLonLat := VPoints[0];

    VProjection := ALocalConverter.Projection;
    VProjection.ProjectionType.ValidateLonLatPos(VLonLat);
    VPosOnMap := VProjection.LonLat2PixelPosFloat(VLonLat);
    VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);

    FPos:= PointFromDoublePoint(VPosOnBitmap, prToTopLeft);
    FRect := FCaption.GetBoundsForPosition(FPos);

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

procedure TMapLayerCalcCircleCaptions.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      FCaption.DrawToBitmap(ABuffer, FPos);
    end;
  end;
end;

procedure TMapLayerCalcCircleCaptions.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
