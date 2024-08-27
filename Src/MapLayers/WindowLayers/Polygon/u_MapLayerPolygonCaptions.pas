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

unit u_MapLayerPolygonCaptions;

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
  i_PolygonCaptionsLayerConfig,
  i_MainFormState,
  u_MapCaptionDrawable,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerPolygonCaptions = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FConfig: IPolygonCaptionsLayerConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FPolygonOnMapEdit: IPolygonOnMapEdit;

    FPolygon: ILonLatPolygonWithSelected;

    FIsValid: Boolean;
    FRect: TRect;
    FPos: TPoint;

    FCaption: TMapCaptionDrawable;

    FArea: Double;
    FPerimeter: Double;
    FProjection: IProjection;
    FNeedUpdateValues: Boolean;

    procedure OnConfigChange;
    procedure OnPolygonChange;
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
      const APolygonOnMapEdit: IPolygonOnMapEdit;
      const AConfig: IPolygonCaptionsLayerConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  i_GeoCalc,
  i_ProjectionType,
  u_GeoFunc,
  u_GeoCalc,
  u_ListenerByEvent,
  u_ResStrings;

{ TMapLayerPolygonCaptions }

constructor TMapLayerPolygonCaptions.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const APolygonOnMapEdit: IPolygonOnMapEdit;
  const AConfig: IPolygonCaptionsLayerConfig;
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
  FPolygonOnMapEdit := APolygonOnMapEdit;

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
    TNotifyNoMmgEventListener.Create(Self.OnPolygonChange),
    FPolygonOnMapEdit.ChangeNotifier
  );

  FNeedUpdateValues := True;
end;

destructor TMapLayerPolygonCaptions.Destroy;
begin
  FreeAndNil(FCaption);
  inherited Destroy;
end;

procedure TMapLayerPolygonCaptions.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FNeedUpdateValues := True;
    Visible := FConfig.Visible and Assigned(FPolygon);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerPolygonCaptions.OnPolygonChange;
begin
  ViewUpdateLock;
  try
    FNeedUpdateValues := True;
    FPolygon := FPolygonOnMapEdit.Polygon;
    if Assigned(FPolygon) then begin
      SetNeedRedraw;
      Visible := FConfig.Visible;
    end else begin
      Hide;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerPolygonCaptions.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);

  procedure _CalcValues(
    const AConfig: IPolygonCaptionsLayerConfigStatic;
    const APolygon: IGeometryLonLatPolygon
  );
  var
    VGeoCalc: IGeoCalc;
  begin
    if FNeedUpdateValues or
       (FProjection = nil) or
       not FProjection.IsSame(ALocalConverter.Projection)
    then begin
      FNeedUpdateValues := False;
      FProjection := ALocalConverter.Projection;
      VGeoCalc := TGeoCalc.Create(FProjection.ProjectionType.Datum);

      if AConfig.ShowArea then begin
        FArea := VGeoCalc.CalcPolygonArea(APolygon);
      end;

      if AConfig.ShowPerimeter then begin
        FPerimeter := VGeoCalc.CalcPolygonPerimeter(APolygon);
      end;
    end;
  end;

const
  CSep: array [Boolean] of string = ('', '; ');
var
  VConfig: IPolygonCaptionsLayerConfigStatic;
  VProjection: IProjection;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  VText: string;
  VLonLat: TDoublePoint;
  VValueConverter: IValueToStringConverter;
  VLonLatPolygon: ILonLatPolygonWithSelected;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  VLonLatPolygon := FPolygon;

  FIsValid :=
    Visible and
    (VLonLatPolygon <> nil) and
    (VLonLatPolygon.Count > 2);

  if FIsValid then begin
    VText := '';

    VConfig := FConfig.GetStatic;
    VValueConverter := FValueToStringConverter.GetStatic;

    _CalcValues(VConfig, VLonLatPolygon.Geometry);

    if VConfig.ShowArea then begin
      VText := VText + CSep[VText <> ''] + Format(SAS_STR_Area, [VValueConverter.AreaConvert(FArea)]);
    end;

    if VConfig.ShowPerimeter then begin
      VText := VText + CSep[VText <> ''] + Format(SAS_STR_Perimeter, [VValueConverter.DistConvert(FPerimeter)]);
    end;

    FIsValid := VText <> '';
    if not FIsValid then begin
      Exit;
    end;

    FCaption.SetText(
      VText,
      VConfig.TextBGColor,
      VConfig.FontName,
      VConfig.FontSize,
      VConfig.TextColor
    );

    VLonLat := VLonLatPolygon.Points[VLonLatPolygon.GetSelectedPointIndex];

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

procedure TMapLayerPolygonCaptions.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      FCaption.DrawToBitmap(ABuffer, FPos);
    end;
  end;
end;

procedure TMapLayerPolygonCaptions.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
