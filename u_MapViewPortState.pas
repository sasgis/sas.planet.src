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

unit u_MapViewPortState;

interface

uses
  Types,
  i_Notifier,
  i_Listener,
  t_GeoTypes,
  i_SimpleFlag,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_MapTypes,
  i_LocalCoordConverterFactorySimpe,
  u_ConfigDataElementBase;

type
  TMapViewPortState = class(TConfigDataElementBase, IViewPortState)
  private
    FVisibleCoordConverterFactory: ILocalCoordConverterFactorySimpe;
    FMainMap: IMapTypeChangeable;
    FBaseScale: Double;

    FPosition: ILocalCoordConverterChangeable;
    FView: ILocalCoordConverterChangeableInternal;
    FMainMapChangeListener: IListener;

    FMainCoordConverter: ICoordConverter;

    function _GetActiveCoordConverter: ICoordConverter;
    procedure _SetActiveCoordConverter;
    procedure OnMainMapChange;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMainCoordConverter: ICoordConverter;
    procedure SetMainCoordConverter(const AValue: ICoordConverter);

    function GetCurrentZoom: Byte;

    function GetPosition: ILocalCoordConverterChangeable;
    function GetView: ILocalCoordConverterChangeable;

    procedure ChangeViewSize(const ANewSize: TPoint);
    procedure ChangeMapPixelByLocalDelta(const ADelta: TDoublePoint);
    procedure ChangeMapPixelToVisualPoint(const AVisualPoint: TPoint);
    procedure ChangeZoomWithFreezeAtVisualPoint(
      const AZoom: Byte;
      const AFreezePoint: TPoint
    );
    procedure ChangeLonLat(const ALonLat: TDoublePoint);
    procedure ChangeLonLatAndZoom(
      const AZoom: Byte;
      const ALonLat: TDoublePoint
    );

    procedure ChangeZoomWithFreezeAtCenter(const AZoom: Byte);
    procedure ChangeZoomWithFreezeAtVisualPointWithScale(
      const AZoom: Byte;
      const AFreezePoint: TPoint
    );

    procedure ScaleTo(
      const AScale: Double;
      const AFreezePoint: TPoint
    );
  public
    constructor Create(
      const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMainMap: IMapTypeChangeable;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock,
  u_LocalCoordConverterChangeable,
  u_LocalConverterChangeableFixedTileRectNoScale,
  u_GeoFunc;

{ TMapViewPortStateNew }

constructor TMapViewPortState.Create(
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMainMap: IMapTypeChangeable;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VGeoConverter: ICoordConverter;
  VLocalConverter: ILocalCoordConverter;
  VCenterPoint: TDoublePoint;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
  VZoom: Byte;
  VViewChangedFlag: ISimpleFlag;
begin
  inherited Create;

  FVisibleCoordConverterFactory := ACoordConverterFactory;
  FMainMap := AMainMap;
  FMainCoordConverter := nil;

  FMainMapChangeListener := TNotifyNoMmgEventListener.Create(Self.OnMainMapChange);
  FBaseScale := 1;

  VGeoConverter := _GetActiveCoordConverter;
  VZoom := 0;
  VGeoConverter.CheckZoom(VZoom);
  VCenterPoint := RectCenter(VGeoConverter.PixelRectAtZoom(VZoom));
  VLocalRect := Rect(0, 0, 1024, 768);
  VLocalCenter := RectCenter(VLocalRect);

  VLocalConverter :=
    FVisibleCoordConverterFactory.CreateConverter(
      VLocalRect,
      VZoom,
      VGeoConverter,
      FBaseScale,
      DoublePoint(VCenterPoint.X - VLocalCenter.X / FBaseScale, VCenterPoint.Y - VLocalCenter.Y / FBaseScale)
    );
  VViewChangedFlag := TSimpleFlagWithInterlock.Create;
  FView :=
    TLocalCoordConverterChangeable.Create(
      VViewChangedFlag,
      VLocalConverter,
      APerfCounterList.CreateAndAddNewCounter('ScaleChange')
    );
  FPosition :=
    TLocalConverterChangeableFixedTileRectNoScale.Create(
      APerfCounterList.CreateAndAddNewCounter('PosChange'),
      FVisibleCoordConverterFactory,
      FView
    );
  FMainMap.ChangeNotifier.Add(FMainMapChangeListener);
end;

destructor TMapViewPortState.Destroy;
begin
  if Assigned(FMainMap) and Assigned(FMainMapChangeListener) then begin
    FMainMap.ChangeNotifier.Remove(FMainMapChangeListener);
    FMainMapChangeListener := nil;
    FMainMap := nil;
  end;
  FVisibleCoordConverterFactory := nil;
  inherited;
end;

procedure TMapViewPortState.ChangeLonLat(const ALonLat: TDoublePoint);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  LockWrite;
  try
    VLocalConverter := FView.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeCenterLonLat(
        VLocalConverter,
        ALonLat
      );
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.ChangeLonLatAndZoom(const AZoom: Byte;
  const ALonLat: TDoublePoint);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VLonLat: TDoublePoint;
  VMapPixelCenter: TDoublePoint;
  VMapTopLeft: TDoublePoint;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
begin
  LockWrite;
  try
    VLocalConverter := FView.GetStatic;
    VConverter := VLocalConverter.GeoConverter;
    VZoom := AZoom;
    VConverter.CheckZoom(VZoom);
    VLonLat := ALonLat;
    VConverter.CheckLonLatPos(VLonLat);
    VMapPixelCenter := VConverter.LonLat2PixelPosFloat(VLonLat, VZoom);
    VLocalRect := VLocalConverter.GetLocalRect;
    VLocalCenter := RectCenter(VLocalRect);
    VMapTopLeft.X := VMapPixelCenter.X - VLocalCenter.X;
    VMapTopLeft.Y := VMapPixelCenter.Y - VLocalCenter.Y;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.CreateConverter(
        VLocalConverter.GetLocalRect,
        VZoom,
        VConverter,
        FBaseScale,
        VMapTopLeft
      );
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.ChangeMapPixelByLocalDelta(const ADelta: TDoublePoint);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  if (Abs(ADelta.X) > 0.001) or (Abs(ADelta.Y) > 0.001) then begin
    LockWrite;
    try
      VLocalConverter := FView.GetStatic;
      VLocalConverterNew :=
        FVisibleCoordConverterFactory.ChangeByLocalDelta(
          VLocalConverter,
          ADelta
        );
      FView.SetConverter(VLocalConverterNew);
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TMapViewPortState.ChangeMapPixelToVisualPoint(
  const AVisualPoint: TPoint
);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  LockWrite;
  try
    VLocalConverter := FView.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeCenterToLocalPoint(
        VLocalConverter,
        AVisualPoint
      );
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.ChangeViewSize(const ANewSize: TPoint);
var
  VLocalConverter: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
  VLocalRectNew: TRect;
  VLocalRectOld: TRect;
  VLocalCenterOld: TDoublePoint;
  VLocalCenterNew: TDoublePoint;
  VTopLeftMapPixel: TDoublePoint;
  VScale: Double;
begin
  if ANewSize.X <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.X > 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.Y <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.Y > 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  VLocalRectNew := Rect(0, 0, ANewSize.X, ANewSize.Y);
  LockWrite;
  try
    VLocalConverter := FView.GetStatic;
    VLocalRectOld := VLocalConverter.GetLocalRect;
    if not EqualRect(VLocalRectOld, VLocalRectNew) then begin
      VGeoConverter := VLocalConverter.GeoConverter;
      VLocalCenterOld := RectCenter(VLocalRectOld);
      VLocalCenterNew := RectCenter(VLocalRectNew);
      VTopLeftMapPixel := VLocalConverter.GetRectInMapPixelFloat.TopLeft;
      VScale := VLocalConverter.GetScale;
      VTopLeftMapPixel.X := VTopLeftMapPixel.X + (VLocalCenterOld.X - VLocalCenterNew.X) / VScale;
      VTopLeftMapPixel.Y := VTopLeftMapPixel.Y + (VLocalCenterOld.Y - VLocalCenterNew.Y) / VScale;
      VLocalConverterNew :=
        FVisibleCoordConverterFactory.CreateConverter(
          VLocalRectNew,
          VLocalConverter.Zoom,
          VGeoConverter,
          VScale,
          VTopLeftMapPixel
        );
      FView.SetConverter(VLocalConverterNew);
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.ChangeZoomWithFreezeAtCenter(const AZoom: Byte);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  LockWrite;
  try
    VLocalConverter := FView.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeZoomWithFreezeAtCenter(
        VLocalConverter,
        AZoom
      );
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.ChangeZoomWithFreezeAtVisualPoint(
  const AZoom: Byte;
  const AFreezePoint: TPoint
);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  LockWrite;
  try
    VLocalConverter := FView.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeZoomWithFreezeAtVisualPoint(
        VLocalConverter,
        AZoom,
        AFreezePoint
      );
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.ChangeZoomWithFreezeAtVisualPointWithScale(
  const AZoom: Byte;
  const AFreezePoint: TPoint
);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalViewConverterNew: ILocalCoordConverter;
  VZoomOld: Byte;
  VZoomNew: Byte;
  VConverter: ICoordConverter;
  VLocalRect: TRect;
  VScaleOld: Double;
  VScaleNew: Double;
  VTopLefAtMapView: TDoublePoint;
  VFreezePoint: TDoublePoint;
  VFreezeMapPoint: TDoublePoint;
  VRelativeFreezePoint: TDoublePoint;
  VMapFreezPointAtNewZoom: TDoublePoint;
begin
  LockWrite;
  try
    VLocalConverter := FView.GetStatic;
    VZoomOld := VLocalConverter.Zoom;
    VZoomNew := AZoom;
    VConverter := VLocalConverter.GeoConverter;
    VConverter.CheckZoom(VZoomNew);
    if VZoomOld = VZoomNew then begin
      Exit;
    end;
    VFreezeMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(AFreezePoint);
    VConverter.CheckPixelPosFloatStrict(VFreezeMapPoint, VZoomOld, False);
    VFreezePoint := VLocalConverter.MapPixelFloat2LocalPixelFloat(VFreezeMapPoint);
    VRelativeFreezePoint := VConverter.PixelPosFloat2Relative(VFreezeMapPoint, VZoomOld);
    VMapFreezPointAtNewZoom := VConverter.Relative2PixelPosFloat(VRelativeFreezePoint, VZoomNew);

    VLocalRect := VLocalConverter.GetLocalRect;
    VScaleOld := VLocalConverter.GetScale;

    VScaleNew := VScaleOld * (VConverter.PixelsAtZoomFloat(VZoomOld) / VConverter.PixelsAtZoomFloat(VZoomNew));
    VTopLefAtMapView.X := VMapFreezPointAtNewZoom.X - VFreezePoint.X / VScaleNew;
    VTopLefAtMapView.Y := VMapFreezPointAtNewZoom.Y - VFreezePoint.Y / VScaleNew;
    VLocalViewConverterNew :=
      FVisibleCoordConverterFactory.CreateConverter(
        VLocalRect,
        VZoomNew,
        VConverter,
        VScaleNew,
        VTopLefAtMapView
      );
    FView.SetConverter(VLocalViewConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VLonLat: TDoublePoint;
  VZoom: Byte;
  VLocalConverter: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  inherited;
  if AConfigData <> nil then begin
    VLocalConverter := FView.GetStatic;
    VGeoConverter := VLocalConverter.GeoConverter;
    VZoom := AConfigData.ReadInteger('Zoom', VLocalConverter.Zoom);
    VGeoConverter.CheckZoom(VZoom);
    VLonLat := VLocalConverter.GetCenterLonLat;
    VLonLat.X := AConfigData.ReadFloat('X', VLonLat.X);
    VLonLat.Y := AConfigData.ReadFloat('Y', VLonLat.Y);
    VGeoConverter.CheckLonLatPos(VLonLat);

    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeCenterLonLatAndZoom(
        VLocalConverter,
        VZoom,
        VLonLat
      );
    FView.SetConverter(VLocalConverterNew);
  end;
end;

procedure TMapViewPortState.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VLonLat: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
begin
  inherited;
  VLocalConverter := FView.GetStatic;
  VLonLat := VLocalConverter.GetCenterLonLat;
  AConfigData.WriteInteger('Zoom', VLocalConverter.Zoom);
  AConfigData.WriteFloat('X', VLonLat.X);
  AConfigData.WriteFloat('Y', VLonLat.Y);
end;

function TMapViewPortState._GetActiveCoordConverter: ICoordConverter;
var
  VMapType: IMapType;
begin
  Result := nil;
  if FMainCoordConverter <> nil then begin
    Result := FMainCoordConverter;
  end else begin
    VMapType := FMainMap.GetStatic;
    if VMapType <> nil then begin
      Result := VMapType.ViewGeoConvert;
    end;
  end;
end;

function TMapViewPortState.GetCurrentZoom: Byte;
begin
  LockRead;
  try
    Result := FView.GetStatic.Zoom;
  finally
    UnlockRead;
  end;
end;

function TMapViewPortState.GetMainCoordConverter: ICoordConverter;
begin
  LockRead;
  try
    Result := FMainCoordConverter;
  finally
    UnlockRead;
  end;
end;

function TMapViewPortState.GetPosition: ILocalCoordConverterChangeable;
begin
  Result := FPosition;
end;

function TMapViewPortState.GetView: ILocalCoordConverterChangeable;
begin
  Result := FView;
end;

procedure TMapViewPortState.OnMainMapChange;
begin
  LockWrite;
  try
    _SetActiveCoordConverter;
  finally
    UnlockWrite
  end;
end;

procedure TMapViewPortState.ScaleTo(
  const AScale: Double;
  const AFreezePoint: TPoint
);
var
  VVisiblePointFixed: TDoublePoint;
  VMapPointFixed: TDoublePoint;
  VNewMapScale: Double;
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VZoom: Byte;
  VTopLeftMapPosNew: TDoublePoint;
  VLocalRect: TRect;
begin
  LockWrite;
  try
    VLocalConverter := FView.GetStatic;
    if Abs(AScale - 1) < 0.001 then begin
      VNewMapScale := FBaseScale;
    end else begin
      VNewMapScale := FBaseScale * AScale;
    end;
    VZoom := VLocalConverter.Zoom;
    VGeoConverter := VLocalConverter.GeoConverter;
    VMapPointFixed := VLocalConverter.LocalPixel2MapPixelFloat(AFreezePoint);
    VGeoConverter.CheckPixelPosFloatStrict(VMapPointFixed, VZoom, False);
    VVisiblePointFixed := VLocalConverter.MapPixelFloat2LocalPixelFloat(VMapPointFixed);

    VLocalRect := VLocalConverter.GetLocalRect;
    VTopLeftMapPosNew.X := VMapPointFixed.X - VVisiblePointFixed.X / VNewMapScale;
    VTopLeftMapPosNew.Y := VMapPointFixed.Y - VVisiblePointFixed.Y / VNewMapScale;

    VLocalConverterNew :=
      FVisibleCoordConverterFactory.CreateConverter(
        VLocalRect,
        VZoom,
        VGeoConverter,
        VNewMapScale,
        VTopLeftMapPosNew
      );
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState._SetActiveCoordConverter;
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
begin
  VLocalConverter := FView.GetStatic;
  VGeoConverter := _GetActiveCoordConverter;
  if not VLocalConverter.GeoConverter.IsSameConverter(VGeoConverter) then begin
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeConverter(
        VLocalConverter,
        VGeoConverter
      );
    FView.SetConverter(VLocalConverterNew);
  end;
end;

procedure TMapViewPortState.SetMainCoordConverter(const AValue: ICoordConverter);
begin
  LockWrite;
  try
    if FMainCoordConverter <> AValue then begin
      FMainCoordConverter := AValue;
      _SetActiveCoordConverter;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
