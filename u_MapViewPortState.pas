{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
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
  i_ActiveMapsConfig,
  i_LocalCoordConverterFactorySimpe,
  u_ConfigDataElementBase;

type
  TMapViewPortState = class(TConfigDataElementBase, IViewPortState)
  private
    FMainCoordConverter: ICoordConverter;
    FVisibleCoordConverterFactory: ILocalCoordConverterFactorySimpe;
    FMainMapConfig: IMainActiveMap;

    FChangedFlag: ISimpleFlag;
    FStopNotifyCounter: ICounter;
    FPosition: ILocalCoordConverterChangeableInternal;
    FView: ILocalCoordConverterChangeableInternal;

    FBaseScale: Double;

    FMainMapChangeListener: IListener;
    function _GetActiveCoordConverter: ICoordConverter;
    function CreateVisibleCoordConverter(
      const AActiveCoordConverter: ICoordConverter;
      AViewSize: TPoint;
      AVisibleMove: TDoublePoint;
      AMapScale: Double;
      ACenterPos: TDoublePoint;
      AZoom: Byte
    ): ILocalCoordConverter;
    procedure _SetActiveCoordConverter;
    procedure OnMainMapChange;
  protected
    procedure DoInChangeNotify; override;
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
    procedure StopNotify; override;
    procedure StartNotify; override;
  private
    function GetMainCoordConverter: ICoordConverter;
    procedure SetMainCoordConverter(const AValue: ICoordConverter);

    function GetCurrentCoordConverter: ICoordConverter;
    function GetCurrentZoom: Byte;

    function GetPosition: ILocalCoordConverterChangeable;
    function GetView: ILocalCoordConverterChangeable;

    procedure ChangeViewSize(const ANewSize: TPoint);
    procedure ChangeMapPixelByDelta(const ADelta: TDoublePoint);
    procedure ChangeMapPixelToVisualPoint(const AVisualPoint: TPoint);
    procedure ChangeZoomWithFreezeAtVisualPoint(
      const AZoom: Byte;
      const AFreezePoint: TPoint
    );
    procedure ChangeZoomWithFreezeAtCenter(const AZoom: Byte);

    procedure ChangeLonLat(const ALonLat: TDoublePoint);
    procedure FitRectToScreen(const ALonLatRect: TDoubleRect);

    procedure MoveTo(const Pnt: TPoint);
    procedure ScaleTo(
      const AScale: Double;
      const ACenterPoint: TPoint
    );
  public
    constructor Create(
      const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMainMapConfig: IMainActiveMap;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_MapTypes,
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock,
  u_LocalCoordConverterChangeable,
  u_GeoFun;

{ TMapViewPortStateNew }

constructor TMapViewPortState.Create(
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMainMapConfig: IMainActiveMap;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VGeoConverter: ICoordConverter;
  VLocalConverter: ILocalCoordConverter;
  VCenterPoint: TDoublePoint;
  VZoom: Byte;
  VPositionChangedFlag: ISimpleFlag;
  VViewChangedFlag: ISimpleFlag;
begin
  FChangedFlag := TSimpleFlagWithInterlock.Create;
  FStopNotifyCounter := TCounterInterlock.Create;
  inherited Create(FChangedFlag, FStopNotifyCounter);

  FVisibleCoordConverterFactory := ACoordConverterFactory;
  FMainMapConfig := AMainMapConfig;
  FMainCoordConverter := nil;

  FMainMapChangeListener := TNotifyNoMmgEventListener.Create(Self.OnMainMapChange);
  FBaseScale := 1;

  VGeoConverter := _GetActiveCoordConverter;
  VZoom := 0;
  VGeoConverter.CheckZoom(VZoom);
  VCenterPoint := RectCenter(VGeoConverter.PixelRectAtZoom(VZoom));

  VLocalConverter :=
    CreateVisibleCoordConverter(
      VGeoConverter,
      Point(1024, 768),
      DoublePoint(0, 0),
      FBaseScale,
      VCenterPoint,
      VZoom
    );
  VPositionChangedFlag := TSimpleFlagWithParent.Create(FChangedFlag);
  FPosition :=
    TLocalCoordConverterChangeable.Create(
      VPositionChangedFlag,
      VLocalConverter,
      APerfCounterList.CreateAndAddNewCounter('PosChange')
    );
  VViewChangedFlag := TSimpleFlagWithParent.Create(FChangedFlag);
  FView :=
    TLocalCoordConverterChangeable.Create(
      VViewChangedFlag,
      VLocalConverter,
      APerfCounterList.CreateAndAddNewCounter('ScaleChange')
    );
  FMainMapConfig.GetChangeNotifier.Add(FMainMapChangeListener);
end;

destructor TMapViewPortState.Destroy;
begin
  if FMainMapConfig <> nil then begin
    FMainMapConfig.GetChangeNotifier.Remove(FMainMapChangeListener);
    FMainMapChangeListener := nil;
    FMainMapConfig := nil;
  end;
  FVisibleCoordConverterFactory := nil;
  inherited;
end;

procedure TMapViewPortState.FitRectToScreen(const ALonLatRect: TDoubleRect);
var
  VCenterLonLat: TDoublePoint;
  VLLRect: TDoubleRect;
  VGeoConverter: ICoordConverter;
  VScreenSize: TPoint;
  VRelativeRect: TDoubleRect;
  VTargetZoom: Byte;
  VZoom: Byte;
  VMarkMapRect: TDoubleRect;
  VMarkMapSize: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  if PointIsEmpty(ALonLatRect.TopLeft) or PointIsEmpty(ALonLatRect.BottomRight) then begin
    Exit;
  end;
  if DoublePointsEqual(ALonLatRect.TopLeft, ALonLatRect.BottomRight) then begin
    Exit;
  end;
  VCenterLonLat.X := (ALonLatRect.Left + ALonLatRect.Right) / 2;
  VCenterLonLat.Y := (ALonLatRect.Top + ALonLatRect.Bottom) / 2;
  VLLRect := ALonLatRect;
  LockWrite;
  try
    VLocalConverter := FPosition.GetStatic;
    VGeoConverter := VLocalConverter.GeoConverter;
    VScreenSize := VLocalConverter.GetLocalRectSize;

    VGeoConverter.CheckLonLatRect(VLLRect);
    VRelativeRect := VGeoConverter.LonLatRect2RelativeRect(VLLRect);

    VTargetZoom := 23;
    for VZoom := 1 to 23 do begin
      VMarkMapRect := VGeoConverter.RelativeRect2PixelRectFloat(VRelativeRect, VZoom);
      VMarkMapSize.X := VMarkMapRect.Right - VMarkMapRect.Left;
      VMarkMapSize.Y := VMarkMapRect.Bottom - VMarkMapRect.Top;
      if (VMarkMapSize.X > VScreenSize.X) or (VMarkMapSize.Y > VScreenSize.Y) then begin
        VTargetZoom := VZoom - 1;
        Break;
      end;
    end;
    VGeoConverter.CheckZoom(VTargetZoom);
    VGeoConverter.CheckLonLatPos(VCenterLonLat);
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeCenterLonLatAndZoom(
        VLocalConverter,
        VTargetZoom,
        VCenterLonLat
      );
    FPosition.SetConverter(VLocalConverterNew);
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.ChangeLonLat(const ALonLat: TDoublePoint);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  LockWrite;
  try
    VLocalConverter := FPosition.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeCenterLonLat(
        VLocalConverter,
        ALonLat
      );
    FPosition.SetConverter(VLocalConverterNew);
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortState.ChangeMapPixelByDelta(const ADelta: TDoublePoint);
var
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  LockWrite;
  try
    VLocalConverter := FPosition.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeByMapPixelDelta(
        VLocalConverter,
        ADelta
      );
    FPosition.SetConverter(VLocalConverterNew);
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
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
    VLocalConverter := FPosition.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeCenterToLocalPoint(
        VLocalConverter,
        AVisualPoint
      );
    FPosition.SetConverter(VLocalConverterNew);
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
  LockWrite;
  try
    VLocalConverter := FPosition.GetStatic;
    if not EqualRect(VLocalConverter.GetLocalRect, Rect(0, 0, ANewSize.X, ANewSize.Y)) then begin
      VGeoConverter := VLocalConverter.GeoConverter;
      VLocalConverterNew :=
        CreateVisibleCoordConverter(
          VGeoConverter,
          ANewSize,
          DoublePoint(0, 0),
          FBaseScale,
          VLocalConverter.GetCenterMapPixelFloat,
          VLocalConverter.Zoom
        );
      FPosition.SetConverter(VLocalConverterNew);
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
    VLocalConverter := FPosition.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeZoomWithFreezeAtCenter(
        VLocalConverter,
        AZoom
      );
    FPosition.SetConverter(VLocalConverterNew);
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
    VLocalConverter := FPosition.GetStatic;
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeZoomWithFreezeAtVisualPoint(
        VLocalConverter,
        AZoom,
        AFreezePoint
      );
    FPosition.SetConverter(VLocalConverterNew);
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
end;

function TMapViewPortState.CreateVisibleCoordConverter(
  const AActiveCoordConverter: ICoordConverter;
  AViewSize: TPoint;
  AVisibleMove: TDoublePoint;
  AMapScale: Double;
  ACenterPos: TDoublePoint;
  AZoom: Byte
): ILocalCoordConverter;
var
  VViewCenter: TPoint;
  VLocalTopLeftAtMap: TDoublePoint;
begin
  VViewCenter := Point(AViewSize.X div 2, AViewSize.Y div 2);
  VLocalTopLeftAtMap.X := (-VViewCenter.X + AVisibleMove.X) / AMapScale + ACenterPos.X;
  VLocalTopLeftAtMap.Y := (-VViewCenter.Y + AVisibleMove.Y) / AMapScale + ACenterPos.Y;

  Result := FVisibleCoordConverterFactory.CreateConverter(
    Rect(0, 0, AViewSize.X, AViewSize.Y),
    AZoom,
    AActiveCoordConverter,
    AMapScale,
    VLocalTopLeftAtMap
  );
end;

procedure TMapViewPortState.DoInChangeNotify;
begin
  FPosition.StartNotify;
  FView.StartNotify;
  inherited;
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
    VLocalConverter := FPosition.GetStatic;
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
    FPosition.SetConverter(VLocalConverterNew);
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
  VLocalConverter := FPosition.GetStatic;
  VLonLat := VLocalConverter.GetCenterLonLat;
  AConfigData.WriteInteger('Zoom', VLocalConverter.Zoom);
  AConfigData.WriteFloat('X', VLonLat.X);
  AConfigData.WriteFloat('Y', VLonLat.Y);
end;

function TMapViewPortState._GetActiveCoordConverter: ICoordConverter;
var
  VMap: IMapType;
begin
  Result := nil;
  if FMainCoordConverter <> nil then begin
    Result := FMainCoordConverter;
  end else begin
    VMap := FMainMapConfig.GetActiveMap.GetStatic;
    if VMap <> nil then begin
      Result := VMap.MapType.ViewGeoConvert;
    end;
  end;
end;

function TMapViewPortState.GetCurrentCoordConverter: ICoordConverter;
begin
  LockRead;
  try
    Result := FPosition.GetStatic.GeoConverter;
  finally
    UnlockRead;
  end;
end;

function TMapViewPortState.GetCurrentZoom: Byte;
begin
  LockRead;
  try
    Result := FPosition.GetStatic.Zoom;
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

procedure TMapViewPortState.MoveTo(const Pnt: TPoint);
var
  VVisibleMove: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
begin
  LockWrite;
  try
    VLocalConverter := FPosition.GetStatic;
    VVisibleMove := DoublePoint(Pnt);
    VLocalConverterNew :=
      CreateVisibleCoordConverter(
        VLocalConverter.GeoConverter,
        VLocalConverter.GetLocalRectSize,
        VVisibleMove,
        FBaseScale,
        VLocalConverter.GetCenterMapPixelFloat,
        VLocalConverter.Zoom
      );
    FView.SetConverter(VLocalConverterNew);
  finally
    UnlockWrite;
  end;
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
  const ACenterPoint: TPoint
);
var
  VVisiblePointFixed: TDoublePoint;
  VMapPointFixed: TDoublePoint;
  VNewMapScale: Double;
  VViewCenter: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
  VLocalConverterNew: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VCenterPos: TDoublePoint;
  VZoom: Byte;
  VCenterPosNew: TDoublePoint;
  VLocalRect: TRect;
begin
  VVisiblePointFixed := DoublePoint(ACenterPoint);
  LockWrite;
  try
    VLocalConverter := FPosition.GetStatic;
    if Abs(AScale - 1) < 0.001 then begin
      VLocalConverterNew := VLocalConverter;
    end else begin
      VZoom := VLocalConverter.Zoom;
      VGeoConverter := VLocalConverter.GeoConverter;
      VNewMapScale := FBaseScale * AScale;
      VMapPointFixed := VLocalConverter.LocalPixelFloat2MapPixelFloat(VVisiblePointFixed);
      VGeoConverter.CheckPixelPosFloatStrict(VMapPointFixed, VZoom, False);
      VVisiblePointFixed := VLocalConverter.MapPixelFloat2LocalPixelFloat(VMapPointFixed);
      VLocalRect := VLocalConverter.GetLocalRect;
      VViewCenter := RectCenter(VLocalRect);
      VCenterPos := VLocalConverter.GetCenterMapPixelFloat;
      VCenterPosNew.X := VMapPointFixed.X + ((VLocalRect.Right - VLocalRect.Left) / 2 - VVisiblePointFixed.X) / VNewMapScale;
      VCenterPosNew.Y := VMapPointFixed.Y + ((VLocalRect.Bottom - VLocalRect.Top) / 2 - VVisiblePointFixed.Y) / VNewMapScale;

      VLocalConverterNew :=
        CreateVisibleCoordConverter(
          VGeoConverter,
          VLocalConverter.GetLocalRectSize,
          DoublePoint(0, 0),
          VNewMapScale,
          VCenterPosNew,
          VZoom
        );
    end;
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
  VLocalConverter := FPosition.GetStatic;
  VGeoConverter := _GetActiveCoordConverter;
  if not VLocalConverter.GeoConverter.IsSameConverter(VGeoConverter) then begin
    VLocalConverterNew :=
      FVisibleCoordConverterFactory.ChangeConverter(
        VLocalConverter,
        VGeoConverter
      );
    FPosition.SetConverter(VLocalConverterNew);
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

procedure TMapViewPortState.StartNotify;
begin
  if FStopNotifyCounter.Dec = 0 then begin
    if FChangedFlag.CheckFlagAndReset then begin
      DoChangeNotify;
    end else begin
      FPosition.StartNotify;
      FView.StartNotify;
    end;
  end else begin
    FPosition.StartNotify;
    FView.StartNotify;
  end;
end;

procedure TMapViewPortState.StopNotify;
begin
  inherited;
  FPosition.StopNotify;
  FView.StopNotify;
end;

end.
