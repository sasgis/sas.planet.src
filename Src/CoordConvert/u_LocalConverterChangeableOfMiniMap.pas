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

unit u_LocalConverterChangeableOfMiniMap;

interface

uses
  i_Listener,
  i_Notifier,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_LocalCoordConverterChangeable,
  i_MiniMapLayerConfig,
  u_BaseInterfacedObject;

type
  TLocalConverterChangeableOfMiniMap = class(TBaseInterfacedObject, ILocalCoordConverterChangeable)
  private
    FInternal: ILocalCoordConverterChangeableInternal;
    FSoruce: ILocalCoordConverterChangeable;
    FConfig: IMiniMapLayerLocationConfig;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FSourceListener: IListener;
    FConfigListener: IListener;
    procedure OnSourceChange;
    procedure OnConfigChange;
    function GetActualZoom(
      AZoomDelta: Integer;
      const AVisualCoordConverter: ILocalCoordConverter
    ): Byte;
    function GetConverterForSource(
      const AVisualCoordConverter: ILocalCoordConverter
    ): ILocalCoordConverter;
  private
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
    function GetStatic: ILocalCoordConverter;
  public
    constructor Create(
      const AChangeCounter: IInternalPerformanceCounter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const ASoruce: ILocalCoordConverterChangeable;
      const AConfig: IMiniMapLayerLocationConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_LocalCoordConverterChangeable,
  u_GeoFunc;

{ TLocalConverterChangeableOfMiniMap }

constructor TLocalConverterChangeableOfMiniMap.Create(
  const AChangeCounter: IInternalPerformanceCounter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ASoruce: ILocalCoordConverterChangeable;
  const AConfig: IMiniMapLayerLocationConfig
);
begin
  inherited Create;
  FSoruce := ASoruce;
  FConfig := AConfig;
  FConverterFactory := AConverterFactory;

  FInternal :=
    TLocalCoordConverterChangeable.Create(
      TSimpleFlagWithInterlock.Create,
      FSoruce.GetStatic,
      AChangeCounter
    );
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);
  FSoruce.ChangeNotifier.Add(FSourceListener);
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
end;

destructor TLocalConverterChangeableOfMiniMap.Destroy;
begin
  if Assigned(FSoruce) and Assigned(FSourceListener) then begin
    FSoruce.ChangeNotifier.Remove(FSourceListener);
    FSoruce := nil;
    FSourceListener := nil;
  end;
  if Assigned(FConfig) and Assigned(FConfigListener) then begin
    FConfig.ChangeNotifier.Remove(FConfigListener);
    FConfig := nil;
    FConfigListener := nil;
  end;
  inherited;
end;

function TLocalConverterChangeableOfMiniMap.GetActualZoom(
  AZoomDelta: Integer;
  const AVisualCoordConverter: ILocalCoordConverter
): Byte;
var
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
begin
  VZoom := AVisualCoordConverter.GetZoom;
  VGeoConvert := AVisualCoordConverter.GetGeoConverter;
  if AZoomDelta = 0 then begin
    Result := VZoom;
  end else if AZoomDelta > 0 then begin
    if VZoom > AZoomDelta then begin
      Result := VZoom - AZoomDelta;
    end else begin
      Result := 0;
    end;
  end else begin
    Result := VZoom - AZoomDelta;
    VGeoConvert.ValidateZoom(Result);
  end;
end;

function TLocalConverterChangeableOfMiniMap.GetAfterChangeNotifier: INotifier;
begin
  Result := FInternal.AfterChangeNotifier;
end;

function TLocalConverterChangeableOfMiniMap.GetBeforeChangeNotifier: INotifier;
begin
  Result := FInternal.BeforeChangeNotifier;
end;

function TLocalConverterChangeableOfMiniMap.GetChangeNotifier: INotifier;
begin
  Result := FInternal.ChangeNotifier;
end;

function TLocalConverterChangeableOfMiniMap.GetConverterForSource(
  const AVisualCoordConverter: ILocalCoordConverter
): ILocalCoordConverter;
var
  VConfig: IMiniMapLayerLocationConfigStatic;
  VVisualMapCenter: TDoublePoint;
  VZoom: Byte;
  VSourceZoom: Byte;
  VConverter: ICoordConverter;
  VVisualMapCenterInRelative: TDoublePoint;
  VVisualMapCenterInLayerMap: TDoublePoint;
  VMapPixelAtLocalZero: TDoublePoint;
  VLayerSize: TPoint;
  VVeiwSize: TPoint;
  VWidth: Integer;
  VBottomMargin: Integer;
  VLocalRect: TRect;
  VScale: Double;
begin
  Result := nil;
  VConfig := FConfig.GetStatic;
  if VConfig.Visible then begin
    VWidth := VConfig.Width;
    VBottomMargin := VConfig.BottomMargin;
    VVeiwSize := AVisualCoordConverter.GetLocalRectSize;
    VLayerSize := Point(VWidth, VWidth);
    VLocalRect.Right := VVeiwSize.X;
    VLocalRect.Bottom := VVeiwSize.Y - VBottomMargin;
    VLocalRect.Left := VLocalRect.Right - VLayerSize.X;
    VLocalRect.Top := VLocalRect.Bottom - VLayerSize.Y;

    VScale := AVisualCoordConverter.GetScale;
    VVisualMapCenter := AVisualCoordConverter.GetCenterMapPixelFloat;
    VSourceZoom := AVisualCoordConverter.GetZoom;
    VConverter := AVisualCoordConverter.GetGeoConverter;
    VConverter.ValidatePixelPosFloatStrict(VVisualMapCenter, VSourceZoom, True);
    VVisualMapCenterInRelative := VConverter.PixelPosFloat2Relative(VVisualMapCenter, VSourceZoom);
    VZoom := GetActualZoom(VConfig.ZoomDelta, AVisualCoordConverter);
    VVisualMapCenterInLayerMap := VConverter.Relative2PixelPosFloat(VVisualMapCenterInRelative, VZoom);
    VMapPixelAtLocalZero :=
      DoublePoint(
        VVisualMapCenterInLayerMap.X - (VLocalRect.Left + VLayerSize.X / 2) / VScale,
        VVisualMapCenterInLayerMap.Y - (VLocalRect.Top + VLayerSize.Y / 2) / VScale
      );

    Result :=
      FConverterFactory.CreateConverter(
        VLocalRect,
        VZoom,
        VConverter,
        VScale,
        VMapPixelAtLocalZero
      );
  end;
end;

function TLocalConverterChangeableOfMiniMap.GetStatic: ILocalCoordConverter;
begin
  Result := FInternal.GetStatic;
end;

procedure TLocalConverterChangeableOfMiniMap.OnConfigChange;
begin
  FInternal.SetConverter(GetConverterForSource(FSoruce.GetStatic));
end;

procedure TLocalConverterChangeableOfMiniMap.OnSourceChange;
begin
  FInternal.SetConverter(GetConverterForSource(FSoruce.GetStatic));
end;

end.
