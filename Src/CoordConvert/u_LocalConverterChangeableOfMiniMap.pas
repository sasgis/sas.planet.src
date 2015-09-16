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
  i_Notifier,
  i_InternalPerformanceCounter,
  i_ProjectionInfo,
  i_ProjectionSet,
  i_ProjectionSetChangeable,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_LocalCoordConverterChangeable,
  i_MiniMapLayerConfig,
  i_ListenerNotifierLinksList,
  u_BaseInterfacedObject;

type
  TLocalConverterChangeableOfMiniMap = class(TBaseInterfacedObject, ILocalCoordConverterChangeable)
  private
    FInternal: ILocalCoordConverterChangeableInternal;
    FSoruce: ILocalCoordConverterChangeable;
    FConfig: IMiniMapLayerLocationConfig;
    FProjectionSet: IProjectionSetChangeable;
    FConverterFactory: ILocalCoordConverterFactorySimpe;

    FLinkList: IListenerNotifierLinksList;

    procedure OnConfigChange;
    function GetActualProjection(
      AZoomDelta: Integer;
      const AProjectionSet: IProjectionSet;
      const AProjection: IProjection
    ): IProjection;
    function GetConverterForSource(
      const AConfig: IMiniMapLayerLocationConfigStatic;
      const AProjectionSet: IProjectionSet;
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
      const AProjectionSet: IProjectionSetChangeable;
      const ASoruce: ILocalCoordConverterChangeable;
      const AConfig: IMiniMapLayerLocationConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  t_GeoTypes,
  i_Listener,
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_LocalCoordConverterChangeable,
  u_GeoFunc;

{ TLocalConverterChangeableOfMiniMap }

constructor TLocalConverterChangeableOfMiniMap.Create(
  const AChangeCounter: IInternalPerformanceCounter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionSet: IProjectionSetChangeable;
  const ASoruce: ILocalCoordConverterChangeable;
  const AConfig: IMiniMapLayerLocationConfig
);
var
  VListener: IListener;
begin
  inherited Create;
  FSoruce := ASoruce;
  FProjectionSet := AProjectionSet;
  FConfig := AConfig;
  FConverterFactory := AConverterFactory;

  FLinkList := TListenerNotifierLinksList.Create;
  FInternal :=
    TLocalCoordConverterChangeable.Create(
      FSoruce.GetStatic,
      AChangeCounter
    );
  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FLinkList.Add(VListener, FSoruce.ChangeNotifier);
  FLinkList.Add(VListener, FConfig.ChangeNotifier);
  FLinkList.Add(VListener, FProjectionSet.ChangeNotifier);
  FLinkList.ActivateLinks;
  OnConfigChange;
end;

destructor TLocalConverterChangeableOfMiniMap.Destroy;
begin
  FLinkList.DeactivateLinks;
  inherited;
end;

function TLocalConverterChangeableOfMiniMap.GetActualProjection(
  AZoomDelta: Integer;
  const AProjectionSet: IProjectionSet;
  const AProjection: IProjection
): IProjection;
var
  VZoom: Byte;
  VProjection: IProjection;
  VResultZoom: Byte;
begin
  VProjection := AProjectionSet.GetSuitableProjection(AProjection);
  if AZoomDelta = 0 then begin
    Result := VProjection;
  end else if AZoomDelta > 0 then begin
    VZoom := VProjection.Zoom;
    if VZoom > AZoomDelta then begin
      VResultZoom := VZoom - AZoomDelta;
    end else begin
      VResultZoom := 0;
    end;
    Result := AProjectionSet.Zooms[VResultZoom];
  end else begin
    VZoom := VProjection.Zoom;
    VResultZoom := VZoom - AZoomDelta;
    AProjectionSet.ValidateZoom(VResultZoom);
    Result := AProjectionSet.Zooms[VResultZoom];
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
  const AConfig: IMiniMapLayerLocationConfigStatic;
  const AProjectionSet: IProjectionSet;
  const AVisualCoordConverter: ILocalCoordConverter
): ILocalCoordConverter;
var
  VVisualMapCenter: TDoublePoint;
  VProjection: IProjection;
  VSourceProjection: IProjection;
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
  if AConfig.Visible then begin
    VWidth := AConfig.Width;
    VBottomMargin := AConfig.BottomMargin;
    VVeiwSize := AVisualCoordConverter.GetLocalRectSize;
    VLayerSize := Point(VWidth, VWidth);
    VLocalRect.Right := VVeiwSize.X;
    VLocalRect.Bottom := VVeiwSize.Y - VBottomMargin;
    VLocalRect.Left := VLocalRect.Right - VLayerSize.X;
    VLocalRect.Top := VLocalRect.Bottom - VLayerSize.Y;

    VScale := AVisualCoordConverter.GetScale;
    VVisualMapCenter := AVisualCoordConverter.GetCenterMapPixelFloat;
    VSourceProjection := AVisualCoordConverter.ProjectionInfo;
    VSourceProjection.ValidatePixelPosFloatStrict(VVisualMapCenter, True);
    VVisualMapCenterInRelative := VSourceProjection.PixelPosFloat2Relative(VVisualMapCenter);
    VProjection := GetActualProjection(AConfig.ZoomDelta, AProjectionSet, VSourceProjection);
    VVisualMapCenterInLayerMap := VProjection.Relative2PixelPosFloat(VVisualMapCenterInRelative);
    VMapPixelAtLocalZero :=
      DoublePoint(
        VVisualMapCenterInLayerMap.X - (VLocalRect.Left + VLayerSize.X / 2) / VScale,
        VVisualMapCenterInLayerMap.Y - (VLocalRect.Top + VLayerSize.Y / 2) / VScale
      );

    Result :=
      FConverterFactory.CreateConverter(
        VLocalRect,
        VProjection,
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
var
  VNewConverter: ILocalCoordConverter;
begin
  VNewConverter :=
    GetConverterForSource(
      FConfig.GetStatic,
      FProjectionSet.GetStatic,
      FSoruce.GetStatic
    );
  FInternal.SetConverter(VNewConverter);
end;

end.
