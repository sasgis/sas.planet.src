{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_VectorTileRendererChangeableForVectorMaps;

interface

uses
  SysUtils,
  i_VectorItemDrawConfig,
  i_Bitmap32BufferFactory,
  i_GeometryProjectedProvider,
  i_MarkerProviderByAppearancePointIcon,
  i_BitmapMarker,
  i_MarkerDrawable,
  i_ListenerNotifierLinksList,
  i_VectorTileRenderer,
  i_VectorTileRendererChangeable,
  u_ChangeableBase;

type
  TVectorTileRendererChangeableForVectorMaps = class(TChangeableWithSimpleLockBase, IVectorTileRendererChangeable)
  private
    FConfig: IVectorItemDrawConfig;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FPointBitmapMarker: IBitmapMarker;
    FPointMarkerDrawable: IMarkerDrawableChangeable;
    FProjectedProvider: IGeometryProjectedProvider;
    FMarkerIconProvider: IMarkerProviderByAppearancePointIcon;

    FLinksList: IListenerNotifierLinksList;
    FResult: IVectorTileRenderer;
    procedure OnConfigChange;
  private
    function GetStatic: IVectorTileRenderer;
  public
    constructor Create(
      const AConfig: IVectorItemDrawConfig;
      const APointBitmapMarker: IBitmapMarker;
      const APointMarkerDrawable: IMarkerDrawableChangeable;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProjectedProvider: IGeometryProjectedProvider;
      const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
    );
  end;

implementation

uses
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_VectorTileRenderer;

{ TVectorTileRendererChangeableForVectorMaps }

constructor TVectorTileRendererChangeableForVectorMaps.Create(
  const AConfig: IVectorItemDrawConfig;
  const APointBitmapMarker: IBitmapMarker;
  const APointMarkerDrawable: IMarkerDrawableChangeable;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProjectedProvider: IGeometryProjectedProvider;
  const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
);
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(APointBitmapMarker) or Assigned(APointMarkerDrawable));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProjectedProvider));
  Assert(Assigned(AMarkerIconProvider));
  inherited Create;
  FConfig := AConfig;
  FPointBitmapMarker := APointBitmapMarker;
  FPointMarkerDrawable := APointMarkerDrawable;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProjectedProvider := AProjectedProvider;
  FMarkerIconProvider := AMarkerIconProvider;

  FLinksList := TListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );

  FLinksList.ActivateLinks;
  OnConfigChange;
end;

function TVectorTileRendererChangeableForVectorMaps.GetStatic: IVectorTileRenderer;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TVectorTileRendererChangeableForVectorMaps.OnConfigChange;
var
  VResult: IVectorTileRenderer;
  VConfig: IVectorItemDrawConfigStatic;
begin
  VConfig := FConfig.GetStatic;

  VResult :=
    TVectorTileRenderer.Create(
      VConfig.MainColor,
      VConfig.ShadowColor,
      FPointBitmapMarker,
      FPointMarkerDrawable,
      FBitmap32StaticFactory,
      FProjectedProvider,
      FMarkerIconProvider
    );

  CS.BeginWrite;
  try
    FResult := VResult;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
