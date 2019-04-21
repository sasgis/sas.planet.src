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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
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
  i_ListenerNotifierLinksList,
  i_VectorTileRenderer,
  i_VectorTileRendererChangeable,
  u_ChangeableBase;

type
  TVectorTileRendererChangeableForVectorMaps = class(TChangeableWithSimpleLockBase, IVectorTileRendererChangeable)
  private
    FConfig: IVectorItemDrawConfig;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FPointMarker: IBitmapMarker;
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
      const APointMarker: IBitmapMarker;
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
  const APointMarker: IBitmapMarker;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProjectedProvider: IGeometryProjectedProvider;
  const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
);
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(APointMarker));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProjectedProvider));
  Assert(Assigned(AMarkerIconProvider));
  inherited Create;
  FConfig := AConfig;
  FPointMarker := APointMarker;
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
      FPointMarker,
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
