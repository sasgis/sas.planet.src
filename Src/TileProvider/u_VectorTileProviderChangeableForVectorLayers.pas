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

unit u_VectorTileProviderChangeableForVectorLayers;

interface

uses
  Types,
  SysUtils,
  i_TileError,
  i_VectorTileProvider,
  i_VectorTileProviderChangeable,
  i_UseTilePrevZoomConfig,
  i_MapTypeSet,
  i_MapTypeSetChangeable,
  i_ListenerNotifierLinksList,
  i_VectorItemSubsetBuilder,
  u_ChangeableBase;

type
  TVectorTileProviderChangeableForVectorLayers = class(TChangeableWithSimpleLockBase, IVectorTileUniProviderChangeable)
  private
    FErrorLogger: ITileErrorLogger;
    FSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FLayesSet: IMapTypeSetChangeable;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FTileSelectOversize: TRect;
    FItemSelectOversize: TRect;

    FLinksList: IListenerNotifierLinksList;

    FResult: IVectorTileUniProvider;

    procedure OnLayerListChange;
  private
    function GetStatic: IVectorTileUniProvider;
  public
    constructor Create(
      const ALayesSet: IMapTypeSetChangeable;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AErrorLogger: ITileErrorLogger;
      const ATileSelectOversize: TRect;
      const AItemSelectOversize: TRect
    );
  end;

implementation

uses
  i_Listener,
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_VectorTileProviderForVectorLayers;

{ TVectorLayerProviderChangeableForMainLayer }

constructor TVectorTileProviderChangeableForVectorLayers.Create(
  const ALayesSet: IMapTypeSetChangeable;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AErrorLogger: ITileErrorLogger;
  const ATileSelectOversize: TRect;
  const AItemSelectOversize: TRect
);
var
  VListener: IListener;
begin
  Assert(Assigned(ALayesSet));
  Assert(Assigned(AUseTilePrevZoomConfig));
  Assert(Assigned(ASubsetBuilderFactory));
  inherited Create;
  FSubsetBuilderFactory := ASubsetBuilderFactory;
  FLayesSet := ALayesSet;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;
  FErrorLogger := AErrorLogger;
  FTileSelectOversize := ATileSelectOversize;
  FItemSelectOversize := AItemSelectOversize;

  FLinksList := TListenerNotifierLinksList.Create;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnLayerListChange);

  FLinksList.Add(VListener, FLayesSet.ChangeNotifier);
  FLinksList.Add(VListener, FUseTilePrevZoomConfig.ChangeNotifier);
  FLinksList.ActivateLinks;
  OnLayerListChange;
end;

function TVectorTileProviderChangeableForVectorLayers.GetStatic: IVectorTileUniProvider;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TVectorTileProviderChangeableForVectorLayers.OnLayerListChange;
var
  VLayers: IMapTypeSet;
  VResult: IVectorTileUniProvider;
begin
  VLayers := FLayesSet.GetStatic;
  VResult := nil;
  if Assigned(VLayers) then begin
    VResult :=
      TVectorTileProviderForVectorLayers.Create(
        FSubsetBuilderFactory,
        VLayers,
        FUseTilePrevZoomConfig.UsePrevZoomAtVectorLayer,
        True,
        FErrorLogger,
        FTileSelectOversize,
        FItemSelectOversize
      );
  end;

  CS.BeginWrite;
  try
    FResult := VResult;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
