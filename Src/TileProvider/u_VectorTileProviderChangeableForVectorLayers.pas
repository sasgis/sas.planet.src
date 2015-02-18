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
  i_MapTypeSet,
  i_MapTypeSetChangeable,
  i_ListenerNotifierLinksList,
  i_VectorItemSubsetBuilder,
  u_ChangeableBase;

type
  TVectorTileProviderChangeableForVectorLayers = class(TChangeableBase, IVectorTileUniProviderChangeable)
  private
    FErrorLogger: ITileErrorLogger;
    FSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FLayesSet: IMapTypeSetChangeable;
    FTileSelectOversize: TRect;
    FItemSelectOversize: TRect;

    FLinksList: IListenerNotifierLinksList;

    FResult: IVectorTileUniProvider;
    FResultCS: IReadWriteSync;

    procedure OnLayerListChange;
  private
    function GetStatic: IVectorTileUniProvider;
  public
    constructor Create(
      const ALayesSet: IMapTypeSetChangeable;
      const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AErrorLogger: ITileErrorLogger;
      const ATileSelectOversize: TRect;
      const AItemSelectOversize: TRect
    );
  end;

implementation

uses
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_VectorTileProviderForVectorLayers,
  u_Synchronizer;

{ TVectorLayerProviderChangeableForMainLayer }

constructor TVectorTileProviderChangeableForVectorLayers.Create(
  const ALayesSet: IMapTypeSetChangeable;
  const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AErrorLogger: ITileErrorLogger;
  const ATileSelectOversize: TRect;
  const AItemSelectOversize: TRect
);
begin
  Assert(Assigned(ALayesSet));
  Assert(Assigned(ASubsetBuilderFactory));
  inherited Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifiers'));
  FSubsetBuilderFactory := ASubsetBuilderFactory;
  FLayesSet := ALayesSet;
  FErrorLogger := AErrorLogger;
  FTileSelectOversize := ATileSelectOversize;
  FItemSelectOversize := AItemSelectOversize;

  FResultCS := GSync.SyncVariable.Make(Self.ClassName);
  FLinksList := TListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerListChange),
    FLayesSet.ChangeNotifier
  );
  FLinksList.ActivateLinks;
  OnLayerListChange;
end;

function TVectorTileProviderChangeableForVectorLayers.GetStatic: IVectorTileUniProvider;
begin
  FResultCS.BeginRead;
  try
    Result := FResult;
  finally
    FResultCS.EndRead;
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
        True,
        FErrorLogger,
        FTileSelectOversize,
        FItemSelectOversize
      );
  end;

  FResultCS.BeginWrite;
  try
    FResult := VResult;
  finally
    FResultCS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
