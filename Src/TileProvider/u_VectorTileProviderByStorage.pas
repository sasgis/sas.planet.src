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

unit u_VectorTileProviderByStorage;

interface

uses
  Types,
  SysUtils,
  i_NotifierOperation,
  i_MapVersionRequest,
  i_VectorItemSubset,
  i_Projection,
  i_VectorTileProvider,
  i_VectorDataLoader,
  i_VectorDataFactory,
  i_TileStorage,
  u_BaseInterfacedObject;

type
  TVectorTileProviderByStorage = class(TBaseInterfacedObject, IVectorTileProvider)
  private
    FProjection: IProjection;
    FVersion: IMapVersionRequest;
    FLoaderFromStorage: IVectorDataLoader;
    FStorage: ITileStorage;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FIsIgnoreError: Boolean;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVersionConfig: IMapVersionRequest;
      const ALoaderFromStorage: IVectorDataLoader;
      const AProjection: IProjection;
      const AStorage: ITileStorage
    );
  end;

implementation

uses
  i_TileInfoBasic;

{ TVectorTileProviderByStorage }

constructor TVectorTileProviderByStorage.Create(
  const AIsIgnoreError: Boolean;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVersionConfig: IMapVersionRequest;
  const ALoaderFromStorage: IVectorDataLoader;
  const AProjection: IProjection;
  const AStorage: ITileStorage
);
begin
  Assert(AVectorDataItemMainInfoFactory <> nil);
  Assert(AVersionConfig <> nil);
  Assert(ALoaderFromStorage <> nil);
  Assert(AStorage <> nil);
  Assert(AProjection <> nil);
  Assert(AStorage.ProjectionSet.IsProjectionFromThisSet(AProjection));
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FStorage := AStorage;
  FProjection := AProjection;
  FVersion := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
end;

function TVectorTileProviderByStorage.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TVectorTileProviderByStorage.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IVectorItemSubset;
var
  VTileInfo: ITileInfoWithData;
  VZoom: Byte;
begin
  Result := nil;
  try
    VZoom := FProjection.Zoom;
    if Supports(FStorage.GetTileInfoEx(ATile, VZoom, FVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
      Result := FLoaderFromStorage.Load(VTileInfo.TileData, nil, FVectorDataItemMainInfoFactory);
    end;
  except
    if not FIsIgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
end;

end.
