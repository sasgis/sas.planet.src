{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_InfoTileProviderByStorage;

interface

uses
  Types,
  SysUtils,
  i_NotifierOperation,
  i_TileInfoBasic,
  i_MapVersionRequest,
  i_Projection,
  i_InfoTileProvider,
  i_TileStorage,
  u_BaseInterfacedObject;

type
  TInfoTileProviderByStorage = class(TBaseInterfacedObject, IInfoTileProvider)
  private
    FProjection: IProjection;
    FVersion: IMapVersionRequest;
    FStorage: ITileStorage;
    FIsIgnoreError: Boolean;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): ITileInfoBasic;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AVersionConfig: IMapVersionRequest;
      const AProjection: IProjection;
      const AStorage: ITileStorage
    );
  end;

implementation

{ TInfoTileProviderByStorage }

constructor TInfoTileProviderByStorage.Create(
  const AIsIgnoreError: Boolean;
  const AVersionConfig: IMapVersionRequest;
  const AProjection: IProjection;
  const AStorage: ITileStorage
);
begin
  Assert(Assigned(AVersionConfig));
  Assert(Assigned(AStorage));
  Assert(Assigned(AProjection));
  Assert(AStorage.ProjectionSet.IsProjectionFromThisSet(AProjection));
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FStorage := AStorage;
  FProjection := AProjection;
  FVersion := AVersionConfig;
end;

function TInfoTileProviderByStorage.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TInfoTileProviderByStorage.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): ITileInfoBasic;
var
  VZoom: Byte;
begin
  Result := nil;
  try
    VZoom := FProjection.Zoom;
    Result := FStorage.GetTileInfoEx(ATile, VZoom, FVersion, gtimWithData);
  except
    if not FIsIgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
end;

end.
