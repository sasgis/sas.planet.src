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

unit u_VectorTileProviderByMatrix;

interface

uses
  Types,
  i_NotifierOperation,
  i_VectorItemSubset,
  i_Projection,
  i_VectorTileProvider,
  i_VectorTileMatrix,
  u_BaseInterfacedObject;

type
  TVectorTileProviderByMatrix = class(TBaseInterfacedObject, IVectorTileProvider)
  private
    FMatrix: IVectorTileMatrix;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const AMatrix: IVectorTileMatrix
    );
  end;

implementation

{ TVectorTileProviderByMatrix }

constructor TVectorTileProviderByMatrix.Create(
  const AMatrix: IVectorTileMatrix
);
begin
  Assert(Assigned(AMatrix));
  inherited Create;
  FMatrix := AMatrix;
end;

function TVectorTileProviderByMatrix.GetProjection: IProjection;
begin
  Result := FMatrix.TileRect.Projection;
end;

function TVectorTileProviderByMatrix.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IVectorItemSubset;
begin
  Result := FMatrix.GetElementByTile(ATile);
end;

end.
