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

unit u_BitmapTileProviderByVectorTileProvider;

interface

uses
  Types,
  i_Projection,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_VectorItemSubset,
  i_VectorTileProvider,
  i_VectorTileRenderer,
  i_BitmapTileProvider,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderByVectorTileProvider = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FProjection: IProjection;
    FProvider: IVectorTileUniProvider;
    FRenderer: IVectorTileRenderer;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const AProjection: IProjection;
      const AProvider: IVectorTileUniProvider;
      const ARenderer: IVectorTileRenderer
    );
  end;

implementation

{ TBitmapTileProviderByVectorTileProvider }

constructor TBitmapTileProviderByVectorTileProvider.Create(
  const AProjection: IProjection;
  const AProvider: IVectorTileUniProvider;
  const ARenderer: IVectorTileRenderer
);
begin
  Assert(Assigned(AProjection));
  Assert(Assigned(AProvider));
  Assert(Assigned(ARenderer));
  inherited Create;
  FProjection := AProjection;
  FProvider := AProvider;
  FRenderer := ARenderer;
end;

function TBitmapTileProviderByVectorTileProvider.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TBitmapTileProviderByVectorTileProvider.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VVectorTile: IVectorItemSubset;
begin
  Result := nil;
  VVectorTile := FProvider.GetTile(AOperationID, ACancelNotifier, FProjection, ATile);
  if Assigned(VVectorTile) and not VVectorTile.IsEmpty then begin
    Result :=
      FRenderer.RenderVectorTile(
        AOperationID,
        ACancelNotifier,
        FProjection,
        ATile,
        VVectorTile
      );
  end;
end;

end.
