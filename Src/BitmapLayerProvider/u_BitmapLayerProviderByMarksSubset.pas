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

unit u_BitmapLayerProviderByMarksSubset;

interface

uses
  Types,
  i_ProjectionInfo,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_VectorItemSubset,
  i_VectorTileProvider,
  i_VectorTileRenderer,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByMarksSubset = class(TBaseInterfacedObject, IBitmapTileUniProvider)
  private
    FProvider: IVectorTileUniProvider;
    FRenderer: IVectorTileRenderer;
  private
    function GetTile(AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjectionInfo;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const AProvider: IVectorTileUniProvider;
      const ARenderer: IVectorTileRenderer
    );
  end;

implementation

{ TMapMarksBitmapLayerProviderByMarksSubset }

constructor TBitmapLayerProviderByMarksSubset.Create(
  const AProvider: IVectorTileUniProvider;
  const ARenderer: IVectorTileRenderer
);
begin
  Assert(Assigned(AProvider));
  Assert(Assigned(ARenderer));
  inherited Create;
  FProvider := AProvider;
  FRenderer := ARenderer;
end;

function TBitmapLayerProviderByMarksSubset.GetTile(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint
): IBitmap32Static;
var
  VVectorTile: IVectorItemSubset;
begin
  Result := nil;
  VVectorTile := FProvider.GetTile(AOperationID, ACancelNotifier, AProjectionInfo, ATile);
  if Assigned(VVectorTile) and not VVectorTile.IsEmpty then begin
    Result :=
      FRenderer.RenderVectorTile(
        AOperationID,
        ACancelNotifier,
        AProjectionInfo,
        ATile,
        VVectorTile
      );
  end;
end;

end.

