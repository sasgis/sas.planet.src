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

unit u_BitmapTileProviderByMatrix;

interface

uses
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_ProjectionInfo,
  i_BitmapTileProvider,
  i_BitmapTileMatrix,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderByMatrix = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FMatrix: IBitmapTileMatrix;
  private
    function GetProjectionInfo: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const AMatrix: IBitmapTileMatrix
    );
  end;

implementation

{ TBitmapTileProviderByMatrix }

constructor TBitmapTileProviderByMatrix.Create(
  const AMatrix: IBitmapTileMatrix
);
begin
  Assert(Assigned(AMatrix));
  inherited Create;
  FMatrix := AMatrix;
end;

function TBitmapTileProviderByMatrix.GetProjectionInfo: IProjection;
begin
  Result := FMatrix.TileRect.ProjectionInfo;
end;

function TBitmapTileProviderByMatrix.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
begin
  Result := FMatrix.GetElementByTile(ATile);
end;

end.
