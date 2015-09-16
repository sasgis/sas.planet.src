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

unit u_BitmapTileProviderByBitmapTileUniProvider;

interface

uses
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_ProjectionInfo,
  i_BitmapTileProvider,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderByBitmapTileUniProvider = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FProjection: IProjection;
    FSource: IBitmapTileUniProvider;
  private
    function GetProjectionInfo: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const AProjection: IProjection;
      const ASource: IBitmapTileUniProvider
    );
  end;

implementation

{ TBitmapTileProviderByBitmapUniTileProvider }

constructor TBitmapTileProviderByBitmapTileUniProvider.Create(
  const AProjection: IProjection;
  const ASource: IBitmapTileUniProvider
);
begin
  Assert(Assigned(AProjection));
  Assert(Assigned(ASource));
  inherited Create;
  FProjection := AProjection;
  FSource := ASource;
end;

function TBitmapTileProviderByBitmapTileUniProvider.GetProjectionInfo: IProjection;
begin
  Result := FProjection;
end;

function TBitmapTileProviderByBitmapTileUniProvider.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
begin
  Result := FSource.GetTile(AOperationID, ACancelNotifier, FProjection, ATile);
end;

end.
