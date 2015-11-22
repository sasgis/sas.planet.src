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

unit u_InfoTileProviderByBitmapTileProvider;

interface

uses
  Types,
  i_Projection,
  i_NotifierOperation,
  i_ContentTypeInfo,
  i_BitmapTileSaveLoad,
  i_TileInfoBasic,
  i_MapVersionInfo,
  i_InfoTileProvider,
  i_BitmapTileProvider,
  u_BaseInterfacedObject;

type
  TInfoTileProviderByBitmapTileProvider = class(TBaseInterfacedObject, IInfoTileProvider)
  private
    FProvider: IBitmapTileProvider;
    FContentType: IContentTypeInfoBitmap;
    FProjection: IProjection;
    FVersion: IMapVersionInfo;
    FSaver: IBitmapTileSaver;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): ITileInfoBasic;
  public
    constructor Create(
      const AProvider: IBitmapTileProvider;
      const AVersion: IMapVersionInfo;
      const ASaver: IBitmapTileSaver;
      const AContentType: IContentTypeInfoBitmap
    );
  end;

implementation

uses
  SysUtils,
  i_BinaryData,
  i_Bitmap32Static,
  u_TileInfoBasic;

{ TInfoTileProviderByBitmapTileProvider }

constructor TInfoTileProviderByBitmapTileProvider.Create(
  const AProvider: IBitmapTileProvider;
  const AVersion: IMapVersionInfo;
  const ASaver: IBitmapTileSaver;
  const AContentType: IContentTypeInfoBitmap
);
begin
  Assert(Assigned(AProvider));
  Assert(Assigned(AContentType));
  Assert(Assigned(AContentType.GetSaver));
  inherited Create;
  FProvider := AProvider;
  FVersion := AVersion;
  FSaver := ASaver;
  FContentType := AContentType;
  FProjection := FProvider.Projection;
  if not Assigned(FSaver) then begin
    FSaver := FContentType.GetSaver;
  end;
end;

function TInfoTileProviderByBitmapTileProvider.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TInfoTileProviderByBitmapTileProvider.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): ITileInfoBasic;
var
  VBitmapTile: IBitmap32Static;
  VBinary: IBinaryData;
begin
  Result := nil;
  VBitmapTile := FProvider.GetTile(AOperationID, ACancelNotifier, ATile);
  if Assigned(VBitmapTile) then begin
    VBinary := FSaver.Save(VBitmapTile);
    if Assigned(VBinary) then begin
      Result := TTileInfoBasicExistsWithTile.Create(Now, VBinary, FVersion, FContentType);
    end;
  end;
end;

end.
