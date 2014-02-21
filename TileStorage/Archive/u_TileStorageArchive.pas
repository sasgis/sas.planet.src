{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_TileStorageArchive;

interface

uses
  Types,
  SysUtils,
  i_BinaryData,
  i_TileStorage,
  i_StorageState,
  i_TileInfoBasic,
  i_CoordConverter,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_MapVersionListStatic,
  i_ContentTypeInfo,
  i_ArchiveReadWrite,
  i_ContentTypeManager,
  i_TileFileNameParser,
  i_TileFileNameGenerator,
  i_TileStorageAbilities,
  i_NotifierTilePyramidUpdate,
  u_BaseInterfacedObject;

type
  TTileStorageArchive = class(TBaseInterfacedObject, ITileStorage)
  protected
    FSync: IReadWriteSync;
    FWriter: IArchiveWriter;
    FArchiveFileName: string;
    FContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FTileNotifier: INotifierTilePyramidUpdate;
    FState: IStorageStateChangeble;
    FCoordConverter: ICoordConverter;
    FTileNameParser: ITileFileNameParser;
    FTileNameGenerator: ITileFileNameGenerator;
  protected
    function GetArchiveWriter: IArchiveWriter; virtual;
  protected
    { ITileStorage }
    function GetStorageTypeAbilities: ITileStorageTypeAbilities;
    function GetTileNotifier: INotifierTilePyramidUpdate;
    function GetState: IStorageStateChangeble;
    function GetCoordConverter: ICoordConverter;
    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): string;
    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;
    function GetTileInfoEx(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;
    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): Boolean;
    function SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): IMapVersionListStatic;
    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): ITileRectInfo;
    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo; virtual; abstract;
  public
    constructor Create(
      const AArchiveFileName: string;
      const AContentType: IContentTypeInfoBasic;
      const AContentTypeManager: IContentTypeManager;
      const ACoordConverter: ICoordConverter;
      const ATileNameParser: ITileFileNameParser;
      const ATileNameGenerator: ITileFileNameGenerator
    );
  end;

implementation

uses
  u_Synchronizer,
  u_BinaryData;

{ TTileStorageArchive }

constructor TTileStorageArchive.Create(
  const AArchiveFileName: string;
  const AContentType: IContentTypeInfoBasic;
  const AContentTypeManager: IContentTypeManager;
  const ACoordConverter: ICoordConverter;
  const ATileNameParser: ITileFileNameParser;
  const ATileNameGenerator: ITileFileNameGenerator
);
begin
  Assert(AArchiveFileName <> '');
  inherited Create;
  FArchiveFileName := AArchiveFileName;
  FContentType := AContentType;
  FContentTypeManager := AContentTypeManager;
  FTileNotifier := nil;
  FState := nil;
  FCoordConverter := ACoordConverter;
  FTileNameParser := ATileNameParser;
  FTileNameGenerator := ATileNameGenerator;
  FSync := MakeSyncRW_Big(Self, False); 
end;

function TTileStorageArchive.GetArchiveWriter: IArchiveWriter;
begin
  Result := nil;
end;

function TTileStorageArchive.GetTileNotifier: INotifierTilePyramidUpdate;
begin
  Result := FTileNotifier;
end;

function TTileStorageArchive.GetState: IStorageStateChangeble;
begin
  Result := FState;
end;

function TTileStorageArchive.GetStorageTypeAbilities: ITileStorageTypeAbilities;
begin
  Result := nil;
end;

function TTileStorageArchive.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TTileStorageArchive.GetTileFileName(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo
): string;
begin
  Result := FArchiveFileName + PathDelim + FTileNameGenerator.GetTileFileName(AXY, AZoom) + FContentType.GetDefaultExt;
end;

function TTileStorageArchive.GetTileInfo(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
begin
  Result := nil;
end;

function TTileStorageArchive.GetTileInfoEx(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
begin
  Result := nil;
end;

function TTileStorageArchive.DeleteTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo
): Boolean;
begin
  Result := False;
end;

function TTileStorageArchive.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData;
  const AIsOverwrite: Boolean
): Boolean;
var
  VTileName: string;
  VData: IBinaryData;
  VArchiveWriter: IArchiveWriter;
begin
  Result := False;
  FSync.BeginWrite;
  try
    VArchiveWriter := GetArchiveWriter;
    if Assigned(VArchiveWriter) then begin
      if Assigned(AContentType) and Assigned(AData) then begin
        VData := AData;
        VTileName := FTileNameGenerator.GetTileFileName(AXY, AZoom) + FContentType.GetDefaultExt;
      end else begin
        VData := TBinaryData.Create(0, nil);
        VTileName := FTileNameGenerator.GetTileFileName(AXY, AZoom) + '.tne';
      end;
      VArchiveWriter.AddFile(VData, VTileName, ALoadDate);
      Result := True;
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TTileStorageArchive.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): IMapVersionListStatic;
begin
  Result := nil;
end;

function TTileStorageArchive.GetTileRectInfo(
  const ARect: TRect;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): ITileRectInfo;
begin
  Result := nil;
end;

end.
