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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_EnumTileInfoByBerkeleyDB;

interface

uses
  i_TileInfoBasic,
  i_TileStorage,
  i_MapVersionInfo,
  i_FileNameIterator,
  i_MapVersionConfig,
  i_TileFileNameParser,
  i_TileStorageBerkeleyDBHelper,
  u_BaseInterfacedObject;

type
  TEnumTileInfoByBerkeleyDB = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FIgnoreMultiVersionTiles: Boolean;
    FFilesIterator: IFileNameIterator;
    FTileFileNameParser: ITileFileNameParser;
    FEmptyVersionInfo: IMapVersionInfo;
    FStorage: ITileStorage;
    FHelper: ITileStorageBerkeleyDBHelper;
    FCurFileTilesArray: TPointArray;
    FCurFileIndex: Integer;
    FCurFileZoom: Byte;
    FCurMapVersionList: IMapVersionListStatic;
    FCurMapVersionIndex: Integer;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AIgnoreMultiVersionTiles: Boolean;
      const AFilesIterator: IFileNameIterator;
      const ATileFileNameParser: ITileFileNameParser;
      const AMapVersionFactory: IMapVersionFactory;
      const AStorage: ITileStorage;
      const AHelper: ITileStorageBerkeleyDBHelper
    );
  end;

implementation

uses
  Types,
  SysUtils,
  i_BinaryData;

{ TEnumTileInfoByBerkeleyDB }

constructor TEnumTileInfoByBerkeleyDB.Create(
  const AIgnoreMultiVersionTiles: Boolean;
  const AFilesIterator: IFileNameIterator;
  const ATileFileNameParser: ITileFileNameParser;
  const AMapVersionFactory: IMapVersionFactory;
  const AStorage: ITileStorage;
  const AHelper: ITileStorageBerkeleyDBHelper
);
begin
  inherited Create;
  FIgnoreMultiVersionTiles := AIgnoreMultiVersionTiles;
  FFilesIterator := AFilesIterator;
  FTileFileNameParser := ATileFileNameParser;
  FEmptyVersionInfo := AMapVersionFactory.CreateByStoreString('', True);
  FStorage := AStorage;
  FHelper := AHelper;
  FCurFileIndex := 0;
  SetLength(FCurFileTilesArray, 0);
  FCurMapVersionList := nil;
  FCurMapVersionIndex := 0;
end;

function TEnumTileInfoByBerkeleyDB.Next(var ATileInfo: TTileInfo): Boolean;
var
  VTileFileName: string;
  VTileFileNameW: WideString;
  VTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VData: IBinaryData;
  VTileXY: TPoint;
  VVersionInfo: IMapVersionInfo;
begin
  Result := False;
  while FCurFileIndex >= 0 do begin
    if FCurFileIndex < Length(FCurFileTilesArray) then begin
      ATileInfo.FZoom := FCurFileZoom;
      ATileInfo.FTile := FCurFileTilesArray[FCurFileIndex];
      if not FIgnoreMultiVersionTiles then begin
        if not Assigned(FCurMapVersionList) then begin
          // get new list of versions for tile
          FCurMapVersionList := FStorage.GetListOfTileVersions(ATileInfo.FTile, FCurFileZoom, nil);
          FCurMapVersionIndex := 0;
        end;
        if Assigned(FCurMapVersionList) and (FCurMapVersionIndex < FCurMapVersionList.Count) then begin
          // process tile with version
          VVersionInfo := FCurMapVersionList.Item[FCurMapVersionIndex];
          VTileInfo := FStorage.GetTileInfo(ATileInfo.FTile, FCurFileZoom, VVersionInfo, gtimWithData);
          // prepare process for next version of same tile
          Inc(FCurMapVersionIndex);
        end else begin
          // process tile without version
          VTileInfo := FStorage.GetTileInfo(ATileInfo.FTile, FCurFileZoom, nil, gtimWithData);
          // prepare process for next tile
          Inc(FCurFileIndex);
          FCurMapVersionList := nil;
        end;
      end else begin
        VTileInfo := FStorage.GetTileInfo(ATileInfo.FTile, FCurFileZoom, FEmptyVersionInfo, gtimWithData);
        Inc(FCurFileIndex);
      end;
      if Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
        VData := VTileInfoWithData.TileData;
      end else begin
        VData := nil;
      end;
      ATileInfo.FLoadDate := VTileInfo.LoadDate;
      ATileInfo.FVersionInfo := VTileInfo.VersionInfo;
      ATileInfo.FContentType := VTileInfo.ContentType;
      ATileInfo.FData := VData;
      ATileInfo.FSize := VTileInfo.Size;
      if VTileInfo.IsExists then begin
        ATileInfo.FInfoType := titExists;
        Result := True;
        Break;
      end else if VTileInfo.IsExistsTNE then begin
        ATileInfo.FInfoType := titTneExists;
        Result := True;
        Break;
      end else begin
        ATileInfo.FInfoType := titNotExists;
      end;
    end else begin
      if FFilesIterator.Next(VTileFileNameW) then begin
        // start process new cache file
        VTileFileName := FFilesIterator.GetRootFolderName + VTileFileNameW;
        if FTileFileNameParser.GetTilePoint(VTileFileName, VTileXY, FCurFileZoom) and
          // get new array of tiles
          FHelper.GetTileExistsArray(VTileFileName, FCurFileZoom, nil, FCurFileTilesArray) then begin
          FCurFileIndex := 0;
        end else begin
          // skip file - tile name parser error
          FCurFileIndex := Length(FCurFileTilesArray);
        end;
      end else begin
        // fin enum - no any files found
        FCurFileIndex := -1;
      end;
    end;
  end;
end;

end.
