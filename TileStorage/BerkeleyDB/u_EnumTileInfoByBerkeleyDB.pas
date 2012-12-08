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
  i_FileNameIterator,
  i_TileFileNameParser,
  u_TileStorageBerkeleyDBHelper,
  u_BaseInterfacedObject;

type
  TEnumTileInfoByBerkeleyDB = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FFilesIterator: IFileNameIterator;
    FTileFileNameParser: ITileFileNameParser;
    FStorage: ITileStorage;
    FHelper: TTileStorageBerkeleyDBHelper;
    FCurFileTilesArray: TPointArray;
    FCurFileIndex: Integer;
    FCurFileZoom: Byte;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AFilesIterator: IFileNameIterator;
      const ATileFileNameParser: ITileFileNameParser;
      const AStorage: ITileStorage;
      const AHelper: TTileStorageBerkeleyDBHelper
    );
  end;

implementation

uses
  Types,
  SysUtils,
  i_BinaryData;

{ TEnumTileInfoByBerkeleyDB }

constructor TEnumTileInfoByBerkeleyDB.Create(
  const AFilesIterator: IFileNameIterator;
  const ATileFileNameParser: ITileFileNameParser;
  const AStorage: ITileStorage;
  const AHelper: TTileStorageBerkeleyDBHelper
);
begin
  inherited Create;
  FFilesIterator := AFilesIterator;
  FTileFileNameParser := ATileFileNameParser;
  FStorage := AStorage;
  FHelper := AHelper;
  FCurFileIndex := 0;
  SetLength(FCurFileTilesArray, 0);
end;

function TEnumTileInfoByBerkeleyDB.Next(var ATileInfo: TTileInfo): Boolean;
var
  VTileFileName: string;
  VTileFileNameW: WideString;
  VTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VData: IBinaryData;
  VTileXY: TPoint;
begin
  Result := False;
  while FCurFileIndex >= 0 do begin
    if FCurFileIndex < Length(FCurFileTilesArray) then begin
      ATileInfo.FZoom := FCurFileZoom;
      ATileInfo.FTile := FCurFileTilesArray[FCurFileIndex];
      Inc(FCurFileIndex);
      VTileInfo := FStorage.GetTileInfo(ATileInfo.FTile, FCurFileZoom, nil, gtimWithData);
      VData := nil;
      if Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
        VData := VTileInfoWithData.TileData;
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
        Continue;
      end;
    end;
    if FFilesIterator.Next(VTileFileNameW) then begin
      VTileFileName := FFilesIterator.GetRootFolderName + VTileFileNameW;
      if FTileFileNameParser.GetTilePoint(VTileFileName, VTileXY, FCurFileZoom) and
        FHelper.GetTileExistsArray(VTileFileName, FCurFileZoom, nil, FCurFileTilesArray) then begin
        FCurFileIndex := 0;
      end else begin
        FCurFileIndex := Length(FCurFileTilesArray);
      end;
    end else begin
      FCurFileIndex := -1;
    end;
  end;
end;

end.
