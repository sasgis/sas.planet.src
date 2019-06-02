{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_EnumTileInfoByFileIterator;

interface

uses
  Types,
  SysUtils,
  i_StorageState,
  i_TileInfoBasic,
  i_ContentTypeInfo,
  i_FileNameIterator,
  i_TileFileNameParser,
  u_BaseInterfacedObject;

type
  TEnumTileInfoByFileIterator = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FTneExt: string;
    FFilesIterator: IFileNameIterator;
    FTileFileNameParser: ITileFileNameParser;
    FStorageState: IStorageStateChangeble;
    FMainContentType: IContentTypeInfoBasic;
    FLock: IReadWriteSync;
    function IsTne(const AFileName: string): Boolean; inline;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AFilesIterator: IFileNameIterator;
      const ATileFileNameParser: ITileFileNameParser;
      const ALock: IReadWriteSync;
      const AStorageState: IStorageStateChangeble;
      const AMainContentType: IContentTypeInfoBasic
    );
  end;

implementation

uses
  u_StrFunc,
  u_FileSystemTools;

{ TEnumTileInfoByFileIterator }

constructor TEnumTileInfoByFileIterator.Create(
  const AFilesIterator: IFileNameIterator;
  const ATileFileNameParser: ITileFileNameParser;
  const ALock: IReadWriteSync;
  const AStorageState: IStorageStateChangeble;
  const AMainContentType: IContentTypeInfoBasic
);
begin
  inherited Create;
  FTneExt := TFileSystemTools.GetTileNotFoundExt;
  FFilesIterator := AFilesIterator;
  FTileFileNameParser := ATileFileNameParser;
  FLock := ALock;
  FStorageState := AStorageState;
  FMainContentType := AMainContentType;
end;

function TEnumTileInfoByFileIterator.IsTne(const AFileName: string): Boolean;
begin
  Result := LowerCase(ExtractFileExt(AFileName)) = FTneExt;
end;

function TEnumTileInfoByFileIterator.Next(var ATileInfo: TTileInfo): Boolean;
var
  VTileFileName: AnsiString;
  VTileFileNameW: string;
  VTileXY: TPoint;
  VTileZoom: Byte;
  VFileName: string;
begin
  Result := False;
  while FFilesIterator.Next(VTileFileNameW) do begin
    if IsAscii(VTileFileNameW) then begin
      VTileFileName := StringToAsciiSafe(VTileFileNameW);
      if FTileFileNameParser.GetTilePoint(VTileFileName, VTileXY, VTileZoom) then begin
        VFileName := FFilesIterator.GetRootFolderName + VTileFileNameW;
        if FStorageState.GetStatic.ScanAccess then begin
          FLock.BeginRead;
          try
            TFileSystemTools.UpdateTileInfoByFile(
              IsTne(VFileName),
              True,
              VFileName,
              ATileInfo
            );
            ATileInfo.FTile := VTileXY;
            ATileInfo.FZoom := VTileZoom;
            ATileInfo.FVersionInfo := nil;
            ATileInfo.FContentType := FMainContentType;
            if ATileInfo.FInfoType <> titNotExists then begin
              Result := True;
              Break;
            end;
          finally
            FLock.EndRead;
          end;
        end;
      end;
    end;
  end;
end;

end.
