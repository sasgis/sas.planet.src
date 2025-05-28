{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileStorageSQLiteFileConnectionBuilder;

interface

uses
  SyncObjs,
  SysUtils,
  t_TileStorageSQLiteFile,
  i_ContentTypeInfo,
  i_ProjectionSet,
  i_TileStorageSQLiteFileInfo,
  i_TileStorageSQLiteFileConnectionBuilder,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteFileConnectionBuilder = class(TBaseInterfacedObject, ITileStorageSQLiteFileConnectionBuilder)
  private
    FIsFailed: Boolean;
    FFileName: string;
    FMainContentType: IContentTypeInfoBasic;
    FProjectionSet: IProjectionSet;
    FFormatId: TTileStorageSQLiteFileFormatId;
    FFileInfo: ITileStorageSQLiteFileInfo;
    FLock: TCriticalSection;
    class function GetDbNameById(const AFormatId: TTileStorageSQLiteFileFormatId): string;
  private
    { ITileStorageSQLiteFileConnectionBuilder }
    function MakeNewConnection: TObject;
  public
    constructor Create(
      const AFileName: string;
      const AMainContentType: IContentTypeInfoBasic;
      const AProjectionSet: IProjectionSet;
      const AFormatId: TTileStorageSQLiteFileFormatId
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_TileStorageSQLiteFileConnection,
  u_TileStorageSQLiteFileConnectionMBTiles,
  u_TileStorageSQLiteFileConnectionRMaps,
  u_TileStorageSQLiteFileConnectionOruxMaps;

{ TTileStorageSQLiteFileConnectionBuilder }

constructor TTileStorageSQLiteFileConnectionBuilder.Create(
  const AFileName: string;
  const AMainContentType: IContentTypeInfoBasic;
  const AProjectionSet: IProjectionSet;
  const AFormatId: TTileStorageSQLiteFileFormatId
);
begin
  inherited Create;

  FFileName := AFileName;
  FMainContentType := AMainContentType;
  FProjectionSet := AProjectionSet;
  FFormatId := AFormatId;

  FLock := TCriticalSection.Create;
end;

destructor TTileStorageSQLiteFileConnectionBuilder.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TTileStorageSQLiteFileConnectionBuilder.MakeNewConnection: TObject;
var
  VConnection: TTileStorageSQLiteFileConnection;
begin
  if FIsFailed then begin
    raise Exception.CreateFmt(
      'Can''t open file: "%s" as %s database!', [FFileName, GetDbNameById(FFormatId)]
    );
  end;

  try
    FLock.Acquire;
    try
      if not FileExists(FFileName) then begin
        raise Exception.CreateFmt('File not found: "%s"', [FFileName]);
      end;

      case FFormatId of
        sfMBTiles: begin
          VConnection :=
            TTileStorageSQLiteFileConnectionMBTiles.Create(
              FFileName, FFileInfo, FMainContentType
            );
        end;

        sfOsmAnd, sfLocus, sfRMaps: begin
          VConnection :=
            TTileStorageSQLiteFileConnectionRMaps.Create(
              FFileName, FFileInfo, FMainContentType, FFormatId
            );
        end;

        sfOruxMaps: begin
          VConnection :=
            TTileStorageSQLiteFileConnectionOruxMaps.Create(
              FFileName, FFileInfo, FMainContentType, FProjectionSet
            );
        end;
      else
        raise Exception.CreateFmt(
          Self.ClassName + ': ' + 'Unexpected FormatID = %d', [Integer(FFormatId)]
        );
      end;

      if FFileInfo = nil then begin
        FFileInfo := VConnection.FileInfo;
      end;

      Result := VConnection;
    finally
      FLock.Release;
    end;
  except
    FIsFailed := True;
    raise;
  end;
end;

class function TTileStorageSQLiteFileConnectionBuilder.GetDbNameById(
  const AFormatId: TTileStorageSQLiteFileFormatId
): string;
begin
  case AFormatId of
    sfMBTiles:  Result := 'MBTiles (SQLite3)';
    sfOsmAnd:   Result := 'OsmAnd (SQLite3)';
    sfLocus:    Result := 'Locus (SQLite3)';
    sfRMaps:    Result := 'RMaps (SQLite3)';
    sfOruxMaps: Result := 'OruxMaps (SQLite3)';
  end;
end;

end.
