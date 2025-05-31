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
  i_StorageStateInternal,
  i_TileStorageSQLiteFileInfo,
  i_TileStorageSQLiteFileConnectionBuilder,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteFileConnectionBuilder = class(TBaseInterfacedObject, ITileStorageSQLiteFileConnectionBuilder)
  private
    FIsFailed: Boolean;
    FFileName: string;
    FStorageStateInternal: IStorageStateInternal;
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
    class function PrepareFileName(
      const AFileName: string;
      const AFormatId: TTileStorageSQLiteFileFormatId
    ): string;
  public
    constructor Create(
      const AFileName: string;
      const AStorageStateInternal: IStorageStateInternal;
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
  const AStorageStateInternal: IStorageStateInternal;
  const AMainContentType: IContentTypeInfoBasic;
  const AProjectionSet: IProjectionSet;
  const AFormatId: TTileStorageSQLiteFileFormatId
);
begin
  inherited Create;

  FFileName := AFileName;
  FStorageStateInternal := AStorageStateInternal;
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
  VIsReadOnly: Boolean;
  VConnection: TTileStorageSQLiteFileConnection;
begin
  try
    FLock.Acquire;
    try
      if FIsFailed then begin
        // disable access to the storage
        FStorageStateInternal.ReadAccess := False;
        FStorageStateInternal.ScanAccess := False;
        FStorageStateInternal.AddAccess := False;
        FStorageStateInternal.DeleteAccess := False;
        FStorageStateInternal.ReplaceAccess := False;

        raise Exception.CreateFmt(
          'Can''t open file: "%s" as %s database!', [FFileName, GetDbNameById(FFormatId)]
        );
      end;

      VIsReadOnly :=
        not FStorageStateInternal.AddAccess and
        not FStorageStateInternal.ReplaceAccess and
        not FStorageStateInternal.DeleteAccess;

      case FFormatId of
        sfMBTiles: begin
          VConnection :=
            TTileStorageSQLiteFileConnectionMBTiles.Create(
              VIsReadOnly, FFileName, FFileInfo, FMainContentType, FProjectionSet
            );
        end;

        sfOsmAnd, sfLocus, sfRMaps: begin
          VConnection :=
            TTileStorageSQLiteFileConnectionRMaps.Create(
              VIsReadOnly, FFileName, FFileInfo, FMainContentType, FProjectionSet, FFormatId
            );
        end;

        sfOruxMaps: begin
          if VIsReadOnly then begin
            VConnection :=
              TTileStorageSQLiteFileConnectionOruxMaps.Create(
                FFileName, FFileInfo, FMainContentType, FProjectionSet
              );
          end else begin
            raise Exception.Create('OruxMaps (SQLite3) does not support write access!');
          end;
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

class function TTileStorageSQLiteFileConnectionBuilder.PrepareFileName(
  const AFileName: string;
  const AFormatId: TTileStorageSQLiteFileFormatId
): string;

  function GetDefaultExt: string;
  begin
    case AFormatId of
      sfMBTiles:  Result := '.mbtiles';
      sfOsmAnd:   Result := '.sqlitedb';
      sfLocus:    Result := '.sqlitedb';
      sfRMaps:    Result := '.sqlitedb';
      sfOruxMaps: Result := '.db';
    else
      Assert(False);
    end;
  end;

var
  VLastDir: string;
begin
  // This function expects a path as input.
  // This path can be either a folder name (directory) or a file name that ends with PathDelim.
  // If it's a file name, we simply remove the trailing PathDelim.
  // If it's a folder, we need to append a file name to it that matches the name of the last folder in the path.

  // c:/osm/ --> c:/osm/osm.mbtiles
  // c:/osm.mbtiles/ --> c:/osm.mbtiles

  Result := AFileName;

  if Result[High(Result)] = PathDelim then begin
    SetLength(Result, Length(Result) - 1);
  end;

  if ExtractFileExt(Result) = '' then begin
    // input is a path: create a file inside it with the same name as its last directory
    VLastDir := ExtractFileName(Result);
    if VLastDir <> '' then begin
      Result := Result + PathDelim + VLastDir + GetDefaultExt;
    end else begin
      Result := Result + GetDefaultExt;
    end;
  end else begin
    // input is a file: nothing to do anymore
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
