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
  i_TileStorageSQLiteFileInfo,
  i_TileStorageSQLiteFileConnectionBuilder,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteFileConnectionBuilder = class(TBaseInterfacedObject, ITileStorageSQLiteFileConnectionBuilder)
  private
    FIsFailed: Boolean;
    FFileName: string;
    FMainContentType: IContentTypeInfoBasic;
    FFormatId: TTileStorageSQLiteFileFormatId;
    FFileInfo: ITileStorageSQLiteFileInfo;
    FLock: TCriticalSection;
  private
    { ITileStorageSQLiteFileConnectionBuilder }
    function MakeNewConnection: TObject;
  public
    constructor Create(
      const AFileName: string;
      const AMainContentType: IContentTypeInfoBasic;
      const AFormatId: TTileStorageSQLiteFileFormatId
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_TileStorageSQLiteFileConnection,
  u_TileStorageSQLiteFileConnectionMBTiles;

{ TTileStorageSQLiteFileConnectionBuilder }

constructor TTileStorageSQLiteFileConnectionBuilder.Create(
  const AFileName: string;
  const AMainContentType: IContentTypeInfoBasic;
  const AFormatId: TTileStorageSQLiteFileFormatId
);
begin
  inherited Create;

  FFileName := AFileName;
  FMainContentType := AMainContentType;
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
  VConnection := nil;

  if FIsFailed then begin
    raise Exception.CreateFmt('Can''t open file %s as ', [FFileName]);
  end;

  try
    FLock.Acquire;
    try
      if not FileExists(FFileName) then begin
        raise Exception.CreateFmt('File not found: %s', [FFileName]);
      end;

      case FFormatId of
        sfMBTiles: begin
          VConnection :=
            TTileStorageSQLiteFileConnectionMBTiles.Create(
              FFileName, FFileInfo, FMainContentType
            );
        end;

        sfOsmAnd: begin
          // todo
        end;

        sfLocus: begin
          // todo
        end;

        sfRMaps: begin
          // todo
        end;

        sfOruxMaps: begin
          // todo
        end;
      else
        raise Exception.CreateFmt(
          Self.ClassName + ': ' + 'Unexpected FormatID = %d', [Integer(FFormatId)]
        );
      end;

      if VConnection = nil then begin
        raise Exception.Create('This format is not supported yet!');
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

end.
