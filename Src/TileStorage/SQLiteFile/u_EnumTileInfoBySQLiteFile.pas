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

unit u_EnumTileInfoBySQLiteFile;

interface

uses
  SysUtils,
  i_TileInfoBasic,
  i_TileStorageSQLiteFileConnectionBuilder,
  u_TileStorageSQLiteFileConnection,
  u_BaseInterfacedObject;

type
  TEnumTileInfoBySQLiteFile = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FIsDone: Boolean;
    FLock: IReadWriteSync;
    FConnection: TTileStorageSQLiteFileConnection;
    FConnectionBuilder: ITileStorageSQLiteFileConnectionBuilder;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AConnectionBuilder: ITileStorageSQLiteFileConnectionBuilder;
      const ALock: IReadWriteSync
    );
    destructor Destroy; override;
  end;

implementation

{ TEnumTileInfoBySQLiteFile }

constructor TEnumTileInfoBySQLiteFile.Create(
  const AConnectionBuilder: ITileStorageSQLiteFileConnectionBuilder;
  const ALock: IReadWriteSync
);
begin
  inherited Create;
  FConnectionBuilder := AConnectionBuilder;
  FLock := ALock;
end;

destructor TEnumTileInfoBySQLiteFile.Destroy;
begin
  FreeAndNil(FConnection);
  inherited Destroy;
end;

function TEnumTileInfoBySQLiteFile.Next(var ATileInfo: TTileInfo): Boolean;
begin
  Result := False;

  if FIsDone then begin
    Exit;
  end;

  if FConnection = nil then begin
    FConnection := TTileStorageSQLiteFileConnection(FConnectionBuilder.MakeNewConnection);
  end;

  FLock.BeginRead;
  try
    Result := FConnection.FetchNext(ATileInfo);
  finally
    FLock.EndRead;
  end;

  if not Result then begin
    FIsDone := True;
    FreeAndNil(FConnection);
  end;
end;

end.
