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

unit u_TileStorageSQLiteFileConnectionPool;

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  i_TileStorageSQLiteFileConnectionBuilder,
  u_TileStorageSQLiteFileConnection;

type
  TTileStorageSQLiteFileConnectionPool = class
  private
    FList: TList;
    FLock: TCriticalSection;
    FBuilder: ITileStorageSQLiteFileConnectionBuilder;
  public
    function Acquire: TTileStorageSQLiteFileConnection;
    procedure Release(const AConnection: TTileStorageSQLiteFileConnection);

    constructor Create(const ABuilder: ITileStorageSQLiteFileConnectionBuilder);
    destructor Destroy; override;
  end;

implementation

{ TTileStorageSQLiteFileConnectionPool }

constructor TTileStorageSQLiteFileConnectionPool.Create(const ABuilder: ITileStorageSQLiteFileConnectionBuilder);
begin
  inherited Create;

  FBuilder := ABuilder;

  FList := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TTileStorageSQLiteFileConnectionPool.Destroy;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do begin
    TTileStorageSQLiteFileConnection(FList[I]).Free;
  end;

  FreeAndNil(FList);
  FreeAndNil(FLock);

  inherited Destroy;
end;

function TTileStorageSQLiteFileConnectionPool.Acquire: TTileStorageSQLiteFileConnection;
var
  I: Integer;
begin
  FLock.Acquire;
  try
    I := FList.Count - 1;
    if I >= 0 then begin
      Result := TTileStorageSQLiteFileConnection(FList[I]);
      FList[I] := nil;
      FList.Delete(I);
    end else begin
      Result := TTileStorageSQLiteFileConnection(FBuilder.MakeNewConnection);
    end;
  finally
    FLock.Release;
  end;
end;

procedure TTileStorageSQLiteFileConnectionPool.Release(const AConnection: TTileStorageSQLiteFileConnection);
begin
  FLock.Acquire;
  try
    FList.Add(Pointer(AConnection));
  finally
    FLock.Release;
  end;
end;

end.
