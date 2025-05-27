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

unit u_TileStorageSQLiteFileInfo;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  i_TileStorageSQLiteFileInfo,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteFileInfo = class(TBaseInterfacedObject, ITileStorageSQLiteFileInfo)
  private
    type TMetadata = TDictionary<string, string>;
  private
    FFileName: string;
    FFileDate: TDateTime;
    FMetadata: TMetadata;
  private
    { ITileStorageSQLiteFileInfo }
    function GetFileName: string;
    function GetFileDate: TDateTime;

    procedure AddOrSetMetadataValue(const AKey, AValue: string);
    function TryGetMetadataValue(const AKey: string; var AValue: string): Boolean;
  public
    constructor Create(
      const AFileName: string;
      const AFileDate: TDateTime
    );
    destructor Destroy; override;
  end;

implementation

{ TTileStorageSQLiteFileInfo }

constructor TTileStorageSQLiteFileInfo.Create(
  const AFileName: string;
  const AFileDate: TDateTime
);
begin
  inherited Create;

  FFileName := AFileName;
  FFileDate := AFileDate;
  FMetadata := TMetadata.Create;
end;

destructor TTileStorageSQLiteFileInfo.Destroy;
begin
  FreeAndNil(FMetadata);
  inherited Destroy;
end;

function TTileStorageSQLiteFileInfo.GetFileName: string;
begin
  Result := FFileName;
end;

function TTileStorageSQLiteFileInfo.GetFileDate: TDateTime;
begin
  Result := FFileDate;
end;

procedure TTileStorageSQLiteFileInfo.AddOrSetMetadataValue(const AKey, AValue: string);
begin
  FMetadata.AddOrSetValue(AKey, AValue);
end;

function TTileStorageSQLiteFileInfo.TryGetMetadataValue(const AKey: string; var AValue: string): Boolean;
begin
  Result := FMetadata.TryGetValue(AKey, AValue);
end;

end.
