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

unit t_TileStorageImporter;

interface

uses
  Types,
  Generics.Collections,
  t_GeoTypes;

type
  TTileStorageImporterMetadataInfo = TDictionary<string, string>;

  TTileStorageImporterGotoInfo = record
    Zoom: Integer;
    LonLat: TDoublePoint;
    TilePos: TPoint;
    Status: (gtsError, gtsTilePos, gtsLonLat);
  end;

  TTileStorageImporterFileInfo = class
  public
    FFileName: string;

    FCacheTypeCode: Integer;
    FContentType: string;
    FProjectionEpsg: Integer;
    FIsBitmapTile: Boolean;
    FIsLayer: Boolean;
    FExt: string;
    FNameInCache: string;
    FMapName: string;
    FParentSubMenu: string;

    FGotoInfo: TTileStorageImporterGotoInfo;

    FMetadata: TTileStorageImporterMetadataInfo;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TTileStorageImporterFileInfo }

constructor TTileStorageImporterFileInfo.Create(const AFileName: string);
begin
  inherited Create;

  FFileName := AFileName;

  FCacheTypeCode := 0;
  FProjectionEpsg := 3857;
  FGotoInfo.Status := gtsError;

  FMetadata := TTileStorageImporterMetadataInfo.Create;
end;

destructor TTileStorageImporterFileInfo.Destroy;
begin
  FreeAndNil(FMetadata);
  inherited Destroy;
end;

end.
