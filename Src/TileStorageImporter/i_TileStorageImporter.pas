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

unit i_TileStorageImporter;

interface

uses
  t_GeoTypes,
  t_TileStorageImporter,
  i_MapType,
  i_MapTypeSet;

type
  TTileStorageImportResultStatus = (
    tsiOk,
    tsiUnsupportedFormat,
    tsiUnsupportedContentType,
    tsiCanceled,
    tsiInternalError
  );

  TTileStorageImportResult = record
    Status: TTileStorageImportResultStatus;
    MapType: IMapType;
    GoToZoom: Byte;
    GoToPoint: TDoublePoint;
  end;

  TDoShowImportDialog = procedure(
    const AFileInfo: TTileStorageImporterFileInfo;
    out AIsCanceled: Boolean
  ) of object;

  ITileStorageImporter = interface
    ['{8F520C27-B41E-41E6-A357-959E05D04B9C}']
    function ProcessFile(
      const AFileName: string;
      const AAllMapsSet: IMapTypeSet;
      const AShowImportDialod: Boolean = False;
      const ADoShowImportDialog: TDoShowImportDialog = nil
    ): TTileStorageImportResult;
  end;

implementation

end.
