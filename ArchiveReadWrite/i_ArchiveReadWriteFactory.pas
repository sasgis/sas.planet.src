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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit i_ArchiveReadWriteFactory;

interface

uses
  Classes,
  i_ArchiveReadWrite;

type
  IArchiveReadWriteFactory = interface
    ['{53564F3B-8122-4968-A676-F02D4FE3276A}']
    function CreateZipReaderByName(const AFileName: string): IArchiveReader;
    function CreateZipReaderByStream(const AStream: TStream): IArchiveReader;

    function CreateZipWriterByName(
      const AFileName: string;
      const AAllowOpenExisting: Boolean = FALSE
    ): IArchiveWriter;
    function CreateZipWriterByStream(const AStream: TStream): IArchiveWriter;

    function CreateTarReaderByName(const AFileName: string): IArchiveReader;
    function CreateTarReaderByStream(const AStream: TStream): IArchiveReader;

    function CreateTarWriterByName(const AFileName: string): IArchiveWriter;
    function CreateTarWriterByStream(const AStream: TStream): IArchiveWriter;

    function Create7ZipReaderByName(const AFileName: string): IArchiveReader;
    function Create7ZipReaderByStream(const AStream: TStream): IArchiveReader;

    function Create7ZipWriterByName(const AFileName: string): IArchiveWriter;
    function Create7ZipWriterByStream(const AStream: TStream): IArchiveWriter;

    function CreateRarReaderByName(const AFileName: string): IArchiveReader;
    function CreateRarReaderByStream(const AStream: TStream): IArchiveReader;
  end;

implementation

end.
