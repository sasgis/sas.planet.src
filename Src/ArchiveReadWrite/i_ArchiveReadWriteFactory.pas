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
  IArchiveReaderFactory = interface
    ['{EEC4958E-B843-413D-8BAF-30FCC216C577}']
    function BuildByFileName(const AFileName: string): IArchiveReader;
    function BuildByStream(const AStream: TStream): IArchiveReader;
  end;

  IArchiveWriterFactory = interface
    ['{F83F0DE4-162C-40C5-AC38-BD131A3D78CB}']
    function BuildByFileName(const AFileName: string): IArchiveWriter;
    function BuildByStream(const AStream: TStream): IArchiveWriter;
  end;

  IArchiveType = interface
    ['{279A0A59-CF26-4198-980E-385E509F84DF}']
    function GetReaderFactory: IArchiveReaderFactory;
    property ReaderFactory: IArchiveReaderFactory read GetReaderFactory;

    function GetWriterFactory: IArchiveWriterFactory;
    property WriterFactory: IArchiveWriterFactory read GetWriterFactory;
  end;

  IArchiveReadWriteFactory = interface
    ['{53564F3B-8122-4968-A676-F02D4FE3276A}']
    function GetZip: IArchiveType;
    property Zip: IArchiveType read GetZip;

    function GetTar: IArchiveType;
    property Tar: IArchiveType read GetTar;

    function GetSevenZip: IArchiveType;
    property SevenZip: IArchiveType read GetSevenZip;

    function GetRar: IArchiveType;
    property Rar: IArchiveType read GetRar;
  end;

implementation

end.
