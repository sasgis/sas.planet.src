{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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
  // reader factory
  IArchiveReaderFactoryBase = interface
    ['{BA48F3A9-C93F-4AEA-9939-85E2FFD3427F}']
  end;

  IArchiveReaderFactory = interface(IArchiveReaderFactoryBase)
    ['{EEC4958E-B843-413D-8BAF-30FCC216C577}']
    function BuildByFileName(const AFileName: string): IArchiveReader;
    function BuildByStreamWithOwn(var AStream: TStream): IArchiveReader;
    function BuildByStream(const AStream: TStream): IArchiveReader;
  end;

  IArchiveReaderSequentialFactory = interface(IArchiveReaderFactoryBase)
    ['{EEC4958E-B843-413D-8BAF-30FCC216C577}']
    function Build(
      const AFileName: string;
      const AOptions: Pointer = nil
    ): IArchiveReaderSequential;
  end;

  // writer factory
  IArchiveWriterFactoryBase = interface
    ['{3B461025-15B7-4300-87D4-E9AF73FA2E05}']
  end;

  IArchiveWriterFactory = interface(IArchiveWriterFactoryBase)
    ['{F83F0DE4-162C-40C5-AC38-BD131A3D78CB}']
    function BuildByFileName(const AFileName: string): IArchiveWriter;
    function BuildByStreamWithOwn(var AStream: TStream): IArchiveWriter;
    function BuildByStream(const AStream: TStream): IArchiveWriter;
  end;

  IArchiveWriterSequentialFactory = interface(IArchiveWriterFactoryBase)
    ['{F83F0DE4-162C-40C5-AC38-BD131A3D78CB}']
    function Build(
      const AFileName: string;
      const AOptions: Pointer = nil
    ): IArchiveWriterSequential;
  end;

  // archive type
  IArchiveTypeBase = interface
    ['{24595AD2-CB9C-4D49-9FCF-4C6197FE8E82}']
  end;

  IArchiveType = interface(IArchiveTypeBase)
    ['{279A0A59-CF26-4198-980E-385E509F84DF}']
    function GetReaderFactory: IArchiveReaderFactory;
    property ReaderFactory: IArchiveReaderFactory read GetReaderFactory;

    function GetWriterFactory: IArchiveWriterFactory;
    property WriterFactory: IArchiveWriterFactory read GetWriterFactory;
  end;

  IArchiveTypeSequential = interface(IArchiveTypeBase)
    ['{279A0A59-CF26-4198-980E-385E509F84DF}']
    function GetReaderFactory: IArchiveReaderSequentialFactory;
    property ReaderFactory: IArchiveReaderSequentialFactory read GetReaderFactory;

    function GetWriterFactory: IArchiveWriterSequentialFactory;
    property WriterFactory: IArchiveWriterSequentialFactory read GetWriterFactory;
  end;

  // main r/w factory
  IArchiveReadWriteFactory = interface
    ['{53564F3B-8122-4968-A676-F02D4FE3276A}']
    function GetZip: IArchiveType;
    property Zip: IArchiveType read GetZip;

    function GetZipSequential: IArchiveTypeSequential;
    property ZipSequential: IArchiveTypeSequential read GetZipSequential;

    function GetTar: IArchiveType;
    property Tar: IArchiveType read GetTar;

    function GetTarSequential: IArchiveTypeSequential;
    property TarSequential: IArchiveTypeSequential read GetTarSequential;

    function GetSevenZip: IArchiveType;
    property SevenZip: IArchiveType read GetSevenZip;

    function GetRar: IArchiveType;
    property Rar: IArchiveType read GetRar;
  end;

implementation

end.
