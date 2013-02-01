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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ArchiveReadWriteFactory;

interface

uses
  Classes,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TArchiveReadWriteFactory = class(TBaseInterfacedObject, IArchiveReadWriteFactory)
  private
    // Zip reader
    function CreateZipReaderByName(const AFileName: string): IArchiveReader;
    function CreateZipReaderByStream(const AStream: TStream): IArchiveReader;
    // Zip writer
    function CreateZipWriterByName(
      const AFileName: string;
      const AAllowOpenExisting: Boolean
    ): IArchiveWriter;
    function CreateZipWriterByStream(const AStream: TStream): IArchiveWriter;
    // Tar reader
    function CreateTarReaderByName(const AFileName: string): IArchiveReader;
    function CreateTarReaderByStream(const AStream: TStream): IArchiveReader;
    // Tar writer
    function CreateTarWriterByName(const AFileName: string): IArchiveWriter;
    function CreateTarWriterByStream(const AStream: TStream): IArchiveWriter;
    // 7z reader
    function Create7ZipReaderByName(const AFileName: string): IArchiveReader;
    function Create7ZipReaderByStream(const AStream: TStream): IArchiveReader;
    // 7z writer
    function Create7ZipWriterByName(const AFileName: string): IArchiveWriter;
    function Create7ZipWriterByStream(const AStream: TStream): IArchiveWriter;
    // Rar reader
    function CreateRarReaderByName(const AFileName: string): IArchiveReader;
    function CreateRarReaderByStream(const AStream: TStream): IArchiveReader;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_ArchiveWriteLibTar,
  u_ArchiveReadWrite7Zip,
  u_ArchiveReadWriteKaZip;

{ TArchiveReadWriteFactory }

constructor TArchiveReadWriteFactory.Create;
begin
  inherited Create;
end;

destructor TArchiveReadWriteFactory.Destroy;
begin
  inherited Destroy;
end;

function TArchiveReadWriteFactory.CreateZipReaderByName(
  const AFileName: string
): IArchiveReader;
begin
  Result := TArchiveReadByKaZip.Create(AFileName);
end;

function TArchiveReadWriteFactory.CreateZipReaderByStream(
  const AStream: TStream
): IArchiveReader;
begin
  Result := TArchiveReadByKaZip.Create(AStream);
end;

function TArchiveReadWriteFactory.CreateZipWriterByName(
  const AFileName: string;
  const AAllowOpenExisting: Boolean
): IArchiveWriter;
begin
  Result := TArchiveWriteByKaZip.Create(AFileName, AAllowOpenExisting);
end;

function TArchiveReadWriteFactory.CreateZipWriterByStream(
  const AStream: TStream
): IArchiveWriter;
begin
  Result := TArchiveWriteByKaZip.Create(AStream);
end;

function TArchiveReadWriteFactory.CreateTarReaderByName(
  const AFileName: string
): IArchiveReader;
begin
  Result := TArchiveReadBy7Zip.Create(AFileName, atTar);
end;

function TArchiveReadWriteFactory.CreateTarReaderByStream(
  const AStream: TStream
): IArchiveReader;
begin
  Result := TArchiveReadBy7Zip.Create(AStream, atTar);
end;

function TArchiveReadWriteFactory.CreateTarWriterByName(
  const AFileName: string
): IArchiveWriter;
begin
  Result := TArchiveWriteByLibTar.Create(AFileName);
end;

function TArchiveReadWriteFactory.CreateTarWriterByStream(
  const AStream: TStream
): IArchiveWriter;
begin
  Result := TArchiveWriteByLibTar.Create(AStream);
end;

function TArchiveReadWriteFactory.Create7ZipReaderByName(
  const AFileName: string
): IArchiveReader;
begin
  Result := TArchiveReadBy7Zip.Create(AFileName, at7Zip);
end;

function TArchiveReadWriteFactory.Create7ZipReaderByStream(
  const AStream: TStream
): IArchiveReader;
begin
  Result := TArchiveReadBy7Zip.Create(AStream, at7Zip);
end;

function TArchiveReadWriteFactory.Create7ZipWriterByName(
  const AFileName: string
): IArchiveWriter;
begin
  Result := TArchiveWriteBy7Zip.Create(AFileName, at7Zip);
end;

function TArchiveReadWriteFactory.Create7ZipWriterByStream(
  const AStream: TStream
): IArchiveWriter;
begin
  Result := TArchiveWriteBy7Zip.Create(AStream, at7Zip);
end;

function TArchiveReadWriteFactory.CreateRarReaderByName(
  const AFileName: string
): IArchiveReader;
begin
  Result := TArchiveReadBy7Zip.Create(AFileName, atRar);
end;

function TArchiveReadWriteFactory.CreateRarReaderByStream(
  const AStream: TStream
): IArchiveReader;
begin
  Result := TArchiveReadBy7Zip.Create(AStream, atRar);
end;

end.
