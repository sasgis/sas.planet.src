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

    function CreateZipWriterByName(const AFileName: string): IArchiveWriter;
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
