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

    function CreateTarWriterByName(const AFileName: string): IArchiveWriter;
    function CreateTarWriterByStream(const AStream: TStream): IArchiveWriter;
  end;

implementation

end.
