unit u_ArchiveReadWriteFactory;

interface

uses
  Classes,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory;

type
  TArchiveReadWriteFactory = class(TInterfacedObject, IArchiveReadWriteFactory)
  private
    // Zip reader
    function CreateZipReaderByName(const AFileName: string): IArchiveReader;
    function CreateZipReaderByStream(const AStream: TStream): IArchiveReader;
    // Zip writer
    function CreateZipWriterByName(const AFileName: string): IArchiveWriter;
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
  const AFileName: string
): IArchiveWriter;
begin
  Result := TArchiveWriteByKaZip.Create(AFileName);
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
  Result := TArchiveReadBy7Zip.Create(AFileName, at7zip);
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
