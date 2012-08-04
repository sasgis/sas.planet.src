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
    // Tar writer
    function CreateTarWriterByName(const AFileName: string): IArchiveWriter;
    function CreateTarWriterByStream(const AStream: TStream): IArchiveWriter;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_ArchiveWriteLibTar,
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

end.
