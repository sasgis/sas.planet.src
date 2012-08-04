unit u_ArchiveWriteLibTar;

interface

uses
  Classes,
  LibTar,
  i_BinaryData,
  i_ArchiveReadWrite;

type
  TArchiveWriteByLibTar = class(TInterfacedObject, IArchiveWriter)
  private
    FTar: TTarWriter;
    FFilesCount: Integer;
  private
    function AddFile(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    ): Integer;
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(const AStream: TStream); overload;
    destructor Destroy; override;
  end;

implementation

uses
  u_StreamReadOnlyByBinaryData;

{ TArchiveWriteByLibTar }

constructor TArchiveWriteByLibTar.Create(const AFileName: string);
begin
  inherited Create;
  FFilesCount := 0;
  FTar := TTarWriter.Create(AFileName);
end;

constructor TArchiveWriteByLibTar.Create(const AStream: TStream);
begin
  inherited Create;
  FFilesCount := 0;
  FTar := TTarWriter.Create(AStream);
end;

destructor TArchiveWriteByLibTar.Destroy;
begin
  FTar.Free;
  inherited Destroy;
end;

function TArchiveWriteByLibTar.AddFile(
  const AFileData: IBinaryData;
  const AFileNameInArchive: string;
  const AFileDate: TDateTime
): Integer;
var
  VStream: TStream;
begin
  VStream := TStreamReadOnlyByBinaryData.Create(AFileData);
  try
    FTar.AddStream(
      VStream,
      AFileNameInArchive,
      AFileDate
    );
    Inc(FFilesCount);
    Result := FFilesCount;
  finally
    VStream.Free;
  end;
end;

end.
