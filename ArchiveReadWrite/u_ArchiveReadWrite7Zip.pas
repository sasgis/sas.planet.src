unit u_ArchiveReadWrite7Zip;

interface

uses
  Classes,
  SevenZip,
  i_BinaryData,
  i_ArchiveReadWrite;

type
  TArchiveType = (atTar = 0, atZip = 1, atRar = 2, at7Zip = 3);

  TArchiveReadBy7Zip = class(TInterfacedObject, IArchiveReader)
  private
    FArch: I7zInArchive;
    FOwnStream: Boolean;
    FStream: TStream;
  private
    function CreateArchive(const AArchiveType: TArchiveType): I7zInArchive;
    // IArchiveReader
    function GetItemsCount: Integer;
    function GetItemByName(const AItemName: string): IBinaryData;
    function GetItemNameByIndex(const AItemIndex: Integer): string;
    function GetItemByIndex(
      const AItemIndex: Integer;
      out AItemName: string
    ): IBinaryData;
  public
    constructor Create(
      const AFileName: string;
      const AArchiveType: TArchiveType
    ); overload;
    constructor Create(
      const AStream: TStream;
      const AArchiveType: TArchiveType
    ); overload;
    destructor Destroy; override;
  end;

  TArchiveWriteBy7Zip = class(TInterfacedObject, IArchiveWriter)
  private
    FArch: I7zOutArchive;
    FOwnStream: Boolean;
    FStream: TStream;
    FFilesCount: Integer;
  private
    function CreateArchive(const AArchiveType: TArchiveType): I7zOutArchive;
    // IArchiveWriter
    function AddFile(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    ): Integer;
  public
    constructor Create(
      const AFileName: string;
      const AArchiveType: TArchiveType
    ); overload;
    constructor Create(
      const AStream: TStream;
      const AArchiveType: TArchiveType
    ); overload;
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData;

type
  EArchiveWriteBy7Zip = class (Exception);

{ TArchiveReadBy7Zip }

constructor TArchiveReadBy7Zip.Create(
  const AFileName: string;
  const AArchiveType: TArchiveType
);
begin
  inherited Create;
  FOwnStream := True;
  FStream := TFileStream.Create(AFileName, fmOpenRead);
  FArch := CreateArchive(AArchiveType);
end;

constructor TArchiveReadBy7Zip.Create(
  const AStream: TStream;
  const AArchiveType: TArchiveType
);
begin
  inherited Create;
  FOwnStream := False;
  FStream := AStream;
end;

destructor TArchiveReadBy7Zip.Destroy;
begin
  FArch.Close;
  if FOwnStream then begin
    FStream.Free;
  end;
  inherited Destroy;
end;

function TArchiveReadBy7Zip.CreateArchive(
  const AArchiveType: TArchiveType
): I7zInArchive;
begin
  case AArchiveType of
    atTar:  Result := CreateInArchive(CLSID_CFormatTar);
    atZip:  Result := CreateInArchive(CLSID_CFormatZip);
    at7Zip: Result := CreateInArchive(CLSID_CFormat7z);
  else // atRar
    Result := CreateInArchive(CLSID_CFormatRar);
  end;
  if Result <> nil then begin
    Result.OpenStream(T7zStream.Create(FStream, soReference));
  end;
end;

function TArchiveReadBy7Zip.GetItemsCount: Integer;
begin
  Result := FArch.NumberOfItems;
end;

function TArchiveReadBy7Zip.GetItemByName(const AItemName: string): IBinaryData;
var
  I: Integer;
  VItemName: string;
begin
  for I := 0 to FArch.NumberOfItems - 1 do begin
    VItemName := FArch.ItemPath[I];
    if AItemName = VItemName then begin
      Result := GetItemByIndex(I, VItemName);
      Break;
    end;
  end; 
end;

function TArchiveReadBy7Zip.GetItemNameByIndex(const AItemIndex: Integer): string;
begin
  Result := FArch.ItemPath[AItemIndex];
end;

function TArchiveReadBy7Zip.GetItemByIndex(
  const AItemIndex: Integer;
  out AItemName: string
): IBinaryData;
var
  VStream: TMemoryStream;
begin
  if not FArch.ItemIsFolder[AItemIndex] then begin
    AItemName := FArch.ItemPath[AItemIndex];
    if AItemName <> '' then begin
      VStream := TMemoryStream.Create;
      try
        FArch.ExtractItem(AItemIndex, VStream, False);
        VStream.Position := 0;
        Result := TBinaryDataByMemStream.CreateWithOwn(VStream);
        VStream := nil;
      finally
        VStream.Free;
      end;
    end;
  end else begin
    AItemName := '';
  end;
end;

{ TArchiveWriteBy7Zip }

constructor TArchiveWriteBy7Zip.Create(
  const AFileName: string;
  const AArchiveType: TArchiveType
);
begin
  inherited Create;
  FFilesCount := 0;
  FOwnStream := True;
  FStream := TFileStream.Create(AFileName, fmCreate);
  FArch := CreateArchive(AArchiveType);
end;

constructor TArchiveWriteBy7Zip.Create(
  const AStream: TStream;
  const AArchiveType: TArchiveType
);
begin
  inherited Create;
  FFilesCount := 0;
  FOwnStream := False;
  FStream := AStream;
  FArch := CreateArchive(AArchiveType);
end;

destructor TArchiveWriteBy7Zip.Destroy;
begin
  FArch.SaveToStream(FStream);
  if FOwnStream then begin
    FStream.Free;
  end;
  inherited Destroy;
end;

function TArchiveWriteBy7Zip.CreateArchive(
  const AArchiveType: TArchiveType
): I7zOutArchive;
begin
  case AArchiveType of
    atTar: Result := CreateOutArchive(CLSID_CFormatTar);
    atZip:
      begin
        Result := CreateOutArchive(CLSID_CFormatZip);
        SetCompressionMethod(Result, mzDeflate);
      end;
    at7Zip: Result := CreateOutArchive(CLSID_CFormat7z);
  else // atRar
    raise EArchiveWriteBy7Zip.Create('Unsupport open RAR in write mode!');
  end;
end;

function TArchiveWriteBy7Zip.AddFile(
  const AFileData: IBinaryData;
  const AFileNameInArchive: string;
  const AFileDate: TDateTime
): Integer;
var
  VStream: TStream;
  VFileTime: TFileTime;
begin
  VFileTime := DateTimeToFileTime(AFileDate);
  VStream := TStreamReadOnlyByBinaryData.Create(AFileData);
  try
    {$WARN SYMBOL_PLATFORM OFF}
    FArch.AddStream(
      VStream,
      soOwned,
      faArchive, // (!) platform
      VFileTime,
      VFileTime,
      AFileNameInArchive,
      False,
      False
    );
    {$WARN SYMBOL_PLATFORM ON}
    VStream := nil;
    Inc(FFilesCount);
    Result := FFilesCount;
  finally
    VStream.Free;
  end;
end;

end.
