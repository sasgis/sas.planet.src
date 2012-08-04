unit u_ArchiveReadWriteKaZip;

interface

uses
  Classes,
  KAZip,
  i_BinaryData,
  i_ArchiveReadWrite;

type
  TArchiveReadByKaZip = class(TInterfacedObject, IArchiveReader)
  private
    FZip: TKAZip;
  private
    function GetItemsCount: Integer;
    function GetItemByName(const AItemName: string): IBinaryData;
    function GetItemNameByIndex(const AItemIndex: Integer): string;
    function GetItemByIndex(
      const AItemIndex: Integer;
      out AItemName: string
    ): IBinaryData;
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(const AStream: TStream); overload;
    destructor Destroy; override;
  end;

  TArchiveWriteByKaZip = class(TInterfacedObject, IArchiveWriter)
  private
    FZip: TKAZip;
    FIsFromFileName: Boolean;
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
  SysUtils,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData;

{ TArchiveReadByKaZip }

constructor TArchiveReadByKaZip.Create(const AFileName: string);
begin
  inherited Create;
  FZip := TKAZip.Create(nil);
  FZip.Open(AFileName);
end;

constructor TArchiveReadByKaZip.Create(const AStream: TStream);
begin
  inherited Create;
  FZip := TKAZip.Create(nil);
  FZip.Open(AStream);
end;

destructor TArchiveReadByKaZip.Destroy;
begin
  FZip.Free;
  inherited Destroy;
end;

function TArchiveReadByKaZip.GetItemsCount: Integer;
begin
  Result := FZip.Entries.Count;
end;

function TArchiveReadByKaZip.GetItemByName(const AItemName: string): IBinaryData;
var
  VMemStream: TMemoryStream;
  VItemIndex: Integer;
begin
  VItemIndex := FZip.Entries.IndexOf(AItemName);
  if VItemIndex >= 0 then begin
    VMemStream := TMemoryStream.Create;
    try
      FZip.Entries.Items[VItemIndex].ExtractToStream(VMemStream);
      VMemStream.Position := 0;
      Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VMemStream := nil;
    finally
      VMemStream.Free;
    end;
  end;
end;

function TArchiveReadByKaZip.GetItemNameByIndex(const AItemIndex: Integer): string;
begin
  if FZip.Entries.Count >= AItemIndex then begin
    Result := FZip.Entries.Items[AItemIndex].FileName;
  end else begin
    Result := '';
  end;
end;

function TArchiveReadByKaZip.GetItemByIndex(
  const AItemIndex: Integer;
  out AItemName: string
): IBinaryData;
begin
  AItemName := GetItemNameByIndex(AItemIndex);
  if AItemName <> '' then begin
    Result := GetItemByName(AItemName);
  end;
end;

{ TArchiveWriteByKaZip }

constructor TArchiveWriteByKaZip.Create(const AFileName: string);
begin
  inherited Create;
  FIsFromFileName := True;
  FZip := TKAZip.Create(nil);
  FZip.FileName := AFileName;
  FZip.CreateZip(AFileName);
  FZip.CompressionType := ctFast;
  FZip.Active := True;
end;

constructor TArchiveWriteByKaZip.Create(const AStream: TStream);
begin
  inherited Create;
  FIsFromFileName := False;
  FZip := TKAZip.Create(nil);
  FZip.CreateZip(AStream);
  FZip.CompressionType := ctFast;
  FZip.Open(AStream);
end;

destructor TArchiveWriteByKaZip.Destroy;
begin
  if FIsFromFileName then begin
    FZip.Active := False;
    FZip.Close;
  end;
  FZip.Free;
  inherited Destroy;
end;

function TArchiveWriteByKaZip.AddFile(
  const AFileData: IBinaryData;
  const AFileNameInArchive: string;
  const AFileDate: TDateTime
): Integer;
var
  VDataStream: TStream;
  VEntry: TKAZipEntriesEntry;
begin
  VDataStream := TStreamReadOnlyByBinaryData.Create(AFileData);
  try
    {$WARN SYMBOL_PLATFORM OFF}
    VEntry := FZip.AddStream(
      AFileNameInArchive,
      faArchive, // (!) platform
      AFileDate,
      VDataStream
    );
    {$WARN SYMBOL_PLATFORM ON}
    Result := VEntry.Index;
  finally
    VDataStream.Free;
  end;
end;

end.
