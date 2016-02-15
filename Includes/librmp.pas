(*******************************************************************************

  .RMP file structure:

  4 bytes           - Number of Entries
  4 bytes           - Number of Entries

  24*N bytes        - Entries Meta:
    --- Entry Meta Start ---
    9 bytes         - Entry File Name: 8 chars + #0
    7 bytes         - Entry File Ext: 6 chars + #0
    4 bytes         - Entry Data Offset
    4 bytes         - Entry Data Size
    --- Entry Meta End ---

  2 bytes           - ?? MOBAC write checksum / RMPCreator write 0xE5E5

  8 bytes           - 'MAGELLAN' chars
  22 bytes          - 0x00 - Reserved
  XX bytes          - Entries Data (every entry aligned by 2 bytes (!))
  8 bytes           - 'MAGELLAN' chars

  2 bytes           - ?? MOBAC write checksum / RMPCreator write chars '};'

********************************************************************************

  .A00 file structure:

  4 bytes           - Number of Tiles
  ZZ*N bytes        - Tiles:
    --- Tile Start ---
    4 bytes         - Tile Size
    XX bytes        - Tile Data
    --- Tile End ---

********************************************************************************

  .TLM file structure:

0x00000000 (0):

      4 bytes           - 0x01 - Start of block
      4 bytes           - Total number of tiles in this TLM file
      2 bytes           - 256 - Height of tile in pixel
      2 bytes           - 256 - Width of tile in pixel

0x0000000C (+12 (+0x0C)):

      4 bytes           - 0x01 - Start of block
      8 bytes (float)   - Height of tile in degree
      8 bytes (float)   - Width  of tile in degree
      8 bytes (float)   - Left (Long)
      8 bytes (float)   - Top (Lat) - must be negated (!)
      8 bytes (float)   - Right (Long)
      8 bytes (float)   - Bottom (Lat) - must be negated (!)
      88 bytes          - 0x00 - Reserved

0x00000098 (+140 (+0x8C) -> 152):

      2 bytes           - 0x0001 (256)  - ??
      2 bytes           - 0x0000        - ??
      4 bytes           - TLM file size
      96 bytes          - 0x00 - Reserved

0x00000100 (+104 (+0x68) -> 256):

      4 bytes           - 0x01 - Start of block
      4 bytes           - 0x63 (99) - Max records count per block
      4 bytes           - First block offset (0x0F5C if no index block or 0x1724 if it is)
      3920 bytes        - 0x00 - Reserved

0x0000105C (+3932 (+0x0F5C) -> 4188):

      0x07C8 bytes      - First block with Data (tiles meta)
        --- Block Start ---
        4 bytes         - Number of records in this block
        2 bytes         - Number of records in this block
        2 bytes         - 0x01 - block with Data ID

        16*99 bytes     - Tile Meta records:
          --- Tile Meta Start ---
          4 bytes       - X
          4 bytes       - Y
          4 bytes       - 0x00 - Reserved
          4 bytes       - Tile offset in .A00 file
          --- Tile Meta End ---

        4*99 bytes      - 0x00 - Reserved
        4 bytes         - 0x00 - Reserved
        --- Block End ---

  Index present only if file contain more then 1 block with data. It always
  placed as a second block.

  Index contain one Tile Meta record per each additional block with Data.

0x00001824 (+1992 (+0x07C8) -> 6180):

      1992 (0x07C8) bytes - Block with Index:

        --- Index Block Start ---
        4 bytes          - Total number of tiles in this TLM file
        2 bytes          - Number of records in this block
        2 bytes          - 0x00 - block with Index ID

        16*99 bytes      - Tile Meta records:
          --- Tile Meta Start (same struct as in the blocks with Data) ---
          4 bytes       - X
          4 bytes       - Y
          4 bytes       - 0x00 - Reserved
          4 bytes       - Tile offset in .A00 file
          --- Tile Meta End ---

        4*99 bytes      - Offsets of bloks with Data
        4 bytes         - 0x00 Reserved
        --- Index Block End ---

  Then follows blocks with data + 2 empty blocks (or just 2 empty blocks if
  there is only one block with data).


*******************************************************************************)

unit librmp;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Math;

{$A4}

const
  cTLMFileMaxTilesPerBlockCount = 99; // 0x63
  cTLMFileTilesPerBlockCount = 70;
  cTLMFileDataBlockID = 1;
  cTLMFileIndexBlockID = 0;

type
  TRMPEntryMeta = packed record
    Name: array [0..9-1] of AnsiChar;
    Ext: array [0..7-1] of AnsiChar;
    Offset: Integer;
    Size: Integer;
  end;
  PRMPFileMeta = ^TRMPEntryMeta;

  TTLMFileHeader = packed record
    StartFlag_1: Integer;
    TilesCount: Integer;
    TileHeight: Word; // 0x0001 (256)
    TileWidth: Word;  // 0x0001 (256)
    StartFlag_2: Integer;
    TileHeightResolution: Double;
    TileWidthResolution: Double;
    Left: Double;
    Top: Double;
    Right: Double;
    Bottom: Double;
    Reserved_1: array [0..88-1] of Byte;
    UnkVal_1: Word; // 0x0001
    UnkVal_2: Word; // 0x0000
    TLMFileSize: Integer;
    Reserved_2: array [0..96-1] of Byte;
    StartFlag_3: Integer;
    MaxTilesPerBlockCount: Integer; // 0x63 (99)
    FirstBlockOffset: Integer;
    Reserved_3: array [0..3920-1] of Byte;
  end;
  PTLMFileHeader = ^TTLMFileHeader;

  TTLMFileTileMetaRec = packed record
    X: Integer;
    Y: Integer;
    UnkVal: Integer; // 0x0000
    Offset: Integer;
  end;
  PTLMFileTileMetaRec = ^TTLMFileTileMetaRec;

  TTLMFileBlock = packed record
    TilesMetaCount: Integer;
    TilesMetaCount_1: Word;
    BlockID: Word;
    TilesMeta: array [0..cTLMFileMaxTilesPerBlockCount-1] of TTLMFileTileMetaRec;
    BlocksOffset: array [0..cTLMFileMaxTilesPerBlockCount-1] of Integer;
    Reserved: Integer;
  end;
  PTLMFileBlock = ^TTLMFileBlock;

  TRMPFile = class;

  TRMPFileEntryWriter = class(TObject)
  private
    FStart: Int64;
    FIsOpened: Boolean;
    FRMPFile: TRMPFile;
    FMeta: TRMPEntryMeta;
  public
    constructor Create(const ARMPFile: TRMPFile);
  public
    function Position: Integer;
    function Seek(Offset: Longint; Origin: Word): Longint;
    procedure WriteBuffer(const Buffer; Count: Longint);
    procedure WriteFile(const AFileName: string);
    procedure Open(
      const AFileName: AnsiString;
      const AFileExt: AnsiString
    );
    procedure Close;
  end;

  TRMPFile = class(TObject)
  private
    FFileName: string;
    FFileNameTmp: string;
    FDataStartOffset: Integer;
    FPreallocEntriesCount: Integer;
    FEntriesMeta: array of TRMPEntryMeta;
    FStream: TFileStream;
    FOffset: Int64;
    FEntryWriter: TRMPFileEntryWriter;
    procedure Erase(const AStream: TStream; const ACount: Integer);
    function GetDataStartOffset(const AEntriesCount: Integer): Integer;
    function GetSize: Int64;
  public
    procedure AppendFromFile(const AFileName: string);
    procedure AppendFromDir(const ADirName: string);
    procedure AppendMeta(const AFileMeta: TRMPEntryMeta);
    procedure Finish;
    procedure IncOffset(const ACount: Integer);
    property Stream: TFileStream read FStream;
    property Offset: Int64 read FOffset write FOffset;
    property EntryWriter: TRMPFileEntryWriter read FEntryWriter;
    property Size: Int64 read GetSize;
  public
    constructor Create(
      const AFileName: string;
      const APreallocEntriesCount: Integer
    );
    destructor Destroy; override;
  end;

  TTLMFile = class(TObject)
  private
    FHeader: TTLMFileHeader;
    FBlocks: array of TTLMFileBlock;
    FBaseName: AnsiString;
    FLayerIndex: Integer;
    FTilesPerBlock: Integer;
    FMaxTilesCount: Integer;
    FLeftLimit: Double;
    FTopLimit: Double;
    FRightLimit: Double;
    FBottomLimit: Double;
    FIsHeaderBoundingInitilized: Boolean;
    procedure CheckBoundingLimit;
    procedure UpdateBoundingRect(const ALeft, ATop, ARight, ABottom: Double);
    function GetBlock: PTLMFileBlock;
    function GetTilesCount: Integer;
  public
    constructor Create(
      const ABaseName: AnsiString;
      const ALayerIndex: Integer;
      const ATilesPerBlock: Integer = cTLMFileTilesPerBlockCount
    );
    procedure AddTile(
      const AX, AY: Integer;
      const ALeft, ATop, ARight, ABottom: Double;
      const AOffset: Integer
    );
    procedure SetBoundingLimit(
      const ALeftLimit: Double;
      const ATopLimit: Double;
      const ARightLimit: Double;
      const ABottomLimit: Double
    );
    procedure WriteEntry(const AEntryWriter: TRMPFileEntryWriter);
    property MaxTilesCount: Integer read FMaxTilesCount;
    property TilesCount: Integer read GetTilesCount;
  end;

  TRMPIniFile = class(TObject)
  private
    FBaseName: AnsiString;
    FLayersCount: Integer;
  public
    constructor Create(
      const ABaseName: AnsiString;
      const ALayersCount: Integer
    );
    procedure WriteEntry(const AEntryWriter: TRMPFileEntryWriter);
  end;

  TRMPDescriptionFile = class(TObject)
  private
    FImgName: AnsiString;
    FProduct: AnsiString;
    FProvider: AnsiString;
    FComments: AnsiString;
  public
    constructor Create(
      const AImgName: AnsiString;
      const AProduct: AnsiString;
      const AProvider: AnsiString;
      const AComments: AnsiString
    );
    procedure WriteEntry(const AEntryWriter: TRMPFileEntryWriter);
  end;

  TRMPICSFileType = (icsBmp4bit, icsChunk);

  TRMPICSFile = class(TObject)
  private
    FType: TRMPICSFileType;
  public
    constructor Create(const AType: TRMPICSFileType);
    procedure WriteEntry(const AEntryWriter: TRMPFileEntryWriter);
  end;

  TRMPLayerWriter = class(TObject)
  private
    FName: AnsiString;
    FCount: Integer;
    FIsOpened: Boolean;
    FEntryWriter: TRMPFileEntryWriter;
    procedure CheckOpen;
    procedure CheckClose;
  public
    constructor Create(
      const ABaseName: AnsiString;
      const ALayerIndex: Integer;
      const AEntryWriter: TRMPFileEntryWriter
    );
    destructor Destroy; override;
  public
    procedure WriteTileData(
      const AData;
      const ASize: Integer;
      out ATileOffset: Integer
    );
  end;

procedure CalcFirstRmpTilePos(
  const ALeft, ATop, scalex, scaley: Double;
  out firstTilex, firstTiley: Integer
);

implementation

var
  cZeroByte: Byte = 0;
  cMagellan: AnsiString = 'MAGELLAN';

const
  cBufSize = $01000000; // 16 Mb
  cKnownExt: array [0..4] of string = ('.ini', '.msf', '.ics', '.a00', '.tlm');
  cCRLF: AnsiString = #13#10;

function BuildLayerName(
  const ABaseName: AnsiString;
  const ALayerIndex: Integer
): AnsiString; inline;
var
  VName, VIndex: AnsiString;
begin
  VName := ABaseName;
  VIndex := AnsiString(IntToStr(ALayerIndex));
  if Length(VName) + Length(VIndex) > 0 then begin
    VName := Copy(VName, 1, 8 - Length(VIndex));
  end;
  Result := VName + VIndex;
end;

function CopyFrom(ASrc, ADest: TStream; ACount: Int64; ABufSize: Int64 = cBufSize): Int64;
var
  I: Integer;
  VBuffer: PByte;
begin
  Result := 0;
  GetMem(VBuffer, ABufSize);
  try
    while ACount <> 0 do begin
      if ACount > ABufSize then I := ABufSize else I := ACount;
      ASrc.ReadBuffer(VBuffer^, I);
      ADest.WriteBuffer(VBuffer^, I);
      Dec(ACount, I);
      Inc(Result, I);
    end;
  finally
    FreeMem(VBuffer, cBufSize);
  end;
end;

{ TRMPFile }

constructor TRMPFile.Create(
  const AFileName: string;
  const APreallocEntriesCount: Integer
);
begin
  inherited Create;
  FFileName := AFileName;
  FFileNameTmp := FFileName + '.tmp';

  FPreallocEntriesCount := APreallocEntriesCount;
  FDataStartOffset := GetDataStartOffset(FPreallocEntriesCount);

  if FileExists(FFileName) then begin
    if not DeleteFile(FFileName) then begin
      RaiseLastOSError;
    end;
  end;

  if FileExists(FFileNameTmp) then begin
    if not DeleteFile(FFileNameTmp) then begin
      RaiseLastOSError;
    end;
  end;

  FStream := TFileStream.Create(FFileNameTmp, fmCreate or fmShareDenyWrite);

  FStream.Seek(0, soFromBeginning);
  Erase(FStream, FDataStartOffset);
  FStream.Seek(FDataStartOffset, soFromBeginning);

  FOffset := 0;
  SetLength(FEntriesMeta, 0);

  FEntryWriter := TRMPFileEntryWriter.Create(Self);
end;

destructor TRMPFile.Destroy;
begin
  FreeAndNil(FEntryWriter);
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TRMPFile.GetDataStartOffset(const AEntriesCount: Integer): Integer;
begin
  Result := 4 + 4 + AEntriesCount * SizeOf(TRMPEntryMeta) + 2 + 8 + 22;
end;

function TRMPFile.GetSize: Int64;
begin
  Result := FStream.Size;
end;

procedure TRMPFile.Erase(const AStream: TStream; const ACount: Integer);
var
  VBuffer: PByte;
begin
  GetMem(VBuffer, ACount);
  try
    FillChar(VBuffer^, ACount, 0);
    AStream.WriteBuffer(VBuffer^, ACount);
  finally
    FreeMem(VBuffer, ACount);
  end;
end;

procedure TRMPFile.IncOffset(const ACount: Integer);
begin
  Inc(FOffset, ACount);
end;

procedure TRMPFile.AppendFromFile(const AFileName: string);
var
  I: Integer;
  VStr: string;
  VName, VExt: AnsiString;
begin
  VStr := ExtractFileName(AFileName);
  I := Pos('.', VStr);
  if I > 0 then begin
    VName := AnsiString(Copy(VStr, 1, I - 1));
    VExt := AnsiString(Copy(VStr, I + 1, Length(VStr) - I));
    FEntryWriter.Open(VName, VExt);
    FEntryWriter.WriteFile(AFileName);
    FEntryWriter.Close;
  end else begin
    Assert(False);
  end;
end;

procedure TRMPFile.AppendFromDir(const ADirName: string);
var
  I: Integer;
  SR: TSearchRec;
  VDir, VExt: string;
  VFiles: TStringList;
begin
  VFiles := TStringList.Create;
  try
    VDir := ExcludeTrailingPathDelimiter(ADirName);
    if FindFirst(IncludeTrailingPathDelimiter(VDir) + '*.*', faAnyFile, SR) = 0 then begin
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') and ((SR.Attr and faDirectory) = 0) then begin
          VExt := LowerCase(ExtractFileExt(SR.Name));
          for I := Low(cKnownExt) to High(cKnownExt) do begin
            if VExt = cKnownExt[I] then begin
              VFiles.Add(IncludeTrailingPathDelimiter(VDir) + SR.Name);
              Break;
            end;
          end;
        end;
      until FindNext(SR) <> 0;
      FindClose(SR) ;
    end;
    for I := 0 to VFiles.Count - 1 do begin
      AppendFromFile(VFiles.Strings[I]);
    end;
  finally
    VFiles.Free;
  end;
end;

procedure TRMPFile.AppendMeta(const AFileMeta: TRMPEntryMeta);
var
  I: Integer;
begin
  Assert(AFileMeta.Size > 0);
  I := Length(FEntriesMeta);
  SetLength(FEntriesMeta, I + 1);
  Move(AFileMeta, FEntriesMeta[I], SizeOf(TRMPEntryMeta));
end;

procedure TRMPFile.Finish;
var
  I: Integer;
  VCount: Integer;
  VChecksum: Word;
  VEntriesCount: Integer;
  VDataStartOffset: Integer;
  VFileStream: TFileStream;
begin
  VEntriesCount := Length(FEntriesMeta);

  if VEntriesCount > FPreallocEntriesCount then begin
    VFileStream := FStream;
    try
      FStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
      VDataStartOffset := GetDataStartOffset(VEntriesCount);
      FStream.Seek(0, soFromBeginning);
      Erase(FStream, VDataStartOffset);
      FStream.Seek(VDataStartOffset, soFromBeginning);
      VFileStream.Seek(FDataStartOffset, soFromBeginning);
      VCount := VFileStream.Size - FDataStartOffset;
      I := CopyFrom(VFileStream, FStream, VCount);
      Assert(I = VCount);
    finally
      FreeAndNil(VFileStream);
    end;
    DeleteFile(FFileNameTmp);
  end else begin
    FreeAndNil(FStream);
    if not RenameFile(FFileNameTmp, FFileName) then begin
      RaiseLastOSError;
    end;
    FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyWrite);
    VDataStartOffset := FDataStartOffset;
  end;

  for I := 0 to VEntriesCount - 1 do begin
    Inc(FEntriesMeta[I].Offset, VDataStartOffset);
  end;

  FStream.Seek(0, soFromBeginning);
  FStream.WriteBuffer(VEntriesCount, SizeOf(VEntriesCount));
  FStream.WriteBuffer(VEntriesCount, SizeOf(VEntriesCount));
  FStream.WriteBuffer(FEntriesMeta[0], VEntriesCount * SizeOf(TRMPEntryMeta));
  VChecksum := $E5E5; // ToDo
  FStream.WriteBuffer(VChecksum, SizeOf(VChecksum));
  FStream.WriteBuffer(cMagellan[1], Length(cMagellan));

  FStream.Seek(0, soFromEnd);
  FStream.WriteBuffer(cMagellan[1], Length(cMagellan));
  VChecksum := $3B7D; // ToDo
  FStream.WriteBuffer(VChecksum, SizeOf(VChecksum));
end;

{ TRMPFileAppender }

constructor TRMPFileEntryWriter.Create(const ARMPFile: TRMPFile);
begin
  Assert(ARMPFile <> nil);
  FRMPFile := ARMPFile;
  FIsOpened := False;
end;

function TRMPFileEntryWriter.Seek(Offset: Integer; Origin: Word): Longint;
var
  I: Integer;
begin
  Assert(FIsOpened);
  I := Offset;
  if Origin = soFromBeginning then begin
    Inc(I, FStart);
  end;
  Result := FRMPFile.Stream.Seek(I, Origin);
end;

function TRMPFileEntryWriter.Position: Integer;
begin
  Assert(FIsOpened);
  Result := FRMPFile.Stream.Position - FStart;
  Assert(Result >= 0);
end;

procedure TRMPFileEntryWriter.WriteBuffer(const Buffer; Count: Integer);
begin
  Assert(FIsOpened);
  FRMPFile.Stream.WriteBuffer(Buffer, Count);
end;

procedure TRMPFileEntryWriter.WriteFile(const AFileName: string);
var
  VFileStream: TFileStream;
begin
  Assert(FIsOpened);
  VFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    CopyFrom(VFileStream, FRMPFile.Stream, VFileStream.Size);
  finally
    VFileStream.Free;
  end;
end;

procedure TRMPFileEntryWriter.Open(
  const AFileName: AnsiString;
  const AFileExt: AnsiString
);

  procedure _StrToBuff(const AStr: AnsiString; var ABuff; const ABuffLen: Integer);
  begin
    Assert(Length(AStr) < ABuffLen);
    FillChar(ABuff, ABuffLen, 0);
    Move(AStr[1], ABuff, Length(AStr));
  end;

begin
  Assert(not FIsOpened);
  Assert(AFileName <> '', 'Entry filename can''t be empty!');
  Assert(AFileExt <> '', 'Entry extention can''t be empty!');
  if Length(AFileName) > 8 then begin
    raise Exception.CreateFmt('Entry filename is too long: "%s"', [AFileName]);
  end;
  if Length(AFileExt) > 6 then begin
    raise Exception.CreateFmt('Entry extention is too long: "%s"', [AFileExt]);
  end;
  _StrToBuff(AFileName, FMeta.Name[0], Length(FMeta.Name));
  _StrToBuff(AFileExt, FMeta.Ext[0], Length(FMeta.Ext));
  FMeta.Size := 0;
  FMeta.Offset := FRMPFile.Offset;
  FStart := FRMPFile.Stream.Position;
  FIsOpened := True;
end;

procedure TRMPFileEntryWriter.Close;
begin
  FRMPFile.Stream.Seek(0, soFromEnd);
  FMeta.Size := FRMPFile.Stream.Position - FStart;
  Assert(FMeta.Size > 0);
  FRMPFile.AppendMeta(FMeta);
  FRMPFile.IncOffset(FMeta.Size);
  if FMeta.Size mod 2 <> 0 then begin
    FRMPFile.Stream.WriteBuffer(cZeroByte, 1);
    FRMPFile.IncOffset(1);
  end;
  FIsOpened := False;
end;

{ TTLMFile }

constructor TTLMFile.Create(
  const ABaseName: AnsiString;
  const ALayerIndex: Integer;
  const ATilesPerBlock: Integer
);
begin
  Assert(SizeOf(TTLMFileHeader) = $105C { 4188 bytes}, 'TLM file HEADER size mismatch!');
  Assert(SizeOf(TTLMFileBlock) = $07C8 { 1992 bytes}, 'TLM file BLOCK size mismatch!');

  Assert(ATilesPerBlock < cTLMFileMaxTilesPerBlockCount,
    Format('Tiles per block can''t be more then %d', [cTLMFileMaxTilesPerBlockCount])
  );

  inherited Create;

  FBaseName := ABaseName;
  FLayerIndex := ALayerIndex;
  FTilesPerBlock := ATilesPerBlock;

  FLeftLimit := NaN;
  FTopLimit := NaN;
  FRightLimit := NaN;
  FBottomLimit := NaN;

  FMaxTilesCount := (FTilesPerBlock - 1) * FTilesPerBlock;

  FillChar(FHeader, SizeOf(FHeader), 0);
  FHeader.StartFlag_1 := 1;
  FHeader.TileHeight := 256;
  FHeader.TileWidth := 256;
  FHeader.StartFlag_2 := 1;
  FHeader.UnkVal_1 := 256;
  FHeader.StartFlag_3 := 1;
  FHeader.MaxTilesPerBlockCount := cTLMFileMaxTilesPerBlockCount;

  FIsHeaderBoundingInitilized := False;
end;

function TTLMFile.GetBlock: PTLMFileBlock;

const
  cFirstDataBlockOffset = $0F5C;
  cIndexBlockOffset = $1724;
  cIndexBlockIdx = 1; // Item number in FBlocks array wich used as Index Block

  function _New(const AIsIndex: Boolean): PTLMFileBlock;
  var
    I: Integer;
  begin
    I := Length(FBlocks);
    SetLength(FBlocks, I + 1);
    FillChar(FBlocks[I], SizeOf(TTLMFileBlock), 0);
    Result := @FBlocks[I];
    if AIsIndex then begin
      Result.BlockID := cTLMFileIndexBlockID;
    end else begin
      Result.BlockID := cTLMFileDataBlockID;
    end;
  end;

var
  I: Integer;
  VBlock: PTLMFileBlock;
begin
  I := Length(FBlocks);
  if I = 0 then begin
    Result := _New(False);
    FHeader.FirstBlockOffset := cFirstDataBlockOffset;
  end else begin
    VBlock := @FBlocks[I-1];
    Assert(VBlock.BlockID = cTLMFileDataBlockID);
    if VBlock.TilesMetaCount_1 < FTilesPerBlock then begin
      Result := VBlock;
    end else begin

      // When we start a new additional block with data, we must write first
      // tile meta and additional block offset to the index.
      // Additional blocks contain tiles meta starting from second tile only.

      if Length(FBlocks) = 1 then begin
        // create block with index
        VBlock := _New(True);
        // save existing block offset to index and fix header - now it points
        // to the index block
        VBlock.BlocksOffset[0] := cFirstDataBlockOffset;
        FHeader.FirstBlockOffset := cIndexBlockOffset;
      end;

      // add new block with data (for the next writes)
      _New(False);

      // save new block offset
      VBlock := @FBlocks[cIndexBlockIdx];

      I := VBlock.TilesMetaCount_1;

      VBlock.BlocksOffset[I+1] :=
        cFirstDataBlockOffset +
        $0F90 + // ??
        SizeOf(TTLMFileBlock) * I;

      Result := VBlock;
    end;
  end;
end;

function TTLMFile.GetTilesCount: Integer;
begin
  Result := FHeader.TilesCount;
end;

procedure TTLMFile.CheckBoundingLimit;
begin
  if not FIsHeaderBoundingInitilized then begin
    Exit;
  end;
  if not IsNan(FLeftLimit) and (FHeader.Left < FLeftLimit) then begin
    FHeader.Left := FLeftLimit;
  end;
  if not IsNan(FTopLimit) and (FHeader.Top > FTopLimit) then begin
    FHeader.Top := FTopLimit;
  end;
  if not IsNan(FRightLimit) and (FHeader.Right > FRightLimit) then begin
    FHeader.Right := FRightLimit;
  end;
  if not IsNan(FBottomLimit) and (FHeader.Bottom < FBottomLimit) then begin
    FHeader.Bottom := FBottomLimit;
  end;
end;

procedure TTLMFile.SetBoundingLimit(
  const ALeftLimit: Double;
  const ATopLimit: Double;
  const ARightLimit: Double;
  const ABottomLimit: Double
);
begin
  FLeftLimit := ALeftLimit;
  FTopLimit := ATopLimit;
  FRightLimit := ARightLimit;
  FBottomLimit := ABottomLimit;

  CheckBoundingLimit;
end;

procedure TTLMFile.UpdateBoundingRect(const ALeft, ATop, ARight, ABottom: Double);
begin
  if not FIsHeaderBoundingInitilized then begin
    FHeader.Left := ALeft;
    FHeader.Top := ATop;
    FHeader.Right := ARight;
    FHeader.Bottom := ABottom;

    FIsHeaderBoundingInitilized := True;
  end else begin
    if ALeft < FHeader.Left then begin
      FHeader.Left := ALeft;
    end;
    if ATop > FHeader.Top then begin
      FHeader.Top := ATop;
    end;
    if ARight > FHeader.Right then begin
      FHeader.Right := ARight;
    end;
    if ABottom < FHeader.Bottom then begin
      FHeader.Bottom := ABottom;
    end;
  end;

  CheckBoundingLimit;
end;

procedure TTLMFile.AddTile(
  const AX, AY: Integer;
  const ALeft, ATop, ARight, ABottom: Double;
  const AOffset: Integer
);
var
  I: Integer;
  VBlock: PTLMFileBlock;
begin
  Assert(FHeader.TilesCount < FMaxTilesCount, 'TLM tiles count overflow!');

  if FHeader.TilesCount = 0 then begin
    FHeader.TileHeightResolution := Abs(ATop - ABottom);
    FHeader.TileWidthResolution := Abs(ARight - ALeft);
  end;

  UpdateBoundingRect(ALeft, ATop, ARight, ABottom);

  VBlock := GetBlock;

  I := VBlock.TilesMetaCount_1;

  with VBlock.TilesMeta[I] do begin
    X := AX;
    Y := AY;
    Offset := AOffset;
  end;

  Inc(VBlock.TilesMetaCount);
  Inc(VBlock.TilesMetaCount_1);
  Inc(FHeader.TilesCount);
end;

procedure TTLMFile.WriteEntry(const AEntryWriter: TRMPFileEntryWriter);
var
  VBuffer: PByte;
  VSize: Integer;
  VBlockSize: Integer;
  VLayerName: AnsiString;
begin
  VLayerName := BuildLayerName(FBaseName, FLayerIndex);

  if (FHeader.TilesCount <= 0) or (Length(FBlocks) = 0) then begin
    raise Exception.CreateFmt('TLM file %s.tlm is empty!', [VLayerName]);
  end;

  AEntryWriter.Open(VLayerName, 'tlm');

  VBlockSize := SizeOf(TTLMFileBlock);
  FHeader.TLMFileSize := SizeOf(FHeader) + (Length(FBlocks) + 2) * VBlockSize;

  // (!!!) North and South have to be negated - this really strange!
  FHeader.Top := -FHeader.Top;
  FHeader.Bottom := -FHeader.Bottom;

  // (!!) save total number of tiles in TLM file to the Index
  if Length(FBlocks) > 1 then begin
    FBlocks[1].TilesMetaCount := FHeader.TilesCount;
  end;

  AEntryWriter.WriteBuffer(FHeader, SizeOf(FHeader));
  AEntryWriter.WriteBuffer(FBlocks[0], Length(FBlocks) * VBlockSize);

  // add 2 empty blocks to the end
  VSize := VBlockSize*2;
  GetMem(VBuffer, VSize);
  try
    FillChar(VBuffer^, VSize, 0);
    AEntryWriter.WriteBuffer(VBuffer^, VSize);
  finally
    FreeMem(VBuffer, VSize);
  end;

  AEntryWriter.Close;
end;

{ TRMPIniFile }

constructor TRMPIniFile.Create(
  const ABaseName: AnsiString;
  const ALayersCount: Integer
);
begin
  inherited Create;
  FBaseName := ABaseName;
  FLayersCount := ALayersCount;
end;

procedure TRMPIniFile.WriteEntry(const AEntryWriter: TRMPFileEntryWriter);
var
  I: Integer;
  VData: AnsiString;
begin
  VData := '[T_Layers]' + cCRLF;
  for I := 0 to FLayersCount - 1 do begin
    VData := VData +
      AnsiString(Format('%d=%s', [I, BuildLayerName(FBaseName, I+1)])) + cCRLF;
  end;
  VData := VData + #0;

  AEntryWriter.Open('rmp', 'ini');
  AEntryWriter.WriteBuffer(VData[1], Length(VData));
  AEntryWriter.Close;
end;

{ TRMPDescriptionFile }

constructor TRMPDescriptionFile.Create(const AImgName, AProduct, AProvider,
  AComments: AnsiString);
begin
  inherited Create;
  FImgName := AImgName;
  FProduct := AProduct;
  FProvider := AProvider;
  FComments := AComments;
end;

procedure TRMPDescriptionFile.WriteEntry(
  const AEntryWriter: TRMPFileEntryWriter
);
var
  VData: AnsiString;
begin
  VData :=
    ';Map Support File : Contains Meta Data Information about the Image' + cCRLF +
    'IMG_NAME = ' + FImgName + cCRLF +
    'PRODUCT = ' + FProduct + cCRLF +
    'PROVIDER = ' + FProvider + cCRLF +
    'IMG_DATE = ' + AnsiString(DateToStr(Date)) + ' ' + AnsiString(TimeToStr(Time)) + cCRLF +
    'IMG_VERSION = 31' + cCRLF +
    'Version = 31' + cCRLF +
    'BUILD=' + cCRLF +
    'VENDOR_ID = -1' + cCRLF +
    'REGION_ID = -1' + cCRLF +
    'MAP_TYPE = TNDB_RASTER_MAP' + cCRLF +
    'ADDITIONAL_COMMENTS = ' + FComments + cCRLF;

  AEntryWriter.Open('cvg_map', 'msf');
  AEntryWriter.WriteBuffer(VData[1], Length(VData));
  AEntryWriter.Close;
end;

{$REGION 'ICS_FILES_CONTENT'}

const
  bmp4bit_ics: array[0..575] of Byte = (
	$49, $63, $6F, $6E, $20, $66, $69, $6C, $65, $20, $76, $65, $72, $73, $69, $6F,
	$6E, $20, $31, $2E, $30, $2E, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$03, $00, $00, $00, $00, $01, $00, $00, $3C, $00, $00, $00, $F0, $FF, $FF, $7F,
	$58, $01, $00, $00, $F1, $FF, $FF, $7F, $F4, $01, $00, $00, $00, $01, $00, $00,
	$00, $00, $00, $00, $58, $00, $00, $00, $D8, $00, $00, $00, $00, $00, $00, $00,
	$10, $00, $10, $00, $04, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$03, $33, $33, $33, $33, $33, $33, $30, $03, $00, $00, $00, $00, $00, $00, $30,
	$03, $00, $00, $00, $00, $00, $00, $30, $03, $00, $00, $00, $00, $00, $00, $30,
	$03, $00, $00, $00, $00, $00, $00, $30, $03, $00, $00, $00, $00, $00, $00, $30,
	$03, $00, $00, $00, $00, $00, $00, $30, $03, $00, $00, $00, $00, $00, $00, $30,
	$03, $00, $00, $00, $00, $00, $00, $30, $03, $00, $00, $00, $00, $00, $00, $30,
	$03, $00, $00, $00, $00, $00, $00, $30, $03, $00, $00, $00, $00, $00, $00, $30,
	$03, $00, $00, $00, $00, $00, $00, $30, $03, $33, $33, $33, $33, $33, $33, $30,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $0F, $FF, $FF, $FF, $FF, $F0, $00, $00, $0F, $FF, $FF, $FF, $FF, $F0, $00,
	$00, $0F, $FF, $FF, $FF, $FF, $F0, $00, $00, $0F, $FF, $FF, $FF, $FF, $F0, $00,
	$00, $0F, $FF, $FF, $FF, $FF, $F0, $00, $00, $0F, $FF, $FF, $FF, $FF, $F0, $00,
	$00, $0F, $FF, $FF, $FF, $FF, $F0, $00, $00, $0F, $FF, $FF, $FF, $FF, $F0, $00,
	$00, $0F, $FF, $FF, $FF, $FF, $F0, $00, $00, $0F, $FF, $FF, $FF, $FF, $F0, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $F0, $FF, $FF, $7F, $00, $00, $00, $00,
	$74, $01, $00, $00, $B4, $01, $00, $00, $00, $00, $00, $00, $01, $00, $40, $00,
	$04, $08, $00, $00, $51, $FD, $D8, $BA, $84, $18, $9D, $ED, $2E, $DC, $FD, $EB,
	$80, $62, $60, $B5, $52, $9D, $4E, $6C, $B1, $12, $F7, $DC, $EC, $A8, $D7, $54,
	$9D, $58, $62, $28, $4A, $5D, $76, $E1, $B7, $C2, $9E, $D5, $D7, $0C, $B5, $3C,
	$45, $36, $CA, $D8, $33, $B7, $EE, $24, $53, $56, $F8, $50, $FE, $69, $C3, $86,
	$25, $72, $07, $F9, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF,
	$FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF,
	$00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF,
	$FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF,
	$FF, $FF, $FF, $00, $F1, $FF, $FF, $7F, $00, $00, $00, $00, $10, $02, $00, $00,
	$28, $02, $00, $00, $00, $00, $00, $00, $01, $00, $18, $00, $04, $08, $00, $00,
	$31, $32, $33, $34, $35, $36, $37, $38, $39, $30, $61, $62, $63, $64, $65, $66,
	$67, $68, $69, $6A, $6B, $6C, $6D, $6E, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00,
	$FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF
);

  chunk_ics: array[0..1755] of Byte = (
	$49, $63, $6F, $6E, $20, $66, $69, $6C, $65, $20, $76, $65, $72, $73, $69, $6F,
	$6E, $20, $31, $2E, $30, $2E, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$0A, $00, $00, $00, $00, $01, $00, $00, $74, $00, $00, $00, $01, $01, $00, $00,
	$10, $01, $00, $00, $02, $01, $00, $00, $AC, $01, $00, $00, $03, $01, $00, $00,
	$48, $02, $00, $00, $04, $01, $00, $00, $E4, $02, $00, $00, $05, $01, $00, $00,
	$80, $03, $00, $00, $06, $01, $00, $00, $1C, $04, $00, $00, $07, $01, $00, $00,
	$B8, $04, $00, $00, $F0, $FF, $FF, $7F, $F4, $05, $00, $00, $F1, $FF, $FF, $7F,
	$90, $06, $00, $00, $00, $01, $00, $00, $FD, $FF, $02, $00, $90, $00, $00, $00,
	$D0, $00, $00, $00, $FF, $FF, $FF, $FF, $10, $00, $10, $00, $04, $02, $00, $00,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $20, $20, $20, $20, $20, $20,
	$54, $72, $69, $74, $6F, $6E, $20, $52, $4D, $50, $20, $6D, $61, $6B, $65, $72,
	$20, $20, $20, $20, $20, $FF, $FF, $FF, $FF, $FF, $20, $20, $20, $20, $20, $20,
	$77, $77, $77, $2E, $6D, $73, $68, $2D, $74, $6F, $6F, $6C, $73, $2E, $63, $6F,
	$6D, $20, $20, $32, $30, $30, $38, $20, $20, $20, $20, $20, $20, $20, $20, $FF,
	$00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $00, $FF, $00, $00, $00, $FF,
	$00, $00, $00, $FF, $C0, $00, $03, $FF, $FF, $FF, $CF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$01, $01, $00, $00, $FD, $FF, $02, $00, $2C, $01, $00, $00, $6C, $01, $00, $00,
	$FF, $FF, $FF, $FF, $10, $00, $10, $00, $04, $02, $00, $00, $00, $00, $00, $00,
	$00, $00, $30, $00, $00, $FF, $F0, $00, $0F, $FF, $F0, $00, $3F, $FF, $CC, $00,
	$3F, $FF, $CC, $00, $0F, $FF, $F0, $00, $00, $FF, $F0, $00, $00, $00, $30, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $03, $FF,
	$F0, $00, $00, $FF, $C0, $00, $00, $FF, $00, $00, $00, $FF, $00, $00, $00, $3F,
	$00, $00, $00, $3F, $00, $00, $00, $FF, $C0, $00, $00, $FF, $F0, $00, $00, $FF,
	$FF, $00, $03, $FF, $FF, $FF, $CF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $02, $01, $00, $00,
	$FD, $FF, $FD, $FF, $C8, $01, $00, $00, $08, $02, $00, $00, $FF, $FF, $FF, $FF,
	$10, $00, $10, $00, $04, $02, $00, $00, $00, $00, $00, $00, $3F, $FF, $00, $00,
	$3F, $FF, $00, $00, $3F, $FF, $00, $00, $3F, $FF, $00, $00, $3F, $FF, $00, $00,
	$3F, $FF, $00, $00, $3F, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3F, $FF, $00, $00, $0F, $FF,
	$00, $00, $0F, $FF, $00, $00, $0F, $FF, $00, $00, $0F, $FF, $00, $00, $0F, $FF,
	$00, $00, $0F, $FF, $00, $00, $0F, $FF, $00, $00, $3F, $FF, $C0, $00, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $03, $01, $00, $00, $FE, $FF, $FD, $FF,
	$64, $02, $00, $00, $A4, $02, $00, $00, $FF, $FF, $FF, $FF, $10, $00, $10, $00,
	$04, $02, $00, $00, $00, $00, $00, $00, $00, $3C, $00, $00, $00, $CC, $00, $00,
	$03, $0C, $00, $00, $0C, $0C, $00, $00, $30, $0C, $00, $00, $0C, $0C, $00, $00,
	$03, $0C, $00, $00, $00, $CC, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $FF, $00, $FF, $FF, $FC, $00, $3F, $FF, $F0, $00, $3F, $FF,
	$C0, $00, $3F, $FF, $00, $00, $3F, $FF, $00, $00, $3F, $FF, $00, $00, $3F, $FF,
	$C0, $00, $3F, $FF, $F0, $00, $3F, $FF, $FC, $00, $3F, $FF, $FF, $00, $FF, $FF,
	$FF, $C3, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $04, $01, $00, $00, $FA, $FF, $FA, $FF, $00, $03, $00, $00,
	$40, $03, $00, $00, $FF, $FF, $FF, $FF, $10, $00, $10, $00, $04, $02, $00, $00,
	$00, $00, $00, $00, $3C, $00, $00, $00, $3F, $C0, $00, $00, $0C, $F0, $00, $00,
	$0F, $0C, $00, $00, $03, $03, $C0, $00, $00, $C0, $30, $00, $00, $30, $0F, $00,
	$00, $30, $00, $C0, $00, $0C, $00, $30, $00, $03, $00, $30, $00, $03, $00, $30,
	$00, $00, $C0, $C0, $00, $00, $3F, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $3F, $FF, $FF, $00, $0F, $FF, $FF, $00, $03, $FF, $FF, $00, $00, $3F, $FF,
	$00, $00, $0F, $FF, $C0, $00, $00, $FF, $F0, $00, $00, $3F, $FC, $00, $00, $0F,
	$FC, $00, $00, $03, $FF, $00, $00, $00, $FF, $C0, $00, $00, $FF, $C0, $00, $00,
	$FF, $F0, $00, $03, $FF, $FC, $00, $0F, $FF, $FF, $00, $3F, $FF, $FF, $C0, $FF,
	$05, $01, $00, $00, $FA, $FF, $FA, $FF, $9C, $03, $00, $00, $DC, $03, $00, $00,
	$FF, $FF, $FF, $FF, $10, $00, $10, $00, $04, $02, $00, $00, $00, $00, $00, $00,
	$3C, $00, $00, $00, $3F, $C0, $00, $00, $0F, $F0, $00, $00, $0F, $FC, $00, $00,
	$03, $FF, $C0, $00, $00, $FF, $F0, $00, $00, $3F, $FF, $00, $00, $3F, $FF, $C0,
	$00, $0F, $FF, $F0, $00, $03, $FF, $F0, $00, $03, $FF, $F0, $00, $00, $FF, $C0,
	$00, $00, $3F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3F, $FF, $FF,
	$00, $0F, $FF, $FF, $00, $03, $FF, $FF, $00, $00, $3F, $FF, $00, $00, $0F, $FF,
	$C0, $00, $00, $FF, $F0, $00, $00, $3F, $FC, $00, $00, $0F, $FC, $00, $00, $03,
	$FF, $00, $00, $00, $FF, $C0, $00, $00, $FF, $C0, $00, $00, $FF, $F0, $00, $03,
	$FF, $FC, $00, $0F, $FF, $FF, $00, $3F, $FF, $FF, $C0, $FF, $06, $01, $00, $00,
	$06, $00, $FA, $FF, $38, $04, $00, $00, $78, $04, $00, $00, $FF, $FF, $FF, $FF,
	$10, $00, $10, $00, $04, $02, $00, $00, $00, $00, $00, $00, $3F, $FC, $00, $00,
	$00, $03, $C0, $00, $3F, $F0, $3C, $00, $00, $0F, $03, $00, $3F, $C0, $F3, $00,
	$00, $3C, $30, $C0, $3F, $03, $0C, $C0, $00, $F0, $CC, $30, $3F, $0C, $C3, $30,
	$00, $CC, $33, $30, $00, $33, $33, $30, $00, $33, $33, $30, $00, $33, $33, $30,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3F, $FF, $00, $00, $03, $FF,
	$00, $00, $00, $FF, $00, $00, $00, $3F, $00, $00, $00, $0F, $00, $00, $00, $0F,
	$00, $00, $00, $03, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $C0, $00, $00, $00, $FC, $00, $00, $00, $FC, $00, $00, $00,
	$FF, $00, $00, $03, $FF, $CC, $CC, $CF, $07, $01, $00, $00, $FC, $FF, $FC, $FF,
	$D4, $04, $00, $00, $64, $05, $00, $00, $FF, $FF, $FF, $FF, $18, $00, $18, $00,
	$04, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $C0, $00, $00, $00,
	$00, $00, $C0, $00, $00, $00, $03, $00, $C0, $30, $00, $00, $00, $C0, $C0, $C0,
	$00, $00, $00, $33, $F3, $00, $00, $00, $00, $0C, $0C, $00, $00, $00, $00, $30,
	$03, $00, $00, $00, $3F, $F0, $03, $FF, $00, $00, $00, $30, $03, $00, $00, $00,
	$00, $0C, $0C, $00, $00, $00, $00, $33, $F3, $00, $00, $00, $00, $C0, $C0, $C0,
	$00, $00, $03, $00, $C0, $30, $00, $00, $00, $00, $C0, $00, $00, $00, $00, $00,
	$C0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $FF, $FC, $0F, $FF, $FF, $FF, $FC, $F0, $03, $CF, $FF, $FF,
	$F0, $30, $03, $03, $FF, $FF, $C0, $00, $00, $00, $FF, $FF, $F0, $00, $00, $03,
	$FF, $FF, $FC, $00, $00, $0F, $FF, $FF, $C0, $00, $00, $00, $FF, $FF, $00, $00,
	$00, $00, $3F, $FF, $00, $00, $C0, $00, $0F, $FF, $00, $00, $00, $00, $3F, $FF,
	$C0, $00, $00, $00, $FF, $FF, $FC, $00, $00, $0F, $FF, $FF, $F0, $00, $00, $03,
	$FF, $FF, $C0, $00, $00, $00, $FF, $FF, $F0, $30, $03, $03, $FF, $FF, $FC, $F0,
	$03, $CF, $FF, $FF, $FF, $FC, $0F, $FF, $FF, $FF, $FF, $FF, $3F, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $F0, $FF, $FF, $7F, $00, $00, $00, $00, $10, $06, $00, $00,
	$50, $06, $00, $00, $FF, $FF, $FF, $FF, $01, $00, $40, $00, $04, $08, $00, $00,
	$51, $FD, $D8, $BA, $84, $18, $9D, $ED, $2E, $DC, $FD, $EB, $80, $62, $60, $B5,
	$52, $9D, $4E, $6C, $B1, $12, $F7, $DC, $EC, $A8, $D7, $54, $9D, $58, $62, $28,
	$4A, $5D, $76, $E1, $B7, $C2, $9E, $D5, $D7, $0C, $B5, $3C, $45, $36, $CA, $D8,
	$33, $B7, $EE, $24, $53, $56, $F8, $50, $FE, $69, $C3, $86, $25, $72, $07, $F9,
	$00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF,
	$FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF,
	$FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF,
	$FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00,
	$F1, $FF, $FF, $7F, $00, $00, $00, $00, $AC, $06, $00, $00, $C4, $06, $00, $00,
	$FF, $FF, $FF, $FF, $01, $00, $18, $00, $04, $08, $00, $00, $31, $32, $33, $34,
	$35, $36, $37, $38, $39, $30, $61, $62, $63, $64, $65, $66, $67, $68, $69, $6A,
	$6B, $6C, $6D, $6E, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF, $FF, $FF,
	$FF, $FF, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $FF, $FF
);

{$ENDREGION}

{ TRMPICSFile }

constructor TRMPICSFile.Create(const AType: TRMPICSFileType);
begin
  inherited Create;
  FType := AType;
end;

procedure TRMPICSFile.WriteEntry(const AEntryWriter: TRMPFileEntryWriter);
begin
  case FType of
    icsBmp4bit: begin
      AEntryWriter.Open('BMP4BIT', 'ICS');
      AEntryWriter.WriteBuffer(bmp4bit_ics[0], Length(bmp4bit_ics));
      AEntryWriter.Close;
    end;
    icsChunk: begin
      AEntryWriter.Open('chunk', 'ics');
      AEntryWriter.WriteBuffer(chunk_ics[0], Length(chunk_ics));
      AEntryWriter.Close;
    end;
  else
    Assert(False, 'Unknown ICS file type!');
  end;
end;

{ TRMPLayerWriter }

constructor TRMPLayerWriter.Create(
  const ABaseName: AnsiString;
  const ALayerIndex: Integer;
  const AEntryWriter: TRMPFileEntryWriter
);
begin
  inherited Create;
  FCount := 0;
  FEntryWriter := AEntryWriter;
  FName := BuildLayerName(ABaseName, ALayerIndex);
end;

destructor TRMPLayerWriter.Destroy;
begin
  CheckClose;
  FEntryWriter := nil;
  inherited Destroy;
end;

procedure TRMPLayerWriter.CheckClose;
begin
  if FIsOpened then begin
    FEntryWriter.Close;
    FIsOpened := False;
  end;
end;

procedure TRMPLayerWriter.CheckOpen;
begin
  if not FIsOpened then begin
    FEntryWriter.Open(FName, 'a00');
    FEntryWriter.WriteBuffer(FCount, SizeOf(Integer));
    FIsOpened := True;
  end;
end;

procedure TRMPLayerWriter.WriteTileData(
  const AData;
  const ASize: Integer;
  out ATileOffset: Integer
);
begin
  CheckOpen;

  Inc(FCount);

  ATileOffset := FEntryWriter.Position;

  FEntryWriter.Seek(0, soFromBeginning);
  FEntryWriter.WriteBuffer(FCount, 4);

  FEntryWriter.Seek(ATileOffset, soFromBeginning);
  FEntryWriter.WriteBuffer(ASize, 4);
  FEntryWriter.WriteBuffer(AData, ASize);
end;

// *****************************************************************************
//
// Calculate RMP-specific first (top-left) tile position.
//
// Input params:
//
// ALeft - longitude of top-left tile corner
// ATop - latitude of top-left tile corner
// scalex - tile resolution in degree by X
// scaley - tile resolution in degree by Y
//
// copy-pasted from RMPCreator/geo_raster.pas
//
procedure CalcFirstRmpTilePos(
  const ALeft, ATop, scalex, scaley: Double;
  out firstTilex, firstTiley: Integer
);
var
  i: Integer;
  x, y: Integer;
  tlx, tly, cmin, cmax: Double;
begin
  tlx := ALeft;
  tly := -ATop; // (!!!)

  firstTilex := 0;
  firstTiley := 0;

  x := Round( ceil( (tlx + 180) / abs(scalex) ) - 10 );
  y := 22;
  for i:=x to x + 20 do begin
    cmin := i * scalex;
    if (cmin < 0) then cmin := 0 - (cmin + 180) else cmin := cmin - 180;

    cmax := (i + 1) * scalex;
    if (cmax < 0) then cmax := 0 - (cmax + 180) else cmax := cmax - 180;

    if (cmin <= tlx) and (tlx < cmax ) then begin
      firstTilex := i;
      y := 1;
      break;
    end;
  end;

  if (y = 22) then begin
    raise Exception.Create('[RMP] Can''t get tile X');
  end;

  y := Round( ceil( (tly + 90) / abs(scaley) ) - 10 );
  x := 22;
  for i:=y to y + 20 do begin
    cmin := i * scaley;
    if (cmin < 0) then cmin := 0 - (cmin + 90) else cmin := cmin - 90;

    cmax := (i + 1) * scaley;
    if (cmax < 0) then cmax := 0 - (cmax + 90) else cmax := cmax - 90;

    if (cmin <= tly) and (tly < cmax ) then begin
      firstTiley := i;
      x := 1;
      break;
    end;
  end;

  if (x = 22) then begin
    raise Exception.Create('[RMP] Can''t get tile Y');
  end;
end;

end.
