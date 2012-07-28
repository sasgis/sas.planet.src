unit YaMobileCache;

interface

uses
  Windows,
  SysUtils,
  Classes,
  MD5,
  i_BinaryData;

type
  TTileStream = record
    Data: IBinaryData;
    Point: TPoint;
    Zoom: Byte;
    MapVersion: Word;
  end;

  TYaMobileCacheHeader = packed record                                            // Формат заголовка файла кэша
    Magic: LongWord;                                                              // 'YMCF' -> Yandex Mobile Cache File
    Size: Word;                                                                   // Размер заголовка, в килобайтах (по-умолчанию 32)
    FormatVersion: Word;                                                          // Верси формата заголовка = 1
    CreationPlatform: LongWord;                                                   // Платформа, на которой создали: 'SYMB', 'ANDR', 'IOSX'
    RegularBlockSize: Word;                                                       // Размер блока регулярных данных, в килобайтах = 32
    RemainderFragmentSize: Word;                                                  // Размер фрагмента блока остаточных данных, в килобайтах = 1 ?? а в реале всегда = 0 ??
    BitTable: array [0..8191] of Byte;                                            // Битовая таблица свободных блоков данных (8k)
    RemainderTable: array [0..255] of Word;                                       // Таблица номеров остаточных блоков данных (512 byte)
    Reserved: array [0..495] of Byte;                                             // Резерв (496 byte)
    FirstRemainderBlock: array [0..23551] of Byte;                                // Первый остаточный блок (23k)
  end;

  TYaMobileOffsetTable = packed record                                            // Формат таблицы смещений
    BlockNumber: array [0..16383] of Word;                                        // Номер блока
  end;

  TYaMobileTileDataElementsTable = packed record                                  // Формат записи в таблице разметки данных внутри тайла
    DataID: LongWord;                                                             // Идентификатор данных (сейчас всегда = 0)
    DataSize: LongWord;                                                           // Размер данных
  end;

  TYaMobileTileData = packed record                                               // Формат записи хранения тайла
    Magic: LongWord;                                                              // 'YTLD'
    ElementsCount: Word;                                                          // Число записей в таблице разметки (пока всегда = 1)
    FormatVersion: Word;                                                          // Версия формата = 1
    MD5: array [0..15] of Byte;                                                   // Контрольная сумма
    MapVersion: Word;                                                             // Версия карт-основы
    DateTime: LongWord;                                                           // Время (пока не используется = 0)
    ElementsTable: array of TYaMobileTileDataElementsTable;                       // Таблица разметки данных внутри тайла
    Data: Pointer;                                                                // Данные
  end;

  TYaMobileDataTableElement = packed record                                       // Формат таблицы размещения данных
    Size: LongWord;                                                               // Размер элемента
    Index: Word;                                                                  // Индекс тайла
  end;

  TYaMobileRegularBlock = packed record                                           // Формат регулярного блока данных (начинающего группу)
    Magic: LongWord;                                                              // 'YBLK' -> Yandex Block
    FormatVersion: Word;                                                          // Версия формата блока = 1
    Flags: Byte;                                                                  // Флаги (11000000 -> регулярный блок + начало группы)
    NextBlocksTableSize: Byte;                                                    // Размер таблицы размещения следующих блоков
    DataCellsCount: Word;                                                         // Количество ячеек данных
    NextBlocksTable: array of Word;                                               // Таблица размещения следующих блоков
    DataTable: array of TYaMobileDataTableElement;                                // Таблица размещения самих данных
    Tiles: array of TYaMobileTileData;                                            // Данные (YTLD)
  end;

  TYaMobileCacheFile = class
  private
    FInitialized: Boolean;
    FFilePath: string;
    FReplaceTiles: Boolean;
    FStream: TFileStream;
    FHeader: TYaMobileCacheHeader;
    FOffsetTable: TYaMobileOffsetTable;
    FRegularBlock: TYaMobileRegularBlock;
    FRegularBlockNumber: Word;
    function InitializeCacheStructures: Boolean;
    // Заголовок
    function InitializeHeader: Boolean;
    function ReadHeader: Boolean;
    function WriteHeader: Boolean;
    // Таблица смещений
    function InitializeOffsetTable: Boolean;
    function ReadOffsetTable: Boolean;
    function WriteOffsetTable: Boolean;
    // Регулярный блок
    function InitializeRegularBlock: Boolean;
    function ReadRegularBlock(
      var ARegularBlock: TYaMobileRegularBlock;
      ABlockNumber: Word
    ): Boolean;
    function WriteRegularBlock(
      var ARegularBlock: TYaMobileRegularBlock;
      ABlockNumber: Word
    ): Boolean;
    function AddTileDataToRegularBlock(
      var ARegularBlock: TYaMobileRegularBlock;
      ATile: TTileStream
    ): Boolean;
    function RemoveTileDataFromRegularBlock(
      var ARegularBlock: TYaMobileRegularBlock;
      ATileIndex: Word
    ): Boolean;
    function RegularBlockOverflow(
      var ARegularBlock: TYaMobileRegularBlock;
      AAddTileDataSize: Integer
    ): Boolean;
    // Тайловый блок
    procedure CreateTileDataBlock(
      ATile: TTileStream;
      var ATileData: TYaMobileTileData
    );
    procedure FreeTileDataBlock(var ATileData: TYaMobileTileData);
    function ReadTileDataBlock(
      var ATileData: TYaMobileTileData;
      AStreamPos: Int64
    ): Boolean;
    function WriteTileDataBlock(
      var ATileData: TYaMobileTileData;
      AStreamPos: Int64
    ): Boolean;
    // Вспомогательные функции
    function GetFirstEmptyBlockNumber(out ABlockNumber: Word): Boolean; inline;
    procedure MarkBlockNumberAsNotEmpty(ABlockNumber: Word); inline;
    procedure MarkBlockNumberAsEmpty(ABlockNumber: Word); inline;
    function GetTileIndex(APoint: TPoint): Integer; inline;
    function GetTileDataBlock(ATileIndex: Word): Word; inline;
    procedure SetTileDataBlock(ATileIndex: Word; ABlockNumber: Word); inline;
    function GetRegularBlockOffset(ABlockNumber: Word): Int64; inline;
    function StrToMagic(const AStr: string): LongWord; inline;
  public
    constructor Create(const AFilePath: string; AReplaceTiles: Boolean = True);
    destructor Destroy; override;
    function AddTile(ATile: TTileStream): Boolean;
    property FilePath: string read FFilePath;
  end;

function GetFilePath(const ARootPath: string; APoint: TPoint; AZoom: Byte; AMapID: Integer): string;

implementation

{ TYaMobileCacheFile }

const
  YaCacheHeaderSize = 32768; // 32k
  YaCacheOffsetTablePosition = 32768; // 32k
  YaCacheOffsetTableSize = 32768; // 32k
  YaCacheBlockSize = 32768; // 32k
  YaCacheTotalBlocksCount = 65536; // 64k
  YaCacheYTLDHeaderSize = 38;

constructor TYaMobileCacheFile.Create(
  const AFilePath: string;
  AReplaceTiles: Boolean = True
);
var
  VPath: string;
begin
  inherited Create;
  FInitialized := False;
  FFilePath := AFilePath;
  FReplaceTiles := AReplaceTiles;
  if FileExists(FFilePath) then begin
    FStream := TFileStream.Create(FFilePath, fmOpenReadWrite);
  end else begin
    VPath := ExtractFilePath(FFilePath);
    if VPath <> '' then begin
      if not DirectoryExists(VPath) then begin
        if not ForceDirectories(VPath) then begin
          raise Exception.Create('Can''t create dir: ' + VPath);
        end;
      end;
      FStream := TFileStream.Create(FFilePath, fmCreate);
    end;
  end;
  if Assigned(FStream) then begin
    if not InitializeCacheStructures then begin
      raise Exception.Create('Can''t initialize cache file: ' + AFilePath);
    end else begin
      FInitialized := True;
    end;
  end else begin
    raise Exception.Create('Can''t open cache file: ' + AFilePath);
  end;
end;

destructor TYaMobileCacheFile.Destroy;
begin
  try
    if FInitialized then begin
      if FRegularBlock.DataCellsCount > 0 then begin
        WriteRegularBlock(FRegularBlock, FRegularBlockNumber);
        MarkBlockNumberAsNotEmpty(FRegularBlockNumber);
      end;
      WriteOffsetTable;
      WriteHeader;
    end;
    FreeAndNil(FStream);
  finally
    inherited Destroy;
  end;
end;

function TYaMobileCacheFile.AddTile(ATile: TTileStream): Boolean;

  function AddTileToMemRegularBlock(ATileIndex: Word): Boolean;
  begin
    if RegularBlockOverflow(FRegularBlock, ATile.Data.Size) then begin
      WriteRegularBlock(FRegularBlock, FRegularBlockNumber);
      MarkBlockNumberAsNotEmpty(FRegularBlockNumber);
      InitializeRegularBlock;
    end;
    AddTileDataToRegularBlock(FRegularBlock, ATile);
    SetTileDataBlock(ATileIndex, FRegularBlockNumber);
    Result := True;
  end;

var
  VTileIndex: Word;
  VTileBlockNumber: Word;
  VTempRegularBlock: TYaMobileRegularBlock;
begin
  Result := False;
  VTileIndex := GetTileIndex(ATile.Point);
  VTileBlockNumber := GetTileDataBlock(VTileIndex);
  if VTileBlockNumber > 0 then begin                                             // Тайл уже есть в кэше
    if FReplaceTiles then begin
      if ReadRegularBlock(VTempRegularBlock, VTileBlockNumber) then begin
        if RemoveTileDataFromRegularBlock(VTempRegularBlock, VTileIndex) then begin
          if not RegularBlockOverflow(VTempRegularBlock, ATile.Data.Size) then begin
            AddTileDataToRegularBlock(VTempRegularBlock, ATile)                  // Дописываем тайл во временный блок
          end else begin
            SetTileDataBlock(VTileIndex, 0);                                     // Удаляем информацию о тайле из таблицы смещений
            AddTileToMemRegularBlock(VTileIndex);
          end;
          Result := WriteRegularBlock(VTempRegularBlock, VTileBlockNumber);      // Сохраняем временный блок назад в кэш
        end;
      end;
    end else begin
      Result := True;                                                            // Тайл уже в кэше и замена отключена
    end;
  end else begin
    Result := AddTileToMemRegularBlock(VTileIndex);
  end;
end;

function TYaMobileCacheFile.InitializeCacheStructures: Boolean;
begin
  Result := InitializeHeader and
            InitializeOffsetTable and
            InitializeRegularBlock;
end;

function TYaMobileCacheFile.InitializeHeader: Boolean;
begin
  if FStream.Size = 0 then begin
    ZeroMemory(@FHeader, SizeOf(TYaMobileCacheHeader));
    FHeader.Magic := StrToMagic('YMCF');
    FHeader.Size := YaCacheHeaderSize div 1024;
    FHeader.FormatVersion := 1;
    FHeader.CreationPlatform := 0;
    FHeader.RegularBlockSize := YaCacheBlockSize div 1024;
    FHeader.RemainderFragmentSize := 0;
    FHeader.FirstRemainderBlock[0] := Ord('Y');
    FHeader.FirstRemainderBlock[1] := Ord('B');
    FHeader.FirstRemainderBlock[2] := Ord('L');
    FHeader.FirstRemainderBlock[3] := Ord('K');
    FHeader.FirstRemainderBlock[4] := 1;
    Result := WriteHeader;
  end else begin
    Result := ReadHeader;
  end;
end;

function TYaMobileCacheFile.ReadHeader: Boolean;
begin
  Result := False;
  ZeroMemory(@FHeader, SizeOf(TYaMobileCacheHeader));
  if FStream.Size >= YaCacheHeaderSize then begin
    FStream.Position := 0;
    FStream.Read(FHeader, YaCacheHeaderSize);
    Result := FHeader.Magic = StrToMagic('YMCF');
  end;
end;

function TYaMobileCacheFile.WriteHeader: Boolean;
begin
  FStream.Position := 0;
  Result := FStream.Write(FHeader, (FHeader.Size * 1024) ) = FHeader.Size * 1024;
end;

function TYaMobileCacheFile.InitializeOffsetTable: Boolean;
begin
  if FStream.Size < YaCacheHeaderSize + YaCacheOffsetTableSize then begin
    ZeroMemory(@FOffsetTable, SizeOf(TYaMobileOffsetTable));
    Result := WriteOffsetTable;
  end else begin
    Result := ReadOffsetTable;
  end;
end;

function TYaMobileCacheFile.ReadOffsetTable: Boolean;
begin
  Result := False;
  ZeroMemory(@FOffsetTable, SizeOf(TYaMobileOffsetTable));
  if FStream.Size >= YaCacheHeaderSize + YaCacheOffsetTableSize then begin
    FStream.Position := YaCacheOffsetTablePosition;
    Result := FStream.Read(FOffsetTable, YaCacheOffsetTableSize) = YaCacheOffsetTableSize;
  end;
end;

function TYaMobileCacheFile.WriteOffsetTable: Boolean;
begin
  FStream.Position := YaCacheOffsetTablePosition;
  Result := FStream.Write(FOffsetTable, YaCacheOffsetTableSize) = YaCacheOffsetTableSize;
end;

function TYaMobileCacheFile.InitializeRegularBlock: Boolean;
begin
  FRegularBlock.Magic := StrToMagic('YBLK');
  FRegularBlock.FormatVersion := 1;
  FRegularBlock.Flags := 3;
  FRegularBlock.NextBlocksTableSize := 0;
  FRegularBlock.DataCellsCount := 0;
  SetLength(FRegularBlock.NextBlocksTable, 0);
  SetLength(FRegularBlock.DataTable, 0);
  SetLength(FRegularBlock.Tiles, 0);
  Result := GetFirstEmptyBlockNumber(FRegularBlockNumber);
end;

function TYaMobileCacheFile.ReadRegularBlock(
  var ARegularBlock: TYaMobileRegularBlock;
  ABlockNumber: Word
): Boolean;
var
  VReadCount: Integer;
  I: Integer;
  VPos: Int64;
  ECode: Byte;
begin
  ECode := 0;
  FStream.Position := GetRegularBlockOffset(ABlockNumber);
  ZeroMemory(@ARegularBlock, SizeOf(TYaMobileRegularBlock));
  VReadCount := SizeOf(ARegularBlock.Magic) +
                SizeOf(ARegularBlock.FormatVersion) +
                SizeOf(ARegularBlock.Flags) +
                SizeOf(ARegularBlock.NextBlocksTableSize) +
                SizeOf(ARegularBlock.DataCellsCount);
  if FStream.Read(ARegularBlock, VReadCount) = VReadCount then begin
    if (ARegularBlock.Magic = StrToMagic('YBLK')) and
       (ARegularBlock.FormatVersion = 1) and
       (ARegularBlock.Flags = 3) then
    begin
      // Таблица размещения следующих блоков
      SetLength(ARegularBlock.NextBlocksTable, ARegularBlock.NextBlocksTableSize);
      for I := 0 to Length(ARegularBlock.NextBlocksTable) - 1 do begin
        FStream.Read( ARegularBlock.NextBlocksTable[I], SizeOf(ARegularBlock.NextBlocksTable[I]) );
      end;
      // Таблица размещения самих данных
      SetLength(ARegularBlock.DataTable, ARegularBlock.DataCellsCount);
      for I := 0 to Length(ARegularBlock.DataTable) - 1 do begin
        FStream.Read( ARegularBlock.DataTable[I], SizeOf(ARegularBlock.DataTable[I]) );
      end;
      // Данные (YTLD)
      VPos := FStream.Position;
      SetLength(ARegularBlock.Tiles, ARegularBlock.DataCellsCount);
      for I := 0 to Length(ARegularBlock.Tiles) - 1 do begin
        if ReadTileDataBlock(ARegularBlock.Tiles[I], VPos) then begin
          VPos := VPos + ARegularBlock.DataTable[I].Size;
          if FStream.Position <> VPos then begin
            ECode := 4; // прочитали больше чем нужно
            Break;
          end;
        end else begin
          ECode := 3; // ошибка чтения/разбора блока с данными YTLD
          Break;
        end;
      end;
    end else begin
      ECode := 2; // неправильный хидер регулярного блока
    end;
  end else begin
    ECode := 1; // не смогли считать хидер регулярного блока
  end;
  Result := ECode = 0;
end;

function TYaMobileCacheFile.WriteRegularBlock(
  var ARegularBlock: TYaMobileRegularBlock;
  ABlockNumber: Word
): Boolean;
var
  I: Integer;
  VPos: Int64;
  VEndPos: Int64;
begin
  if ARegularBlock.DataCellsCount = 0 then begin
    MarkBlockNumberAsEmpty(ABlockNumber);
  end;
  FStream.Position := GetRegularBlockOffset(ABlockNumber);
  VEndPos := FStream.Position + FHeader.RegularBlockSize * 1024;
  FStream.Write(ARegularBlock.Magic, SizeOf(ARegularBlock.Magic));
  FStream.Write(ARegularBlock.FormatVersion, SizeOf(ARegularBlock.FormatVersion));
  FStream.Write(ARegularBlock.Flags, SizeOf(ARegularBlock.Flags));
  FStream.Write(ARegularBlock.NextBlocksTableSize, SizeOf(ARegularBlock.NextBlocksTableSize));
  FStream.Write(ARegularBlock.DataCellsCount, SizeOf(ARegularBlock.DataCellsCount));
  // Таблица размещения следующих блоков
  for I := 0 to Length(ARegularBlock.NextBlocksTable) - 1 do begin
    FStream.Write(ARegularBlock.NextBlocksTable[I], SizeOf(ARegularBlock.NextBlocksTable[I]));
  end;
  // Таблица размещения самих данных
  for I := 0 to Length(ARegularBlock.DataTable) - 1 do begin
    FStream.Write(ARegularBlock.DataTable[I], SizeOf(ARegularBlock.DataTable[I]));
  end;
  // Данные (YTLD)
  VPos := FStream.Position;
  for I := 0 to Length(ARegularBlock.Tiles) - 1 do begin
    WriteTileDataBlock(ARegularBlock.Tiles[I], VPos);
    VPos := VPos + ARegularBlock.DataTable[I].Size;
  end;
  while FStream.Position < VEndPos do begin
    FStream.Write(#00#00, 1); // добиваем нулями до конца блока
  end;
  Result := True;
end;

function TYaMobileCacheFile.AddTileDataToRegularBlock(
  var ARegularBlock: TYaMobileRegularBlock;
  ATile: TTileStream
): Boolean;
var
  VCount: Word;
begin
  VCount := ARegularBlock.DataCellsCount + 1;
  SetLength(ARegularBlock.DataTable, VCount);
  ARegularBlock.DataTable[VCount-1].Size := YaCacheYTLDHeaderSize + ATile.Data.Size;
  ARegularBlock.DataTable[VCount-1].Index := GetTileIndex(ATile.Point);
  SetLength(ARegularBlock.Tiles, VCount);
  CreateTileDataBlock(ATile, ARegularBlock.Tiles[VCount-1]);
  ARegularBlock.DataCellsCount := VCount;
  Result := True;
end;

function TYaMobileCacheFile.RemoveTileDataFromRegularBlock(
  var ARegularBlock: TYaMobileRegularBlock;
  ATileIndex: Word
): Boolean;
var
  I,J: Integer;
  VCount: Word;
  VDataTable: array of TYaMobileDataTableElement;
  VTiles: array of TYaMobileTileData;
begin
  Result := False;
  VCount := ARegularBlock.DataCellsCount;
  if VCount = 1 then begin
    FreeTileDataBlock(ARegularBlock.Tiles[VCount - 1]);
    SetLength(ARegularBlock.DataTable, 0);
    SetLength(ARegularBlock.Tiles, 0);
    ARegularBlock.DataCellsCount := 0;
    Result := True;
  end else if VCount > 1 then begin
    SetLength(VDataTable, VCount);
    SetLength(VTiles, VCount);
    J := 0;
    for I := 0 to Length(ARegularBlock.DataTable) - 1 do begin
      if ARegularBlock.DataTable[I].Index = ATileIndex then begin
        FreeTileDataBlock(ARegularBlock.Tiles[I]);
      end else begin
        VDataTable[J] := ARegularBlock.DataTable[I];
        VTiles[J] := ARegularBlock.Tiles[I];
        Inc(J);
      end;
    end;
    SetLength(VDataTable, J);
    SetLength(VTiles, J);
    ARegularBlock.DataCellsCount := J;
    SetLength(ARegularBlock.DataTable, J);
    SetLength(ARegularBlock.Tiles, J);
    for I := 0 to ARegularBlock.DataCellsCount - 1 do begin
      ARegularBlock.DataTable[I] := VDataTable[I];
      ARegularBlock.Tiles[I] := VTiles[I];
    end;
    Result := True;
  end;
end;

function TYaMobileCacheFile.RegularBlockOverflow(
  var ARegularBlock: TYaMobileRegularBlock;
  AAddTileDataSize: Integer
): Boolean;
var
  I: Integer;
  VRegularBlockSize: Integer;
begin
  if ARegularBlock.DataCellsCount > 0 then begin
    VRegularBlockSize :=
      SizeOf(ARegularBlock.Magic) +                                              // 4b
      SizeOf(ARegularBlock.FormatVersion) +                                      // 2b
      SizeOf(ARegularBlock.Flags) +                                              // 1b
      SizeOf(ARegularBlock.NextBlocksTableSize) +                                // 1b
      SizeOf(ARegularBlock.DataCellsCount) +                                     // 2b
      Length(ARegularBlock.NextBlocksTable) * SizeOf(Word) +                     // 2b на элемент
      Length(ARegularBlock.DataTable) * SizeOf(TYaMobileDataTableElement);       // 6b на элемент
    for I := 0 to Length(ARegularBlock.DataTable) - 1 do begin
      VRegularBlockSize := VRegularBlockSize +
                           Integer(ARegularBlock.DataTable[I].Size);
    end;
    Result := ( VRegularBlockSize +                                              // Текущий размер блока
                SizeOf(TYaMobileDataTableElement) +                              // Добавка в хидер в YBLK (6b)
                YaCacheYTLDHeaderSize +                                          // Собственный хидер в YTLD (38b)
                AAddTileDataSize                                                 // Собственно, тайл
              ) >= (FHeader.RegularBlockSize * 1024);                            // Def = 32k
  end else begin
    Result := False;
  end;
end;

procedure TYaMobileCacheFile.CreateTileDataBlock(
  ATile: TTileStream;
  var ATileData: TYaMobileTileData
);
var
  MD5: TMD5Digest;
  I: Integer;
begin
  ZeroMemory(@ATileData, SizeOf(TYaMobileTileData));
  MD5 := MD5Buffer(ATile.Data.Buffer^, ATile.Data.Size);
  ATileData.Magic := StrToMagic('YTLD');
  ATileData.ElementsCount := 1;
  ATileData.FormatVersion := 1;
  for I := 0 to 15 do begin
    ATileData.MD5[I] := MD5.v[I];
  end;
  ATileData.MapVersion := ATile.MapVersion;
  SetLength(ATileData.ElementsTable, ATileData.ElementsCount);
  ATileData.ElementsTable[0].DataID := 0;
  ATileData.ElementsTable[0].DataSize := ATile.Data.Size;
  GetMem(ATileData.Data, ATile.Data.Size);
  Move(ATile.Data.Buffer^, ATileData.Data^, ATile.Data.Size);
end;

procedure TYaMobileCacheFile.FreeTileDataBlock(var ATileData: TYaMobileTileData);
var
  VTileSize: LongWord;
  I: Integer;
begin
  VTileSize := 0;
  for I := 0 to Length(ATileData.ElementsTable) - 1 do begin
    VTileSize := VTileSize + ATileData.ElementsTable[I].DataSize;
  end;
  FreeMem(ATileData.Data, VTileSize);
  SetLength(ATileData.ElementsTable, 0);
  ZeroMemory(@ATileData, SizeOf(TYaMobileTileData));
end;

function TYaMobileCacheFile.ReadTileDataBlock(
  var ATileData: TYaMobileTileData;
  AStreamPos: Int64
): Boolean;
var
  I: Integer;
  VReadCount: Integer;
begin
  Result := False;
  ZeroMemory(@ATileData, SizeOf(TYaMobileTileData));
  VReadCount := SizeOf(ATileData.Magic) +
                SizeOf(ATileData.ElementsCount) +
                SizeOf(ATileData.FormatVersion) +
                Length(ATileData.MD5) +
                SizeOf(ATileData.MapVersion) +
                SizeOf(ATileData.DateTime);
  FStream.Position := AStreamPos;
  if FStream.Read(ATileData, VReadCount) = VReadCount then begin
    if ATileData.Magic = StrToMagic('YTLD') then begin
      // Таблица разметки данных внутри тайла
      VReadCount := 0;
      SetLength(ATileData.ElementsTable, ATileData.ElementsCount);
      for I := 0 to Length(ATileData.ElementsTable) - 1 do begin
        FStream.Read(ATileData.ElementsTable[I], SizeOf(ATileData.ElementsTable[I]));
        VReadCount := VReadCount + Integer(ATileData.ElementsTable[I].DataSize);
      end;
      // Данные
      if (ATileData.ElementsCount > 0) and (VReadCount > 0) then begin
        GetMem(ATileData.Data, VReadCount);
        Result := FStream.Read(ATileData.Data^, VReadCount) = VReadCount;
      end;
    end;
  end;
end;

function TYaMobileCacheFile.WriteTileDataBlock(
  var ATileData: TYaMobileTileData;
  AStreamPos: Int64
): Boolean;
var
  I: Integer;
  VWriteCount: Integer;
begin
  try
    Result := False;
    FStream.Position := AStreamPos;
    FStream.Write(ATileData.Magic, SizeOf(ATileData.Magic));
    FStream.Write(ATileData.ElementsCount, SizeOf(ATileData.ElementsCount));
    FStream.Write(ATileData.FormatVersion, SizeOf(ATileData.FormatVersion));
    FStream.Write(ATileData.MD5[0], Length(ATileData.MD5));
    FStream.Write(ATileData.MapVersion, SizeOf(ATileData.MapVersion));
    FStream.Write(ATileData.DateTime, SizeOf(ATileData.DateTime));
    VWriteCount := 0;
    for I := 0 to Length(ATileData.ElementsTable) - 1 do begin
      FStream.Write(ATileData.ElementsTable[I], SizeOf(ATileData.ElementsTable[I]));
      VWriteCount := VWriteCount + Integer(ATileData.ElementsTable[I].DataSize);
    end;
    if (ATileData.ElementsCount > 0) and (VWriteCount > 0) then begin
      Result := FStream.Write(ATileData.Data^, VWriteCount) = VWriteCount;
    end;
  finally
    FreeTileDataBlock(ATileData);
  end;
end;

function TYaMobileCacheFile.GetFirstEmptyBlockNumber(out ABlockNumber: Word): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to YaCacheTotalBlocksCount - 1 do begin
    if ( (FHeader.BitTable[I div 8] shr (7-(I mod 8))) and 1 = 0 ) then begin
      ABlockNumber := I + 1;
      Result := True;
      Break;
    end;
  end;
end;

procedure TYaMobileCacheFile.MarkBlockNumberAsNotEmpty(ABlockNumber: Word);
var
  VBuf: Byte;
  VBitTablePos: Integer;
begin
  VBitTablePos := (ABlockNumber - 1) div 8;
  VBuf := FHeader.BitTable[VBitTablePos];
  VBuf := VBuf or (Byte(1) shl (7 - ( (ABlockNumber - 1) mod 8)));                // Set bit
  FHeader.BitTable[VBitTablePos] := VBuf;
end;

procedure TYaMobileCacheFile.MarkBlockNumberAsEmpty(ABlockNumber: Word);
var
  VBuf: Byte;
  VBitTablePos: Integer;
begin
  VBitTablePos := (ABlockNumber - 1) div 8;
  VBuf := FHeader.BitTable[VBitTablePos];
  VBuf := VBuf and ( (Byte(1) shl (7 - ( (ABlockNumber - 1) mod 8))) xor $FF);    // UnSet bit
  FHeader.BitTable[VBitTablePos] := VBuf;
end;

function TYaMobileCacheFile.GetTileIndex(APoint: TPoint): Integer;
begin
  Result := APoint.X or (APoint.Y shl 7);
end;

function TYaMobileCacheFile.GetTileDataBlock(ATileIndex: Word): Word;
begin
  Result := FOffsetTable.BlockNumber[ATileIndex];
end;

procedure TYaMobileCacheFile.SetTileDataBlock(ATileIndex: Word; ABlockNumber: Word);
begin
  FOffsetTable.BlockNumber[ATileIndex] := ABlockNumber;
end;

function TYaMobileCacheFile.GetRegularBlockOffset(ABlockNumber: Word): Int64;
begin
  Result := SizeOf(FHeader) +
            SizeOf(FOffsetTable) +
            (FHeader.RegularBlockSize * 1024) * (ABlockNumber - 1);
end;

function TYaMobileCacheFile.StrToMagic(const AStr: string): LongWord;
var
  I: Integer;
begin
  Result := 0;
  if Length(AStr) <= 4 then begin
    for I := 0 to Length(AStr) - 1 do begin
      Result := Result + ( Ord(AStr[I + 1]) shl (I * 8) );
    end;
  end;
end;

function GetFilePath(const ARootPath: string; APoint: TPoint; AZoom: Byte; AMapID: Integer): string;

  function GetHeightTreeForZoom(AZoom: Byte): Integer; inline;
  var
    VHeightZoom: Integer;
    VTilesInZoom, I: Int64;
  begin
    VHeightZoom := 0;
    VTilesInZoom := Int64(4) shl (AZoom shl 1);
    I := 1;
    while I < VTilesInZoom do begin
      Inc(VHeightZoom);
      I := I shl 8;
    end;
    Result := VHeightZoom;
  end;

const
  HexSymbol: array [0..15] of PChar =
    ( '0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' );
var
  VFilePath: string;
  VHeightTreeForZoom: Integer;
  VNode, VChild, VFile: TPoint;
  VNodeSize: Integer;
  VBlock127: Integer;
  I: Integer;
begin
  VFilePath := ARootPath + IntToStr(AMapID) + PathDelim + IntToStr(AZoom) + PathDelim;
  VHeightTreeForZoom := GetHeightTreeForZoom(AZoom);
  VNodeSize := 1 shl (4*(VHeightTreeForZoom - 1) );
  VNode.X := 0;
  VNode.Y := 0;
  VChild.X := 0;
  VChild.Y := 0;
  I := 0;
  while ( I < (VHeightTreeForZoom - 2) ) do begin
    VChild.X := (APoint.X - VNode.X) div VNodeSize;
    VChild.Y := (APoint.Y - VNode.Y) div VNodeSize;
    VNode.X := VNode.X + VNodeSize * VChild.X;
    VNode.Y := VNode.Y + VNodeSize * VChild.Y;
    VNodeSize := VNodeSize shr 4;
    if ( I < (VHeightTreeForZoom - 3) ) then begin
      if (VChild.X > 15) or (VChild.X < 0) or (VChild.Y > 15) or (VChild.Y < 0) then begin
        raise Exception.Create(
                'Child value overflow: ' + #13#10 +
                'X = ' + IntToStr(VChild.X) + #13#10 +
                'Y = ' + IntToStr(VChild.Y)
              );
      end;
      VFilePath := VFilePath + HexSymbol[VChild.X] + HexSymbol[VChild.Y] + PathDelim;
    end;
    Inc(I);
  end;
  VFile.X := APoint.X - VNode.X;
  VFile.Y := APoint.Y - VNode.Y;
  VBlock127 := ( (VFile.X shr 7) shl 1 ) or (VFile.Y shr 7);
  if (VChild.X > 15) or (VChild.X < 0) or (VChild.Y > 15) or (VChild.Y < 0) then begin
    raise Exception.Create(
            'Child value overflow: ' + #13#10 +
            'X = ' + IntToStr(VChild.X) + #13#10 +
            'Y = ' + IntToStr(VChild.Y)
          );
  end;
  if (VBlock127 > 15) or (VBlock127 < 0) then begin
    raise Exception.Create('Block127 value overflow: ' + IntToStr(VBlock127));
  end;
  Result := VFilePath + HexSymbol[VChild.X] + HexSymbol[VChild.Y] + HexSymbol[VBlock127];
end;

end.