unit u_ThreadExportYaMobileV4;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  MD5,
  t_GeoTypes,
  i_CoordConverterFactory,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TTileStream = record
    Data: TMemoryStream;
    X: Integer;
    Y: Integer;
  end;

  TMemCachedRec = record
    Name: string;
    Stream: TFileStream;
    BitTable: TMemoryStream;
    OffsetTable: TMemoryStream;
    Tiles2Block: array of TTileStream;
  end;

  TThreadExportYaMobileV4 = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FIsReplace: Boolean;
    FExportPath: string;
    FCacheFile: TMemCachedRec;
    cSat, cMap: Byte;
    FCoordConverterFactory: ICoordConverterFactory;
    function GetTileIndex(X, Y: Integer): Integer; inline;
    function GetHeightTreeForZoom(AZoom: Byte): Integer; inline;
    function GetFilePath(
      APoint: TPoint;
      AZoom: Byte;
      AMapID: Integer
    ): string;
    procedure WriteHeader(ACacheFile: TFileStream);
    procedure WriteBlockData(
      ACacheFile: TFileStream;
      ATilesArray: array of TTileStream;
      APosition: Int64
    );
    procedure WriteTileData(
      ACacheFile: TFileStream;
      ATileData: TMemoryStream;
      APosition: Int64
    );       
    procedure WriteBlock(
      ACacheFile: TFileStream;
      ATilesArray: array of TTileStream
    );
    procedure CloseCacheFile;
    function OpenCacheFile(AFilePath: string): Boolean;
    procedure AddTileToCache(
      ATileData: TMemoryStream;
      ATilePoint: TPoint;
      AZoom: Byte;
      AMapID: Integer
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      ACoordConverterFactory: ICoordConverterFactory;
      APath: string;
      APolygon: TArrayOfDoublePoint;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Areplace: boolean;
      Acsat: byte;
      Acmap: byte
    );
  end;

implementation

uses
  c_CoordConverter,
  i_CoordConverter,
  i_BitmapTileSaveLoad,
  i_TileIterator,
  u_TileIteratorStuped,
  u_ARGBToPaletteConverter,
  u_BitmapTileVampyreSaver;

const
  YaCacheHeaderSize      : Word = 32768; // 32k
  YaCacheOffsetTableSize : Word = 32768; // 32k
  YaCacheBlockSize       : Word = 32768; // 32k
  YaCacheBitTableOffset         = 16;
  YaCacheTotalBlocksCount       = 65536; // 64k
  YaCacheYTLDHeaderSize         = 38;

constructor TThreadExportYaMobileV4.Create(
  ACoordConverterFactory: ICoordConverterFactory;
  APath: string;
  APolygon: TArrayOfDoublePoint;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Areplace: boolean;
  Acsat, Acmap: byte
);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  FCoordConverterFactory := ACoordConverterFactory;
  cSat := Acsat;
  cMap := Acmap;
  FExportPath := APath;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
  ZeroMemory(@FCacheFile, SizeOf(FCacheFile));
end;

function TThreadExportYaMobileV4.GetTileIndex(X, Y: Integer): Integer;
begin
  Result := X or (Y shl 7);
end;

function TThreadExportYaMobileV4.GetHeightTreeForZoom(AZoom: Byte): Integer;
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

function TThreadExportYaMobileV4.GetFilePath(
  APoint: TPoint;
  AZoom: Byte;
  AMapID: Integer
): string;
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
  VFilePath := FExportPath + IntToStr(AMapID) + PathDelim + IntToStr(AZoom) + PathDelim;
  VHeightTreeForZoom := GetHeightTreeForZoom(AZoom);
  VNodeSize := 1 shl (4*(VHeightTreeForZoom - 1) {- 1}); // ?? -1 - опечатка в даташите ??
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

procedure TThreadExportYaMobileV4.WriteHeader(ACacheFile: TFileStream);
begin
  ACacheFile.Size := 2*32*1024;      // Заголовок (32k) + таблица смещений (32k)
  ACacheFile.Position := 0;
  ACacheFile.Write('YMCF', 4);       // YMCF -> Yandex Mobile Cache File
  ACacheFile.Write(#32#00, 2);       // Размер заголовка, в килобайтах = 32
  ACacheFile.Write(#01#00, 2);       // Версия формата = 1
  ACacheFile.Write(#00#00#00#00, 4); // Платформа, на которой создали: SYMB, ANDR, IOSX
  ACacheFile.Write(#32#00, 2);       // Размер блока регулярных данных, в килобайтах = 32
  ACacheFile.Write(#00#00, 2);       // Размер фрагмента блока остаточных данных, в килобайтах = 1 ?? а в реале всегда = 0 ??
  ACacheFile.Position := 9*1024;     // Первый остаточный блок = 23k
  ACacheFile.Write('YBLK', 4);       // YBLK -> Yandex Block
  ACacheFile.Write(#01#00, 2);       // Версия формата блока = 1
  ACacheFile.Position := ACacheFile.Size;
end;

procedure TThreadExportYaMobileV4.WriteBlockData(
  ACacheFile: TFileStream;
  ATilesArray: array of TTileStream;
  APosition: Int64
);
var
  I: Integer;
  VRecordsCount: Word;
  VRecordSize: Integer;
  VTileIndex: Word;
begin
  VRecordsCount := Length(ATilesArray);
  ACacheFile.Position := APosition;
  ACacheFile.Write('YBLK', 4);        // YBLK -> Yandex Block
  ACacheFile.Write(#01#00, 2);        // Версия формата блока = 1
  ACacheFile.Write(#03#00, 1);        // Флаги (11000000 -> регулярный блок + начало группы)
  ACacheFile.Write(#00#00, 1);        // Размер таблицы размещения следующих блоков
  ACacheFile.Write(VRecordsCount, 2); // Количество ячеек данных
  for I := 0 to VRecordsCount - 1 do begin
    VRecordSize := YaCacheYTLDHeaderSize + ATilesArray[I].Data.Size;
    VTileIndex := GetTileIndex(ATilesArray[I].X, ATilesArray[I].Y);
    ACacheFile.Write(VRecordSize, 4); // Размер элемента
    ACacheFile.Write(VTileIndex, 2);  // Индекс тайла
  end;
  for I := 0 to VRecordsCount - 1 do begin
    WriteTileData(ACacheFile, ATilesArray[I].Data, ACacheFile.Position);
  end;
end;

procedure TThreadExportYaMobileV4.WriteTileData(
  ACacheFile: TFileStream;
  ATileData: TMemoryStream;
  APosition: Int64
);
var
  VTileSize: Integer;
  MD5: TMD5Digest;
begin
  VTileSize := ATileData.Size;
  ATileData.Position := 0;
  MD5 := MD5Buffer(ATileData.Memory, VTileSize);
  ACacheFile.Position := APosition;
  ACacheFile.Write('YTLD', 4);                    // YTLD -> Yandex Tile Data
  ACacheFile.Write(#01#00, 2);                    // количество записей в таблице разметки данных (пока всегда = 1)
  ACacheFile.Write(#01#00, 2);                    // номер версии формата = 1
  ACacheFile.Write(MD5.v, 16);                    // MD5-чексумма
  ACacheFile.Write(#01#00, 2);                    // номер версии карт-основы
  ACacheFile.Write(#00#00#00#00, 4);              // время (пока не используется = 0)
  ACacheFile.Write(#00#00#00#00, 4);              // идентификатор данных (сейчас всегда 0)
  ACacheFile.Write(VTileSize, 4);                 // размер данных
  ACacheFile.Write(ATileData.Memory^, VTileSize); // данные
end;

procedure TThreadExportYaMobileV4.WriteBlock(
  ACacheFile: TFileStream;
  ATilesArray: array of TTileStream
);

  function GetFirstEmptyBlock(out ABlock: Word): Boolean;
  // Сканируем битовую таблицу (в заголовке) на предмет наличия свободного блока
  var
    VBuf: Byte;
    VBlockNumber: Word;
  begin
    Result := False;
    FCacheFile.BitTable.Position := 0;
    for VBlockNumber := 0 to YaCacheTotalBlocksCount - 1 do begin
      if (VBlockNumber mod 8) = 0 then begin
        FCacheFile.BitTable.Read(VBuf, 1);
      end;
      if ( (VBuf shr (7-(VBlockNumber mod 8))) and 1 = 0) then begin
        ABlock := VBlockNumber; // Нумерация блоков с 0
        Result := True;
        Break;
      end;
    end;
  end;

  procedure MarkBlockAsNotEmpty(ABlockNumber: Word);
  // В битовой таблице устанавливаем бит, соответствующий номеру блока
  var
    VBuf: Byte;
    VBitTablePos: Integer;
  begin
    VBitTablePos := ABlockNumber div 8;
    FCacheFile.BitTable.Position := VBitTablePos;
    FCacheFile.BitTable.Read(VBuf, 1);
    VBuf := VBuf or (Byte(1) shl (7 - (ABlockNumber mod 8))); // set bit
    FCacheFile.BitTable.Position := VBitTablePos;
    FCacheFile.BitTable.Write(VBuf, 1);
  end;

  procedure UpdateOffsetTableState(ABlockNumber: Word);
  var
    I: Integer;
    VBlock: Word;
  begin
    VBlock := ABlockNumber + 1; // В таблице смещений нумерация блоков с 1
    for I := 0 to Length(ATilesArray) - 1 do begin
      FCacheFile.OffsetTable.Position := 2 * GetTileIndex(ATilesArray[i].X, ATilesArray[i].Y);
      FCacheFile.OffsetTable.Write(VBlock, 2);
    end;
  end;

var
  VEmptyBlockNumber: Word;
  VEmptyBlockOffset: Int64;
begin
  if Length(ATilesArray) > 0 then begin
    if GetFirstEmptyBlock(VEmptyBlockNumber) then begin
      VEmptyBlockOffset := YaCacheHeaderSize + YaCacheOffsetTableSize + YaCacheBlockSize * VEmptyBlockNumber;
      if ACacheFile.Size < VEmptyBlockOffset + YaCacheBlockSize then begin
        ACacheFile.Size := VEmptyBlockOffset + YaCacheBlockSize;
      end;
      WriteBlockData(ACacheFile, ATilesArray, VEmptyBlockOffset);
      MarkBlockAsNotEmpty(VEmptyBlockNumber);
      UpdateOffsetTableState(VEmptyBlockNumber);
    end;
  end;
end;

procedure TThreadExportYaMobileV4.CloseCacheFile;
var
  I: Integer;
begin
  try
    if Assigned(FCacheFile.Stream) then
    try
      WriteBlock(FCacheFile.Stream, FCacheFile.Tiles2Block);
      FCacheFile.BitTable.Position := 0;
      FCacheFile.Stream.Position := YaCacheBitTableOffset;
      FCacheFile.Stream.Write(FCacheFile.BitTable.Memory^, 8*1024);
      FCacheFile.OffsetTable.Position := 0;
      FCacheFile.Stream.Position := 32*1024;
      FCacheFile.Stream.Write(FCacheFile.OffsetTable.Memory^, 32*1024);
    finally
      try
        for I := 0 to Length(FCacheFile.Tiles2Block) - 1 do begin
          FreeAndNil(FCacheFile.Tiles2Block[I].Data);
          ZeroMemory(@FCacheFile.Tiles2Block[I], SizeOf(FCacheFile.Tiles2Block[I]));
        end;
      finally
        SetLength(FCacheFile.Tiles2Block, 0);
      end;
    end;
  finally
    FCacheFile.Name := '';
    FreeAndNil(FCacheFile.Stream);
    FreeAndNil(FCacheFile.BitTable);
    FreeAndNil(FCacheFile.OffsetTable);
  end;
end;

function TThreadExportYaMobileV4.OpenCacheFile(AFilePath: string): Boolean;
var
  VPath: string;
begin
  if FCacheFile.Name <> AFilePath then begin
    CloseCacheFile;
  end else begin
    Result := Assigned(FCacheFile.Stream) and
              Assigned(FCacheFile.BitTable) and
              Assigned(FCacheFile.OffsetTable);
    Exit;
  end;
  FCacheFile.Name := AFilePath;
  if FileExists(FCacheFile.Name) then begin
    FCacheFile.Stream := TFileStream.Create(FCacheFile.Name, fmOpenReadWrite);
  end else begin
    VPath := ExtractFilePath(FCacheFile.Name);
    if VPath <> '' then begin
      if not DirectoryExists(VPath) then begin
        if not ForceDirectories(VPath) then begin
          raise Exception.Create('Can''t create dir: ' + VPath);
        end;
      end;
      FCacheFile.Stream := TFileStream.Create(FCacheFile.Name, fmCreate);
    end;
  end;
  if Assigned(FCacheFile.Stream) then begin
    if FCacheFile.Stream.Size = 0 then begin
      WriteHeader(FCacheFile.Stream);
    end;
    FCacheFile.BitTable := TMemoryStream.Create;
    FCacheFile.Stream.Position := YaCacheBitTableOffset;
    FCacheFile.BitTable.CopyFrom(FCacheFile.Stream, 8*1024);
    FCacheFile.OffsetTable := TMemoryStream.Create;
    FCacheFile.Stream.Position := 32*1024;
    FCacheFile.OffsetTable.CopyFrom(FCacheFile.Stream, 32*1024);
    SetLength(FCacheFile.Tiles2Block, 0);
    Result := True;
  end else begin
    raise Exception.Create('Can''t open cache file: ' + AFilePath);
  end;
end;

procedure TThreadExportYaMobileV4.AddTileToCache(
  ATileData: TMemoryStream;
  ATilePoint: TPoint;
  AZoom: Byte;
  AMapID: Integer
);

  function BlockDataSizeOverflow(
    AMemCachedTilesSize: Integer;
    AMemCachedTilesCount: Word;
    ANewTileAddSize: Integer
  ): Boolean; inline;
  begin
    Result := ( AMemCachedTilesSize + ANewTileAddSize +          // Собственно данные
                10 + 6*(AMemCachedTilesCount + 1) +              // Хедер в YBLK
                YaCacheYTLDHeaderSize*(AMemCachedTilesCount + 1) // Хедер в YTLD
              ) >= YaCacheBlockSize                              // def = 32k
  end;

var
  VCacheFilePath: string;
  VTilesSize: Int64;
  I:Integer;
begin
  VCacheFilePath := GetFilePath(ATilePoint, AZoom, AMapID);
  if OpenCacheFile(VCacheFilePath) then begin
    VTilesSize:=0;
    for I := 0 to Length(FCacheFile.Tiles2Block) - 1 do begin
      if Assigned(FCacheFile.Tiles2Block[I].Data) then begin
        VTilesSize := VTilesSize + FCacheFile.Tiles2Block[I].Data.Size;
      end;
    end;
    if BlockDataSizeOverflow(VTilesSize, Length(FCacheFile.Tiles2Block), ATileData.Size) then
    try
      WriteBlock(FCacheFile.Stream, FCacheFile.Tiles2Block);
    finally
      try
        for I := 0 to Length(FCacheFile.Tiles2Block) - 1 do begin
          FreeAndNil(FCacheFile.Tiles2Block[I].Data);
          ZeroMemory(@FCacheFile.Tiles2Block[I], SizeOf(FCacheFile.Tiles2Block[I]));
        end;
      finally
        SetLength(FCacheFile.Tiles2Block, 0);
      end;
    end;
    if Assigned(ATileData) then begin
      ATileData.Position := 0;
      I := Length(FCacheFile.Tiles2Block);
      SetLength(FCacheFile.Tiles2Block, I + 1);
      FCacheFile.Tiles2Block[I].Data := TMemoryStream.Create;
      FCacheFile.Tiles2Block[I].Data.CopyFrom(ATileData, ATileData.Size);
      FCacheFile.Tiles2Block[I].X := ATilePoint.X mod 128;
      FCacheFile.Tiles2Block[I].Y := ATilePoint.Y mod 128;
    end;
  end;
end;

procedure TThreadExportYaMobileV4.ProcessRegion;
var
  i, j, xi, yi, hxyi, sizeim: integer;
  VZoom: Byte;
  bmp32, bmp322, bmp32crop: TCustomBitmap32;
  TileStream: TMemoryStream;
  tc: cardinal;
  VGeoConvert: ICoordConverter;
  JPGSaver, PNGSaver: IBitmapTileSaver;
  VTile: TPoint;
  VMapType: TMapType;
  VSaver: IBitmapTileSaver;
  VTileIterators: array of ITileIterator;
begin
  inherited;
  if (FMapTypeArr[0] = nil) and (FMapTypeArr[1] = nil) and (FMapTypeArr[2] = nil) then begin
    exit;
  end;
  bmp32 := TCustomBitmap32.Create;
  bmp322 := TCustomBitmap32.Create;
  try
    hxyi := 1;
    sizeim := 128;
    JPGSaver := TVampyreBasicBitmapTileSaverJPG.create(cSat);
    PNGSaver := TVampyreBasicBitmapTileSaverPNGPalette.create(TARGBToPaletteConverter.Create, cMap);
    TileStream := TMemoryStream.Create;
    try
      bmp32.DrawMode := dmBlend;
      bmp322.DrawMode := dmBlend;
      bmp32crop := TCustomBitmap32.Create;
      try
        bmp32crop.Width := sizeim;
        bmp32crop.Height := sizeim;
        VGeoConvert := FCoordConverterFactory.GetCoordConverterByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
        FTilesToProcess := 0;
        SetLength(VTileIterators,Length(FZooms));

        for i := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[i];
          VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
          for j := 0 to 2 do begin
            if (FMapTypeArr[j] <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
              FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal;
            end;
          end;
        end;
        try
          FTilesProcessed := 0;

          ProgressFormUpdateCaption(SAS_STR_ExportTiles, SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files);
          ProgressFormUpdateOnProgress;

          tc := GetTickCount;
          try
            for j := 0 to length(FMapTypeArr)-1 do begin
              VMapType:=FMapTypeArr[j];
              for i := 0 to Length(FZooms) - 1 do begin
                VZoom := FZooms[i];
                VTileIterators[i].Reset;
                while VTileIterators[i].Next(VTile) do begin
                  if CancelNotifier.IsOperationCanceled(OperationID) then begin
                    exit;
                  end;
                  if (VMapType <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
                    bmp322.Clear;
                    if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                      FMapTypeArr[0].LoadTileUni(bmp322, VTile, VZoom, VGeoConvert, False, False, True);
                    end;
                    bmp32.Clear;
                    if VMapType.LoadTileUni(bmp32, VTile, VZoom, VGeoConvert, False, False, True) then begin
                      if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                        bmp322.Draw(0, 0, bmp32);
                        bmp32.Draw(0, 0, bmp322);
                      end;
                      if (j = 2) or (j = 0) then begin
                        VSaver := JPGSaver;
                      end else begin
                        VSaver := PNGSaver;
                      end;
                      for xi := 0 to hxyi do begin
                        for yi := 0 to hxyi do begin
                          bmp32crop.Clear;
                          bmp32crop.Draw(0, 0, bounds(sizeim * xi, sizeim * yi, sizeim, sizeim), bmp32);
                          TileStream.Clear;
                          VSaver.SaveToStream(bmp32crop, TileStream);
                          AddTileToCache(
                            TileStream,
                            Types.Point(2*VTile.X + Xi, 2*VTile.Y + Yi),
                            VZoom,
                            (10 + j)
                          );
                        end;
                      end;
                    end;
                    inc(FTilesProcessed);
                    if (GetTickCount - tc > 1000) then begin
                      tc := GetTickCount;
                      ProgressFormUpdateOnProgress;
                    end;
                  end;
                end;
              end;
            end;
          finally
            CloseCacheFile;
          end;
        finally
          for i := 0 to Length(FZooms)-1 do begin
            VTileIterators[i] := nil;
          end;
        end;
        ProgressFormUpdateOnProgress
      finally
        bmp32crop.Free;
      end;
    finally
      TileStream.Free;
    end;
  finally
    bmp32.Free;
    bmp322.Free;
  end;
end;

end.
