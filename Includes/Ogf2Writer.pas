unit Ogf2Writer;

interface

uses
  Classes,
  SysUtils;

type

(* Structure of an OGF2 file appears to be this:
    - SIGNATURE
    - MAPDATA
    - UNIQUEMAPSEQUENCE
    - LAYERDATA***
    - PREVIEWIMAGE
    - [UNIQUELAYERSEQUENCE|LAYERTILEALLOCATIONTABLE|LAYERTILEIMAGES]***

  ***Repeated as many times as there are layers (zoom levels) in the map.

  If Additional zoom levels to store in the file is set to None, there will be
  only 100% zoom level, From 25% adds levels for 25%, 10%, 5%, 2% and 1% zoom,
  From 50% adds levels for 50%, 25%, 10%, 5%, 2% and 1% zoom.
 
  Note that there's no georeference data in OGF2. SmartComGPS uses
  OziExplorer MAP files for georeference.                                     *)

(* 1. SIGNATURE                                                               *)
  TOgf2Signature = packed record // 12 bytes
(*  Each map starts with the English letters 'OVGM'                           *)
    Magic: array [0..3] of AnsiChar;
(*  Always = 0x01010000 - is most likely part of the signature                *)
    SignUnk1: Cardinal;
(*  Offset of the first (100% zoom) LAYERDATA section                         *)
    FirstLayerDataOffset: Cardinal;
  end;

(* 2. MAPDATA                                                                 *)
  TOgf2MapData = packed record // 48 bytes
(*  Offset of the first (100% zoom) LAYERTILEIMAGES section                   *)
    FirstLayerTileImagesOffset: Cardinal;
(*  0x00000000                                                                *)
    MapDataUnk1: Cardinal;
(*  Offset of the first (100% zoom) UNIQUELAYERSEQUENCE section               *)
    FirstUniqueLayerSequenceOffset: Cardinal;
(*  0x10000000                                                                *)
    MapDataUnk2: Cardinal;
(*  Total number of the tiles in the 100% zoom layer plus 16                  *)
    TilesTotal: Cardinal;
(*  Frame (tile) width                                                        *)
    TileWidth: Cardinal;
(*  Frame (tile) height                                                       *)
    TileHeight: Cardinal;
(*  Number of the tiles in a row                                              *)
    TilesPerRow: Cardinal;
(*  Number of the tiles in a column                                           *)
    TilesPerCol: Cardinal;
(*  Original map width in pixels (real, not encoded)                          *)
    OrigMapWidth: Cardinal;
(*  Original map height in pixels (real, not encoded)                         *)
    OrigMapHeight: Cardinal;
(*  Offset of the first (100% zoom) LAYERDATA section                         *)
    FirstLayerDataOffset: Cardinal;
  end;

(* 3. UNIQUEMAPSEQUENCE                                                       *)
  TOgf2UniqueMapSequence = packed record // 384 bytes
(*  It appears, it always starts with 1000 and ends with either 0100 or 0200.
    Each created map will have its own unique sequence, even maps created from
    the same source image with the same settings will have different sequences.

    I have no idea about the meaning or the origin of the unique sequences,
    but they don't appear to be a form of copy protection, because maps with
    the unique sequences replaced by binary zeroes still appear to be usable in
    SmartComGPS and OGF2 converter.                                           *)
    Magic: Cardinal;
    Unk: array [0..379] of Byte; // set all to zero
  end;

(* 4. LAYERDATA                                                               *)
  TOgf2LayerData = packed record  // 40 byte + 600 unk
(*  Layer zoom level (10000 decimal for 100%).
    Other possible values are:
      5000 for 50%,
      2500 for 25%,
      1000 for 10%,
      500 for 5%,
      200 for 2%
      100 for 1%                                                              *)
    LayerZoom: Cardinal;
(*  Offset of the LAYERTILEIMAGES section for this layer                      *)
    LayerTileImagesOffset: Cardinal;
(*  0x00000000                                                                *)
    LayerDataUnk1: Cardinal;
(*  Offset of the UNIQUELAYERSEQUENSE section for this layer                  *)
    UniqueLayerSequenseOffset: Cardinal;
(*  0x10000000                                                                *)
    LayerDataUnk2: Cardinal;
(*  Total number of the tiles in the layer plus 16                            *)
    LayerTilesTotal: Cardinal;
(*  Frame (tile) width                                                        *)
    LayerTileWidth: Cardinal;
(*  Frame (tile) height                                                       *)
    LayerTileHeight: Cardinal;
(*  Number of the tiles in a row                                              *)
    LayerTilesPerRow: Cardinal;
(*  Number of the tiles in a column                                           *)
    LayerTilesPerCol: Cardinal;
(*  ???                                                                       *)
    Unk: array [0..599] of Byte; // set to zero
  end;

(* 5. PREVIEWIMAGE                                                            *)
(*  Used in OGF2 converter for previewing the highlighted map, maybe in some
    (not mine) versions of SmartComGPS, too. Its dimensions can be specified as
    Preview width and Preview height, the format will be the same as OGF2 frames
    encoder algorithm.                                                        *)
  TOgf2PrevieImage = array of Byte;

(* 6. UNIQUELAYERSEQUENCE                                                     *)
  TOgf2UniqueLayerSequense = packed record // 128 byte
(*  Those always start with 0x3C040000 and have the length of 128 bytes.
    Unlike UNIQUEMAPSEQUENCE, they seem to be depended on the operations
    performed with the converter before the map was created, because conversions
    from the same image after identical operations may produce identical
    sequences. As with UNIQUEMAPSEQUENCE, replacing them with binary zeroes
    doesn't appear to render them unusable in SmartComGPS and OGF2 converter. *)
    Magic: Cardinal;
    PreviewSize: Cardinal;
    Unk: array [0..119] of Byte; // set to zero
  end;

(* 7. LAYERTILEALLOCATIONTABLE                                                *)
(*  It occupies 8 bytes for every tile in the layer. The first four bytes of
    each 8 byte section is tile offset, the second four is the tile size.
    The tile allocation data is given in the western reading order
    (from top left, to bottom right).                                         *)
  TOgf2LayerTileAllocationTableRec = packed record
    TileOffset: Cardinal;
    TileSize: Cardinal;
  end;

  TOgf2LayerTileAllocationTable = array of TOgf2LayerTileAllocationTableRec;

(* 8. LAYERTILEIMAGES                                                         *)
(*  Just like tile allocation data, they are concatenated in the western
    reading order, from top left, to bottom right. The format is determined
    by setting OGF2 frames encoder algorithm, the dimensions by setting
    Frame width and Frame height. The compression level for jpeg can be set
    as Jpeg quality; bmp will be uncompressed; tiff will have LZW compression.

    Do note that when Color depth is set to 8bpp, the color is indexed for the
    whole map image, rather that for each tile, meaning all the tiles will have
    the identical palette. That's why when Color depth is set to 8bpp and OGF2
    frames encoder algorithm to gif or png, only the first tile image will be
    fully stored in the OGF2 file and the rest will have the beginning missing
    and only the ending (bitmap) included. In that case, entries in
    LAYERTILEALLOCATIONTABLE will list bitmaps, rather than complete images.

    If the source width/height is not equal to the multiple of the set frame
    width/height, the remainder of the left/bottom tiles will be filled with
    white (hex FF FF FF) pixels.                                              *)
  TOgf2LayerTileImages = array of Byte;

type
  EOgf2WriterError = class(Exception);

  TOgf2LayerTileAllocationTableRecWithXY = record
    X: Int64;
    Y: Int64;
    LayerTileAllocationTableRec: TOgf2LayerTileAllocationTableRec;
  end;

  TOgf2LayerTileAllocationTableWithXY = array of TOgf2LayerTileAllocationTableRecWithXY;

  TOgf2Writer = class (TObject)
  private
    FSignature: TOgf2Signature;
    FMapData: TOgf2MapData;
    FUniqueMapSequence: TOgf2UniqueMapSequence;
    FLayerData: TOgf2LayerData;
    FUniqueLayerSequense: TOgf2UniqueLayerSequense;
    FLayerTileAllocationTable: TOgf2LayerTileAllocationTable;
    FLayerTileAllocationTableOffset: Integer;

    FLayerTileAllocationTableWithXY: TOgf2LayerTileAllocationTableWithXY;

    FStream: TStream;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FTileWidth: Integer;
    FTileHeight: Integer;
    FPreviewImageWidth: Integer;
    FPreviewImageHeight: Integer;
    FPreviewImageStream: TMemoryStream;

    FEmtyTileBlank: TMemoryStream;
    FEmptyTileOffset: Integer;

    FCount: Integer;

    procedure CreateOgf2File;

    function Ceil(const X: Extended): Integer; // copy-past from Math.pas

    procedure QuickSortByXY(
      const ATable: TOgf2LayerTileAllocationTableWithXY;
      const Low, Hi: Integer;
      const ASortByX: Boolean
    );
  public
    constructor Create(
      const AOutPutStream: TStream;
      const AImageWidth: Integer;
      const AImageHeight: Integer;
      const ATileWidth: Integer;
      const ATileHeight: Integer;
      const APreviewImageWidth: Integer;
      const APreviewImageHeight: Integer;
      const APreviewImage: Pointer;
      const APreviewImageSize: Integer;
      const AEmptyTile: Pointer = nil;
      const AEmptyTileSize: Integer = -1
    );
    destructor Destroy; override;
    procedure Add(
      const ATileX: Int64;
      const ATileY: Int64;
      const ATile: Pointer;
      const ATileSize: Integer
    );
    procedure AddEmpty(const ATileX, ATileY: Int64);
    procedure SaveAllocationTable;
  end;

implementation

const
  COgf2Magic = 'OVGM';
  COgf2UniqueLayerSequenseMagic = $0000043C;
  c00000101h = $00000101;
  c00000010h = $00000010;

{ TOgf2Writer }

constructor TOgf2Writer.Create(
  const AOutPutStream: TStream;
  const AImageWidth: Integer;
  const AImageHeight: Integer;
  const ATileWidth: Integer;
  const ATileHeight: Integer;
  const APreviewImageWidth: Integer;
  const APreviewImageHeight: Integer;
  const APreviewImage: Pointer;
  const APreviewImageSize: Integer;
  const AEmptyTile: Pointer = nil;
  const AEmptyTileSize: Integer = -1
);
begin
  inherited Create;

  FStream := AOutPutStream;
  FImageWidth := AImageWidth;
  FImageHeight := AImageHeight;
  FTileWidth := ATileWidth;
  FTileHeight := ATileHeight;
  FPreviewImageWidth := APreviewImageWidth;
  FPreviewImageHeight := APreviewImageHeight;

  FPreviewImageStream := TMemoryStream.Create;
  FPreviewImageStream.WriteBuffer(APreviewImage^, APreviewImageSize);
  FPreviewImageStream.Position := 0;

  if (AEmptyTile <> nil) and (AEmptyTileSize > 0) then begin
    FEmtyTileBlank := TMemoryStream.Create;
    FEmtyTileBlank.WriteBuffer(AEmptyTile^, AEmptyTileSize);
    FEmtyTileBlank.Position :=0;
  end else begin
    FEmtyTileBlank := nil;
  end;
  FEmptyTileOffset := -1;

  FCount := 0;

  CreateOgf2File;
end;

destructor TOgf2Writer.Destroy;
begin
  SetLength(FLayerTileAllocationTable, 0);
  SetLength(FLayerTileAllocationTableWithXY, 0);
  FPreviewImageStream.Free;
  FEmtyTileBlank.Free;
  inherited Destroy;
end;

procedure TOgf2Writer.CreateOgf2File;
var
  VTilesPerRow, VTilesPerCol: Cardinal;
  VTilesTotal: Cardinal;
  VFirstLayerDataOffset: Cardinal;
  VFirstLayerTileImagesOffset: Cardinal;
  VFirstUniqueLayerSequenceOffset: Cardinal;
begin
  VTilesPerRow := Ceil(FImageWidth/FTileWidth);
  VTilesPerCol := Ceil(FImageHeight/FTileHeight);
  VTilesTotal := VTilesPerRow * VTilesPerCol;

  // (!) allocate dynamic size type first
  SetLength(FLayerTileAllocationTable, VTilesTotal);
  SetLength(FLayerTileAllocationTableWithXY, VTilesTotal);

  // calc offsets
  VFirstUniqueLayerSequenceOffset :=
    SizeOf(FSignature) +
    SizeOf(FMapData) +
    SizeOf(FUniqueMapSequence) +
    SizeOf(FLayerData) +
    FPreviewImageStream.Size;

  VFirstLayerTileImagesOffset :=
    VFirstUniqueLayerSequenceOffset +
    SizeOf(FUniqueLayerSequense) +
    Cardinal(SizeOf(FLayerTileAllocationTable[0]) * Length(FLayerTileAllocationTable));

  VFirstLayerDataOffset :=
    SizeOf(FSignature) +
    SizeOf(FMapData) +
    SizeOf(FUniqueMapSequence);

  // fill data
  FSignature.Magic := COgf2Magic;
  FSignature.SignUnk1 := c00000101h;
  FSignature.FirstLayerDataOffset := VFirstLayerDataOffset;

  FMapData.FirstLayerTileImagesOffset :=  VFirstLayerTileImagesOffset;
  FMapData.MapDataUnk1 := 0;
  FMapData.FirstUniqueLayerSequenceOffset := VFirstUniqueLayerSequenceOffset;
  FMapData.MapDataUnk2 := c00000010h;
  FMapData.TilesTotal := VTilesTotal + FMapData.MapDataUnk2;
  FMapData.TileWidth := FTileWidth;
  FMapData.TileHeight := FTileHeight;
  FMapData.TilesPerRow := VTilesPerRow;
  FMapData.TilesPerCol := VTilesPerCol;
  FMapData.OrigMapWidth := FImageWidth;
  FMapData.OrigMapHeight := FImageHeight;
  FMapData.FirstLayerDataOffset := FSignature.FirstLayerDataOffset;

  FUniqueMapSequence.Magic := c00000010h;
  FillChar(FUniqueMapSequence.Unk[0], Length(FUniqueMapSequence.Unk), 0);

  FLayerData.LayerZoom := 10000;
  FLayerData.LayerTileImagesOffset := FMapData.FirstLayerTileImagesOffset;
  FLayerData.LayerDataUnk1 := FMapData.MapDataUnk1;
  FLayerData.UniqueLayerSequenseOffset := FMapData.FirstUniqueLayerSequenceOffset;
  FLayerData.LayerDataUnk2 := FMapData.MapDataUnk2;
  FLayerData.LayerTilesTotal := FMapData.TilesTotal;
  FLayerData.LayerTileWidth := FMapData.TileWidth;
  FLayerData.LayerTileHeight := FMapData.TileHeight;
  FLayerData.LayerTilesPerRow := FMapData.TilesPerRow;
  FLayerData.LayerTilesPerCol := FMapData.TilesPerCol;
  FillChar(FLayerData.Unk[0], Length(FLayerData.Unk), 0);

  FUniqueLayerSequense.Magic := COgf2UniqueLayerSequenseMagic;
  FUniqueLayerSequense.PreviewSize := FPreviewImageStream.Size;
  FillChar(FUniqueLayerSequense.Unk[0], Length(FUniqueLayerSequense.Unk), 0);

  // write headers to ogf2
  FStream.WriteBuffer(FSignature, SizeOf(FSignature));
  FStream.WriteBuffer(FMapData, SizeOf(FMapData));
  FStream.WriteBuffer(FUniqueMapSequence, SizeOf(FUniqueMapSequence));
  FStream.WriteBuffer(FLayerData, SizeOf(FLayerData));
  FStream.WriteBuffer(FPreviewImageStream.Memory^, FPreviewImageStream.Size);
  FStream.WriteBuffer(FUniqueLayerSequense, SizeOf(FUniqueLayerSequense));
  FStream.WriteBuffer(
    FLayerTileAllocationTable[0],
    SizeOf(FLayerTileAllocationTable[0]) * Length(FLayerTileAllocationTable)
  );

  // remember offset of LayerTileAllocationTable for update it state at finish
  FLayerTileAllocationTableOffset :=
    FLayerData.UniqueLayerSequenseOffset +
    SizeOf(FUniqueLayerSequense);
end;

procedure TOgf2Writer.Add(
  const ATileX, ATileY: Int64;
  const ATile: Pointer;
  const ATileSize: Integer
);
var
  VOffset: Cardinal;
begin
  VOffset := FStream.Size;

  FLayerTileAllocationTableWithXY[FCount].X := ATileX;
  FLayerTileAllocationTableWithXY[FCount].Y := ATileY;
  FLayerTileAllocationTableWithXY[FCount].LayerTileAllocationTableRec.TileOffset := VOffset;
  FLayerTileAllocationTableWithXY[FCount].LayerTileAllocationTableRec.TileSize := ATileSize;

  Inc(FCount);

  FStream.Position := VOffset;
  FStream.WriteBuffer(ATile^, ATileSize);
end;

procedure TOgf2Writer.AddEmpty(const ATileX, ATileY: Int64);
begin
  if Assigned(FEmtyTileBlank) then begin
    if FEmptyTileOffset > 0 then begin
      FLayerTileAllocationTableWithXY[FCount].X := ATileX;
      FLayerTileAllocationTableWithXY[FCount].Y := ATileY;
      FLayerTileAllocationTableWithXY[FCount].LayerTileAllocationTableRec.TileOffset := FEmptyTileOffset;
      FLayerTileAllocationTableWithXY[FCount].LayerTileAllocationTableRec.TileSize := FEmtyTileBlank.Size;
      Inc(FCount);
    end else begin
      FEmptyTileOffset := FStream.Size;
      Add(ATileX, ATileY, FEmtyTileBlank.Memory, FEmtyTileBlank.Size);
    end;
  end else begin
    raise EOgf2WriterError.Create('Empty tile blank not assigned!');
  end;
end;

procedure TOgf2Writer.SaveAllocationTable;
const
  CSortByX = True;
  CSortByY = False;
var
  I: Integer;
begin
  if FCount = Integer(FMapData.TilesTotal - FMapData.MapDataUnk2) then begin
    // sort temp table by Y
    QuickSortByXY(
      FLayerTileAllocationTableWithXY,
      0, Length(FLayerTileAllocationTableWithXY) - 1,
      CSortByY
    );
    // sort temp table by X
    for I := 0 to FMapData.TilesPerCol - 1 do begin
      QuickSortByXY(
        FLayerTileAllocationTableWithXY,
        I * Integer(FMapData.TilesPerRow), ((I + 1) * Integer(FMapData.TilesPerRow)) - 1,
        CSortByX
      );
    end;
    // copy data from temp table to ogf2 table
    for I := 0 to Length(FLayerTileAllocationTableWithXY) - 1 do begin
      FLayerTileAllocationTable[I] :=
        FLayerTileAllocationTableWithXY[I].LayerTileAllocationTableRec;
    end;
    // write sorted table to ogf2 file
    FStream.Position := FLayerTileAllocationTableOffset;
    FStream.WriteBuffer(
      FLayerTileAllocationTable[0],
      SizeOf(FLayerTileAllocationTable[0]) * Length(FLayerTileAllocationTable)
    );
  end else begin
    raise EOgf2WriterError.CreateFmt(
      'Tile''s count not match! ' + #13#10 +
      'Current count is %d, but tile''s total count for map is %d',
      [(FCount), Integer(FMapData.TilesTotal - FMapData.MapDataUnk2)]
    );
  end;    
end;

function TOgf2Writer.Ceil(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then begin
    Inc(Result);
  end;
end;

procedure TOgf2Writer.QuickSortByXY(
  const ATable: TOgf2LayerTileAllocationTableWithXY;
  const Low, Hi: Integer;
  const ASortByX: Boolean
);
var
  X, Y: Int64;
  I, J: Integer;
  VBuf: TOgf2LayerTileAllocationTableRecWithXY;
begin
  if ASortByX then begin
    X := ATable[(Low + Hi) div 2].X;
    Y := 0;
  end else begin
    X := 0;
    Y := ATable[(Low + Hi) div 2].Y;
  end;
  I := Low;
  J := Hi;
  while I <= J do begin
    if ASortByX then begin
      while ATable[I].X < X do begin
        I := I + 1;
      end;
      while ATable[J].X > X do begin
        J := J - 1;
      end;
    end else begin
      while ATable[I].Y < Y do begin
        I := I + 1;
      end;
      while ATable[J].Y > Y do begin
        J := J - 1;
      end;
    end;
    if I <= J then begin
      VBuf := ATable[I];
      ATable[I] := ATable[J];
      ATable[J] := VBuf;
      I := I + 1;
      J := J - 1;
    end;
  end;
  if Low < J then begin
    QuickSortByXY(ATable, Low, J, ASortByX);
  end;
  if I < Hi then begin
    QuickSortByXY(ATable, I, Hi, ASortByX);
  end;
end;

end.
