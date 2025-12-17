{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ElevationReaderTIFF;

interface

{$IFDEF DEBUG}
  {.$DEFINE DUMP_TIFF_TAGS}
{$ENDIF}

uses
  Windows,
  Types,
  SysUtils,
  libtiff,
  u_ElevationValue,
  u_ElevationReader;

const
  c_TIFF_II = $4949;
  // byte order is always from the least significant byte to the most significant byte,
  // for both 16-bit and 32-bit integers This is called little-endian byte order
  c_42_II = $002A; // 42


  c_TIFF_MM = $4D4D;
  // format, byte order is always from most significant to least significant,
  // for both 16-bit and 32-bit integers. This is called big-endian byte order
  c_42_MM = $2A00; // 42

type
  TTiffHeader = packed record
    ByteOrder: Word;  // 'II' (4949.H) or 'MM' (4D4D.H)
    type_: Word;      // 42
    IFDOffset: DWORD; // offset of first IFD
  end;
  PTiffHeader = ^TTiffHeader;

  // 4.6.2 IFD Structure
  // The IFD used in this standard consists of:
  // a 2-byte count (number of fields),
  // 12-byte field Interoperability arrays,
  // and 4-byte offset to the next IFD, in conformance with TIFF Rev. 6.0.
  TIFD_12 = packed record
    tag: Word;     // Bytes 0-1 Tag
    type_: Word;   // Bytes 2-3 Type
    count: DWORD;  // Bytes 4-7 Count
    value: DWORD;  // Bytes 8-11 Value Offset
  end;
  PIFD_12 = ^TIFD_12;

  TIFD_NN = packed record
    number_of_fields: Word;
    items: array of TIFD_12;
    offset_to_next: DWORD;
  end;
  PIFD_NN = ^TIFD_NN;

type
  TTiffItemType = (titShort = 2, titLong = 4); // do not change!

  TTiffArray = record
    Offset: Int64;
    Count: Cardinal;
    ItemType: TTiffItemType;
    Data: Pointer;
    IsWithData: Boolean;
  end;
  PTiffArray = ^TTiffArray;

  TCompressedTiffDataProvider = class
  private
    FTiff: PTIFF;

    FTileSize: TPoint;
    FTilesCount: TPoint;

    FTiles: array of Pointer;
    FTileBytes: Integer;
    FIsTileWithData: array of Boolean;
  public
    function Open(
      const AFileName: string;
      const ATileSize: TPoint;
      const ATilesCount: TPoint;
      const ASampleSize: Integer
    ): Boolean;

    function GetTileData(
      const ATile: TPoint
    ): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TElevationReaderTIFF = class(TElevationReader)
  private
    FOffsets: PTiffArray;

    FIsTiled: Boolean;
    FIsStripped: Boolean;

    FImageWidth: Integer;
    FImageLength: Integer;

    FCompressionFormat: Integer;

    FSampleFormat: Integer;
    FSampleSize: Integer;

    FTileWidth: Integer;
    FTileLength: Integer;

    FTilesCount: TPoint;

    FValueType: TElevationValueType;

    FCompressedDataProvider: TCompressedTiffDataProvider;

    function FindElevationInStripped(
      const ARow, ACol: Integer;
      var AValue: TElevationValue
    ): Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}

    function FindElevationInTiled(
      const ARow, ACol: Integer;
      var AValue: TElevationValue
    ): Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
  public
    function Open(
      const AParams: TElevationReaderParams
    ): Boolean; override;

    function ReadElevationValue(
      const ARow, ACol: Integer;
      out AValue: TElevationValue
    ): Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  NTFiles,
  {$IFDEF DUMP_TIFF_TAGS}
  u_DebugLogger,
  {$ENDIF}
  u_ByteSwapFunc,
  u_GlobalDllName;

function _GetTiffDWORD(const AByteOrder: Word; const ASource: DWORD): DWORD; inline;
begin
  if AByteOrder = c_TIFF_MM then begin
    Result := Swap32(ASource);
  end else begin
    Result := ASource;
  end;
end;

function _GetTiffWORD(const AByteOrder: Word; const ASource: WORD): WORD; inline;
begin
  if AByteOrder = c_TIFF_MM then begin
    Result := Swap16(ASource);
  end else begin
    Result := ASource;
  end;
end;

function _CheckTiffHeader(const ATiffHeader: TTiffHeader): Boolean; inline;
begin
  Result :=
    ((ATiffHeader.ByteOrder = c_TIFF_II) and (ATiffHeader.type_ = c_42_II)) or
    ((ATiffHeader.ByteOrder = c_TIFF_MM) and (ATiffHeader.type_ = c_42_MM));
end;

procedure _AllocTiffArray(const ATag: TIFD_12; var P: PTiffArray); inline;
begin
  if P = nil then begin
    GetMem(P, SizeOf(TTiffArray));
    P.Data := nil;
  end;

  P.Offset := ATag.value;
  P.Count := ATag.count;

  if ATag.type_ = 2 then begin
    P.ItemType := titShort;
  end else begin
    P.ItemType := titLong;
  end;

  P.IsWithData := False;
end;

procedure _FreeTiffArray(const P: PTiffArray);
begin
  if P <> nil then begin
    if P.Data <> nil then begin
      FreeMem(P.Data);
    end;
    FreeMem(P);
  end;
end;

procedure _ResetTiffArray(const P: PTiffArray); inline;
begin
  if P <> nil then begin
    P.IsWithData := False;
  end;
end;

function _ReadTiffArrayData(const AFileHandle: THandle; const P: PTiffArray): Boolean; inline;
var
  VDataSize: Cardinal;
begin
  Assert(P <> nil);

  VDataSize := P.Count * Cardinal(P.ItemType);

  ReallocMem(P.Data, VDataSize);
  P.IsWithData := True;

  Result := NtReadFromFile(AFileHandle, P.Data, VDataSize, P.Offset);
end;

{ TElevationReaderTIFF }

constructor TElevationReaderTIFF.Create;
begin
  inherited Create;

  FOffsets := nil;
  FCompressedDataProvider := nil;
end;

destructor TElevationReaderTIFF.Destroy;
begin
  FreeAndNil(FCompressedDataProvider);
  _FreeTiffArray(FOffsets);

  inherited Destroy;
end;

function TElevationReaderTIFF.Open(
  const AParams: TElevationReaderParams
): Boolean;
var
  I: Integer;
  VTiffHeader: TTiffHeader;
  VOffset: Int64;
  VNumberOfIFDs: Word;
  VCurrentTag: TIFD_12;
begin
  FParams := AParams;

  _ResetTiffArray(FOffsets);

  // read header
  Result := NtReadFromFile(FParams.FileHandle, @VTiffHeader, SizeOf(VTiffHeader), 0);
  if not Result then begin
    Exit;
  end;

  // check header
  Result := _CheckTiffHeader(VTiffHeader);
  if not Result then begin
    Exit;
  end;

  VOffset := _GetTiffDWORD(VTiffHeader.ByteOrder, VTiffHeader.IFDOffset);

  // get number of IFDs
  Result := NtReadFromFile(FParams.FileHandle, @VNumberOfIFDs, SizeOf(VNumberOfIFDs), VOffset);
  if not Result then begin
    Exit;
  end;

  VNumberOfIFDs := _GetTiffWORD(VTiffHeader.ByteOrder, VNumberOfIFDs);
  Inc(VOffset, SizeOf(VNumberOfIFDs));

  FImageWidth := 0;
  FImageLength := 0;

  FSampleSize := 2; // 16 bit
  FSampleFormat := 2; // signed

  FIsTiled := False;
  FIsStripped := False;

  if VNumberOfIFDs > 0 then begin
    for I := 0 to VNumberOfIFDs - 1 do begin
      // get IFD
      Result := NtReadFromFile(FParams.FileHandle, @VCurrentTag, SizeOf(VCurrentTag), VOffset);
      if not Result then begin
        Exit;
      end;

      case VCurrentTag.tag of
        $100 {256}: begin
          // ImageWidth
          FImageWidth := VCurrentTag.value;
        end;

        $101 {257}: begin
          // ImageLength
          FImageLength := VCurrentTag.value;
        end;

        $102 {258}: begin
          // BitsPerSample
          FSampleSize := VCurrentTag.value div 8;
        end;

        $103 {259}: begin
          // Compression
          FCompressionFormat := VCurrentTag.value;
        end;

        $111 {273}: begin
          // StripOffsets
          _AllocTiffArray(VCurrentTag, FOffsets);
          FIsStripped := True;
        end;

        $117 {279}: begin
          // StripBytesCount
          // _AllocTiffArray(VCurrentTag, FBytes);
        end;

        $142 {322}: begin
          // TileWidth
          FTileWidth := VCurrentTag.value;
        end;

        $143 {323}: begin
          // TileLength
          FTileLength := VCurrentTag.value;
        end;

        $144 {324}: begin
          // TileOffsets
          _AllocTiffArray(VCurrentTag, FOffsets);
          FIsTiled := True;
        end;

        $145 {325}: begin
          // TileByteCounts
          // _AllocTiffArray(VCurrentTag, FBytes);
        end;

        $153 {339}: begin
          // SampleFormat: Specifies how to interpret each data sample in a pixel.
          // 1 = unsigned integer data
          // 2 = two's complement signed integer data
          // 3 = IEEE floating point data
          // 4 = undefined data format
          FSampleFormat := VCurrentTag.value;
        end;
      end;

      Inc(VOffset, SizeOf(VCurrentTag));

      {$IFDEF DUMP_TIFF_TAGS}
      GLog.Write(Self,
        'tag: ' + IntToStr(VCurrentTag.tag) + ' ' +
        'type: ' + IntToStr(VCurrentTag.type_) + ' ' +
        'count: ' + IntToStr(VCurrentTag.count) + ' ' +
        'value: ' + IntToStr(VCurrentTag.value) + ' ' +
        'value: 0x' + IntToHex(VCurrentTag.value, 8)
      );
      {$ENDIF}
    end;
  end;

  Result := False;

  if not FIsTiled and not FIsStripped then begin
    Assert(False);
    Exit;
  end;

  if (FParams.ColsCount <> FImageWidth) or (FParams.RowsCount <> FImageLength) then begin
    Exit;
  end;

  case FSampleFormat of
    2: begin // integer
      if FSampleSize = 2 then begin
        FValueType := evtSmallInt;
      end else
      if FSampleSize = 4 then begin
        FValueType := evtLongInt;
      end else begin
        Assert(False, 'TIFF: Invalid SampleSize value: ' + IntToStr(FSampleSize));
        Exit;
      end;
    end;

    3: begin // float
      if FSampleSize = 4 then begin
        FValueType := evtSingle;
      end else begin
        Assert(False, 'TIFF: Invalid SampleSize value: ' + IntToStr(FSampleSize));
        Exit;
      end;
    end
  else
    Assert(False, 'TIFF: Unexpected SampleFormat value: ' + IntToStr(FSampleFormat));
    Exit;
  end;

  if FIsTiled then begin
    FTilesCount.X := (FImageWidth + FTileWidth - 1) div FTileWidth;
    FTilesCount.Y := (FImageLength + FTileLength - 1) div FTileLength;

    if FOffsets.Count <> Cardinal(FTilesCount.X * FTilesCount.Y) then begin
      Assert(False);
      Exit;
    end;

    if FCompressionFormat <> 1 then begin
      InitLibTiff(GDllName.Tiff);

      if FCompressedDataProvider = nil then begin
        FCompressedDataProvider := TCompressedTiffDataProvider.Create;
      end;

      Result :=
        FCompressedDataProvider.Open(
          FParams.FileName,
          Point(FTileWidth, FTileLength),
          FTilesCount,
          FSampleSize
        );

      if not Result then begin
        Exit;
      end;
    end;
  end;

  if FIsStripped and (FCompressionFormat <> 1) then begin
    // not implemented
    Exit;
  end;

  Result := True;
end;

function TElevationReaderTIFF.ReadElevationValue(
  const ARow, ACol: Integer;
  out AValue: TElevationValue
): Boolean;
begin
  AValue.TypeId := FValueType;

  if FIsStripped then begin
    Result := FindElevationInStripped(ARow, ACol, AValue);
  end else
  if FIsTiled then begin
    Result := FindElevationInTiled(ARow, ACol, AValue);
  end else begin
    Assert(False);
    Result := False;
    Exit;
  end;
end;

function TElevationReaderTIFF.FindElevationInStripped(
  const ARow, ACol: Integer;
  var AValue: TElevationValue
): Boolean;
var
  VStripOffset: Int64;
  VPixelOffset: Int64;
begin
  if FOffsets.Count = 1 then begin
    VPixelOffset := (ARow * FParams.ColsCount + ACol) * FSampleSize;
    Result := AValue.ReadFromFile(FParams.FileHandle, FOffsets.Offset + VPixelOffset);
  end else
  if FOffsets.Count = Cardinal(FParams.RowsCount) then begin

    if not FOffsets.IsWithData then begin
      Result := _ReadTiffArrayData(FParams.FileHandle, FOffsets);
      if not Result then begin
        Exit;
      end;
    end;
    Assert(FOffsets.Data <> nil);

    if FOffsets.ItemType = titLong then begin
      VStripOffset := PLongWord(INT_PTR(FOffsets.Data) + ARow * SizeOf(DWORD))^;
    end else begin
      VStripOffset := PWord(INT_PTR(FOffsets.Data) + ARow * SizeOf(WORD))^;
    end;

    VPixelOffset := VStripOffset + ACol * FSampleSize;
    Result := AValue.ReadFromFile(FParams.FileHandle, VPixelOffset);
  end else begin
    Result := False;
  end;
end;

function TElevationReaderTIFF.FindElevationInTiled(
  const ARow, ACol: Integer;
  var AValue: TElevationValue
): Boolean;
var
  VTile: TPoint;
  VPixel: TPoint;
  VTileNum: Integer;
  VTileOffset: Int64;
  VPixelOffset: Int64;
  VTileData: Pointer;
begin
  VTile.X := ACol div FTileWidth;
  VTile.Y := ARow div FTileLength;

  VPixel.X := ACol - VTile.X * FTileWidth;
  VPixel.Y := ARow - VTile.Y * FTileLength;

  if FCompressionFormat = 1 then begin

    if not FOffsets.IsWithData then begin
      Result := _ReadTiffArrayData(FParams.FileHandle, FOffsets);
      if not Result then begin
        Exit;
      end;
    end;
    Assert(FOffsets.Data <> nil);

    VTileNum := VTile.Y * FTilesCount.X + VTile.X;

    if FOffsets.ItemType = titLong then begin
      VTileOffset := PLongWord(INT_PTR(FOffsets.Data) + VTileNum * SizeOf(DWORD))^;
    end else begin
      VTileOffset := PWord(INT_PTR(FOffsets.Data) + VTileNum * SizeOf(WORD))^;
    end;

    VPixelOffset := VTileOffset + (VPixel.Y * FTileWidth + VPixel.X) * FSampleSize;
    Result := AValue.ReadFromFile(FParams.FileHandle, VPixelOffset);
  end else begin

    VTileData := FCompressedDataProvider.GetTileData(VTile);
    if VTileData = nil then begin
      Result := False;
      Exit;
    end;

    VPixelOffset := (VPixel.Y * FTileWidth + VPixel.X) * FSampleSize;
    Result := AValue.ReadFromMem(VTileData, VPixelOffset);
  end;
end;

{ TCompressedTiffDataProvider }

constructor TCompressedTiffDataProvider.Create;
begin
  FTiff := nil;
  FTileBytes := 0;
  FTiles := nil;
  FIsTileWithData := nil;
end;

destructor TCompressedTiffDataProvider.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FTiles) - 1 do begin
    FreeMem(FTiles[I]);
  end;

  if FTiff <> nil then begin
    TIFFClose(FTiff);
    FTiff := nil;
  end;

  inherited Destroy;
end;

function TCompressedTiffDataProvider.Open(
  const AFileName: string;
  const ATileSize: TPoint;
  const ATilesCount: TPoint;
  const ASampleSize: Integer
): Boolean;
var
  VLen: Integer;
  VCount: Integer;
begin
  FTileSize := ATileSize;
  FTilesCount := ATilesCount;

  if FTiff <> nil then begin
    TIFFClose(FTiff);
    FTiff := nil;
  end;

  FTiff := TIFFOpenW(PWideChar(AFileName), 'r');
  if FTiff = nil then begin
    Result := False;
    Exit;
  end;

  FTileBytes := FTileSize.X * FTileSize.Y * ASampleSize;

  VCount := FTilesCount.X * FTilesCount.Y;

  VLen := Length(FIsTileWithData);
  if VLen < VCount then begin
    SetLength(FIsTileWithData, VCount);
  end;
  if VCount > 0 then begin
    // reset all values to False
    FillChar(FIsTileWithData[0], VCount * SizeOf(FIsTileWithData[0]), 0);
  end;

  VLen := Length(FTiles);
  if VLen < VCount then begin
    SetLength(FTiles, VCount);
    // reset new values to nil
    FillChar(FTiles[VLen], (VCount - VLen) * SizeOf(FTiles[0]), 0);
  end;

  Result := True;
end;

function TCompressedTiffDataProvider.GetTileData(const ATile: TPoint): Pointer;
var
  I: Integer;
  VSize: tsize_t;
begin
  Result := nil;

  I := ATile.Y * FTilesCount.X + ATile.X; // tile number

  if (I < 0) or (I >= Length(FTiles)) then begin
    Assert(False);
    Exit;
  end;

  if not FIsTileWithData[I] then begin
    ReallocMem(FTiles[I], FTileBytes);

    VSize := TIFFReadTile(FTiff, FTiles[I], ATile.X * FTileSize.X, ATile.Y * FTileSize.Y, 0, 0);
    if VSize <> FTileBytes then begin
      Exit;
    end;

    FIsTileWithData[I] := True;
  end;

  Result := FTiles[I];
end;

end.
