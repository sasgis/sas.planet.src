{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_ExternalTerrainAPI;

interface

{.$DEFINE DUMP_TIFF_TAGS}

uses
  Windows,
  Math;

type
  TElevationValueType = (evtSmallInt, evtLongInt, evtSingle);
  
  TElevationValue = record
    case TypeId: TElevationValueType of
      evtSmallInt : (ValueSmall: SmallInt);
      evtLongInt  : (ValueLong: LongInt);
      evtSingle   : (ValueSingle: Single);
  end;

  TStripOffsetsType = (sotShort = 2, sotLong = 4); // do not change!

  TTerrainFile = class
  private
    FFileName: string;
    FFileHandle: THandle;

    FRowsCount: Integer;
    FColsCount: Integer;

    FByteOrder: Integer;
    FVoidValue: Integer;

    FIsFileOk: Boolean;
    FIsTiff: Boolean;

    FImageWidth: Integer;
    FImageLength: Integer;

    FSampleFormat: Integer;
    FSampleSize: Integer;

    FStripOffsets: Int64;
    FStripOffsetsCount: Cardinal;
    FStripOffsetsType: TStripOffsetsType;
    FStripOffsetsBuffer: Pointer;

    FElevationData: TElevationValue;

    function FindElevationInPlain(
      const ARow, ACol: Integer
    ): Boolean;

    function FindElevationInTiff(
      const ARow, ACol: Integer
    ): Boolean;

    procedure InternalClose;
    function ReadTiffMetadata: Boolean;
  public
    function Open(
      const AFileName: string;
      const ARowsCount: Integer;
      const AColsCount: Integer
    ): Boolean;

    function FindElevation(
      ARow, ACol: Integer;
      out AElevation: Single
    ): Boolean; inline;
  public
    constructor Create(
      const AByteOrder: Integer;
      const AVoidValue: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  NTFiles;

//-----------------------------------------------------------------------------

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
    offset: DWORD; // Bytes 8-11 Value Offset
  end;
  PIFD_12 = ^TIFD_12;

  TIFD_NN = packed record
    number_of_fields: Word;
    items: array of TIFD_12;
    offset_to_next: DWORD;
  end;
  PIFD_NN = ^TIFD_NN;

//-----------------------------------------------------------------------------

procedure SwapInWord(const AWordPtr: PWord); inline;
var
  w: Word;
begin
  w := ((AWordPtr^ and $00ff) shl 8) or
       ((AWordPtr^ and $ff00) shr 8);
  AWordPtr^ := w;
end;

procedure SwapInDWord(const ADWordPtr: PDWord); inline;
var
  d: DWord;
begin
  d := ((ADWordPtr^ and $000000ff) shl 24) or
       ((ADWordPtr^ and $0000ff00) shl 8) or
       ((ADWordPtr^ and $00ff0000) shr 8) or
       ((ADWordPtr^ and $ff000000) shr 24);
  ADWordPtr^ := d;
end;

function GetTiffDWORD(const AByteOrder: Word; const ASource: DWORD): DWORD; inline;
begin
  if AByteOrder = c_TIFF_MM then begin
    // revert
    Result := ASource;
    SwapInDWord(@Result);
  end else begin
    // common
    Result := ASource;
  end;
end;

function GetTiffWORD(const AByteOrder: Word; const ASource: WORD): WORD; inline;
begin
  if AByteOrder = c_TIFF_MM then begin
    // revert
    Result := ASource;
    SwapInWord(@Result);
  end else begin
    // common
    Result := ASource;
  end;
end;

function ReadElevationValue(
  const AFile: THandle;
  const AOffset: Int64;
  var AValue: TElevationValue
): Boolean; inline;
begin
  Result := False;
  case AValue.TypeId of
    evtSmallInt: Result := NtReadFromFile(AFile, @AValue.ValueSmall, SizeOf(AValue.ValueSmall), AOffset);
    evtLongInt: Result := NtReadFromFile(AFile, @AValue.ValueLong, SizeOf(AValue.ValueLong), AOffset);
    evtSingle: Result := NtReadFromFile(AFile, @AValue.ValueSingle, SizeOf(AValue.ValueSingle), AOffset);
  else
    Assert(False);
  end;
end;

{ TTerrainFile }

constructor TTerrainFile.Create(
  const AByteOrder: Integer;
  const AVoidValue: Integer
);
begin
  inherited Create;

  FByteOrder := AByteOrder;
  FVoidValue := AVoidValue;

  FFileHandle := 0;
  FFileName := '';
  FIsFileOk := False;
  FIsTiff := False;

  FStripOffsetsBuffer := nil;
end;

destructor TTerrainFile.Destroy;
begin
  InternalClose;
  inherited Destroy;
end;

function TTerrainFile.Open(
  const AFileName: string;
  const ARowsCount: Integer;
  const AColsCount: Integer
): Boolean;
begin
  // don't try reopen same file on failue
  if (not FIsFileOk) and (FFileName = AFileName) then begin
    Result := False;
    Exit;
  end;

  // if file not opened or opened another file - open this
  if (FFileHandle = 0) or (FFileName <> AFileName) then begin
    InternalClose;

    FFileName := AFileName;
    FRowsCount := ARowsCount;
    FColsCount := AColsCount;

    // open file
    FFileHandle := CreateFile(PChar(FFileName), GENERIC_READ, FILE_SHARE_READ,
      nil, OPEN_EXISTING, 0, 0);

    FIsFileOk := FFileHandle <> INVALID_HANDLE_VALUE;
    if not FIsFileOk then begin
      FFileHandle := 0;
    end;

    if FIsFileOk then begin
      FIsTiff := SameText(ExtractFileExt(FFileName), '.tif');
      if FIsTiff then begin
        FIsFileOk := ReadTiffMetadata;
      end;
    end;
  end;

  Result := FIsFileOk;
end;

procedure TTerrainFile.InternalClose;
begin
  if FStripOffsetsBuffer <> nil then begin
    HeapFree(GetProcessHeap, 0, FStripOffsetsBuffer);
    FStripOffsetsBuffer := nil;
  end;

  if FFileHandle <> 0 then begin
    CloseHandle(FFileHandle);
    FFileHandle := 0;
  end;

  FFileName := '';
  FIsFileOk := False;
end;

function IsVoid(
  const AElevationData: TElevationValue;
  const AVoidValue: Integer
): Boolean; inline;
begin
  if AVoidValue = 0 then begin
    Result := False;
    Exit;
  end;

  case AElevationData.TypeId of
    evtSmallInt: Result := AVoidValue = AElevationData.ValueSmall;
    evtLongInt: Result := AVoidValue = AElevationData.ValueLong;
    evtSingle: Result := AVoidValue = Round(AElevationData.ValueSingle);
  else
    Result := True;
    Assert(False);
  end;
end;

function TTerrainFile.FindElevation(
  ARow, ACol: Integer;
  out AElevation: Single
): Boolean;
begin
  Assert(ARow >= 0);
  Assert(ARow < FRowsCount);

  Assert(ACol >= 0);
  Assert(ACol < FColsCount);

  //ARow := Max(0, ARow);
  //ARow := Min(ARow, FRowsCount-1);

  //ACol := Max(0, ACol);
  //ACol := Min(ACol, FColsCount-1);

  // rows stored from top to bottom
  ARow := (FRowsCount - 1) - ARow;

  if FIsTiff then begin
    Result := FindElevationInTiff(ARow, ACol);
  end else begin
    Result := FindElevationInPlain(ARow, ACol);
  end;

  if not Result then begin
    // fatal error
    FIsFileOk := False;
    Exit;
  end;

  // check byte inversion
  if FByteOrder <> 0 then begin
    case FElevationData.TypeId of
      evtSmallInt: SwapInWord(@FElevationData.ValueSmall);
      evtLongInt: SwapInDWord(@FElevationData.ValueLong);
    end;
  end;

  // check voids
  Result := not IsVoid(FElevationData, FVoidValue);
  if not Result then begin
    Exit;
  end;

  // ok
  case FElevationData.TypeId of
    evtSmallInt: AElevation := FElevationData.ValueSmall;
    evtLongInt: AElevation := FElevationData.ValueLong;
    evtSingle: AElevation := FElevationData.ValueSingle;
  else
    Result := False;
    Assert(False);
  end;
end;

function TTerrainFile.FindElevationInPlain(const ARow, ACol: Integer): Boolean;
var
  VOffset: Int64;
begin
  FElevationData.TypeId := evtSmallInt;

  // The data are stored in row major order
  // (all the data for row 1, followed by all the data for row 2, etc.).

  VOffset := ARow * FColsCount + ACol;
  VOffset := VOffset * SizeOf(FElevationData.ValueSmall);

  Result := NtReadFromFile(
    FFileHandle,
    @FElevationData.ValueSmall,
    SizeOf(FElevationData.ValueSmall),
    VOffset
  );
end;

function TTerrainFile.FindElevationInTiff(const ARow, ACol: Integer): Boolean;
var
  VOffset: Int64;
  VOffsetsSize: Cardinal;
begin
  Result := False;

  if FStripOffsetsCount = 1 then begin
    VOffset := (ARow * FColsCount + ACol) * FSampleSize;
    Result := ReadElevationValue(FFileHandle, FStripOffsets + VOffset, FElevationData);
  end else
  if FStripOffsetsCount = Cardinal(FRowsCount) then begin

    // read StripOffsets into buffer
    if FStripOffsetsBuffer = nil then begin
      VOffsetsSize := FStripOffsetsCount * Cardinal(FStripOffsetsType);

      FStripOffsetsBuffer := HeapAlloc(GetProcessHeap, 0, VOffsetsSize);
      if FStripOffsetsBuffer = nil then begin
        Exit;
      end;

      Result := NtReadFromFile(FFileHandle, FStripOffsetsBuffer, VOffsetsSize, FStripOffsets);
      if not Result then begin
        Exit;
      end;
    end;

    if FStripOffsetsType = sotLong then begin
      // as LONG
      VOffset := PLongWord(INT_PTR(FStripOffsetsBuffer)+ARow*SizeOf(DWORD))^;
      VOffset := VOffset + ACol * FSampleSize;
      Result := ReadElevationValue(FFileHandle, VOffset, FElevationData);
    end else begin
      // as SHORT
      VOffset := PWord(INT_PTR(FStripOffsetsBuffer)+ARow*SizeOf(WORD))^;
      VOffset := VOffset + ACol * FSampleSize;
      Result := ReadElevationValue(FFileHandle, VOffset, FElevationData);
    end;
  end;
end;

function CheckTiffHeader(const ATiffHeader: TTiffHeader): Boolean;
begin
  Result := ((ATiffHeader.ByteOrder = c_TIFF_II) and (ATiffHeader.type_ = c_42_II)) or
            ((ATiffHeader.ByteOrder = c_TIFF_MM) and (ATiffHeader.type_ = c_42_MM));
end;

function TTerrainFile.ReadTiffMetadata: Boolean;
var
  I: Integer;
  VTiffHeader: TTiffHeader;
  VOffset: Int64;
  VNumberOfIFDs: Word;
  VCurrentTag: TIFD_12;
begin
  // read header
  Result := NtReadFromFile(FFileHandle, @VTiffHeader, SizeOf(VTiffHeader), 0);
  if not Result then
    Exit;

  // check header
  Result := CheckTiffHeader(VTiffHeader);
  if not Result then
    Exit;

  VOffset := GetTiffDWORD(VTiffHeader.ByteOrder, VTiffHeader.IFDOffset);

  // get number of IFDs
  Result := NtReadFromFile(FFileHandle, @VNumberOfIFDs, SizeOf(VNumberOfIFDs), VOffset);
  if not Result then
    Exit;

  VNumberOfIFDs := GetTiffWORD(VTiffHeader.ByteOrder, VNumberOfIFDs);
  VOffset := VOffset + SizeOf(VNumberOfIFDs);

  FImageWidth := 0;
  FImageLength := 0;

  FSampleSize := 2; // 16 bit
  FSampleFormat := 2; // signed

  FStripOffsetsCount := 0;

  if VNumberOfIFDs > 0 then begin
    for I := 0 to VNumberOfIFDs - 1 do begin
      // get IFD
      Result := NtReadFromFile(FFileHandle, @VCurrentTag, SizeOf(VCurrentTag), VOffset);
      if not Result then
        Exit;

      case VCurrentTag.tag of
        $100: begin
          // ImageWidth (SHORT or LONG)
          FImageWidth := VCurrentTag.offset;
        end;
        $101: begin
          // ImageLength (SHORT or LONG)
          FImageLength := VCurrentTag.offset;
        end;
        $102: begin
          // BitsPerSample
          FSampleSize := VCurrentTag.offset div 8;
        end;
        $111: begin
          // StripOffsets
          FStripOffsets := VCurrentTag.offset;
          FStripOffsetsCount := VCurrentTag.count;
          if VCurrentTag.type_ = 2 then begin
            FStripOffsetsType := sotShort;
          end else begin
            FStripOffsetsType := sotLong;
          end;
        end;
        $153: begin
          // Specifies how to interpret each data sample in a pixel.
          // 1 = unsigned integer data
          // 2 = two's complement signed integer data
          // 3 = IEEE floating point data
          // 4 = undefined data format
          FSampleFormat := VCurrentTag.offset;
        end;
      end;

      VOffset := VOffset + SizeOf(VCurrentTag);

      {$IFDEF DUMP_TIFF_TAGS}
      OutputDebugString(
        PChar(
          'tag: ' + IntToStr(VCurrentTag.tag) + ' ' +
          'type: ' + IntToStr(VCurrentTag.type_) + ' ' +
          'count: ' + IntToStr(VCurrentTag.count) + ' ' +
          'offset: ' + IntToStr(VCurrentTag.offset) + ' ' +
          'offset: 0x' + IntToHex(VCurrentTag.offset, 8)
        )
      );
      {$ENDIF}
    end;
  end;

  Result := False;

  if FStripOffsetsCount = 0 then begin
    // ToDo: add tiled tiff support
    Exit;
  end;

  if (FColsCount <> FImageWidth) or (FRowsCount <> FImageLength) then begin
    Exit;
  end;
    
  case FSampleFormat of
    2: begin
      if FSampleSize = 2 then begin
        FElevationData.TypeId := evtSmallInt;
      end else
      if FSampleSize = 4 then begin
        FElevationData.TypeId := evtLongInt;
      end else begin
        Assert(False, 'TIFF: Invalid SampleSize value: ' + IntToStr(FSampleSize));
        Exit;
      end;
    end;
    3: begin
      if FSampleSize = 4 then begin
        FElevationData.TypeId := evtSingle;
      end else begin
        Assert(False, 'TIFF: Invalid SampleSize value: ' + IntToStr(FSampleSize));
        Exit;
      end;
    end
  else
    Assert(False, 'TIFF: Unexpected SampleFormat value: ' + IntToStr(FSampleFormat));
    Exit;
  end;

  Result := True;
end;

end.
