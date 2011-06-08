unit u_YaMobileWrite;

interface

uses
  Windows,
  SysUtils,
  Classes;

procedure WriteTileToYaCache(ATile: TPoint; AZoom, AMapType, sm_xy: Byte; AExportPath: string; ATileStream: TMemoryStream; AReplace: Boolean);

implementation

const
  YaHeaderSize: integer = 1024;

function GetMobileFile(X,Y: Integer; Z: Byte; AMapType: Byte): string;
var
  Mask, Num: Integer;
begin
  Result := IntToStr(Z) + PathDelim;
  if(Z > 15) then
  begin
    Mask := (1 shl (Z-15))-1;
    Num  := (((X shr 15) and Mask) shl 4) + ((Y shr 15) and Mask);
    Result := Result + IntToHex(Num, 2) + PathDelim;
  end;
  if(Z > 11) then
  begin
    Mask := (1 shl (Z-11))-1;
    Mask := Mask and $F;
    Num  := (((X shr 11) and Mask) shl 4) + ((Y shr 11) and Mask);
    Result := Result + IntToHex(Num, 2) + PathDelim;
  end;
  if(Z > 7) then
  begin
    Mask := (1 shl (Z-7))-1;
    Mask := Mask and $F;
    Num  := (((X shr 7) and Mask) shl 8) + (((Y shr 7) and Mask) shl 4) + AMapType;
  end
  else
    Num := AMapType;    
  Result := LowerCase(Result + IntToHex(Num, 3));
end;

function TileToTablePos(ATile: TPoint): Integer;
var
  X,Y: Integer;
begin
  X := ATile.X and $7F;
  Y := ATile.Y and $7F;
  Result := ((Y and $40) shl 9) +
            ((X and $40) shl 8) +
            ((Y and $20) shl 8) +
            ((X and $20) shl 7) +
            ((Y and $10) shl 7) +
            ((X and $10) shl 6) +
            ((Y and $08) shl 6) +
            ((X and $08) shl 5) +
            ((Y and $04) shl 5) +
            ((X and $04) shl 4) +
            ((Y and $02) shl 4) +
            ((X and $02) shl 3) +
            ((Y and $01) shl 3) +
            ((X and $01) shl 2);
end;

procedure CreateNilFile(AFileName: string; ATableSize: Integer);
var
  VYaMob: TMemoryStream;
  VInitSize: Integer;
  VPath: string;
begin
  VYaMob := TMemoryStream.Create;
  try
    VPath := copy(AFileName, 1, LastDelimiter(PathDelim, AFileName));
    if not(DirectoryExists(VPath)) then
      if not ForceDirectories(VPath) then
        Exit;
    VInitSize := YaHeaderSize + 6*(sqr(ATableSize));
    VYaMob.SetSize(VInitSize);
    FillChar(VYaMob.Memory^, VInitSize, 0);
    VYaMob.Position := 0;
    VYaMob.Write('YNDX', 4);        // Magic = "YNDX"
    VYaMob.Write(#01#00, 2);        // Reserved
    VYaMob.Write(YaHeaderSize, 4);  // HeadSize = 1024 byte
    VYaMob.Write(#00#00#00, 3);     // "Author"
    VYaMob.SaveToFile(AFileName);
  finally
    VYaMob.Free;
  end;
end;

procedure WriteTileToYaCache(ATile: TPoint; AZoom, AMapType, sm_xy: Byte; AExportPath: string; ATileStream: TMemoryStream; AReplace: Boolean);
var
  VYaMobileFile: string;
  VYaMobileStream: TFileStream;
  VTablePos: Integer;
  VTableOffset: Integer;
  VTableSize: Integer;
  VTileOffset: Integer;
  VExistsTileOffset: Integer;
  VTileSize: SmallInt;
  VHead: array [0..12] of byte;
begin
  if AZoom > 7 then
    VTableSize := 256
  else
    VTableSize := 2 shl AZoom;

  VYaMobileFile := AExportPath + GetMobileFile(ATile.X, ATile.Y, AZoom, AMapType);

  if not FileExists(VYaMobileFile) then
    CreateNilFile(VYaMobileFile, VTableSize);

  VYaMobileStream := TFileStream.Create(VYaMobileFile, fmOpenReadWrite or fmShareExclusive);
  try
    VYaMobileStream.Read(VHead, Length(VHead));
    VTableOffset := ( VHead[6] or (VHead[7] shl 8) or (VHead[8] shl 16) or (VHead[9] shl 24) );
    VTablePos := TileToTablePos(ATile)*6 + sm_xy*6;
    VTileOffset := VYaMobileStream.Size;
    VTileSize := ATileStream.Size;
    VYaMobileStream.Position := VTableOffset + VTablePos;
    VYaMobileStream.Read(VExistsTileOffset, 4);
    if (VExistsTileOffset = 0) or AReplace then
    begin
      VYaMobileStream.Position := VTableOffset + VTablePos;
      VYaMobileStream.Write(VTileOffset, 4);
      VYaMobileStream.Write(VTileSize, 2);
      VYaMobileStream.Position := VYaMobileStream.Size;
      VYaMobileStream.Write(ATileStream.Memory^, VTileSize);
    end;
  finally
    VYaMobileStream.Free;
  end;
end;

end.
