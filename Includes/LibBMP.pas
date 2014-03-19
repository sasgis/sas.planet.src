{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit LibBMP;

interface

uses
  Windows,
  Classes,
  SysUtils;

type
  TBitmapFileHeader = packed record  // File Header for Windows/OS2 bitmap file
    Magic: Word;                     // Сигнатура: 'BM'
    Size: LongWord;                  // Размер файла
    Reserved1: Word;                 // -
    Reserved2: Word;                 // -
    Offset: LongWord;                // Смещение от начала файла до начала изображения
  end;

  TBitmapInfoHeader = packed record  // Info Header for Windows bitmap file
    Size: LongWord;                  // Длина заголовка, byte (=40)
    Width: LongInt;                  // Ширина изображения, pix
    Height: LongInt;                 // Высота изображения, pix
    Planes: Word;                    // Число плоскостей (=1)
    BitCount: Word;                  // Глубина цвета, bit/pix (=24)
    Compression: LongWord;           // Тип сжатия для сжатых изображений (=0)
    SizeImage: LongWord;             // Размер изображения, byte
    XPelsPerMeter: LongInt;          // Горизонтальное разрешение в пикселах на метр
    YPelsPerMeter: LongInt;          // Вертикальное разрешение в пикселах на метр
    ClrUsed: LongInt;                // Число цветов (0 - использовать максимально-допустимое)
    ClrImportant: LongInt;           // Число основных цветов
  end;

  TBitmapStream = class(TObject)
  protected
    FStream: TStream;
    FBitmapSize: Int64;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FEnabled: Boolean;
    k1,k2: Int64;
    function WriteHeader(AWidth: Cardinal; AHeight: Cardinal): Boolean;
  public
    constructor Create(
      AOutPutStream: TStream;
      AWidth: Cardinal;
      AHeight: Cardinal
    );
    function WriteLine(ALineNumber: Integer; APLine: Pointer): Boolean;
  end;

  TBitmapFile = class(TBitmapStream)
  public
    constructor Create(
      const AFileName: string;
      AWidth: Cardinal;
      AHeight: Cardinal
    );
    destructor Destroy; override;
  end;

implementation

const
  BMP_MAGIC: Word = $4D42; // 'BM'
  BMP_SIZE_LIMIT = $80000000; // 2Gb

{ TBitmapFile }

constructor TBitmapFile.Create(
  const AFileName: string;
  AWidth: Cardinal;
  AHeight: Cardinal
);
begin
  inherited Create(TFileStream.Create(AFileName, fmCreate), AWidth, AHeight);
end;

destructor TBitmapFile.Destroy;
begin
  if Assigned(FStream) then begin
    FreeAndNil(FStream);
  end;
  inherited Destroy;
end;

{ TBitmapStream }

constructor TBitmapStream.Create(
  AOutPutStream: TStream;
  AWidth: Cardinal;
  AHeight: Cardinal
);
const
  BMP_ERR_MSG = 'Output image size is too big!'#13#10'Maximum size = %d Mb (output size = %d Mb)';
begin
  inherited Create;
  FEnabled := False;
  FStream := AOutPutStream;
  FWidth := AWidth;
  FHeight := AHeight;
  FBitmapSize := FWidth * FHeight * 3 + (FWidth mod 4) * FHeight;
  if FBitmapSize < BMP_SIZE_LIMIT then begin
    FStream.Size := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + FBitmapSize;
    FEnabled := WriteHeader(AWidth, AHeight);
    if FEnabled then begin
      k1 := FWidth * 3 + (FWidth mod 4);
      k2 := (FHeight - 1) * k1 + SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader);
    end;
  end else begin
    raise Exception.CreateFmt(BMP_ERR_MSG, [BMP_SIZE_LIMIT div 1024 div 1024, FBitmapSize div 1024 div 1024]);
  end;
end;

function TBitmapStream.WriteHeader(AWidth: Cardinal; AHeight: Cardinal): Boolean;
var
  VFileHeader: TBitmapFileHeader;
  VInfoHeader: TBitmapInfoHeader;
begin
  Result := False;

  ZeroMemory(@VFileHeader, SizeOf(TBitmapFileHeader));
  VFileHeader.Magic := BMP_MAGIC;
  VFileHeader.Size := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + FBitmapSize;
  VFileHeader.Offset := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader);

  ZeroMemory(@VInfoHeader, SizeOf(TBitmapInfoHeader));
  VInfoHeader.Size := SizeOf(TBitmapInfoHeader);
  VInfoHeader.Width := AWidth;
  VInfoHeader.Height := AHeight;
  VInfoHeader.Planes := 1;
  VInfoHeader.BitCount := 24;
  VInfoHeader.SizeImage := FBitmapSize;

  if Assigned(FStream) then begin
    FStream.Position := 0;
    if FStream.Write(VFileHeader, SizeOf(TBitmapFileHeader)) = SizeOf(TBitmapFileHeader) then begin
      Result := FStream.Write(VInfoHeader, SizeOf(TBitmapInfoHeader)) = SizeOf(TBitmapInfoHeader);
    end;
  end;
end;

function TBitmapStream.WriteLine(ALineNumber: Integer; APLine: Pointer): Boolean;
begin
  Result := False;
  if FEnabled then begin
    FStream.Position := k2 - ALineNumber * k1;
    Result := FStream.Write(APLine^, k1) = k1;
  end;
end;

end.

