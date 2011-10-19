{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BmpUtil;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_OperationNotifier;

type
  TBGR= record
   B: Byte;
   G: Byte;
   R: Byte;
  end;

  PLineRGBb = ^TlineRGBb;
  TLineRGBb = array[0..0] of TBGR;

  TBMPRead = procedure(ALine: Cardinal; AInputArray: PLineRGBb) of object;

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

  TBitmapFile = class
  private
    FStream: TFileStream;
    FBitmapSize: Int64;
    FWidth: Int64;
    FHeight: Int64;
    function WriteHeader(AWidth: Integer; AHeight: Integer): Boolean;
  public
    constructor Create(
      const AFileName: string;
      AWidth: LongInt;
      AHeight: LongInt
    );
    destructor Destroy; override;
    function Write(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AReadCallBack: TBMPRead
    ): Boolean;
  end;

implementation

const
  BMP_MAGIC: Word = $4D42;     // 'BM'
  BMP_SIZE_LIMIT = $FFFFFFFF;  // 4Gb

{ TBitmapFile }

constructor TBitmapFile.Create(
  const AFileName: string;
  AWidth: LongInt;
  AHeight: LongInt
);
begin
  inherited Create;
  FStream := nil;
  FWidth := AWidth;
  FHeight := AHeight;
  FBitmapSize := FWidth * FHeight * 3 + (FWidth mod 4) * FHeight;
  if FBitmapSize < BMP_SIZE_LIMIT then begin
    FStream := TFileStream.Create(AFileName, fmCreate);
    FStream.Size := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + FBitmapSize;
    WriteHeader(AWidth, AHeight);
  end else begin
    raise Exception.Create('Image is too big! Maximum size = 4Gb (current size = '
      + IntToStr(FBitmapSize div (1024*1024*1024)) + ' Gb)');
  end;
end;

destructor TBitmapFile.Destroy;
begin
  if Assigned(FStream) then begin
    FreeAndNil(FStream);
  end;
  inherited Destroy;    
end;

function TBitmapFile.WriteHeader(AWidth: Integer; AHeight: Integer): Boolean;
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

function TBitmapFile.Write(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  AReadCallBack: TBMPRead
): Boolean;
var
  VInputArray: PLineRGBb;
  I: Integer;
  k1,k2: Int64;
  ReadLine: TBMPRead;
begin
  Result := False;
  ReadLine := AReadCallBack;
  if Assigned(FStream) and (Addr(ReadLine) <> nil) then begin
    GetMem(VInputArray, FWidth * 3);
    try
      k1 := FWidth * 3 + (FWidth mod 4);
      k2 := (FHeight - 1) * k1 + SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader);
      for I := 0 to FHeight - 1 do begin
       if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
         Break;
       end;
       ReadLine(I, VInputArray);
       FStream.Position := k2 - I * k1;
       Result := FStream.Write(VInputArray^, k1) = k1;
       if not Result then begin
         Break;
       end;
      end;
    finally
      FreeMem(VInputArray);
    end;
  end;
end;

end.

