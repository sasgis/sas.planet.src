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

unit u_ContentDecoder;

interface

uses
  Classes,
  SysUtils;

type
  TContentDecoder = record
    class function GetDecodersStr: RawByteString; static;
    class procedure RemoveUnsupportedDecoders(var AEncoders: RawByteString); static;

    class procedure DoDecodeGZip(var AContent: TMemoryStream); static;
    class procedure DoDecodeDeflate(var AContent: TMemoryStream); static;
    class procedure DoDecodeBrotli(var AContent: TMemoryStream); static;
    class procedure DoDecodeZstd(var AContent: TMemoryStream); static;

    class procedure Decode(const AContentEncoding: RawByteString; var AContent: TMemoryStream); static;
  end;

  EContentDecoderError = class(Exception);

implementation

uses
  Types,
  StrUtils,
  mormot.lib.z,
  mormot.core.zip,
  libbrotli,
  libzstd,
  u_GlobalDllName;

type
  TContentEncodingType = (
    etGZip,
    etDeflate,
    etBrotli,
    etZstd,
    etIdentity,
    etUnknown
  );

function GetEncodingType(const AContentEncoding: RawByteString): TContentEncodingType;
begin
  if AContentEncoding = 'gzip' then begin
    Result := etGZip;
  end else
  if AContentEncoding = 'deflate' then begin
    Result := etDeflate;
  end else
  if AContentEncoding = 'br' then begin
    Result := etBrotli;
  end else
  if AContentEncoding = 'zstd' then begin
    Result := etZstd;
  end else
  if AContentEncoding = 'identity' then begin
    Result := etIdentity;
  end else begin
    Result := etUnknown;
  end;
end;

{ TContentDecoder }

class function TContentDecoder.GetDecodersStr: RawByteString;
begin
  Result := 'gzip, deflate';

  if LoadLibBrotliDec(GDllName.BrotliDec, False) then begin
    Result := Result + ', br';
  end;

  if LoadLibZstd(GDllName.Zstd, False) then begin
    Result := Result + ', zstd';
  end;
end;

class procedure TContentDecoder.RemoveUnsupportedDecoders(var AEncoders: RawByteString);
var
  I: Integer;
  VEncoders: TStringDynArray;
  VEncoder: RawByteString;
  VEncodingType: TContentEncodingType;
  VResult: RawByteString;
  VIsSupported: Boolean;
begin
  if AEncoders = '' then begin
    Exit;
  end;

  VResult := '';
  VEncoders := SplitString(LowerCase(string(AEncoders)), ',');

  for I := 0 to High(VEncoders) do begin
    VEncoder := RawByteString(Trim(VEncoders[I]));

    if VEncoder = '' then begin
      Continue;
    end;

    VEncodingType := GetEncodingType(VEncoder);

    case VEncodingType of
      etGZip, etDeflate, etIdentity: begin
        VIsSupported := True;
      end;

      etBrotli: begin
        VIsSupported := LoadLibBrotliDec(GDllName.BrotliDec, False);
      end;

      etZstd: begin
        VIsSupported := LoadLibZstd(GDllName.Zstd, False);
      end;
    else
      VIsSupported := False;
    end;

    if not VIsSupported then begin
      Continue;
    end;

    if VResult <> '' then begin
      VResult := VResult + ', ' + VEncoder;
    end else begin
      VResult := VEncoder;
    end;
  end;

  if AEncoders <> VResult then begin
    AEncoders := VResult;
  end;
end;

class procedure TContentDecoder.DoDecodeGZip(var AContent: TMemoryStream);
var
  VGZRead: TGZRead;
  VStream: TMemoryStream;
begin
  Assert((AContent <> nil) and (AContent.Size > 0));

  VStream := TMemoryStream.Create;
  try
    if VGZRead.Init(AContent.Memory, AContent.Size) and VGZRead.ToStream(VStream) then begin
      FreeAndNil(AContent);
      AContent := VStream;
      AContent.Position := 0;
      VStream := nil;
    end else begin
      raise EContentDecoderError.Create('Gzip decompression failed!');
    end;
  finally
    VStream.Free;
  end;
end;

class procedure TContentDecoder.DoDecodeDeflate(var AContent: TMemoryStream);
var
  VStream: TMemoryStream;
begin
  Assert((AContent <> nil) and (AContent.Size > 0));

  VStream := TMemoryStream.Create;
  try
    try
      UnCompressStream(AContent.Memory, AContent.Size, VStream, nil, True {as zlib format});
    except
      on E1: EZLib do begin
        VStream.Clear;
        try
          UnCompressStream(AContent.Memory, AContent.Size, VStream, nil, False {as raw deflate});
        except
          on E2: EZLib do begin
            raise EContentDecoderError.CreateFmt(
              'Deflate decompression failed (zlib and raw formats): %s | %s', [E1.Message, E2.Message]
            );
          end;
        end;
      end;
    end;
    FreeAndNil(AContent);
    AContent := VStream;
    AContent.Position := 0;
    VStream := nil;
  finally
    VStream.Free;
  end;
end;

class procedure TContentDecoder.DoDecodeBrotli(var AContent: TMemoryStream);
var
  VStream: TMemoryStream;
begin
  Assert((AContent <> nil) and (AContent.Size > 0));

  if not LoadLibBrotliDec(GDllName.BrotliDec, True) then begin
    raise EContentDecoderError.Create('Cannot load Brotli decoder library!');
  end;

  VStream := TMemoryStream.Create;
  try
    DecompressBrotli(AContent.Memory, AContent.Size, VStream);
    FreeAndNil(AContent);
    AContent := VStream;
    AContent.Position := 0;
    VStream := nil;
  finally
    VStream.Free;
  end;
end;

class procedure TContentDecoder.DoDecodeZstd(var AContent: TMemoryStream);
var
  VStream: TMemoryStream;
begin
  Assert((AContent <> nil) and (AContent.Size > 0));

  if not LoadLibZstd(GDllName.Zstd, True) then begin
    raise EContentDecoderError.Create('Cannot load zstd library!');
  end;

  VStream := TMemoryStream.Create;
  try
    DecompressZstd(AContent.Memory, AContent.Size, VStream);
    FreeAndNil(AContent);
    AContent := VStream;
    AContent.Position := 0;
    VStream := nil;
  finally
    VStream.Free;
  end;
end;

class procedure TContentDecoder.Decode(const AContentEncoding: RawByteString; var AContent: TMemoryStream);
var
  VEncoding: TContentEncodingType;
begin
  if (AContent.Size = 0) or (AContentEncoding = '') then begin
    Exit;
  end;

  AContent.Position := 0;

  VEncoding := GetEncodingType(AContentEncoding);

  case VEncoding of
    etGZip     : DoDecodeGZip(AContent);
    etDeflate  : DoDecodeDeflate(AContent);
    etBrotli   : DoDecodeBrotli(AContent);
    etZstd     : DoDecodeZstd(AContent);
    etIdentity : { nothing to do } ;
    etUnknown  : raise EContentDecoderError.CreateFmt('Unknown Content-Encoding: "%s"', [AContentEncoding]);
  else
    raise EContentDecoderError.CreateFmt('Unexpected encoding type value: %d', [Integer(VEncoding)]);
  end;
end;

end.
