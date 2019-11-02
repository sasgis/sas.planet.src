{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_ContentDecoder;

interface

uses
  Classes,
  SysUtils;

type
  EContentDecoderError = class(Exception);

  TContentDecoder = record
    class function GetDecodersStr: AnsiString; static; inline;

    class procedure DoDecodeGZip(var AContent: TMemoryStream); static; inline;
    class procedure DoDecodeDeflate(var AContent: TMemoryStream); static; inline;

    class procedure Decode(const AContentEncoding: AnsiString; var AContent: TMemoryStream); static;
  end;

implementation

uses
  ALZLibEx,
  ALZlibExGZ;

type
  TContentEncodingType = (
    etUnk,
    etGZip,
    etDeflate,
    etBrotli,
    etIdentity
  );

function GetEncodingType(const AContentEncoding: AnsiString): TContentEncodingType; inline;
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
  if AContentEncoding = 'identity' then begin
    Result := etIdentity;
  end else begin
    Result := etUnk;
  end;
end;

{ TContentDecoder }

class function TContentDecoder.GetDecodersStr: AnsiString;
begin
  Result := 'gzip, deflate';
end;

class procedure TContentDecoder.DoDecodeGZip(var AContent: TMemoryStream);
var
  VStream: TMemoryStream;
begin
  VStream := TMemoryStream.Create;
  try
    GZDecompressStream(AContent, VStream);
    FreeAndNil(AContent);
    AContent := VStream;
    VStream := nil;
  finally
    VStream.Free;
  end;
end;

class procedure TContentDecoder.DoDecodeDeflate(var AContent: TMemoryStream);
var
  VStream: TMemoryStream;
begin
  if AContent.Size < 2 then begin
    raise EContentDecoderError.CreateFmt(
      'Content size is too small (%d byte) for "deflate" encoding!',
      [AContent.Size]
    );
  end;

  VStream := TMemoryStream.Create;
  try
    if PWord(AContent.Memory)^ = $9C78 { Zlib Header Magic } then begin
      // deflate with zlib header
      ZDecompressStream(AContent, VStream);
    end else begin
      // raw deflate without zlib header
      ZDecompressStream2(AContent, VStream, -15);
    end;
    FreeAndNil(AContent);
    AContent := VStream;
    VStream := nil;
  finally
    VStream.Free;
  end;
end;

procedure DoDecodeBrotli(var AContent: TMemoryStream); inline;
begin
  // ToDo
  raise EContentDecoderError.Create('Brotli encoding is not supported yet.');
end;

class procedure TContentDecoder.Decode(const AContentEncoding: AnsiString; var AContent: TMemoryStream);
var
  VEncoding: TContentEncodingType;
begin
  AContent.Position := 0;

  if (AContent.Size = 0) or (AContentEncoding = '') then begin
    Exit;
  end;

  VEncoding := GetEncodingType(AContentEncoding);

  case VEncoding of
    etUnk      : raise EContentDecoderError.Create('Unknown Encoding: ' + AContentEncoding);
    etGZip     : DoDecodeGZip(AContent);
    etDeflate  : DoDecodeDeflate(AContent);
    etBrotli   : DoDecodeBrotli(AContent);
    etIdentity : { nothing to do } ;
  else
    raise EContentDecoderError.CreateFmt('Unexpected encoding type value: %d', [Integer(VEncoding)]);
  end;
end;

end.
