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

unit u_ContentDetecter;

interface

function TryDetectContentType(const AData: Pointer; const ASize: Int64;
  const AProposedContentType: RawByteString): RawByteString; inline;

function TryDetectContentTypeByMagic(const AData: Pointer; const ASize: Int64): RawByteString;

function TryDetectContentTypeByWinApi(const AData: Pointer; const ASize: Int64;
  const AProposedContentType: RawByteString): RawByteString;

implementation

uses
  SysUtils,
  UrlMon;

function TryDetectContentTypeByMagic(const AData: Pointer; const ASize: Int64): RawByteString;

type
  TMagic = record
    T: RawByteString;        // content-type
    B: array [0..3] of Byte; // magic bytes
    S: Integer;              // magic size
  end;

  TContentTypeId = (
    ctJpeg,
    ctPng,
    ctWebP,
    ctGif,
    ctBmp,
    ctTiff,
    ctTiff2
  );

const
  CMagic: array [Low(TContentTypeId)..High(TContentTypeId)] of TMagic = (
    (T: 'image/jpeg';  B: ($FF, $D8, $00, $00); S: 2), // ".."
    (T: 'image/png';   B: ($89, $50, $4E, $47); S: 4), // ".PNG"
    (T: 'image/webp';  B: ($52, $49, $46, $46); S: 4), // "RIFF" + 4 bytes + "WEBP"
    (T: 'image/gif';   B: ($47, $49, $46, $38); S: 4), // "GIF8"
    (T: 'image/bmp';   B: ($42, $4D, $00, $00); S: 2), // "BM"
    (T: 'image/tiff';  B: ($4D, $4D, $00, $2A); S: 4), // "MM.*"
    (T: 'image/tiff';  B: ($49, $49, $2A, $00); S: 4)  // "II*."
  );

  function IsWebP(P: PByte; const ASize: Int64): Boolean;
  begin
    // https://developers.google.com/speed/webp/docs/riff_container#webp_file_header
    Result := ASize > 12;
    if Result then begin
      Inc(P, 8);
      Result := PCardinal(P)^ = $50424557;
    end;
  end;

var
  I: TContentTypeId;
begin
  Result := '';

  if (AData = nil) or (ASize <= 0) then begin
    Exit;
  end;

  for I := Low(CMagic) to High(CMagic) do begin
    with CMagic[I] do begin
      if (ASize > S) and CompareMem(AData, @B[0], S) then begin
        if (I = ctWebP) and not IsWebP(AData, ASize) then begin
          Continue;
        end;
        Result := T;
        Exit;
      end;
    end;
  end;
end;

function TryDetectContentTypeByWinApi(const AData: Pointer; const ASize: Int64;
  const AProposedContentType: RawByteString): RawByteString;
const
  // IE9. Returns image/png and image/jpeg instead of image/x-png and image/pjpeg
  FMFD_RETURNUPDATEDIMGMIMES = $20;
var
  VResult: HRESULT;
  VProposed: UnicodeString;
  VContentType: PWideChar;
begin
  if (AData = nil) or (ASize <= 0) then begin
    Result := '';
    Exit;
  end;

  VProposed := UnicodeString(AProposedContentType);

  VResult := UrlMon.FindMimeFromData(nil, nil, AData, ASize, PWideChar(VProposed),
    FMFD_RETURNUPDATEDIMGMIMES, VContentType, 0);

  if VResult = S_OK then begin
    Result := RawByteString(LowerCase(VContentType));

    // fix detected mime types for IE versions prior IE 9
    if Result = 'image/x-png' then begin
      Result := 'image/png';
    end else
    if Result = 'image/pjpeg' then begin
      Result := 'image/jpeg';
    end;
  end else begin
    Result := '';
  end;
end;

function TryDetectContentType(const AData: Pointer; const ASize: Int64;
  const AProposedContentType: RawByteString): RawByteString;
begin
  Assert(AData <> nil);
  Assert(ASize > 0);

  Result := TryDetectContentTypeByMagic(AData, ASize);

  if Result = '' then begin
    Result := TryDetectContentTypeByWinApi(AData, ASize, AProposedContentType);
  end;
end;

end.
