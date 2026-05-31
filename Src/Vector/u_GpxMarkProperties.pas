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

unit u_GpxMarkProperties;

interface

uses
  i_VectorDataItemSimple;

type
  TGpxMarkProperties = record
  private
    FDesc: string;
    FCmt: string;
    FTime: TDateTime;
    FType: string;
    FSym: string;
    FNotes: string;
    FTrack: string;
  public
    class function Read(const AMark: IVectorDataItem): TGpxMarkProperties; static;
  public
    property Desc: string read FDesc;
    property Cmt: string read FCmt;
    property Time: TDateTime read FTime;
    property TypeId: string read FType;
    property Sym: string read FSym;
    property Notes: string read FNotes;
    property Track: string read FTrack;
  end;

implementation

uses
  SysUtils,
  ShLwApi;

function ReplaceBrTags(const AText: string): string;
var
  VTextPtr, VEndPtr, VOutPtr, P: PChar;
begin
  if AText = '' then Exit('');

  SetLength(Result, Length(AText));

  VTextPtr := Pointer(AText);
  VOutPtr  := Pointer(Result);

  VEndPtr  := VTextPtr + Length(AText);

  while VTextPtr < VEndPtr do begin
    if (VTextPtr + 2 < VEndPtr) and
       (VTextPtr[0] = '<') and
       ((VTextPtr[1] = 'b') or (VTextPtr[1] = 'B')) and
       ((VTextPtr[2] = 'r') or (VTextPtr[2] = 'R'))
    then begin
      P := VTextPtr + 3;

      if (P < VEndPtr) and (P[0] = ' ') then Inc(P);
      if (P < VEndPtr) and (P[0] = '/') then Inc(P);

      if (P < VEndPtr) and (P[0] = '>') then begin
        VOutPtr^ := #13; Inc(VOutPtr);
        VOutPtr^ := #10; Inc(VOutPtr);

        VTextPtr := P + 1;

        if (VTextPtr < VEndPtr) and (VTextPtr[0] = #13) and (VTextPtr + 1 < VEndPtr) and (VTextPtr[1] = #10) then
          Inc(VTextPtr, 2);

        Continue;
      end;
    end;

    VOutPtr^ := VTextPtr^;
    Inc(VOutPtr);
    Inc(VTextPtr);
  end;

  SetLength(Result, Integer(VOutPtr - PChar(Result)));
end;

procedure TrimRange(const S: string; AStartIdx, AEndIdx: Integer; out ATrimStart, ATrimEnd: Integer);
begin
  ATrimStart := AStartIdx;
  ATrimEnd := AEndIdx;
  while (ATrimStart <= ATrimEnd) and (S[ATrimStart] <= ' ') do Inc(ATrimStart);
  while (ATrimEnd >= ATrimStart) and (S[ATrimEnd] <= ' ') do Dec(ATrimEnd);
end;

function ExtractField(var AText: string; const AFieldName: string): string;
var
  VMatchPtr, VTextPtr, VEndPtr: PChar;
  VMatchPos, VLineEnd, VFieldLen: Integer;
  VStart, VEnd: Integer;
  VLeftPart, VRightPart: string;
begin
  Result := '';
  if (AText = '') or (AFieldName = '') then
    Exit;

  VTextPtr := Pointer(AText);

  VMatchPtr := StrStrI(VTextPtr, Pointer(AFieldName));
  if VMatchPtr = nil then
    Exit;

  VMatchPos := VMatchPtr - VTextPtr + 1;
  VFieldLen := Length(AFieldName);

  VEndPtr := VMatchPtr + VFieldLen;
  while not CharInSet(VEndPtr^, [#0, #10, #13]) do
    Inc(VEndPtr);
  VLineEnd := VEndPtr - VTextPtr + 1;

  TrimRange(AText, VMatchPos + VFieldLen, VLineEnd - 1, VStart, VEnd);
  if VStart <= VEnd then
    Result := Copy(AText, VStart, VEnd - VStart + 1)
  else
    Result := '';

  TrimRange(AText, 1, VMatchPos - 1, VStart, VEnd);
  if VStart <= VEnd then
    VLeftPart := Copy(AText, VStart, VEnd - VStart + 1)
  else
    VLeftPart := '';

  TrimRange(AText, VLineEnd, Length(AText), VStart, VEnd);
  if VStart <= VEnd then
    VRightPart := Copy(AText, VStart, VEnd - VStart + 1)
  else
    VRightPart := '';

  if (VLeftPart = '') and (VRightPart = '') then
    AText := ''
  else if VLeftPart = '' then
    AText := VRightPart
  else if VRightPart = '' then
    AText := VLeftPart
  else
    AText := VLeftPart + #13#10 + VRightPart;
end;

function ExtractTime(var AText: string; const AName: string): TDateTime;

  function TryStrToDateTimeSafe(const S: string; out ADateTime: TDateTime): Boolean;
  const
    CMinValidDate = 36526; // 01.01.2000
    CMaxValidDate = 73413; // 31.12.2100
  var
    VDummy: Double;
  begin
    Result :=
      (S <> '') and
      (not TryStrToFloat(S, VDummy)) and
      TryStrToDateTime(S, ADateTime) and
      (ADateTime > CMinValidDate) and
      (ADateTime < CMaxValidDate);
  end;

var
  VStrTime: string;
begin
  if TryStrToDateTimeSafe(AText, Result) then begin
    AText := '';
    Exit;
  end;

  VStrTime := ExtractField(AText, 'time: ');
  if VStrTime = '' then VStrTime := ExtractField(AText, 'datetime: ');
  if VStrTime = '' then VStrTime := ExtractField(AText, 'date: ');

  if (VStrTime <> '') and TryStrToDateTimeSafe(VStrTime, Result) then begin
    Exit;
  end;

  if not TryStrToDateTimeSafe(AName, Result) then begin
    Result := 0;
  end;
end;

{ TGpxMarkProperties }

class function TGpxMarkProperties.Read(const AMark: IVectorDataItem): TGpxMarkProperties;
begin
  Result.FDesc := Trim(AdjustLineBreaks(AMark.Desc, tlbsCRLF));
  Result.FDesc := ReplaceBrTags(Result.FDesc);

  Result.FCmt  := ExtractField(Result.FDesc, 'cmt: ');
  Result.FType := ExtractField(Result.FDesc, 'type: ');
  Result.FSym  := ExtractField(Result.FDesc, 'sym: ');
  Result.FTrack := LowerCase(ExtractField(Result.FDesc, 'track: '));

  Result.FTime := ExtractTime(Result.FDesc, AMark.Name);

  ExtractField(Result.FDesc, 'number: ');
  ExtractField(Result.FDesc, 'kind: ');
  ExtractField(Result.FDesc, 'GPS Coordinates: ');

  if (Result.FCmt = '') and (Result.FDesc <> '') then begin
    Result.FCmt := Result.FDesc;
    Result.FDesc := '';
  end;

  Result.FNotes := Trim(Result.FDesc + sLineBreak + Result.FCmt);
end;

end.
