{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_UpdateCheckerFunc;

interface

uses
  t_UpdateChecker,
  i_DownloadResult;

type
  TUpdateCheckerFunc = record
    class function ParseDownloadResult(
      const AUpdateChannel: TUpdateChannel;
      const AUpdateSource: TUpdateSource;
      const ADownloadResult: IDownloadResultOk
    ): TUpdateCheckerResult; static;

    class function LatestResultFromArray(
      const AResultArray: TUpdateCheckerResultArray
    ): TUpdateCheckerResult; static; inline;
  end;

implementation

uses
  SysUtils,
  RegExpr;

class function TUpdateCheckerFunc.ParseDownloadResult(
  const AUpdateChannel: TUpdateChannel;
  const AUpdateSource: TUpdateSource;
  const ADownloadResult: IDownloadResultOk
): TUpdateCheckerResult;
const
  CBuildName: array [TUpdateChannel] of AnsiString = (
    'Nightly', 'Release'
  );
  CHostName: array [TUpdateSource] of string = (
    'http://sasgis.org', 'https://bitbucket.org', 'https://github.com'
  );
var
  I: Integer;
  VYear, VMonth, VDay: Word;
  VRegExpr: TRegExpr;
  VResponseData: AnsiString;
  VFileNameExpression: AnsiString;
  VResults: TUpdateCheckerResultArray;
begin
  Result.IsFound := False;

  SetLength(VResponseData, ADownloadResult.Data.Size);
  Move(ADownloadResult.Data.Buffer^, VResponseData[1], ADownloadResult.Data.Size);

  VRegExpr := TRegExpr.Create;
  try
    VFileNameExpression :=
      'SAS\.Planet\.' + CBuildName[AUpdateChannel] + // SAS.Planet.Nightly
      '\.(\d\d)(\d\d)(\d\d)' +                       // .210616
      '(\.(\d+))?' +                                 // .10132
      '\.(zip|7z)';                                  // .7z

    VRegExpr.Expression :=
      'href\s?=\s?[''"]?([^''" >]+(' + VFileNameExpression + '))';

    // href=['"]? says to match "href=", possibly followed by a ' or ".
    // "Possibly" because it's hard to say how horrible the HTML we are looking
    // at is, and the quotes aren't strictly required.

    // [^'" >]+ says to match any characters that aren't ', ", >, or a space.
    // Essentially this is a list of characters that are an end to the URL.

    VRegExpr.ModifierI := True;
    VRegExpr.ModifierG := False;

    if not VRegExpr.Exec(VResponseData) then begin
      Exit;
    end;

    repeat
      I := Length(VResults);
      SetLength(VResults, I+1);

      VResults[I].IsFound := False;

      VResults[I].DownloadUrl := string(VRegExpr.Match[1]);
      VResults[I].OutFileName := string(VRegExpr.Match[2]);

      VYear := StrToIntDef(string(VRegExpr.Match[3]), 0);
      VMonth := StrToIntDef(string(VRegExpr.Match[4]), 0);
      VDay := StrToIntDef(string(VRegExpr.Match[5]), 0);

      if not TryEncodeDate(VYear + 2000, VMonth, VDay, VResults[I].BuildDate) then begin
        Continue;
      end;

      VResults[I].BuildRevision := StrToIntDef(string(VRegExpr.Match[7]), 0);
      VResults[I].BuildType := CBuildName[AUpdateChannel];

      VResults[I].IsFound := True;

    until not VRegExpr.ExecNext;
  finally
    VRegExpr.Free;
  end;

  Result := LatestResultFromArray(VResults);

  if Result.IsFound and (Pos('://', Result.DownloadUrl) = 0) then begin
    Result.DownloadUrl := CHostName[AUpdateSource] + Result.DownloadUrl;
  end;
end;

class function TUpdateCheckerFunc.LatestResultFromArray(
  const AResultArray: TUpdateCheckerResultArray
): TUpdateCheckerResult;
var
  I: Integer;
begin
  Result.IsFound := False;

  for I := 0 to Length(AResultArray) - 1 do begin
    if not AResultArray[I].IsFound then begin
      Continue;
    end;
    if
      (not Result.IsFound) or
      (Result.BuildDate < AResultArray[I].BuildDate) or
      (Result.BuildRevision < AResultArray[I].BuildRevision)
    then begin
      Result := AResultArray[I];
    end;
  end;
end;

end.
