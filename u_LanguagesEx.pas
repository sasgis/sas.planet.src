{******************************************************************************}
{* SAS.Planet (SAS.œÎ‡ÌÂÚ‡)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: GTLanguagesEx.PAS, released on 2003-05-25.

The Initial Developer of the Original Code is Peter Thˆrnqvist [peter3@peter3.com]
Portions created by Peter Thˆrnqvist are Copyright (C) 2003 Peter Thˆrnqvist.
All Rights Reserved.

Contributor(s): Olivier Sannier [obones@altern.org].

You may retrieve the latest version of this file at the Connection Manager
home page, located at http://cnxmanager.sourceforge.net

Known Issues: none to date.
-----------------------------------------------------------------------------}
unit u_LanguagesEx;

{$IFDEF LINUX}
{$MESSAGE Fatal 'Not implemented under Linux'}
{$ENDIF}
{$IFDEF MSWINDOWS}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes,
  Windows,
  SysUtils;

type
  TLanguagesEx = class(TLanguages)
  private
    FISO639: TStrings;
    FISO3166: TStrings;
    FGetText: TStrings;
    FEngNames: TStrings;
    function GetIDFromFromISO639Name(const ISO639Name: string): LCID;
    function GetIDFromISO3166Name(const ISO3166Name: string): LCID;
    function GetISO3166NameFromID(ID: LCID): string;
    function GetISO639NameFromID(ID: LCID): string;
    procedure GetExtraInfo;
    function GetGNUGetTextID(const Name: string): LCID;
    function GetGNUGetTextNames(ID: LCID): string;
    function GetEngNameFromLocaleID(ID: LCID): string;
  public
    constructor Create;
    destructor Destroy; override;
    // ISO639 names are the 2 letter language abbreviation
    property IDFromFromISO639Name[const ISO639Name: string]: LCID read GetIDFromFromISO639Name;
    property ISO639NameFromID[ID: LCID]: string read GetISO639NameFromID;
    // ISO3166 names are the 2 letter country abbreviation
    property IDFromISO3166Name[const ISO3166Name: string]: LCID read GetIDFromISO3166Name;
    property ISO3166NameFromID[ID: LCID]: string read GetISO3166NameFromID;

    property GNUGetTextName[ID: LCID]: string read GetGNUGetTextNames;
    property GNUGetTextID[const Name: string]: LCID read GetGNUGetTextID;

    property EngNameFromLocaleID[ID: LCID]: string read GetEngNameFromLocaleID;
  end;

implementation

uses
  SysConst;

{ TLanguagesEx }

constructor TLanguagesEx.Create;
begin
  inherited Create;
  FISO639 := TStringlist.Create;
  //  TStringlist(FISO639).Sorted := true;
  FISO3166 := TStringlist.Create;
  //  TStringlist(FISO3166).Sorted := true;
  FGetText := TStringlist.Create;
  TStringlist(FGetText).Sorted := true;
  FEngNames := TStringList.Create;
  GetExtraInfo;
end;

destructor TLanguagesEx.Destroy;
begin
  FreeAndNil(FISO639);
  FreeAndNil(FISO3166);
  FreeAndNil(FGetText);
  FreeAndNil(FEngNames);
  inherited;
end;

function TLanguagesEx.GetEngNameFromLocaleID(ID: LCID): string;
var
  i: integer;
begin
  i := FEngNames.IndexOfObject(TObject(ID));
  if i >= 0 then begin
    Result := FEngNames[i];
  end else begin
    Result := SUnknown;
  end;
end;

procedure TLanguagesEx.GetExtraInfo;
var
  i, AID: integer;
  S, T: string;
  VEngNameLen: Integer;
  VEngName: string;
begin
  FISO639.Clear;
  FISO3166.Clear;
  FGetText.Clear;
  FEngNames.Clear;
  SetLength(S, 4); // should be enough
  for i := 0 to Count - 1 do begin
    AID := LocaleID[i];
    if GetLocaleInfo(AID, LOCALE_SISO3166CTRYNAME, @S[1], 4) <> 0 then begin
      T := string(PChar(S));
      FISO3166.AddObject(T, TObject(AID));
    end;
    if GetLocaleInfo(AID, LOCALE_SISO639LANGNAME, @S[1], 4) <> 0 then begin
      FISO639.AddObject(string(PChar(S)), TObject(AID));
    end;
    // add both language and lang_country combination
    FGetText.AddObject(string(PChar(S)), TObject(AID));
    FGetText.AddObject(string(PChar(S)) + '_' + T, TObject(AID));
    VEngNameLen := GetLocaleInfo(AID, LOCALE_SENGLANGUAGE, nil, 0);
    if VEngNameLen > 0 then begin
      SetLength(VEngName, VEngNameLen);
      GetLocaleInfo(AID, LOCALE_SENGLANGUAGE, @VEngName[1], VEngNameLen);
      FEngNames.AddObject(string(PChar(VEngName)), TObject(AID));
    end;
  end;
end;

function TLanguagesEx.GetGNUGetTextID(const Name: string): LCID;
var
  i: integer;
begin
  i := FGetText.IndexOf(Name);
  if i >= 0 then begin
    Result := LCID(FGetText.Objects[i]);
  end else begin
    Result := 0;
  end;
end;

function TLanguagesEx.GetGNUGetTextNames(ID: LCID): string;
var
  i: integer;
begin
  i := FGetText.IndexOfObject(TObject(ID));
  if i >= 0 then begin
    Result := FGetText[i];
  end else begin
    Result := SUnknown;
  end;
end;

function TLanguagesEx.GetIDFromFromISO639Name(const ISO639Name: string): LCID;
var
  i: integer;
begin
  i := FISO639.IndexOf(ISO639Name);
  if i >= 0 then begin
    Result := LCID(FISO639.Objects[i]);
  end else begin
    Result := 0;
  end;
end;

function TLanguagesEx.GetIDFromISO3166Name(const ISO3166Name: string): LCID;
var
  i: integer;
begin
  i := FISO3166.IndexOf(ISO3166Name);
  if i >= 0 then begin
    Result := LCID(FISO3166.Objects[i]);
  end else begin
    Result := 0;
  end;
end;

function TLanguagesEx.GetISO3166NameFromID(ID: LCID): string;
var
  i: integer;
begin
  i := FISO3166.IndexOfObject(TObject(ID));
  if i >= 0 then begin
    Result := FISO3166[i];
  end else begin
    Result := '';
  end;
end;

function TLanguagesEx.GetISO639NameFromID(ID: LCID): string;
var
  i: integer;
begin
  i := FISO639.IndexOfObject(TObject(ID));
  if i >= 0 then begin
    Result := FISO639[i];
  end else begin
    Result := '';
  end;
end;

{$ENDIF}

end.
