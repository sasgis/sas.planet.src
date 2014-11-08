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

unit u_ConfigDataProviderByIniFileSection;

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider,
  u_BaseInterfacedObject;

type
  TConfigDataProviderByIniFileSection = class(TBaseInterfacedObject, IConfigDataProvider)
  private
    FIniFile: TCustomIniFile;
    FSection: string;
    FParent: IConfigDataProvider;
    FFormatSettings: TFormatSettings;
  protected
    property IniFile: TCustomIniFile read FIniFile;
    property Section: string read FSection;
    property FormatSettings: TFormatSettings read FFormatSettings;
    function GetSubItemSectionName(const AIdent: string): string;
  protected
    function GetSubItem(const AIdent: string): IConfigDataProvider;
    function ReadBinary(const AIdent: string): IBinaryData;
    function ReadAnsiString(
      const AIdent: string;
      const ADefault: AnsiString
    ): AnsiString;
    function ReadString(
      const AIdent: string;
      const ADefault: string
    ): string;
    function ReadInteger(
      const AIdent: string;
      const ADefault: Longint
    ): Longint;
    function ReadBool(
      const AIdent: string;
      const ADefault: Boolean
    ): Boolean;
    function ReadDate(
      const AIdent: string;
      const ADefault: TDateTime
    ): TDateTime;
    function ReadDateTime(
      const AIdent: string;
      const ADefault: TDateTime
    ): TDateTime;
    function ReadFloat(
      const AIdent: string;
      const ADefault: Double
    ): Double;
    function ReadTime(
      const AIdent: string;
      const ADefault: TDateTime
    ): TDateTime;

    function ReadSubItemsList: IStringListStatic;
    function ReadValuesList: IStringListStatic;
  public
    constructor Create(
      AIniFile: TCustomIniFile;
      const ASection: string;
      const AParent: IConfigDataProvider
    );
  end;


implementation

uses
  Math,
  StrUtils,
  u_StringListStatic,
  u_BinaryDataByMemStream;

{ TConfigDataProviderByIniFileSection }

constructor TConfigDataProviderByIniFileSection.Create(
  AIniFile: TCustomIniFile;
  const ASection: string;
  const AParent: IConfigDataProvider
);
begin
  Assert(AIniFile <> nil);
  inherited Create;
  FIniFile := AIniFile;
  FSection := ASection;
  FParent := AParent;
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.DateSeparator := '.';
  FFormatSettings.ShortDateFormat := 'dd.MM.yyyy';
  FFormatSettings.TimeSeparator := ':';
  FFormatSettings.LongTimeFormat := 'HH:mm:ss';
  FFormatSettings.ShortTimeFormat := 'HH:mm:ss';
  FFormatSettings.ListSeparator := ';';
  FFormatSettings.TwoDigitYearCenturyWindow := 50;
end;

function TConfigDataProviderByIniFileSection.GetSubItemSectionName(
  const AIdent: string): string;
begin
  Result := FSection + '_' + AIdent;
end;

function TConfigDataProviderByIniFileSection.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VSectionName: string;
begin
  Result := nil;
  VSectionName := GetSubItemSectionName(AIdent);
  if FIniFile.SectionExists(VSectionName) then begin
    Result := TConfigDataProviderByIniFileSection.Create(FIniFile, VSectionName, Self);
  end;
end;

function TConfigDataProviderByIniFileSection.ReadBinary(const AIdent: string): IBinaryData;
var
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    FIniFile.ReadBinaryStream(FSection, AIdent, VMemStream);
    Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
    VMemStream := nil;
  finally
    VMemStream.Free;
  end;
end;

function TConfigDataProviderByIniFileSection.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
begin
  Result := FIniFile.ReadBool(FSection, AIdent, ADefault);
end;

function TConfigDataProviderByIniFileSection.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  DateStr: string;
begin
  Result := ADefault;
  DateStr := FIniFile.ReadString(FSection, AIdent, '');
  if DateStr <> '' then begin
    try
      Result := StrToDate(DateStr, FFormatSettings);
    except
      on EConvertError do
        // Ignore EConvertError exceptions
      else
        raise;
    end;
  end;
end;

function TConfigDataProviderByIniFileSection.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  DateStr: string;
begin
  DateStr := FIniFile.ReadString(FSection, AIdent, '');
  Result := ADefault;
  if DateStr <> '' then begin
    try
      Result := StrToDateTime(DateStr, FFormatSettings);
    except
      on EConvertError do
        // Ignore EConvertError exceptions
      else
        raise;
    end;
  end;
end;

function TConfigDataProviderByIniFileSection.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
var
  FloatStr: string;
  VComaPos: Integer;
begin
  FloatStr := FIniFile.ReadString(FSection, AIdent, '');
  Result := ADefault;
  if FloatStr <> '' then begin
    if (FloatStr = 'NAN') then begin
      Result := NaN;
    end else begin
      VComaPos := System.pos(',', FloatStr);
      if VComaPos > 0 then begin
        FloatStr[VComaPos] := FFormatSettings.DecimalSeparator;
      end;

      try
        Result := StrToFloat(FloatStr, FFormatSettings);
      except
        on EConvertError do
          // Ignore EConvertError exceptions
        else
          raise;
      end;
    end;
  end;
end;

function TConfigDataProviderByIniFileSection.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
begin
  Result := FIniFile.ReadInteger(FSection, AIdent, ADefault);
end;

function TConfigDataProviderByIniFileSection.ReadAnsiString(
  const AIdent: string;
  const ADefault: AnsiString
): AnsiString;
begin
  Result := AnsiString(FIniFile.ReadString(FSection, AIdent, String(ADefault)));
end;

function TConfigDataProviderByIniFileSection.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := FIniFile.ReadString(FSection, AIdent, ADefault);
end;

function TConfigDataProviderByIniFileSection.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
  i: Integer;
  VSection: string;
  VSectionLen: Integer;
  VcurSect: string;
  VCurSectLen: Integer;
begin
  VList := TStringList.Create;
  try
    FIniFile.ReadSections(VList);
    VcurSect := FSection + '_';
    VCurSectLen := Length(VcurSect);
    for i := VList.Count - 1 downto 0 do begin
      VSection := VList.Strings[i];
      VSectionLen := Length(VSection);
      if VSectionLen <= VCurSectLen then begin
        VList.Delete(i);
      end else if LeftStr(VSection, VCurSectLen) <> VcurSect then begin
        VList.Delete(i);
      end else begin
        VList.Strings[i] := RightStr(VSection, VCurSectLen - VSectionLen);
      end;
    end;
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

function TConfigDataProviderByIniFileSection.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  TimeStr: string;
begin
  TimeStr := FIniFile.ReadString(FSection, AIdent, '');
  Result := ADefault;
  if TimeStr <> '' then begin
    try
      Result := StrToTime(TimeStr, FFormatSettings);
    except
      on EConvertError do
        // Ignore EConvertError exceptions
      else
        raise;
    end;
  end;
end;

function TConfigDataProviderByIniFileSection.ReadValuesList: IStringListStatic;
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  try
    FIniFile.ReadSection(FSection, VList);
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

end.
