{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
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

unit u_ConfigDataWriteProviderByIniFileSection;

interface

uses
  Classes,
  IniFiles,
  i_BinaryData,
  i_ConfigDataWriteProvider,
  u_ConfigDataProviderByIniFileSection;

type
  TConfigDataWriteProviderByIniFileSection = class(TConfigDataProviderByIniFileSection, IConfigDataWriteProvider)
  protected
    function GetOrCreateSubItem(const AIdent: string): IConfigDataWriteProvider;
    procedure DeleteSubItem(const AIdent: string);
    procedure DeleteValue(const AIdent: string);
    procedure DeleteValues;
    procedure WriteBinary(
      const AIdent: string;
      const AValue: IBinaryData
    );
    procedure WriteString(
      const AIdent: string;
      const AValue: string
    );
    procedure WriteAnsiString(
      const AIdent: string;
      const AValue: AnsiString
    );
    procedure WriteInteger(
      const AIdent: string;
      const AValue: Longint
    );
    procedure WriteBool(
      const AIdent: string;
      const AValue: Boolean
    );
    procedure WriteDate(
      const AIdent: string;
      const AValue: TDateTime
    );
    procedure WriteDateTime(
      const AIdent: string;
      const AValue: TDateTime
    );
    procedure WriteFloat(
      const AIdent: string;
      const AValue: Double
    );
    procedure WriteTime(
      const AIdent: string;
      const AValue: TDateTime
    );
  end;

implementation

uses
  SysUtils,
  u_StreamReadOnlyByBinaryData;

{ TConfigDataWriteProviderByIniFileSection }

procedure TConfigDataWriteProviderByIniFileSection.DeleteSubItem(
  const AIdent: string);
begin
  IniFile.EraseSection(GetSubItemSectionName(AIdent));
end;

procedure TConfigDataWriteProviderByIniFileSection.DeleteValue(
  const AIdent: string);
begin
  IniFile.DeleteKey(Section, AIdent);
end;

procedure TConfigDataWriteProviderByIniFileSection.DeleteValues;
begin
  IniFile.EraseSection(Section);
end;

function TConfigDataWriteProviderByIniFileSection.GetOrCreateSubItem(
  const AIdent: string): IConfigDataWriteProvider;
begin
  Result := TConfigDataWriteProviderByIniFileSection.Create(
    IniFile,
    GetSubItemSectionName(AIdent),
    Self
  );
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteBinary(
  const AIdent: string;
  const AValue: IBinaryData
);
var
  VStream: TStream;
begin
  VStream := TStreamReadOnlyByBinaryData.Create(AValue);
  try
    IniFile.WriteBinaryStream(Section, AIdent, VStream);
  finally
    VStream.Free;
  end;
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteBool(
  const AIdent: string;
  const AValue: Boolean
);
begin
  IniFile.WriteBool(Section, AIdent, AValue);
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteDate(
  const AIdent: string;
  const AValue: TDateTime
);
begin
  IniFile.WriteString(Section, AIdent, DateToStr(AValue, FormatSettings));
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteDateTime(
  const AIdent: string;
  const AValue: TDateTime
);
begin
  IniFile.WriteString(Section, AIdent, DateTimeToStr(AValue, FormatSettings));
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteFloat(
  const AIdent: string;
  const AValue: Double
);
begin
  IniFile.WriteString(Section, AIdent, FloatToStr(AValue, FormatSettings));
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteInteger(
  const AIdent: string;
  const AValue: Integer
);
begin
  IniFile.WriteInteger(Section, AIdent, AValue);
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteString(const AIdent,
  AValue: string);
begin
  IniFile.WriteString(Section, AIdent, AValue);
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteAnsiString(
  const AIdent: string;
  const AValue: AnsiString
);
begin
  IniFile.WriteString(Section, AIdent, string(AValue));
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteTime(
  const AIdent: string;
  const AValue: TDateTime
);
begin
  IniFile.WriteString(Section, AIdent, TimeToStr(AValue, FormatSettings));
end;

end.
