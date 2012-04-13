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
    procedure WriteBinary(const AIdent: string; AValue: IBinaryData);
    procedure WriteString(const AIdent: string; const AValue: string);
    procedure WriteInteger(const AIdent: string; const AValue: Longint);
    procedure WriteBool(const AIdent: string; const AValue: Boolean);
    procedure WriteDate(const AIdent: string; const AValue: TDateTime);
    procedure WriteDateTime(const AIdent: string; const AValue: TDateTime);
    procedure WriteFloat(const AIdent: string; const AValue: Double);
    procedure WriteTime(const AIdent: string; const AValue: TDateTime);
  end;

implementation

uses
  SysUtils,
  u_StreamReadOnlyByBinaryData;

{ TConfigDataWriteProviderByIniFileSection }

procedure TConfigDataWriteProviderByIniFileSection.DeleteSubItem(
  const AIdent: string);
begin
  FIniFile.EraseSection(GetSubItemSectionName(AIdent));
end;

procedure TConfigDataWriteProviderByIniFileSection.DeleteValue(
  const AIdent: string);
begin
  FIniFile.DeleteKey(FSection, AIdent);
end;

procedure TConfigDataWriteProviderByIniFileSection.DeleteValues;
begin
  FIniFile.EraseSection(FSection);
end;

function TConfigDataWriteProviderByIniFileSection.GetOrCreateSubItem(
  const AIdent: string): IConfigDataWriteProvider;
begin
  Result := TConfigDataWriteProviderByIniFileSection.Create(
    FIniFile,
    GetSubItemSectionName(AIdent),
    Self
  );
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteBinary(
  const AIdent: string;
  AValue: IBinaryData
);
var
  VStream: TStream;
begin
  VStream := TStreamReadOnlyByBinaryData.Create(AValue);
  try
    FIniFile.WriteBinaryStream(FSection, AIdent, VStream);
  finally
    VStream.Free;
  end;
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteBool(
  const AIdent: string; const AValue: Boolean);
begin
  FIniFile.WriteBool(FSection, AIdent, AValue);
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteDate(
  const AIdent: string; const AValue: TDateTime);
begin
  FIniFile.WriteString(FSection, AIdent, DateToStr(AValue, FFormatSettings));
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteDateTime(
  const AIdent: string; const AValue: TDateTime);
begin
  FIniFile.WriteString(FSection, AIdent, DateTimeToStr(AValue, FFormatSettings));
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteFloat(
  const AIdent: string; const AValue: Double);
begin
  FIniFile.WriteString(FSection, AIdent, FloatToStr(AValue, FFormatSettings));
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteInteger(
  const AIdent: string; const AValue: Integer);
begin
  FIniFile.WriteInteger(FSection, AIdent, AValue);
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteString(const AIdent,
  AValue: string);
begin
  FIniFile.WriteString(FSection, AIdent, AValue);
end;

procedure TConfigDataWriteProviderByIniFileSection.WriteTime(
  const AIdent: string; const AValue: TDateTime);
begin
  FIniFile.WriteString(FSection, AIdent, TimeToStr(AValue, FFormatSettings));
end;

end.
