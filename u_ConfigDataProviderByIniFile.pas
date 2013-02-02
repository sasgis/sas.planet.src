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

unit u_ConfigDataProviderByIniFile;

interface

uses
  Classes,
  IniFiles,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider,
  u_BaseInterfacedObject;

type
  TConfigDataProviderByIniFile = class(TBaseInterfacedObject, IConfigDataProvider)
  private
    FIniFile: TCustomIniFile;
  protected
    property IniFile: TCustomIniFile read FIniFile;
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
    constructor CreateWithOwn(AIniFile: TCustomIniFile);
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  u_StringListStatic,
  u_ConfigDataProviderByIniFileSection;

{ TConfigDataProviderByIniFile }

constructor TConfigDataProviderByIniFile.CreateWithOwn(AIniFile: TCustomIniFile);
begin
  Assert(AIniFile <> nil);
  inherited Create;
  FIniFile := AIniFile;
end;

destructor TConfigDataProviderByIniFile.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited;
end;

function TConfigDataProviderByIniFile.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  Result := nil;
  if FIniFile.SectionExists(AIdent) then begin
    Result := TConfigDataProviderByIniFileSection.Create(FIniFile, AIdent, Self);
  end;
end;

function TConfigDataProviderByIniFile.ReadBinary(const AIdent: string): IBinaryData;
begin
  Result := nil;
end;

function TConfigDataProviderByIniFile.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadAnsiString(
  const AIdent: string;
  const ADefault: AnsiString
): AnsiString;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  try
    FIniFile.ReadSections(VList);
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

function TConfigDataProviderByIniFile.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadValuesList: IStringListStatic;
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  try
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

end.
