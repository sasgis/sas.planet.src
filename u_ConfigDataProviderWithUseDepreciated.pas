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

unit u_ConfigDataProviderWithUseDepreciated;

interface

uses
  Classes,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider,
  u_BaseInterfacedObject;

type
  TConfigDataProviderWithUseDepreciated = class(TBaseInterfacedObject, IConfigDataProvider)
  private
    FSource: IConfigDataProvider;
    FIdentRenamesList: TStringList;
    function GetDepreciatedName(const AIdent: string): string;
  private
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
      const ASource: IConfigDataProvider;
      AIdentRenamesList: TStringList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TConfigDataProviderWithRenamesList }

constructor TConfigDataProviderWithUseDepreciated.Create(
  const ASource: IConfigDataProvider;
  AIdentRenamesList: TStringList
);
begin
  inherited Create;
  FSource := ASource;
  FIdentRenamesList := TStringList.Create;
  FIdentRenamesList.Assign(AIdentRenamesList);
end;

destructor TConfigDataProviderWithUseDepreciated.Destroy;
begin
  FreeAndNil(FIdentRenamesList);
  FSource := nil;
  inherited;
end;

function TConfigDataProviderWithUseDepreciated.GetDepreciatedName(
  const AIdent: string
): string;
begin
  Result := FIdentRenamesList.Values[AIdent];
end;

function TConfigDataProviderWithUseDepreciated.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  Result := FSource.GetSubItem(AIdent);
end;

function TConfigDataProviderWithUseDepreciated.ReadBinary(const AIdent: string): IBinaryData;
var
  VIdent: string;
begin
  Result := FSource.ReadBinary(AIdent);
  if Result = nil then begin
    VIdent := GetDepreciatedName(AIdent);
    if VIdent <> '' then begin
      Result := FSource.ReadBinary(VIdent);
    end;
  end;
end;

function TConfigDataProviderWithUseDepreciated.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadBool(VIdent, Result);
  end;
  Result := FSource.ReadBool(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadDate(VIdent, Result);
  end;
  Result := FSource.ReadDate(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadDateTime(VIdent, Result);
  end;
  Result := FSource.ReadDateTime(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadFloat(VIdent, Result);
  end;
  Result := FSource.ReadFloat(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadInteger(VIdent, Result);
  end;
  Result := FSource.ReadInteger(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadAnsiString(
  const AIdent: string;
  const ADefault: AnsiString
): AnsiString;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadAnsiString(VIdent, Result);
  end;
  Result := FSource.ReadAnsiString(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadString(const AIdent,
  ADefault: string): string;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadString(VIdent, Result);
  end;
  Result := FSource.ReadString(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadSubItemsList: IStringListStatic;
begin
  Result := FSource.ReadSubItemsList;
end;

function TConfigDataProviderWithUseDepreciated.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadTime(VIdent, Result);
  end;
  Result := FSource.ReadTime(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadValuesList: IStringListStatic;
begin
  Result := FSource.ReadValuesList;
end;

end.
