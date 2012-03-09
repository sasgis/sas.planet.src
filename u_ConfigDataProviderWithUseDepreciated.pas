{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_ConfigDataProvider;

type
  TConfigDataProviderWithUseDepreciated = class(TInterfacedObject, IConfigDataProvider)
  private
    FSource: IConfigDataProvider;
    FIdentRenamesList: TStringList;
    function GetDepreciatedName(AIdent: string): string;
  protected
    function GetSubItem(const AIdent: string): IConfigDataProvider; virtual;
    function ReadBinary(const AIdent: string): IBinaryData; virtual;
    function ReadString(const AIdent: string; const ADefault: string): string; virtual;
    function ReadInteger(const AIdent: string; const ADefault: Longint): Longint; virtual;
    function ReadBool(const AIdent: string; const ADefault: Boolean): Boolean; virtual;
    function ReadDate(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;
    function ReadDateTime(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;
    function ReadFloat(const AIdent: string; const ADefault: Double): Double; virtual;
    function ReadTime(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;

    function ReadSubItemsList: IStringListStatic;
    function ReadValuesList: IStringListStatic;
  public
    constructor Create(
      ASource: IConfigDataProvider;
      AIdentRenamesList: TStringList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TConfigDataProviderWithRenamesList }

constructor TConfigDataProviderWithUseDepreciated.Create(
  ASource: IConfigDataProvider; AIdentRenamesList: TStringList);
begin
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
  AIdent: string): string;
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

function TConfigDataProviderWithUseDepreciated.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
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

function TConfigDataProviderWithUseDepreciated.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
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

function TConfigDataProviderWithUseDepreciated.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
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

function TConfigDataProviderWithUseDepreciated.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
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

function TConfigDataProviderWithUseDepreciated.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
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

function TConfigDataProviderWithUseDepreciated.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
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
