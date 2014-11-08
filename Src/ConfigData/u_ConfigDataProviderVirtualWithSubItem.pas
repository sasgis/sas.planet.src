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

unit u_ConfigDataProviderVirtualWithSubItem;

interface

uses
  Classes,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider,
  u_BaseInterfacedObject;

type
  TConfigDataProviderVirtualWithSubItem = class(TBaseInterfacedObject, IConfigDataProvider)
  private
    FSubItemName: string;
    FSubItem: IConfigDataProvider;
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
      const ASubItemName: string;
      const ASubItem: IConfigDataProvider
    );
  end;

implementation

uses
  u_StringListStatic;

{ TConfigDataProviderVirtualWithSubItem }

constructor TConfigDataProviderVirtualWithSubItem.Create(
  const ASubItemName: string;
  const ASubItem: IConfigDataProvider
);
begin
  inherited Create;
  FSubItemName := ASubItemName;
  FSubItem := ASubItem;
end;

function TConfigDataProviderVirtualWithSubItem.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  if AIdent = FSubItemName then begin
    Result := FSubItem;
  end else begin
    Result := nil;
  end;
end;

function TConfigDataProviderVirtualWithSubItem.ReadBinary(const AIdent: string): IBinaryData;
begin
  Result := nil;
end;

function TConfigDataProviderVirtualWithSubItem.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadAnsiString(
  const AIdent: string;
  const ADefault: AnsiString
): AnsiString;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  try
    VList.Add(FSubItemName);
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

function TConfigDataProviderVirtualWithSubItem.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadValuesList: IStringListStatic;
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
