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

unit u_ConfigDataProviderWithGlobal;

interface

uses
  Classes,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider,
  u_BaseInterfacedObject;

type
  TConfigDataProviderWithGlobal = class(TBaseInterfacedObject, IConfigDataProvider)
  private
    FProviderMain: IConfigDataProvider;
    FProviderGlobalPrefix: string;
    FProviderGlobalPrefixLen: Integer;
    FProviderGlobal: IConfigDataProvider;
  protected
    property ProviderGlobalPrefix: string read FProviderGlobalPrefix;
    property ProviderGlobal: IConfigDataProvider read FProviderGlobal;
    function PrepareIdent(
      const AIdent: string;
      out AUseMain: Boolean
    ): string;
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
      const AProviderMain: IConfigDataProvider;
      const AProviderGlobalPrefix: string;
      const AProviderGlobal: IConfigDataProvider
    );
  public

  end;

implementation

uses
  StrUtils,
  u_StringListStatic;

{ TConfigDataProviderWithGlobal }

constructor TConfigDataProviderWithGlobal.Create(
  const AProviderMain: IConfigDataProvider;
  const AProviderGlobalPrefix: string;
  const AProviderGlobal: IConfigDataProvider
);
begin
  inherited Create;
  FProviderMain := AProviderMain;
  FProviderGlobalPrefix := AProviderGlobalPrefix;
  FProviderGlobalPrefixLen := Length(AProviderGlobalPrefix);
  FProviderGlobal := AProviderGlobal;
end;

function TConfigDataProviderWithGlobal.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VIdent: string;
  VUseMain: Boolean;
  VSubItemMain: IConfigDataProvider;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      VSubItemMain := FProviderMain.GetSubItem(VIdent);
      Result := TConfigDataProviderWithGlobal.Create(VSubItemMain, FProviderGlobalPrefix, FProviderGlobal);
    end else begin
      Result := Self;
    end;
  end else begin
    Result := FProviderGlobal.GetSubItem(VIdent);
  end;
end;

function TConfigDataProviderWithGlobal.PrepareIdent(
  const AIdent: string;
  out AUseMain: Boolean
): string;
begin
  if LeftStr(AIdent, FProviderGlobalPrefixLen) = FProviderGlobalPrefix then begin
    Result := MidStr(AIdent, FProviderGlobalPrefixLen + 1, Length(AIdent));
    AUseMain := False;
  end else begin
    Result := AIdent;
    AUseMain := True;
  end;
end;

function TConfigDataProviderWithGlobal.ReadBinary(const AIdent: string): IBinaryData;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := nil;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadBinary(VIdent);
    end;
  end else begin
    Result := FProviderGlobal.ReadBinary(VIdent);
  end;
end;

function TConfigDataProviderWithGlobal.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadBool(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadBool(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadDate(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadDate(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadDateTime(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadDateTime(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadFloat(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadFloat(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadInteger(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadInteger(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadAnsiString(
  const AIdent: string;
  const ADefault: AnsiString
): AnsiString;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadAnsiString(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadAnsiString(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadString(const AIdent,
  ADefault: string): string;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadString(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadString(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
begin
  if FProviderMain <> nil then begin
    Result := FProviderMain.ReadSubItemsList;
  end else begin
    VList := TStringList.Create;
    try
      Result := TStringListStatic.CreateWithOwn(VList);
      VList := nil;
    finally
      VList.Free;
    end;
  end;
end;

function TConfigDataProviderWithGlobal.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadTime(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadTime(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadValuesList: IStringListStatic;
var
  VList: TStringList;
begin
  if (FProviderMain <> nil) then begin
    Result := FProviderMain.ReadValuesList;
  end else begin
    VList := TStringList.Create;
    try
      Result := TStringListStatic.CreateWithOwn(VList);
      VList := nil;
    finally
      VList.Free;
    end;
  end;
end;

end.
