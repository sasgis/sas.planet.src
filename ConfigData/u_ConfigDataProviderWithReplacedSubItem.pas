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

unit u_ConfigDataProviderWithReplacedSubItem;

interface

uses
  Classes,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider,
  u_BaseInterfacedObject;

type
  TConfigDataProviderWithReplacedSubItem = class(TBaseInterfacedObject, IConfigDataProvider)
  private
    FSource: IConfigDataProvider;
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
      const ASource: IConfigDataProvider;
      const ASubItemName: string;
      const ASubItem: IConfigDataProvider
    );
  end;

implementation

uses
  u_StringListStatic;

{ TConfigDataProviderWithReplacedSubItem }

constructor TConfigDataProviderWithReplacedSubItem.Create(
  const ASource: IConfigDataProvider;
  const ASubItemName: string;
  const ASubItem: IConfigDataProvider
);
begin
  inherited Create;
  FSource := ASource;
  FSubItemName := ASubItemName;
  FSubItem := ASubItem;
end;

function TConfigDataProviderWithReplacedSubItem.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  if AIdent = FSubItemName then begin
    Result := FSubItem;
  end else begin
    Result := FSource.GetSubItem(AIdent);
  end;
end;

function TConfigDataProviderWithReplacedSubItem.ReadBinary(const AIdent: string): IBinaryData;
begin
  Result := FSource.ReadBinary(AIdent);
end;

function TConfigDataProviderWithReplacedSubItem.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
begin
  Result := FSource.ReadBool(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := FSource.ReadDate(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := FSource.ReadDateTime(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
begin
  Result := FSource.ReadFloat(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
begin
  Result := FSource.ReadInteger(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadAnsiString(
  const AIdent: string;
  const ADefault: AnsiString
): AnsiString;
begin
  Result := FSource.ReadAnsiString(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := FSource.ReadString(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadSubItemsList: IStringListStatic;
var
  VIndex: Integer;
  VList: TStringList;
  i: Integer;
begin
  Result := FSource.ReadSubItemsList;
  VIndex := Result.IndexOf(FSubItemName);
  if VIndex < 0 then begin
    if FSubItem <> nil then begin
      VList := TStringList.Create;
      try
        for i := 0 to Result.Count - 1 do begin
          VList.Add(Result.Items[i]);
        end;
        VList.Add(FSubItemName);
        Result := TStringListStatic.CreateWithOwn(VList);
        VList := nil;
      finally
        VList.Free;
      end;
    end;
  end else begin
    if FSubItem = nil then begin
      VList := TStringList.Create;
      try
        for i := 0 to Result.Count - 1 do begin
          if i <> VIndex then begin
            VList.Add(Result.Items[i]);
          end;
        end;
        VList.Add(FSubItemName);
        Result := TStringListStatic.CreateWithOwn(VList);
        VList := nil;
      finally
        VList.Free;
      end;
    end;
  end;
end;

function TConfigDataProviderWithReplacedSubItem.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := FSource.ReadTime(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadValuesList: IStringListStatic;
begin
  Result := FSource.ReadValuesList;
end;

end.
