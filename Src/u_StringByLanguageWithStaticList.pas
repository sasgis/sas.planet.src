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

unit u_StringByLanguageWithStaticList;

interface

uses
  Classes,
  i_StringByLanguage,
  u_BaseInterfacedObject;

type
  TStringByLanguageWithStaticList = class(TBaseInterfacedObject, IStringByLanguage)
  private
    FValueList: TStringList;
  private
    function GetString(ALangIndex: Integer): string;
    function GetDefault: string;
  public
    constructor Create(
      AValueList: TStrings
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TStringByLangByStaticList }

constructor TStringByLanguageWithStaticList.Create(AValueList: TStrings);
begin
  inherited Create;
  FValueList := TStringList.Create;
  Assert(AValueList.Count > 0);
  FValueList.Assign(AValueList);
end;

destructor TStringByLanguageWithStaticList.Destroy;
begin
  FreeAndNil(FValueList);
  inherited;
end;

function TStringByLanguageWithStaticList.GetDefault: string;
begin
  Result := FValueList.Strings[0];
end;

function TStringByLanguageWithStaticList.GetString(ALangIndex: Integer): string;
begin
  if (ALangIndex > 0) and (ALangIndex < FValueList.Count) then begin
    Result := FValueList.Strings[ALangIndex];
  end else begin
    Result := GetDefault;
  end;
end;

end.
