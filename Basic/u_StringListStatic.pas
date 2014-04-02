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

unit u_StringListStatic;

interface

uses
  Classes,
  i_StringListStatic,
  u_BaseInterfacedObject;

type
  TStringListStatic = class(TBaseInterfacedObject, IStringListStatic)
  private
    FList: TStringList;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): string;
    function IndexOf(const S: string): Integer;
  public
    constructor CreateByStrings(AList: TStrings);
    constructor CreateWithOwn(var AList: TStringList);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TStringListStatic }

constructor TStringListStatic.CreateByStrings(AList: TStrings);
var
  VList: TStringList;
begin
  VList := TStringList.Create;
  try
    VList.Assign(AList);
    CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

constructor TStringListStatic.CreateWithOwn(var AList: TStringList);
begin
  inherited Create;
  FList := AList;
  AList := nil;
end;

destructor TStringListStatic.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TStringListStatic.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TStringListStatic.GetItem(AIndex: Integer): string;
begin
  Result := FList.Strings[AIndex];
end;

function TStringListStatic.IndexOf(const S: string): Integer;
begin
  Result := FList.IndexOf(S);
end;

end.
