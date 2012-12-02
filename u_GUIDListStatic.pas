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

unit u_GUIDListStatic;

interface

uses
  i_GUIDListStatic,
  u_BaseInterfacedObject;

type
  TGUIDListStatic = class(TBaseInterfacedObject, IGUIDListStatic)
  private
    FList: array of TGUID;
  private
    function GetItem(AIndex: Integer): TGUID;
    function GetCount: Integer;
  public
    constructor Create(
      const AList: array of TGUID;
      ACount: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TGUIDListStatic }

constructor TGUIDListStatic.Create(
  const AList: array of TGUID;
  ACount: Integer
);
var
  i: Integer;
begin
  inherited Create;
  SetLength(FList, ACount);
  for i := 0 to ACount - 1 do begin
    FList[i] := AList[i];
  end;
end;

destructor TGUIDListStatic.Destroy;
begin
  FList := nil;
  inherited;
end;

function TGUIDListStatic.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TGUIDListStatic.GetItem(AIndex: Integer): TGUID;
begin
  Result := FList[AIndex];
end;

end.
