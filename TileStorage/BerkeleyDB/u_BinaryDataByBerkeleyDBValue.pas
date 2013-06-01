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

unit u_BinaryDataByBerkeleyDBValue;

interface

uses
  i_BinaryData,
  i_BerkeleyDBKeyValue,
  u_BaseInterfacedObject;

type
  TBinaryDataByBerkeleyDBValue = class(TBaseInterfacedObject, IBinaryData)
  private
    FValue: IBerkeleyDBValue;
  private
    function GetBuffer: Pointer;
    function GetSize: Integer;
  public
    constructor Create(const AValue: IBerkeleyDBValue);
    destructor Destroy; override;
  end;

implementation

{ TBinaryDataByBerkeleyDBValue }

constructor TBinaryDataByBerkeleyDBValue.Create(const AValue: IBerkeleyDBValue);
begin
  Assert(AValue <> nil);
  inherited Create;
  FValue := AValue;
end;

destructor TBinaryDataByBerkeleyDBValue.Destroy;
begin
  FValue := nil;
  inherited;
end;

function TBinaryDataByBerkeleyDBValue.GetBuffer: Pointer;
begin
  Result := FValue.TileBody;
end;

function TBinaryDataByBerkeleyDBValue.GetSize: Integer;
begin
  Result := FValue.TileSize;
end;

end.
