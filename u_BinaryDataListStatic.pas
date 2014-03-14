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

unit u_BinaryDataListStatic;

interface

uses
  i_BinaryData,
  i_BinaryDataListStatic,
  u_BaseInterfacedObject;

type
  TBinaryDataListStatic = class(TBaseInterfacedObject, IBinaryDataListStatic)
  private
    FCount: Integer;
    FItems: array of IBinaryData;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IBinaryData;
  public
    constructor Create(const AItems: array of IBinaryData);
    constructor CreateBySource(const ASource: IBinaryDataListStatic; const AItem: IBinaryData);
    constructor CreateByTwoItems(const AItem1, AItem2: IBinaryData);
    destructor Destroy; override;
  end;

implementation

{ TBinaryDataListStatic }

constructor TBinaryDataListStatic.Create(const AItems: array of IBinaryData);
var
  VCount: Integer;
  i: Integer;
begin
  inherited Create;
  VCount := Length(AItems);
  FCount := VCount;
  SetLength(FItems, VCount);
  for i := 0 to VCount - 1 do begin
    FItems[i] := AItems[i];
  end;
end;

constructor TBinaryDataListStatic.CreateBySource(
  const ASource: IBinaryDataListStatic; const AItem: IBinaryData);
var
  VCount: Integer;
  i: Integer;
begin
  inherited Create;
  VCount := ASource.Count;
  FCount := VCount + 1;
  SetLength(FItems, FCount);
  for i := 0 to VCount - 1 do begin
    FItems[i] := ASource.Item[i];
  end;
  FItems[VCount] := AItem;
end;

constructor TBinaryDataListStatic.CreateByTwoItems(const AItem1,
  AItem2: IBinaryData);
begin
  inherited Create;
  FCount := 2;
  SetLength(FItems, 2);
  FItems[0] := AItem1;
  FItems[1] := AItem2;
end;

destructor TBinaryDataListStatic.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FItems) - 1 do begin
    FItems[i] := nil;
  end;
  FItems := nil;

  inherited;
end;

function TBinaryDataListStatic.GetCount: Integer;
begin
  Result := FCount;
end;

function TBinaryDataListStatic.GetItem(AIndex: Integer): IBinaryData;
begin
  Result := FItems[AIndex];
end;

end.
