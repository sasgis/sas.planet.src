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

unit u_BinaryData;

interface

uses
  i_BinaryData,
  u_BaseInterfacedObject;

type
  TBinaryData = class(TBaseInterfacedObject, IBinaryData)
  private
    FBuffer: Pointer;
    FSize: Integer;
  private
    function GetBuffer: Pointer;
    function GetSize: Integer;
  public
    constructor Create(
      const ASize: Integer;
      const ABuffer: Pointer;
      const AOwnBuffer: Boolean
    );
    constructor CreateByAnsiString(const ASource: AnsiString);
    destructor Destroy; override;
  end;

implementation

{ TBinaryData }

constructor TBinaryData.Create(
  const ASize: Integer;
  const ABuffer: Pointer;
  const AOwnBuffer: Boolean
);
begin
  inherited Create;
  FSize := ASize;
  if AOwnBuffer then begin
    FBuffer := ABuffer;
  end else begin
    GetMem(FBuffer, FSize);
    Move(ABuffer^, FBuffer^, FSize);
  end;
end;

constructor TBinaryData.CreateByAnsiString(const ASource: AnsiString);
begin
  inherited Create;
  FSize := Length(ASource);
  GetMem(FBuffer, FSize);
  Move(ASource[1], FBuffer^, FSize);
end;

destructor TBinaryData.Destroy;
begin
  FreeMem(FBuffer);
  inherited;
end;

function TBinaryData.GetBuffer: Pointer;
begin
  Result := FBuffer;
end;

function TBinaryData.GetSize: Integer;
begin
  Result := FSize;
end;

end.
