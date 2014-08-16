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
      const ABuffer: Pointer
    );
    constructor CreateWithOwn(
      const ASize: Integer;
      const ABuffer: Pointer
    );
    constructor CreateByString(const ASource: String);
    constructor CreateByAnsiString(const ASource: AnsiString);
    destructor Destroy; override;
  end;

  TBinaryDataWithMemoryHolder = class(TBaseInterfacedObject, IBinaryData)
  private
    FMemoryHolder: IInterface;
    FBuffer: Pointer;
    FSize: Integer;
  private
    function GetBuffer: Pointer;
    function GetSize: Integer;
  public
    constructor Create(
      const AMemoryHolder: IInterface;
      const ASize: Integer;
      const ABuffer: Pointer
    );
  end;

implementation

{ TBinaryData }

constructor TBinaryData.Create(
  const ASize: Integer;
  const ABuffer: Pointer
);
begin
  inherited Create;
  FSize := ASize;
  if FSize > 0 then begin
    GetMem(FBuffer, FSize);
    Move(ABuffer^, FBuffer^, FSize);
  end else begin
    FBuffer := nil;
  end;
end;

constructor TBinaryData.CreateByAnsiString(const ASource: AnsiString);
begin
  Create(Length(ASource), @ASource[1]);
end;

constructor TBinaryData.CreateByString(const ASource: String);
begin
  Create(Length(ASource) * SizeOf(ASource[1]), @ASource[1]);
end;

constructor TBinaryData.CreateWithOwn(
  const ASize: Integer;
  const ABuffer: Pointer
);
begin
  inherited Create;
  FSize := ASize;
  if FSize > 0 then begin
    FBuffer := ABuffer;
  end else begin
    FBuffer := nil;
  end;
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

{ TBinaryDataWithMemoryHolder }

constructor TBinaryDataWithMemoryHolder.Create(
  const AMemoryHolder: IInterface;
  const ASize: Integer;
  const ABuffer: Pointer
);
begin
  Assert(Assigned(AMemoryHolder));
  Assert(ASize > 0);
  Assert(Assigned(ABuffer));
  inherited Create;
  FMemoryHolder := AMemoryHolder;
  FSize := ASize;
  FBuffer := ABuffer;
end;

function TBinaryDataWithMemoryHolder.GetBuffer: Pointer;
begin
  Result := FBuffer;
end;

function TBinaryDataWithMemoryHolder.GetSize: Integer;
begin
  Result := FSize;
end;

end.
