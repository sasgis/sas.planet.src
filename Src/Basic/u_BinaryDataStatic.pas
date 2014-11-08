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

unit u_BinaryDataStatic;

interface

uses
  t_Hash,
  i_BinaryDataStatic,
  u_BaseInterfacedObject;

type
  TBinaryDataStatic = class(TBaseInterfacedObject, IBinaryDataStatic)
  private
    FHash: THashValue;
    FBuffer: Pointer;
    FSize: Integer;
  private
    function GetBuffer: Pointer;
    function GetSize: Integer;
    function GetHash: THashValue;
  public
    constructor Create(
      const AHash: THashValue;
      const ASize: Integer;
      const ABuffer: Pointer
    );
    constructor CreateWithOwn(
      const AHash: THashValue;
      const ASize: Integer;
      const ABuffer: Pointer
    );
    constructor CreateByString(
      const AHash: THashValue;
      const ASource: String
    );
    constructor CreateByAnsiString(
      const AHash: THashValue;
      const ASource: AnsiString
    );
    destructor Destroy; override;
  end;

  TBinaryDataStaticWithMemoryHolder = class(TBaseInterfacedObject, IBinaryDataStatic)
  private
    FHash: THashValue;
    FMemoryHolder: IInterface;
    FBuffer: Pointer;
    FSize: Integer;
  private
    function GetBuffer: Pointer;
    function GetSize: Integer;
    function GetHash: THashValue;
  public
    constructor Create(
      const AMemoryHolder: IInterface;
      const AHash: THashValue;
      const ASize: Integer;
      const ABuffer: Pointer
    );
  end;

implementation

{ TBinaryDataStatic }

constructor TBinaryDataStatic.Create(
  const AHash: THashValue;
  const ASize: Integer;
  const ABuffer: Pointer
);
begin
  Assert((ASize = 0) or Assigned(ABuffer));
  inherited Create;
  FHash := AHash;
  FSize := ASize;
  if FSize > 0 then begin
    GetMem(FBuffer, FSize);
    Move(ABuffer^, FBuffer^, FSize);
  end else begin
    FBuffer := nil;
  end;
end;

constructor TBinaryDataStatic.CreateByAnsiString(
  const AHash: THashValue;
  const ASource: AnsiString
);
begin
  Create(AHash, Length(ASource), @ASource[1]);
end;

constructor TBinaryDataStatic.CreateByString(
  const AHash: THashValue;
  const ASource: String
);
begin
  Create(AHash, Length(ASource) * SizeOf(ASource[1]), @ASource[1]);
end;

constructor TBinaryDataStatic.CreateWithOwn(
  const AHash: THashValue;
  const ASize: Integer;
  const ABuffer: Pointer
);
begin
  Assert((ASize = 0) or Assigned(ABuffer));
  inherited Create;
  FHash := AHash;
  FSize := ASize;
  if FSize > 0 then begin
    FBuffer := ABuffer;
  end else begin
    FBuffer := nil;
  end;
end;

destructor TBinaryDataStatic.Destroy;
begin
  FreeMem(FBuffer);
  inherited;
end;

function TBinaryDataStatic.GetBuffer: Pointer;
begin
  Result := FBuffer;
end;

function TBinaryDataStatic.GetHash: THashValue;
begin
  Result := FHash;
end;

function TBinaryDataStatic.GetSize: Integer;
begin
  Result := FSize;
end;

{ TBinaryDataStaticWithMemoryHolder }

constructor TBinaryDataStaticWithMemoryHolder.Create(
  const AMemoryHolder: IInterface;
  const AHash: THashValue;
  const ASize: Integer;
  const ABuffer: Pointer
);
begin
  Assert(Assigned(AMemoryHolder));
  Assert(ASize > 0);
  Assert(Assigned(ABuffer));
  inherited Create;
  FHash := AHash;
  FMemoryHolder := AMemoryHolder;
  FSize := ASize;
  FBuffer := ABuffer;
end;

function TBinaryDataStaticWithMemoryHolder.GetBuffer: Pointer;
begin
  Result := FBuffer;
end;

function TBinaryDataStaticWithMemoryHolder.GetHash: THashValue;
begin
  Result := FHash;
end;

function TBinaryDataStaticWithMemoryHolder.GetSize: Integer;
begin
  Result := FSize;
end;

end.
