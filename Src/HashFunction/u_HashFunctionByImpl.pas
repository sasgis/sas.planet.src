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

unit u_HashFunctionByImpl;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_HashFunction,
  i_HashFunctionImpl,
  u_BaseInterfacedObject;

type
  THashFunctionByImpl = class(TBaseInterfacedObject, IHashFunction)
  private
    FFunction: IHashFunctionImpl;
  private
    function CalcHashByString(const AValue: string): THashValue;
    function CalcHashByAnsiString(const AValue: AnsiString): THashValue;
    function CalcHashByGUID(const AValue: TGUID): THashValue;
    function CalcHashByDouble(const AValue: Double): THashValue;
    function CalcHashByInteger(const AValue: Integer): THashValue;
    function CalcHashByPoint(const AValue: TPoint): THashValue;
    function CalcHashByDoublePoint(const AValue: TDoublePoint): THashValue;
    function CalcHashByRect(const AValue: TRect): THashValue;
    function CalcHashByDoubleRect(const AValue: TDoubleRect): THashValue;
    function CalcHashByBuffer(
      ABuffer: Pointer;
      ASize: Integer
    ): THashValue;

    procedure UpdateHashByString(
      var AHash: THashValue;
      const AValue: string
    );
    procedure UpdateHashByAnsiString(
      var AHash: THashValue;
      const AValue: AnsiString
    );
    procedure UpdateHashByGUID(
      var AHash: THashValue;
      const AValue: TGUID
    );
    procedure UpdateHashByDouble(
      var AHash: THashValue;
      const AValue: Double
    );
    procedure UpdateHashByInteger(
      var AHash: THashValue;
      const AValue: Integer
    );
    procedure UpdateHashByPoint(
      var AHash: THashValue;
      const AValue: TPoint
    );
    procedure UpdateHashByDoublePoint(
      var AHash: THashValue;
      const AValue: TDoublePoint
    );
    procedure UpdateHashByRect(
      var AHash: THashValue;
      const AValue: TRect
    );
    procedure UpdateHashByDoubleRect(
      var AHash: THashValue;
      const AValue: TDoubleRect
    );
    procedure UpdateHashByBuffer(
      var AHash: THashValue;
      ABuffer: Pointer;
      ASize: Integer
    );
    procedure UpdateHashByHash(
      var AHash: THashValue;
      AValue: THashValue
    );
  public
    constructor Create(
      const AFunction: IHashFunctionImpl
    );
  end;

implementation

const
  CSmallBufferSize = 32;

{ THashFunctionWithCounter }

constructor THashFunctionByImpl.Create(
  const AFunction: IHashFunctionImpl
);
begin
  Assert(Assigned(AFunction));
  inherited Create;
  FFunction := AFunction;
end;

procedure THashFunctionByImpl.UpdateHashByAnsiString(
  var AHash: THashValue;
  const AValue: AnsiString
);
var
  VSize: Integer;
begin
  VSize := Length(AValue) * SizeOf(AValue[1]);
  if VSize > 0 then begin
    AHash := FFunction.CalcHashWithSeed(@AValue[1], VSize, AHash);
  end;
end;

procedure THashFunctionByImpl.UpdateHashByBuffer(
  var AHash: THashValue;
  ABuffer: Pointer;
  ASize: Integer
);
begin
  if ASize > 0 then begin
    AHash := FFunction.CalcHashWithSeed(ABuffer, ASize, AHash);
  end;
end;

procedure THashFunctionByImpl.UpdateHashByDouble(
  var AHash: THashValue;
  const AValue: Double
);
begin
  AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
end;

procedure THashFunctionByImpl.UpdateHashByDoublePoint(
  var AHash: THashValue;
  const AValue: TDoublePoint
);
begin
  AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
end;

procedure THashFunctionByImpl.UpdateHashByDoubleRect(
  var AHash: THashValue;
  const AValue: TDoubleRect
);
begin
  AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
end;

procedure THashFunctionByImpl.UpdateHashByGUID(
  var AHash: THashValue;
  const AValue: TGUID
);
begin
  AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
end;

procedure THashFunctionByImpl.UpdateHashByHash(
  var AHash: THashValue;
  AValue: THashValue
);
begin
  AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
end;

procedure THashFunctionByImpl.UpdateHashByInteger(
  var AHash: THashValue;
  const AValue: Integer
);
begin
  AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
end;

procedure THashFunctionByImpl.UpdateHashByPoint(
  var AHash: THashValue;
  const AValue: TPoint
);
begin
  AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
end;

procedure THashFunctionByImpl.UpdateHashByRect(
  var AHash: THashValue;
  const AValue: TRect
);
begin
  AHash := FFunction.CalcHashWithSeed(@AValue, SizeOf(AValue), AHash);
end;

procedure THashFunctionByImpl.UpdateHashByString(
  var AHash: THashValue;
  const AValue: string
);
var
  VSize: Integer;
begin
  VSize := Length(AValue) * SizeOf(AValue[1]);
  if VSize > 0 then begin
    AHash := FFunction.CalcHashWithSeed(@AValue[1], VSize, AHash);
  end;
end;

function THashFunctionByImpl.CalcHashByAnsiString(
  const AValue: AnsiString
): THashValue;
var
  VSize: Integer;
begin
  VSize := Length(AValue) * SizeOf(AValue[1]);
  Result := FFunction.CalcHash(@AValue[1], VSize);
end;

function THashFunctionByImpl.CalcHashByBuffer(
  ABuffer: Pointer;
  ASize: Integer
): THashValue;
begin
  Result := FFunction.CalcHash(ABuffer, ASize);
end;

function THashFunctionByImpl.CalcHashByDouble(
  const AValue: Double
): THashValue;
begin
  Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
end;

function THashFunctionByImpl.CalcHashByDoublePoint(
  const AValue: TDoublePoint
): THashValue;
begin
  Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
end;

function THashFunctionByImpl.CalcHashByDoubleRect(
  const AValue: TDoubleRect
): THashValue;
begin
  Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
end;

function THashFunctionByImpl.CalcHashByGUID(
  const AValue: TGUID
): THashValue;
begin
  Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
end;

function THashFunctionByImpl.CalcHashByInteger(
  const AValue: Integer
): THashValue;
begin
  Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
end;

function THashFunctionByImpl.CalcHashByPoint(
  const AValue: TPoint
): THashValue;
begin
  Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
end;

function THashFunctionByImpl.CalcHashByRect(
  const AValue: TRect
): THashValue;
begin
  Result := FFunction.CalcHash(@AValue, SizeOf(AValue));
end;

function THashFunctionByImpl.CalcHashByString(
  const AValue: string
): THashValue;
var
  VSize: Integer;
begin
  VSize := Length(AValue) * SizeOf(AValue[1]);
  Result := FFunction.CalcHash(@AValue[1], VSize);
end;

end.
