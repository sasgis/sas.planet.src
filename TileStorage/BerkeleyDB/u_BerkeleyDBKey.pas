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

unit u_BerkeleyDBKey;

interface

uses
  Types,
  i_BerkeleyDBKeyValue,
  u_BaseInterfacedObject;

const
  cBerkeleyDBMetaKeyX: Cardinal = $FFFFFFFF;
  cBerkeleyDBMetaKeyY: Cardinal = $FFFFFFFF;

type
  TBerkeleyDBKey = class(TBaseInterfacedObject, IBerkeleyDBKey)
  private
    type
      TKey = packed record
        X: Cardinal;
        Y: Cardinal;
      end;
      PKey = ^TKey;
  private
    FPoint: TPoint;
    FData: PKey;
    FSize: Integer;
    FOwnMem: Boolean;
    function PointToKey(const APoint: TPoint): PKey;
    function KeyToPoint(const AKey: PKey): TPoint;
  private
    { IBerkeleyDBKey }
    function GetPoint: TPoint;
    function GetData: Pointer;
    function GetSize: Integer;
    procedure Assign(const AData: Pointer; const ASize: Integer; const AOwnMem: Boolean);
  public
    constructor Create(
      const APoint: TPoint
    ); overload;
    constructor Create(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ); overload;
    destructor Destroy; override;
  end;

implementation

function Swap32(Value: Cardinal): Cardinal; assembler;
asm
  bswap eax
end;

procedure SetBit(var ADest: Cardinal; const ABit: Integer); inline;
begin
  ADest := ADest or (Cardinal(1) shl ABit);
end;

{ TBerkeleyDBKey }

constructor TBerkeleyDBKey.Create(
  const APoint: TPoint
);
begin
  inherited Create;
  FPoint := APoint;
  FData := PointToKey(FPoint);
  FSize := SizeOf(TKey);
  FOwnMem := True;
end;

constructor TBerkeleyDBKey.Create(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
);
begin
  inherited Create;
  FData := nil;
  Assign(AData, ASize, AOwnMem);
end;

procedure TBerkeleyDBKey.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
);
begin
  Assert(AData <> nil);
  Assert(ASize = SizeOf(TKey));
  if (FData <> nil) and FOwnMem then begin
    FreeMemory(FData);
  end;
  FData := AData;
  FSize := ASize;
  FOwnMem := AOwnMem;
  FPoint := KeyToPoint(FData);
end;

destructor TBerkeleyDBKey.Destroy;
begin
  if FOwnMem then begin
    FreeMemory(FData);
    FData := nil;
  end;
  inherited Destroy;
end;

function TBerkeleyDBKey.PointToKey(const APoint: TPoint): PKey;
var
  I: Integer;
  VSetX, VSetY: Boolean;
  VKey: PKey;
begin
  VKey := GetMemory(SizeOf(TKey));

  VKey.X := 0;
  VKey.Y := 0;
  for I := 0 to 31 do begin
    VSetX := ((APoint.X shr I) and 1) = 1;
    VSetY := ((APoint.Y shr I) and 1) = 1;
    if I <= 15 then begin
      if VSetX then begin
        SetBit(VKey.Y, I * 2);
      end;
      if VSetY then begin
        SetBit(VKey.Y, I * 2 + 1);
      end;
    end else begin
      if VSetX then begin
        SetBit(VKey.X, (I - 16) * 2);
      end;
      if VSetY then begin
        SetBit(VKey.X, (I - 16) * 2 + 1);
      end;
    end;
  end;
  VKey.X := Swap32(VKey.X);
  VKey.Y := Swap32(VKey.Y);

  Result := VKey;
end;

function TBerkeleyDBKey.KeyToPoint(const AKey: PKey): TPoint;

  procedure ValueToPoint(
    const AValue: Cardinal;
    const AOffset: Integer;
    var APoint: TPoint
  );
  var
    I: Integer;
    VPoint: TKey;
  begin
    VPoint.X := APoint.X;
    VPoint.Y := APoint.Y;
    for I := 0 to 31 do begin
      if ((AValue shr I) and 1) = 1 then begin
        if (I mod 2 = 0) then begin
          SetBit(VPoint.X, (I + AOffset) div 2);
        end else begin
          SetBit(VPoint.Y, (I + AOffset - 1) div 2);
        end;
      end;
    end;
    APoint.X := VPoint.X;
    APoint.Y := VPoint.Y;
  end;

begin
  Result := Point(0, 0);
  ValueToPoint(Swap32(AKey.Y), 0, Result);
  ValueToPoint(Swap32(AKey.X), 32, Result);
end;

function TBerkeleyDBKey.GetPoint: TPoint;
begin
  Result := FPoint;
end;

function TBerkeleyDBKey.GetData: Pointer;
begin
  Result := FData;
end;

function TBerkeleyDBKey.GetSize: Integer;
begin
  Result := FSize;
end;

end. 
