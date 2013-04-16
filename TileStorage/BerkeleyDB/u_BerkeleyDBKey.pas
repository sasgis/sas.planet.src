{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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
  i_BinaryData,
  i_BerkeleyDBKeyValue,
  u_BaseInterfacedObject;

type
  TBerkeleyDBKey = class(TBaseInterfacedObject, IBerkeleyDBKey, IBinaryData)
  private
    type
      TKey = packed record
        X: Cardinal;
        Y: Cardinal;
      end;
      PKey = ^TKey;
  private
    FPoint: TPoint;
    FData: Pointer;
    FSize: Integer;
    FOwnMem: Boolean;
    function PointToKey(const APoint: TPoint): PKey;
    function KeyToPoint(const AKey: PKey): TPoint;
  private
    { IBerkeleyDBKey }
    function GetPoint: TPoint;
    { IBerkeleyDBKeyValueBase }
    function GetData: Pointer;
    function GetSize: Integer;
    function Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ): Boolean;
    { IBinaryData }
    function IBinaryData.GetBuffer = GetData;
    function IBinaryData.GetSize = GetSize;
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

  TBerkeleyDBMetaKey = class(TBerkeleyDBKey, IBerkeleyDBMetaKey)
  public
    constructor Create;
  end;

  TBerkeleyDBVersionedKey = class(TBerkeleyDBKey, IBerkeleyDBVersionedKey)
  private
    FVersionID: Word;
  private
    { IBerkeleyDBVersionedKey }
    function GetVersionID: Word;
    { IBerkeleyDBKeyValueBase }
    function Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ): Boolean;
  public
    constructor Create(
      const APoint: TPoint;
      const AVersionID: Word
    );
  end;

  TBerkeleyDBVersionedMetaKey = class(TBerkeleyDBVersionedKey, IBerkeleyDBVersionedMetaKey)
  public
    constructor Create(const APoint: TPoint);
  end;

function IsMetaKey(const AKey: IBerkeleyDBKey): Boolean; inline;

const
  cBerkeleyDBMetaKeyX: Cardinal = $FFFFFFFF;
  cBerkeleyDBMetaKeyY: Cardinal = $FFFFFFFF;
  cBerkeleyDBVersionedMetaKeyID: Word = $FFFF;

implementation

uses
  SysUtils;

type
  PWord = ^Word;

function IsMetaKey(const AKey: IBerkeleyDBKey): Boolean; inline;
var
  VVersionedKey: IBerkeleyDBVersionedKey;
begin
  Result := Supports(AKey, IBerkeleyDBMetaKey) or Supports(AKey, IBerkeleyDBVersionedMetaKey);
  if not Result then begin
    if Supports(AKey, IBerkeleyDBVersionedKey, VVersionedKey) then begin
      Result := VVersionedKey.VersionID = cBerkeleyDBVersionedMetaKeyID;
    end else begin
      Result := (Cardinal(AKey.Point.X) = cBerkeleyDBMetaKeyX) and (Cardinal(AKey.Point.Y) = cBerkeleyDBMetaKeyY);
    end;
  end;
end;

function Swap32(Value: Cardinal): Cardinal; assembler;
asm
  bswap eax
end;

function Swap16(Value: Word): Word; assembler;
asm
   xchg al, ah
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
  if not Assign(AData, ASize, AOwnMem) then begin
    Assert(False, 'TBerkeleyDBKey.Assign fail!');
  end;
end;

function TBerkeleyDBKey.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
): Boolean;
begin
  Result := False;
  if (AData <> nil) and (ASize = SizeOf(TKey)) then begin
    if (FData <> nil) and FOwnMem then begin
      FreeMemory(FData);
    end;
    FData := AData;
    FSize := ASize;
    FOwnMem := AOwnMem;
    FPoint := KeyToPoint(FData);
    Result := True;
  end;
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

{ TBerkeleyDBMetaKey }

constructor TBerkeleyDBMetaKey.Create;
begin
  inherited Create(Point(cBerkeleyDBMetaKeyX, cBerkeleyDBMetaKeyY));
end;

{ TBerkeleyDBVersionedKey }

constructor TBerkeleyDBVersionedKey.Create(
  const APoint: TPoint;
  const AVersionID: Word
);
var
  VKey: PKey;
  VData: PByte;
  VVersionID: Word;
begin
  inherited Create;
  FPoint := APoint;
  FVersionID := AVersionID;
  VVersionID := Swap16(FVersionID);
  VKey := inherited PointToKey(FPoint);
  try
    FSize := SizeOf(TKey) + SizeOf(VVersionID);
    FData := GetMemory(FSize);
    VData := FData;
    Move(VKey^, VData^, SizeOf(TKey));
    Inc(VData, SizeOf(TKey));
    Move(VVersionID, VData^, SizeOf(VVersionID));
    FOwnMem := True;
  finally
    FreeMemory(VKey);
  end;
end;

function TBerkeleyDBVersionedKey.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
): Boolean;
var
  VData: PByte;
begin
  Result := False;
  if (AData <> nil) and (ASize = (SizeOf(TKey) + SizeOf(FVersionID))) then begin
    if (FData <> nil) and FOwnMem then begin
      FreeMemory(FData);
    end;
    FData := AData;
    FSize := ASize;
    FOwnMem := AOwnMem;
    FPoint := inherited KeyToPoint(FData);
    VData := FData;
    Inc(VData, SizeOf(TKey));
    FVersionID := Swap16(PWord(VData)^);
    Result := True;
  end;
end;

function TBerkeleyDBVersionedKey.GetVersionID: Word;
begin
  Result := FVersionID;
end;

{ TBerkeleyDBVersionedMetaKey }

constructor TBerkeleyDBVersionedMetaKey.Create(const APoint: TPoint);
begin
  inherited Create(APoint, cBerkeleyDBVersionedMetaKeyID);
end;

end. 
