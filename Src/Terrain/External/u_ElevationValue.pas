{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ElevationValue;

interface

uses
  SysUtils;

type
  TElevationValueType = (
    evtSmallInt, // 16 bit signed int
    evtLongInt,  // 32 bit signed int
    evtSingle    // 32 bit signed float
  );

  TElevationValue = record
  private const
    CInvalidTipeIdMsg = 'Invalid TypeId value: %d';
  public
    function ReadFromMem(AMem: PByte; const AOffset: Int64): Boolean; inline;
    function ReadFromFile(const AFile: THandle; const AOffset: Int64): Boolean; inline;

    procedure SwapIntByteOrder; inline;
    function IsVoid(const AVoidValue: Integer): Boolean; inline;
    function ToSingle: Single; inline;

    case TypeId: TElevationValueType of
      evtSmallInt : (ValueSmall  : SmallInt);
      evtLongInt  : (ValueLong   : LongInt);
      evtSingle   : (ValueSingle : Single);
  end;

  EElevationValue = class(Exception);

implementation

uses
  NTFiles,
  u_ByteSwapFunc;

{ TElevationValue }

procedure TElevationValue.SwapIntByteOrder;
begin
  case TypeId of
    evtSmallInt : ValueSmall := Swap16(ValueSmall);
    evtLongInt  : ValueLong  := Swap32(ValueLong);
  end;
end;

function TElevationValue.IsVoid(const AVoidValue: Integer): Boolean;
begin
  if AVoidValue = 0 then begin
    Result := False;
    Exit;
  end;

  case TypeId of
    evtSmallInt : Result := AVoidValue = ValueSmall;
    evtLongInt  : Result := AVoidValue = ValueLong;
    evtSingle   : Result := AVoidValue = Round(ValueSingle);
  else
    raise EElevationValue.CreateFmt(CInvalidTipeIdMsg, [Integer(TypeId)]);
  end;
end;

function TElevationValue.ToSingle: Single;
begin
  case TypeId of
    evtSmallInt : Result := ValueSmall;
    evtLongInt  : Result := ValueLong;
    evtSingle   : Result := ValueSingle;
  else
    raise EElevationValue.CreateFmt(CInvalidTipeIdMsg, [Integer(TypeId)]);
  end;
end;

function TElevationValue.ReadFromMem(AMem: PByte; const AOffset: Int64): Boolean;
begin
  Inc(AMem, AOffset);
  case TypeId of
    evtSmallInt : ValueSmall  := PWord(AMem)^;
    evtLongInt  : ValueLong   := PCardinal(AMem)^;
    evtSingle   : ValueSingle := PSingle(AMem)^;
  else
    raise EElevationValue.CreateFmt(CInvalidTipeIdMsg, [Integer(TypeId)]);
  end;
  Result := True;
end;

function TElevationValue.ReadFromFile(const AFile: THandle; const AOffset: Int64): Boolean;
begin
  case TypeId of
    evtSmallInt : Result := NtReadFromFile(AFile, @ValueSmall,  SizeOf(ValueSmall),  AOffset);
    evtLongInt  : Result := NtReadFromFile(AFile, @ValueLong,   SizeOf(ValueLong),   AOffset);
    evtSingle   : Result := NtReadFromFile(AFile, @ValueSingle, SizeOf(ValueSingle), AOffset);
  else
    raise EElevationValue.CreateFmt(CInvalidTipeIdMsg, [Integer(TypeId)]);
  end;
end;

end.
