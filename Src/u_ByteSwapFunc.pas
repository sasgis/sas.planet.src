{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_ByteSwapFunc;

interface

function Swap16(const AValue: Word): Word; assembler;
function Swap32(const AValue: Cardinal): Cardinal; assembler;
function Swap64(const AValue: UInt64): UInt64; assembler;

implementation

{$IFDEF FPC}
  {$ASMMODE INTEL}
{$ENDIF}

function Swap16(const AValue: Word): Word; assembler;
{$IFDEF FPC} nostackframe; {$ENDIF}
asm
  {$IFDEF WIN64}
  mov eax, ecx
  {$ENDIF}
  xchg al, ah
end;

function Swap32(const AValue: Cardinal): Cardinal; assembler;
{$IFDEF FPC} nostackframe; {$ENDIF}
asm
  {$IFDEF WIN64}
  mov eax, ecx
  {$ENDIF}
  bswap eax
end;

{$WARNINGS OFF}
function Swap64(const AValue: UInt64): UInt64; assembler;
{$IFDEF WIN64}
{$IFDEF FPC} nostackframe; {$ENDIF}
asm
  mov rax, rcx
  bswap rax
end;
{$ELSE}
asm
  mov edx, [ebp+$08]
  mov eax, [ebp+$0c]
  bswap edx
  bswap eax
end;
{$ENDIF}
{$WARNINGS ON}

end.
