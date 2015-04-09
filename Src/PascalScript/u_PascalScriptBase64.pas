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

unit u_PascalScriptBase64;

interface

uses
  uPSRuntime,
  uPSCompiler;

procedure CompileTimeReg_Base64(const APSComp: TPSPascalCompiler);
procedure ExecTimeReg_Base64(const APSExec: TPSExec);

implementation

uses
  EDBase64;

procedure CompileTimeReg_Base64(const APSComp: TPSPascalCompiler);
begin
  APSComp.AddDelphiFunction('function Base64Encode(const Data: AnsiString): AnsiString');
  APSComp.AddDelphiFunction('function Base64UrlEncode(const Data: AnsiString): AnsiString');
  APSComp.AddDelphiFunction('function Base64Decode(const Data: AnsiString): AnsiString');
end;

function Base64Encode_P(const Data: AnsiString): AnsiString;
begin
  Result := Base64Encode(Data);
end;

function Base64UrlEncode_P(const Data: AnsiString): AnsiString;
begin
  Result := Base64UrlEncode(Data);
end;

function Base64Decode_P(const Data: AnsiString): AnsiString;
begin
  Result := Base64Decode(Data);
end;

procedure ExecTimeReg_Base64(const APSExec: TPSExec);
begin
  APSExec.RegisterDelphiFunction(@Base64Encode_P, 'Base64Encode', cdRegister);
  APSExec.RegisterDelphiFunction(@Base64UrlEncode_P, 'Base64UrlEncode', cdRegister);
  APSExec.RegisterDelphiFunction(@Base64Decode_P, 'Base64Decode', cdRegister);
end;

end.
