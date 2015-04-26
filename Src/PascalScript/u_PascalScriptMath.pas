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

unit u_PascalScriptMath;

interface
uses
  uPSRuntime,
  uPSCompiler;

procedure CompileTimeReg_Math(const APSComp: TPSPascalCompiler);
procedure ExecTimeReg_Math(const APSExec: TPSExec);

implementation

uses
  Math;

procedure CompileTimeReg_Math(const APSComp: TPSPascalCompiler);
begin
  APSComp.AddDelphiFunction('function Random(const X: Integer): Integer');
  APSComp.AddDelphiFunction('function RandomRange(const AFrom, ATo: Integer): Integer');
  APSComp.AddDelphiFunction('function Power(const Base, Exponent: Extended): Extended');
  APSComp.AddDelphiFunction('function IntPower(const Base: Extended; const Exponent: Integer): Extended');
  APSComp.AddDelphiFunction('function Ceil(const X: Extended): Integer');
  APSComp.AddDelphiFunction('function Floor(const X: Extended): Integer');
  APSComp.AddDelphiFunction('function Log2(const X: Extended): Extended');
  APSComp.AddDelphiFunction('function Ln(const X: Extended): Extended');
  APSComp.AddDelphiFunction('function Max(const A, B: Integer): Integer');
  APSComp.AddDelphiFunction('function MaxExt(const A, B: Extended): Extended');
  APSComp.AddDelphiFunction('function Min(const A, B: Integer): Integer');
  APSComp.AddDelphiFunction('function MinExt(const A, B: Extended): Extended');
end;

function RandomInt_P(const X: Integer): Integer;
begin
  Result := Random(X);
end;

function MaxInt_P(const A, B: Integer): Integer;
begin
  Result := Max(A, B);
end;

function MaxExt_P(const A, B: Extended): Extended;
begin
  Result := Max(A, B);
end;

function MinInt_P(const A, B: Integer): Integer;
begin
  Result := Min(A, B);
end;

function MinExt_P(const A, B: Extended): Extended;
begin
  Result := Min(A, B);
end;

function IntPower_P(const Base: Extended; const Exponent: Integer): Extended;
begin
  Result := IntPower(Base, Exponent);
end;

procedure ExecTimeReg_Math(const APSExec: TPSExec);
begin
  APSExec.RegisterDelphiFunction(@RandomInt_P, 'Random', cdRegister);
  APSExec.RegisterDelphiFunction(@RandomRange, 'RandomRange', cdRegister);
  APSExec.RegisterDelphiFunction(@Power, 'Power', cdRegister);
  APSExec.RegisterDelphiFunction(@IntPower_P, 'IntPower', cdRegister);
  APSExec.RegisterDelphiFunction(@Ceil, 'Ceil', cdRegister);
  APSExec.RegisterDelphiFunction(@Floor, 'Floor', cdRegister);
  APSExec.RegisterDelphiFunction(@Log2, 'Log2', cdRegister);
  APSExec.RegisterDelphiFunction(@Ln, 'Ln', cdRegister);
  APSExec.RegisterDelphiFunction(@MaxInt_P, 'Max', cdRegister);
  APSExec.RegisterDelphiFunction(@MaxExt_P, 'MaxExt', cdRegister);
  APSExec.RegisterDelphiFunction(@MinInt_P, 'Min', cdRegister);
  APSExec.RegisterDelphiFunction(@MinExt_P, 'MinExt', cdRegister);
end;

end.
