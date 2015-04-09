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

unit u_PascalScriptRegExpr;

interface

uses
  uPSRuntime,
  uPSCompiler;

procedure CompileTimeReg_RegExpr(const APSComp: TPSPascalCompiler);
procedure ExecTimeReg_RegExpr(const APSExec: TPSExec);

implementation

uses
  RegExprUtils;

procedure CompileTimeReg_RegExpr(const APSComp: TPSPascalCompiler);
begin
  APSComp.AddDelphiFunction('function RegExprGetMatchSubStr(const Str, MatchExpr: AnsiString; AMatchID: Integer): AnsiString');
  APSComp.AddDelphiFunction('function RegExprReplaceMatchSubStr(const Str, MatchExpr, Replace: AnsiString): AnsiString');
end;

procedure ExecTimeReg_RegExpr(const APSExec: TPSExec);
begin
  APSExec.RegisterDelphiFunction(@RegExprGetMatchSubStr, 'RegExprGetMatchSubStr', cdRegister);
  APSExec.RegisterDelphiFunction(@RegExprReplaceMatchSubStr, 'RegExprReplaceMatchSubStr', cdRegister);
end;


end.
