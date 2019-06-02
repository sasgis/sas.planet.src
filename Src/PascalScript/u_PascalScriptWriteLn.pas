{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_PascalScriptWriteLn;

interface

uses
  Classes,
  uPSRuntime,
  uPSCompiler;

procedure CompileTimeReg_WriteLn(const APSComp: TPSPascalCompiler);
procedure ExecTimeReg_WriteLn(const APSExec: TPSExec; const AObj: TObject);

type
  TPascalScriptWriteLn = class(TStringList)
  private
    procedure PSAddString(const s: string);
  end;

implementation

procedure CompileTimeReg_WriteLn(const APSComp: TPSPascalCompiler);
begin
  APSComp.AddDelphiFunction('procedure WriteLn(const s: string)');
end;

{ TPascalScriptWriteLn }

procedure TPascalScriptWriteLn.PSAddString(const s: string);
begin
  Add(s);
end;

procedure FakeWriteLn(const s: string);
begin
  // nothing here
end;

procedure ExecTimeReg_WriteLn(const APSExec: TPSExec; const AObj: TObject);
begin
  if AObj <> nil then begin
    APSExec.RegisterDelphiMethod(
      AObj as TPascalScriptWriteLn,
      @TPascalScriptWriteLn.PSAddString,
      'WriteLn',
      cdRegister
    );
  end else begin
    APSExec.RegisterDelphiFunction(
      @FakeWriteLn,
      'WriteLn',
      cdRegister
    );
  end;
end;

end.
