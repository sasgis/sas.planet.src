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

unit u_PSExecEx;

interface

uses
  uPSRuntime,
  t_PascalScript;

type
  TPSExecEx = class(TPSExec)
  private
    procedure DoExecTimeReg(const ARegProcArray: TOnExecTimeRegProcArray);
  public
    constructor Create(
      const ARegProcArray: TOnExecTimeRegProcArray = nil
    );
  end;

implementation

uses
  uPSR_dll,
  u_PascalScriptBase64,
  u_PascalScriptRegExpr,
  u_PascalScriptMath,
  u_PascalScriptUtils;

{ TPSExecEx }

constructor TPSExecEx.Create(const ARegProcArray: TOnExecTimeRegProcArray);
begin
  inherited Create;
  DoExecTimeReg(ARegProcArray);
end;

procedure TPSExecEx.DoExecTimeReg(const ARegProcArray: TOnExecTimeRegProcArray);
var
  I: Integer;
begin
  RegisterDLLRuntime(Self);

  ExecTimeReg_Math(Self);
  ExecTimeReg_RegExpr(Self);
  ExecTimeReg_Base64(Self);
  ExecTimeReg_Utils(Self);

  for I := Low(ARegProcArray) to High(ARegProcArray) do begin
    ARegProcArray[I](Self);
  end;
end;

end.
