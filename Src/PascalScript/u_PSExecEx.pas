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

unit u_PSExecEx;

interface

uses
  uPSRuntime,
  t_PascalScript;

type
  TPSExecEx = class(TPSExec)
  private
    procedure DoExecTimeReg(
      const ARegProcArray: TOnExecTimeRegProcArray;
      const ARegMethodArray: TOnExecTimeRegMethodArray
    );
  public
    constructor Create(
      const ARegProcArray: TOnExecTimeRegProcArray = nil;
      const ARegMethodArray: TOnExecTimeRegMethodArray = nil
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

constructor TPSExecEx.Create(
  const ARegProcArray: TOnExecTimeRegProcArray;
  const ARegMethodArray: TOnExecTimeRegMethodArray
);
begin
  inherited Create;
  DoExecTimeReg(ARegProcArray, ARegMethodArray);
end;

procedure TPSExecEx.DoExecTimeReg(
  const ARegProcArray: TOnExecTimeRegProcArray;
  const ARegMethodArray: TOnExecTimeRegMethodArray
);
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

  for I := Low(ARegMethodArray) to High(ARegMethodArray) do begin
    ARegMethodArray[I].Func(Self, ARegMethodArray[I].Obj);
  end;
end;

end.
