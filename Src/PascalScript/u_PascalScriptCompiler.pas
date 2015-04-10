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

unit u_PascalScriptCompiler;

interface

uses
  SysUtils,
  uPSUtils,
  t_PascalScript;

type
  TPascalScriptCompiler = class
  private
    FScriptText: TbtString;
    FRegProcArray: TOnCompileTimeRegProcArray;
    FDebugInfo: string;
    FStoreDebugInfo: Boolean;
  public
    constructor Create(
      const AScriptText: TbtString;
      const ARegProcArray: TOnCompileTimeRegProcArray = nil;
      const AStoreDebugInfo: Boolean = False
    );
    function CompileAndGetOutput(out AData: TbtString): Boolean;
    function GetDebugInfo: string;
    function GetDisassemled(const AData: TbtString): string;
  end;

  EPascalScriptCompileError = class(Exception);

implementation

uses
  uPSDisassembly,
  u_PSPascalCompilerEx,
  u_ResStrings;

{ TPascalScriptCompiler }

constructor TPascalScriptCompiler.Create(
  const AScriptText: TbtString;
  const ARegProcArray: TOnCompileTimeRegProcArray;
  const AStoreDebugInfo: Boolean
);
begin
  inherited Create;
  FScriptText := AScriptText;
  FRegProcArray := ARegProcArray;
  FStoreDebugInfo := AStoreDebugInfo;
  FDebugInfo := '';
end;

function TPascalScriptCompiler.CompileAndGetOutput(out AData: TbtString): Boolean;
var
  I: Integer;
  VComp: TPSPascalCompilerEx;
  VCompilerMsg: AnsiString;
begin
  VComp := TPSPascalCompilerEx.Create(FRegProcArray, FStoreDebugInfo);
  try
    Result := VComp.Compile(FScriptText);
    FDebugInfo := VComp.FDebugInfo;
    if not Result then begin
      VCompilerMsg := '';
      if VComp.MsgCount > 0 then begin
        for I := 0 to VComp.MsgCount - 1 do begin
          VCompilerMsg := VCompilerMsg + VComp.Msg[I].MessageToString + #13#10;
        end;
      end;
      raise EPascalScriptCompileError.CreateFmt(SAS_ERR_PascalScriptCompileError, [VCompilerMsg]);
    end else begin
      Result := VComp.GetOutput(AData);
    end;
  finally
    VComp.Free;
  end;
end;

function TPascalScriptCompiler.GetDebugInfo: string;
begin
  Result := FDebugInfo;
end;

function TPascalScriptCompiler.GetDisassemled(const AData: TbtString): string;
begin
  if not IFPS3DataToText(AData, Result) then begin
    Result := '';
  end;
end;

end.
