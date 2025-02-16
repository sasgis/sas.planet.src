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

unit u_PSPascalCompilerEx;

interface

uses
  Classes,
  uPSUtils,
  uPSCompiler,
  t_PascalScript;

type
  TKnownProcsToStringFormat = (kpfNone, kpfWiki, kpfMarkdown);

  TPSPascalCompilerEx = class(TPSPascalCompiler)
  private
    FDebugInfo: string;
    FStoreDebugInfo: Boolean;
    FRegProcArray: TOnCompileTimeRegProcArray;
    FUsedVars: TStringList;
  private
    function KnownVarsToString: string;
    function KnownProcsToString(const AFormat: TKnownProcsToStringFormat): string;
    function KnownConstsToString: string;
    function KnownTypesToString: string;
    procedure DoStoreDebugInfo;
  public
    constructor Create(
      const ARegProcArray: TOnCompileTimeRegProcArray = nil;
      const AStoreDebugInfo: Boolean = False;
      const AStoreUsedVarsInfo: Boolean = False
    );
    destructor Destroy; override;

    procedure AddUsedVar(const AVarName: string); inline;
    function IsVarUsed(const AVarName: string): Boolean; inline;

    property DebugInfo: string read FDebugInfo;
  end;

implementation

uses
  SysUtils,
  uPSC_dll,
  u_PascalScriptBase64,
  u_PascalScriptRegExpr,
  u_PascalScriptMath,
  u_PascalScriptTypes,
  u_PascalScriptUtils;

function PascalScriptOnUses(
  Sender: TPSPascalCompiler;
  const AName: TbtString
): Boolean;
var
  I: Integer;
  VComp: TPSPascalCompilerEx;
begin
  Result := False;

  Assert(Sender is TPSPascalCompilerEx);

  VComp := Sender as TPSPascalCompilerEx;

  if UpperCase(AName) = 'SYSTEM' then begin

    CompileTimeReg_CommonTypes(Sender);

    CompileTimeReg_Math(Sender);
    CompileTimeReg_RegExpr(Sender);
    CompileTimeReg_Base64(Sender);
    CompileTimeReg_Utils(Sender);

    for I := Low(VComp.FRegProcArray) to High(VComp.FRegProcArray) do begin
      VComp.FRegProcArray[I](Sender);
    end;

    if VComp.FStoreDebugInfo then begin
      VComp.DoStoreDebugInfo;
    end;

    Result := True;
  end;
end;

procedure PascalScriptOnUseVariable(
  Sender: TPSPascalCompiler;
  VarType: TPSVariableType;
  VarNo: Longint;
  ProcNo, Position: Cardinal;
  const PropData: tbtString
);
var
  VPSVar: TPSVar;
begin
  VPSVar := Sender.GetVar(VarNo);
  if VPSVar <> nil then begin
    TPSPascalCompilerEx(Sender).AddUsedVar(VPSVar.Name);
  end;
end;

{ TPSPascalCompilerEx }

constructor TPSPascalCompilerEx.Create(
  const ARegProcArray: TOnCompileTimeRegProcArray;
  const AStoreDebugInfo: Boolean;
  const AStoreUsedVarsInfo: Boolean
);
begin
  inherited Create;

  FRegProcArray := ARegProcArray;
  FStoreDebugInfo := AStoreDebugInfo;

  FDebugInfo := '';

  Self.OnExternalProc := DllExternalProc;
  Self.OnUses := PascalScriptOnUses;

  FUsedVars := nil;
  if AStoreUsedVarsInfo then begin
    FUsedVars := TStringList.Create;
    FUsedVars.Duplicates := dupIgnore;
    FUsedVars.CaseSensitive := False;
    FUsedVars.Sorted := True;

    Self.OnUseVariable := PascalScriptOnUseVariable;
  end;
end;

destructor TPSPascalCompilerEx.Destroy;
begin
  FreeAndNil(FUsedVars);
  inherited Destroy;
end;

procedure TPSPascalCompilerEx.AddUsedVar(const AVarName: string);
begin
  if FUsedVars <> nil then begin
    FUsedVars.Add(AVarName);
  end else begin
    raise Exception.Create('UsedVars not assigned!');
  end;
end;

function TPSPascalCompilerEx.IsVarUsed(const AVarName: string): Boolean;
var
  I: Integer;
begin
  if FUsedVars <> nil then begin
    Result := FUsedVars.Find(AVarName, I);
  end else begin
    raise Exception.Create('UsedVars not assigned!');
  end;
end;

function TPSPascalCompilerEx.KnownVarsToString: string;
var
  I: Integer;
  VVar: TPSVar;
begin
  Result := '';
  for I := 0 to Self.GetVarCount - 1 do begin
    VVar := Self.GetVar(I);
    if I > 0 then begin
      Result := Result + #13#10;
    end;
    Result := Result + VVar.OrgName;
    if Assigned(VVar.aType) then begin
      Result := Result + ': ' + VVar.aType.OriginalName;
    end;
  end;
end;

function TPSPascalCompilerEx.KnownProcsToString(const AFormat: TKnownProcsToStringFormat): string;
var
  I, J: Integer;
  VProc: TPSRegProc;
  VListIdent: string;
  VBoldIdent: string;
begin
  Result := '';

  case AFormat of
    kpfNone: begin
      VListIdent := '';
      VBoldIdent := '';
    end;
    kpfWiki: begin
      VListIdent := '  * ';
      VBoldIdent := '**';
    end;
    kpfMarkdown: begin
      VListIdent := ' - ';
      VBoldIdent := '';
    end;
  else
    raise Exception.CreateFmt('Unexpected format value: %d', [Integer(AFormat)]);
  end;

  for I := 0 to Self.GetRegProcCount - 1 do begin
    VProc := Self.GetRegProc(I);
    if I > 0 then begin
      Result := Result + #13#10;
    end;
    if Assigned(VProc.Decl.Result) then begin
      Result := Result + VListIdent + 'function ';
    end else begin
      Result := Result + VListIdent + 'procedure ';
    end;
    Result := Result + VBoldIdent + VProc.OrgName + VBoldIdent;
    if VProc.Decl.ParamCount > 0 then begin
      Result := Result + '(';
      for J := 0 to VProc.Decl.ParamCount - 1 do begin
        if J > 0 then begin
          Result := Result + '; ';
        end;
        case VProc.Decl.Params[J].Mode of
          pmIn: Result := Result + 'const ';
          pmOut: Result := Result + 'out ';
          pmInOut: Result := Result + 'var ';
        end;
        Result := Result + VProc.Decl.Params[J].OrgName;
        if Assigned(VProc.Decl.Params[J].aType) then begin
          Result := Result + ': ' + VProc.Decl.Params[J].aType.OriginalName;
        end;
      end;
      Result := Result + ')';
    end;
    if Assigned(VProc.Decl.Result) then begin
      Result := Result + ': ' + VProc.Decl.Result.OriginalName + ';';
    end else begin
      Result := Result + ';';
    end;
  end;
end;

function TPSPascalCompilerEx.KnownConstsToString: string;
var
  I: Integer;
  VConst: TPSConstant;
begin
  Result := '';
  for I := 0 to Self.GetConstCount - 1 do begin
    VConst := Self.GetConst(I);
    if I > 0 then begin
      Result := Result + #13#10;
    end;
    Result := Result + VConst.OrgName;
  end;
end;

function TPSPascalCompilerEx.KnownTypesToString: string;
var
  I: Integer;
  VType: TPSType;
begin
  for I := 0 to Self.GetTypeCount - 1 do begin
    VType := GetType(I);
    if I > 0 then begin
      Result := Result + #13#10;
    end;
    Result := Result + VType.OriginalName;
  end;
end;

procedure TPSPascalCompilerEx.DoStoreDebugInfo;
const
  CRLF = #13#10#13#10;
begin
  FDebugInfo :=
    'PascalScript ' + PSCurrentversion + CRLF +
    'Types (' + IntToStr(Self.GetTypeCount) + '):' + CRLF + KnownTypesToString + CRLF +
    'Const (' + IntToStr(Self.GetConstCount) + '):' + CRLF + KnownConstsToString + CRLF +
    'Var (' + IntToStr(Self.GetVarCount) + '):' + CRLF + KnownVarsToString + CRLF +
    'Proc (' + IntToStr(Self.GetRegProcCount) + '):' + CRLF + KnownProcsToString(kpfNone);

  FDebugInfo := StringReplace(FDebugInfo, '!OPENARRAYOFCONST', 'array of const', [rfIgnoreCase, rfReplaceAll]);
  FDebugInfo := StringReplace(FDebugInfo, '!OPENARRAYOFVARIANT', 'array of Variant', [rfIgnoreCase, rfReplaceAll]);
end;

end.
