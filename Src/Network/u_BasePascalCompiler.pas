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

unit u_BasePascalCompiler;

interface

{.$DEFINE DO_DUMP_PASCAL_SCRIPT_ENV} // out file: '.\PascalScriptEnv.txt'

uses
  SysUtils,
  uPSC_dll,
  uPSR_dll,
  uPSRuntime,
  uPSCompiler,
  uPSUtils,
  u_BaseInterfacedObject;

type
  EPascalScriptEmptyScript = class(Exception);
  EPascalScriptCompileError = class(Exception);
  EPascalScriptRunError = class(Exception);

  TBasePascalScriptExec = class(TPSExec)
  public
    procedure RegisterAppCommonRoutines;
  end;

  TBasePascalCompiler = class;

  TOnBasePascalCompilerUsesProc =
    function(
      ACompiler: TBasePascalCompiler;
      const AName: tbtString
    ): Boolean of object;

  TBasePascalCompiler = class(TPSPascalCompiler)
  private
    FOnAuxUses: TOnBasePascalCompilerUsesProc;
    FScriptText: AnsiString;
  public
    constructor Create(const AScriptText: AnsiString);
    function CompileAndGetOutput(var AData: TbtString): Boolean;
    {$IFDEF DO_DUMP_PASCAL_SCRIPT_ENV}
    function KnownVarsToString: string;
    function KnownProcsToString(const AWikiFormat: Boolean = True): string;
    function KnownConstsToString: string;
    function KnownTypesToString: string;
    procedure EnvToLog(const AFileName: string);
    {$ENDIF}
    property OnAuxUses: TOnBasePascalCompilerUsesProc read FOnAuxUses write FOnAuxUses;
  end;

  TBaseFactoryPascalScript = class(TBaseInterfacedObject)
  private
    FScriptText: AnsiString;
    FCompiledData: TbtString;
  protected
    property CompiledData: TbtString read FCompiledData;
    function DoCompilerOnAuxUses(
      ACompiler: TBasePascalCompiler;
      const AName: tbtString
    ): Boolean; virtual;
    function PreparePascalScript: Boolean;
  public
    constructor Create(const AScriptText: AnsiString);
  end;

  TBasePascalScriptCompiled = class(TBaseInterfacedObject)
  protected
    FExec: TBasePascalScriptExec;
  protected
    procedure PrepareCompiledScript(const ACompiledData: TbtString);
    procedure RegisterAppRoutines; virtual;
    procedure RegisterAppVars; virtual; abstract;
  end;

implementation

uses
  {$IFDEF DO_DUMP_PASCAL_SCRIPT_ENV}
  Classes,
  {$ENDIF}
  ALString,
  u_ResStrings,
  u_PascalScriptBase64,
  u_PascalScriptRegExpr,
  u_PascalScriptMath,
  u_PascalScriptTypes,
  u_PascalScriptUtils;

function CommonAppScriptOnUses(
  Sender: TPSPascalCompiler;
  const AName: tbtString
): Boolean;
begin
  // common types
  if ALSameText(AName, 'SYSTEM') then begin
    CompileTimeReg_CommonTypes(Sender);
    CompileTimeReg_ProjConverter(Sender);
    CompileTimeReg_ProjConverterFactory(Sender);
    CompileTimeReg_CoordConverterSimple(Sender);
    CompileTimeReg_SimpleHttpDownloader(Sender);
  end;

  // custom vars and functions
  if (Sender is TBasePascalCompiler) then begin
    if Assigned(TBasePascalCompiler(Sender).OnAuxUses) then begin
      TBasePascalCompiler(Sender).OnAuxUses(TBasePascalCompiler(Sender), AName);
    end;
  end;

  // common functions
  if ALSameText(AName, 'SYSTEM') then begin
    CompileTimeReg_Math(Sender);
    CompileTimeReg_RegExpr(Sender);
    CompileTimeReg_Base64(Sender);
    CompileTimeReg_Utils(Sender);
    Result := True;
  end else begin
    Result := False;
  end;

  {$IFDEF DO_DUMP_PASCAL_SCRIPT_ENV}
  if (Sender is TBasePascalCompiler) then begin
    (Sender as TBasePascalCompiler).EnvToLog('.\PascalScriptEnv.txt');
  end;
  {$ENDIF}
end;

{ TBasePascalCompiler }

function TBasePascalCompiler.CompileAndGetOutput(var AData: TbtString): Boolean;
var
  i: Integer;
  VCompilerMsg: AnsiString;
begin
  if (not Self.Compile(FScriptText)) then begin
    VCompilerMsg := '';
    if (0 < Self.MsgCount) then begin
      for i := 0 to Self.MsgCount - 1 do begin
        VCompilerMsg := VCompilerMsg + Self.Msg[i].MessageToString + #13#10;
      end;
    end;
    raise EPascalScriptCompileError.CreateFmt(SAS_ERR_UrlScriptCompileError, [VCompilerMsg]);
  end;
  Result := Self.GetOutput(AData);
end;

constructor TBasePascalCompiler.Create(const AScriptText: AnsiString);
begin
  inherited Create;
  FOnAuxUses := nil;
  FScriptText := AScriptText;
  OnExternalProc := DllExternalProc;
  OnUses := CommonAppScriptOnUses;
end;

{$IFDEF DO_DUMP_PASCAL_SCRIPT_ENV}
function TBasePascalCompiler.KnownVarsToString: string;
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

function TBasePascalCompiler.KnownProcsToString(const AWikiFormat: Boolean = True): string;
var
  I, J: Integer;
  VProc: TPSRegProc;
  VListIdent: string;
  VBoldIdent: string;
begin
  Result := '';
  if AWikiFormat then begin
    VListIdent := '  * ';
    VBoldIdent := '**';
  end else begin
    VListIdent := '';
    VBoldIdent := '';
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

function TBasePascalCompiler.KnownConstsToString: string;
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

function TBasePascalCompiler.KnownTypesToString: string;
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

procedure TBasePascalCompiler.EnvToLog(const AFileName: string);
const
  CRLF = #13#10#13#10;
var
  VStream: TMemoryStream;
  VText: string;
begin
  VStream := TMemoryStream.Create;
  try
    VText :=
      'PascalScript ' + PSCurrentversion + CRLF +
      'Types (' + IntToStr(Self.GetTypeCount) + '):' + CRLF + KnownTypesToString + CRLF +
      'Const (' + IntToStr(Self.GetConstCount) + '):' + CRLF + KnownConstsToString + CRLF +
      'Var (' + IntToStr(Self.GetVarCount) + '):' + CRLF + KnownVarsToString + CRLF +
      'Proc (' + IntToStr(Self.GetRegProcCount) + '):' + CRLF + KnownProcsToString;
    VStream.WriteBuffer(VText[1], Length(VText)*SizeOf(VText[1]));
    VStream.SaveToFile(AFileName);
  finally
    VStream.Free;
  end;
end;
{$ENDIF}

{ TBasePascalScriptExec }

procedure TBasePascalScriptExec.RegisterAppCommonRoutines;
begin
  RegisterDLLRuntime(Self);
  
  ExecTimeReg_Math(Self);
  ExecTimeReg_RegExpr(Self);
  ExecTimeReg_Base64(Self);
  ExecTimeReg_Utils(Self);
end;

{ TBaseFactoryPascalScript }

constructor TBaseFactoryPascalScript.Create(const AScriptText: AnsiString);
begin
  inherited Create;
  FCompiledData := '';
  FScriptText := AScriptText;
end;

function TBaseFactoryPascalScript.DoCompilerOnAuxUses(
  ACompiler: TBasePascalCompiler;
  const AName: tbtString
): Boolean;
begin
  // common routines linked to object (based on TPSPascalCompiler)
  Result := FALSE;
end;

function TBaseFactoryPascalScript.PreparePascalScript: Boolean;
var
  VCompiler: TBasePascalCompiler;
begin
  FCompiledData := '';
  if FScriptText = '' then begin
    raise EPascalScriptEmptyScript.Create('Empty script');
  end;
  try
    VCompiler := TBasePascalCompiler.Create(FScriptText);
    try
      VCompiler.OnExternalProc := DllExternalProc;
      VCompiler.OnAuxUses := DoCompilerOnAuxUses;
      VCompiler.OnUses := CommonAppScriptOnUses;
      Result := VCompiler.CompileAndGetOutput(FCompiledData);
    finally
      FreeAndNil(VCompiler);
    end;
  except
    FCompiledData := '';
    raise;
  end;
end;

{ TBasePascalScriptCompiled }

procedure TBasePascalScriptCompiled.PrepareCompiledScript(const ACompiledData: TbtString);
begin
  // create
  FExec := TBasePascalScriptExec.Create;

  // init by common
  FExec.RegisterAppCommonRoutines;

  // init by self
  Self.RegisterAppRoutines;

  // load
  if not FExec.LoadData(ACompiledData) then begin
    raise Exception.Create(
      SAS_ERR_UrlScriptByteCodeLoad + #13#10 +
      string(TIFErrorToString(FExec.ExceptionCode, FExec.ExceptionString))
    );
  end;

  // loaded - add variables
  Self.RegisterAppVars;
end;

procedure TBasePascalScriptCompiled.RegisterAppRoutines;
begin
  // empty
end;

end.
