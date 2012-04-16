{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BasePascalCompiler;

interface

uses
  SysUtils,
  uPSC_dll,
  uPSR_dll,
  uPSRuntime,
  uPSCompiler,
  uPSUtils,
  i_CoordConverter,
  u_ResStrings;

type
  EPascalScriptCompileError = class(Exception);
  EPascalScriptRunError = class(Exception);

  TBasePascalScriptExec = class(TPSExec)
  public
    procedure RegisterAppCommonRoutines;
  end;
  
  TBasePascalCompiler = class;

  TOnBasePascalCompilerUsesProc = function (ACompiler: TBasePascalCompiler;
                                            const AName: string): Boolean of object;
  
  TBasePascalCompiler = class(TPSPascalCompiler)
  private
    FOnAuxUses: TOnBasePascalCompilerUsesProc;
    FScriptText: String;
  public
    constructor Create(const AScriptText: String);
    function CompileAndGetOutput(var AData: TbtString): Boolean;
    property OnAuxUses: TOnBasePascalCompilerUsesProc read FOnAuxUses write FOnAuxUses;
  end;

  TBaseFactoryPascalScript = class(TInterfacedObject)
  protected
    FScriptText: string;
    FCompiledData: TbtString;
  protected
    function DoCompilerOnAuxUses(ACompiler: TBasePascalCompiler; const AName: string): Boolean; virtual;
    function PreparePascalScript(const APascalScript: String): Boolean;
  public
    constructor Create(const AScriptText: string);
  end;

  TBasePascalScriptCompiled = class(TInterfacedObject)
  protected
    FScriptBuffer: string;
    FExec: TBasePascalScriptExec;
  protected
    procedure PrepareCompiledScript(const ACompiledData: TbtString);
    procedure RegisterAppRoutines; virtual;
    procedure RegisterAppVars; virtual; abstract;
  end;

implementation

uses
  Math,
  RegExprUtils,
  u_GeoToStr,
  u_TileRequestBuilderHelpers;

function CommonAppScriptOnUses(Sender: TPSPascalCompiler;
                               const AName: string): Boolean;
var
  T: TPSType;
  RecT: TPSRecordType;
begin
  // common types
  if SameText(AName,'SYSTEM') then begin
    // TPoint
    T := Sender.FindType('integer');
    RecT := TPSRecordType(Sender.AddType('TPoint', btRecord));
    with RecT.AddRecVal do begin
      FieldOrgName := 'x';
      aType := t;
    end;

    with RecT.AddRecVal do begin
      FieldOrgName := 'y';
      aType := t;
    end;

    // TDoublePoint
    T := Sender.FindType('Double');
    RecT := TPSRecordType(Sender.AddType('TDoublePoint', btRecord));
    with RecT.AddRecVal do begin
      FieldOrgName := 'x';
      aType := t;
    end;

    with RecT.AddRecVal do begin
      FieldOrgName := 'y';
      aType := t;
    end;

    // ICoordConverter
    with Sender.AddInterface(Sender.FindInterface('IUnknown'), ICoordConverterSimple, 'ICoordConverter') do begin
      RegisterMethod('function Pos2LonLat(XY : TPoint; Azoom : byte) : TDoublePoint', cdStdCall);
      RegisterMethod('function LonLat2Pos(Ll : TDoublePoint; Azoom : byte) : Tpoint', cdStdCall);
      RegisterMethod('function LonLat2Metr(Ll : TDoublePoint) : TDoublePoint', cdStdCall);
      RegisterMethod('function Metr2LonLat(Mm : TDoublePoint) : TDoublePoint', cdStdCall);

      RegisterMethod('function TilesAtZoom(AZoom: byte): Longint', cdStdCall);
      RegisterMethod('function PixelsAtZoom(AZoom: byte): Longint', cdStdCall);

      RegisterMethod('function TilePos2PixelPos(const XY : TPoint; Azoom : byte): TPoint', cdStdCall);
      RegisterMethod('function TilePos2PixelRect(const XY : TPoint; Azoom : byte): TRect', cdStdCall);
    end;
  end;

  // custom vars and functions
  if (Sender is TBasePascalCompiler) then
    if Assigned(TBasePascalCompiler(Sender).OnAuxUses) then
      TBasePascalCompiler(Sender).OnAuxUses(TBasePascalCompiler(Sender), AName);

  // common functions
  if SameText(AName,'SYSTEM') then begin
    // numeric routines
    Sender.AddDelphiFunction('function Random(x:integer): integer');
    Sender.AddDelphiFunction('function RoundEx(chislo: Double; Precision: Integer): string');
    Sender.AddDelphiFunction('function IntPower(const Base: Extended; const Exponent: Integer): Extended register');
    Sender.AddDelphiFunction('function IntToHex(Value: Integer; Digits: Integer): string');

    // string routines
    Sender.AddDelphiFunction('function Length(Str: string): integer');
    Sender.AddDelphiFunction('function GetAfter(SubStr, Str: string): string');
    Sender.AddDelphiFunction('function GetBefore(SubStr, Str: string): string');
    Sender.AddDelphiFunction('function GetBetween(Str, After, Before: string): string');
    Sender.AddDelphiFunction('function SubStrPos(const Str, SubStr: String; FromPos: integer): integer');
    Sender.AddDelphiFunction('function RegExprGetMatchSubStr(const Str, MatchExpr: string; AMatchID: Integer): string');
    Sender.AddDelphiFunction('function RegExprReplaceMatchSubStr(const Str, MatchExpr, Replace: string): string');
    Sender.AddDelphiFunction('function GetNumberAfter(const ASubStr, AText: String): String');
    Sender.AddDelphiFunction('function GetDiv3Path(const ANumber: String): String');

    // system routines
    Sender.AddDelphiFunction('function GetUnixTime:int64');
    Sender.AddDelphiFunction('function SetHeaderValue(AHeaders, AName, AValue: string): string');
    Sender.AddDelphiFunction('function GetHeaderValue(AHeaders, AName: string): string');
    Sender.AddDelphiFunction('function DoHttpRequest(const ARequestUrl, ARequestHeader, APostData: string; out AResponseHeader, AResponseData: string): Cardinal');
    Sender.AddDelphiFunction('function DownloadFileToLocal(const AFullRemoteUrl, AFullLocalFilename, AContentType: String): Integer');
    Sender.AddDelphiFunction('function FileExists(const FileName: string): Boolean');
    
    Result := True;
  end else begin
    Result := False;
  end;
end;


{ TBasePascalCompiler }

function TBasePascalCompiler.CompileAndGetOutput(var AData: TbtString): Boolean;
var
  i: Integer;
  VCompilerMsg: string;
begin
  if (not Self.Compile(FScriptText)) then begin
    VCompilerMsg := '';
    if (0<Self.MsgCount) then
    for i := 0 to Self.MsgCount - 1 do
      VCompilerMsg := VCompilerMsg + Self.Msg[i].MessageToString + #13#10;
    raise EPascalScriptCompileError.CreateFmt(SAS_ERR_UrlScriptCompileError, [VCompilerMsg]);
  end;
  Result := Self.GetOutput(AData);
end;

constructor TBasePascalCompiler.Create(const AScriptText: String);
begin
  inherited Create;
  FOnAuxUses := nil;
  FScriptText := AScriptText;
  OnExternalProc := DllExternalProc;
  OnUses := CommonAppScriptOnUses;
end;

{ TBasePascalScriptExec }

procedure TBasePascalScriptExec.RegisterAppCommonRoutines;
begin
  RegisterDLLRuntime(Self);
  
  // numeric routines
  Self.RegisterDelphiFunction(@RoundEx, 'RoundEx', cdRegister);
  Self.RegisterDelphiFunction(@IntPower, 'IntPower', cdRegister);
  Self.RegisterDelphiFunction(@Rand, 'Random', cdRegister);
  Self.RegisterDelphiFunction(@IntToHex, 'IntToHex', cdRegister);

  // string routines
  Self.RegisterDelphiFunction(@StrLength, 'Length', cdRegister);
  Self.RegisterDelphiFunction(@GetAfter, 'GetAfter', cdRegister);
  Self.RegisterDelphiFunction(@GetBefore, 'GetBefore', cdRegister);
  Self.RegisterDelphiFunction(@GetBetween, 'GetBetween', cdRegister);
  Self.RegisterDelphiFunction(@SubStrPos, 'SubStrPos', cdRegister);
  Self.RegisterDelphiFunction(@RegExprGetMatchSubStr, 'RegExprGetMatchSubStr', cdRegister);
  Self.RegisterDelphiFunction(@RegExprReplaceMatchSubStr, 'RegExprReplaceMatchSubStr', cdRegister);
  Self.RegisterDelphiFunction(@GetNumberAfter, 'GetNumberAfter', cdRegister);

  // system routines
  Self.RegisterDelphiFunction(@GetUnixTime, 'GetUnixTime', cdRegister);
  Self.RegisterDelphiFunction(@SetHeaderValue, 'SetHeaderValue', cdRegister);
  Self.RegisterDelphiFunction(@GetHeaderValue, 'GetHeaderValue', cdRegister);
  Self.RegisterDelphiFunction(@DoHttpRequest, 'DoHttpRequest', cdRegister);
  Self.RegisterDelphiFunction(@GetDiv3Path, 'GetDiv3Path', cdRegister);
  Self.RegisterDelphiFunction(@DownloadFileToLocal, 'DownloadFileToLocal', cdRegister);
  Self.RegisterDelphiFunction(@FileExists, 'FileExists', cdRegister);
end;

{ TBaseFactoryPascalScript }

constructor TBaseFactoryPascalScript.Create(const AScriptText: string);
begin
  inherited Create;
  FCompiledData:='';
  FScriptText:=AScriptText;
end;

function TBaseFactoryPascalScript.DoCompilerOnAuxUses(ACompiler: TBasePascalCompiler; const AName: string): Boolean;
begin
  // common routines linked to object (based on TPSPascalCompiler)
  Result:=FALSE;
end;

function TBaseFactoryPascalScript.PreparePascalScript(const APascalScript: String): Boolean;
var
  VCompiler: TBasePascalCompiler;
begin
  FScriptText:=APascalScript;
  VCompiler:=TBasePascalCompiler.Create(APascalScript);
  try
    VCompiler.OnExternalProc := DllExternalProc;
    VCompiler.OnAuxUses := DoCompilerOnAuxUses;
    VCompiler.OnUses := CommonAppScriptOnUses;
    Result := VCompiler.CompileAndGetOutput(FCompiledData);
  finally
    FreeAndNil(VCompiler);
  end;
end;

{ TBasePascalScriptCompiled }

procedure TBasePascalScriptCompiled.PrepareCompiledScript(const ACompiledData: TbtString);
begin
  FScriptBuffer := '';

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
      TIFErrorToString(FExec.ExceptionCode, FExec.ExceptionString)
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
