unit u_UrlGenerator;

interface

Uses
  Windows,
  SysUtils,
  SyncObjs,
  uPSC_dll,
  uPSR_dll,
  uPSRuntime,
  uPSCompiler,
  i_ConfigDataProvider,
  i_CoordConverter;

type
  EUrlGeneratorScriptCompileError = class(Exception);
  EUrlGeneratorScriptRunError = class(Exception);

  TUrlGeneratorBasic = class
  private
    FCS: TCriticalSection;
    FDefURLBase: string;
    FURLBase: String;
    FDefRequestHead: string;
    FRequestHead: string;
    FLastResponseHead: string;
  protected
    procedure Lock;
    procedure Unlock;
    procedure SetURLBase(const Value: string); virtual;
    procedure SetRequestHead(const ARequestHead: string);
    function  GetRequestHead: string; virtual;
    procedure SetResponseHead(const AResponseHead: string); virtual;
    function  GetResponseHead: string; virtual;
  public
    constructor Create(AConfig: IConfigDataProvider);
    destructor Destroy; override;

    function GenLink(Ax, Ay: longint; Azoom: byte): string; virtual;
    procedure GenRequest(Ax, Ay: Integer; Azoom: byte; out AUrl, AHeaders: string); virtual;

    property URLBase: string read FURLBase write SetURLBase;
    property DefURLBase: string read FDefURLBase;
    property RequestHead: string read GetRequestHead write SetRequestHead;
    property DefRequestHead: string read FDefRequestHead;
    property ResponseHead: string read GetResponseHead write SetResponseHead;
  end;

  TUrlGenerator = class(TUrlGeneratorBasic)
  private
    FCoordConverter: ICoordConverterSimple;
    FGetURLScript: string;
    FScriptBuffer: string;
    FExec: TPSExec;
    FpResultUrl: PPSVariantAString;
    FpGetURLBase: PPSVariantAString;
    FpRequestHead: PPSVariantAString;
    FpResponseHead: PPSVariantAString;
    FpScriptBuffer: PPSVariantAString;
    FpGetX: PPSVariantS32;
    FpGetY: PPSVariantS32;
    FpGetZ: PPSVariantS32;
    FpGetLlon: PPSVariantDouble;
    FpGetTLat: PPSVariantDouble;
    FpGetBLat: PPSVariantDouble;
    FpGetRLon: PPSVariantDouble;
    FpGetLMetr: PPSVariantDouble;
    FpGetRMetr: PPSVariantDouble;
    FpGetTMetr: PPSVariantDouble;
    FpGetBMetr: PPSVariantDouble;
    FpConverter: PPSVariantInterface;
    procedure SetVar(AXY: TPoint; AZoom: Byte);
    procedure LoadProjectionInfo(AConfig : IConfigDataProvider);
  public
    constructor Create(
      AConfig: IConfigDataProvider
      );
    destructor Destroy; override;
    function GenLink(Ax, Ay: longint; Azoom: byte): string; override;
    procedure GenRequest(Ax, Ay: Integer; Azoom: byte; out AUrl, AHeaders: string); override;
  end;

implementation

uses
  Math,
  Classes,
  Types,
  uPSUtils,
  u_GeoToStr,
  u_ResStrings,
  t_GeoTypes,
  u_GlobalState,
  u_UrlGeneratorHelpers;

{ TUrlGeneratorBasic }

constructor TUrlGeneratorBasic.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  FCS := TCriticalSection.Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FURLBase := VParams.ReadString('URLBase', '');
  FDefUrlBase := VParams.ReadString('MAIN:URLBase', '');
  FRequestHead := VParams.ReadString('RequestHead', '');
  FRequestHead := StringReplace(FRequestHead, '\r\n', #13#10, [rfIgnoreCase, rfReplaceAll]);
  FDefRequestHead := VParams.ReadString('MAIN:RequestHead', '');
  FDefRequestHead := StringReplace(FDefRequestHead, '\r\n', #13#10, [rfIgnoreCase, rfReplaceAll]);
  FLastResponseHead := '';
end;

destructor TUrlGeneratorBasic.Destroy;
begin
  FreeAndNil(FCS);
end;

function TUrlGeneratorBasic.GenLink(Ax, Ay: Integer; Azoom: byte): string;
begin
  Result := '';
end;

procedure TUrlGeneratorBasic.GenRequest(Ax, Ay: Integer; Azoom: byte; out AUrl, AHeaders: string);
begin
  AUrl := '';
  AHeaders := '';
end;

procedure TUrlGeneratorBasic.SetResponseHead(const AResponseHead: string);
begin
    Lock;
  try
    FLastResponseHead := AResponseHead;
  finally
    Unlock;
  end;
end;

function TUrlGeneratorBasic.GetResponseHead: string;
begin
    Lock;
  try
    Result := FLastResponseHead;
  finally
    Unlock;
  end;
end;

procedure TUrlGeneratorBasic.SetRequestHead(const ARequestHead: string);
begin
    Lock;
  try
    FRequestHead := ARequestHead;
  finally
    Unlock;
  end;
end;

function TUrlGeneratorBasic.GetRequestHead: string;
begin
    Lock;
  try
    Result := FRequestHead;
  finally
    Unlock;
  end;
end;

procedure TUrlGeneratorBasic.SetURLBase(const Value: string);
begin
    Lock;
  try
    FURLBase := Value;
  finally
    Unlock;
  end;
end;

procedure TUrlGeneratorBasic.Lock;
begin
  FCS.Acquire;
end;

procedure TUrlGeneratorBasic.Unlock;
begin
  FCS.Release;
end;


function ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
var
  T: TPSType;
  RecT: TPSRecordType;
begin
  if Name = 'SYSTEM' then begin
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

    with Sender.AddInterface(Sender.FindInterface('IUnknown'), ICoordConverterSimple, 'ICoordConverter') do begin
      RegisterMethod('function Pos2LonLat(XY : TPoint; Azoom : byte) : TDoublePoint', cdStdCall);
      RegisterMethod('function LonLat2Pos(Ll : TDoublePoint; Azoom : byte) : Tpoint', cdStdCall);
      RegisterMethod('function LonLat2Metr(Ll : TDoublePoint) : TDoublePoint', cdStdCall);

      RegisterMethod('function TilesAtZoom(AZoom: byte): Longint', cdStdCall);
      RegisterMethod('function PixelsAtZoom(AZoom: byte): Longint', cdStdCall);

      RegisterMethod('function TilePos2PixelPos(const XY : TPoint; Azoom : byte): TPoint', cdStdCall);
      RegisterMethod('function TilePos2PixelRect(const XY : TPoint; Azoom : byte): TRect', cdStdCall);
    end;
    T := Sender.FindType('ICoordConverter');
    Sender.AddUsedVariable('Converter', t);

    T := Sender.FindType('string');
    Sender.AddUsedVariable('ResultURL', t);
    Sender.AddUsedVariable('GetURLBase', t);
    Sender.AddUsedVariable('RequestHead', t);
    Sender.AddUsedVariable('ResponseHead', t);
    Sender.AddUsedVariable('ScriptBuffer', t);
    T := Sender.FindType('integer');
    Sender.AddUsedVariable('GetX', t);
    Sender.AddUsedVariable('GetY', t);
    Sender.AddUsedVariable('GetZ', t);
    T := Sender.FindType('Double');
    Sender.AddUsedVariable('GetLlon', t);
    Sender.AddUsedVariable('GetTLat', t);
    Sender.AddUsedVariable('GetBLat', t);
    Sender.AddUsedVariable('GetRLon', t);
    Sender.AddUsedVariable('GetLMetr', t);
    Sender.AddUsedVariable('GetRMetr', t);
    Sender.AddUsedVariable('GetTMetr', t);
    Sender.AddUsedVariable('GetBMetr', t);
    Sender.AddDelphiFunction('function Random(x:integer):integer');
    Sender.AddDelphiFunction('function GetUnixTime:int64');
    Sender.AddDelphiFunction('function RoundEx(chislo: Double; Precision: Integer): string');
    Sender.AddDelphiFunction('function IntPower(const Base: Extended; const Exponent: Integer): Extended register');
    Sender.AddDelphiFunction('function IntToHex(Value: Integer; Digits: Integer): string');
    Sender.AddDelphiFunction('function Length(Str: string): integer');
    Sender.AddDelphiFunction('function GetAfter(SubStr, Str: string): string');
    Sender.AddDelphiFunction('function GetBefore(SubStr, Str: string): string');
    Sender.AddDelphiFunction('function GetBetween(Str, After, Before: string): string');
    Sender.AddDelphiFunction('function SubStrPos(const Str, SubStr: String; FromPos: integer): integer');
    Sender.AddDelphiFunction('function RegExprGetMatchSubStr(const Str, MatchExpr: string; AMatchID: Integer): string');
    Sender.AddDelphiFunction('function RegExprReplaceMatchSubStr(const Str, MatchExpr, Replace: string): string');
    Sender.AddDelphiFunction('function SetHeaderValue(AHeaders, AName, AValue: string): string');
    Sender.AddDelphiFunction('function GetHeaderValue(AHeaders, AName: string): string');
    Sender.AddDelphiFunction('function DoRequest(const AHost, ADoc, ARequestHeader, APostData: AnsiString; UseSSL: Boolean; out AResponseHeader, AResponseData: AnsiString): Cardinal');
    Result := True;
  end else begin
    Result := False;
  end;
end;

{ TUrlGenerator }
constructor TUrlGenerator.Create(AConfig: IConfigDataProvider);
var
  i: integer;
  Msg: string;
  VCompiler: TPSPascalCompiler;
  VData: string;
begin
  inherited Create(AConfig);
  LoadProjectionInfo(AConfig);
  FGetURLScript := AConfig.ReadString('GetUrlScript.txt', '');
  FScriptBuffer := '';

  VCompiler := TPSPascalCompiler.Create;       // create an instance of the compiler.
  try
    VCompiler.OnExternalProc := DllExternalProc; // Добавляем стандартный обработчик внешних DLL(находится в модуле uPSC_dll)
    VCompiler.OnUses := ScriptOnUses;            // assign the OnUses event.
    if not VCompiler.Compile(FGetURLScript) then begin  // Compile the Pascal script into bytecode.
      Msg := '';
      For i := 0 to VCompiler.MsgCount - 1 do begin
        MSG := Msg + VCompiler.Msg[i].MessageToString + #13#10;
      end;
      raise EUrlGeneratorScriptCompileError.CreateFmt(SAS_ERR_UrlScriptCompileError, [Msg]);
    end;
    VCompiler.GetOutput(VData); // Save the output of the compiler in the string Data.
  finally
    VCompiler.Free;          // After compiling the script, there is no further need for the compiler.
  end;
  FExec := TPSExec.Create;   // Create an instance of the executer.
  RegisterDLLRuntime(FExec);

  FExec.RegisterDelphiFunction(@RoundEx, 'RoundEx', cdRegister);
  FExec.RegisterDelphiFunction(@IntPower, 'IntPower', cdRegister);
  FExec.RegisterDelphiFunction(@Rand, 'Random', cdRegister);
  FExec.RegisterDelphiFunction(@IntToHex, 'IntToHex', cdRegister);
  FExec.RegisterDelphiFunction(@StrLength, 'Length', cdRegister);
  FExec.RegisterDelphiFunction(@GetAfter, 'GetAfter', cdRegister);
  FExec.RegisterDelphiFunction(@GetBefore, 'GetBefore', cdRegister);
  FExec.RegisterDelphiFunction(@GetBetween, 'GetBetween', cdRegister);
  FExec.RegisterDelphiFunction(@SubStrPos, 'SubStrPos', cdRegister);
  FExec.RegisterDelphiFunction(@RegExprGetMatchSubStr, 'RegExprGetMatchSubStr', cdRegister);
  FExec.RegisterDelphiFunction(@RegExprReplaceMatchSubStr, 'RegExprReplaceMatchSubStr', cdRegister);
  FExec.RegisterDelphiFunction(@SetHeaderValue, 'SetHeaderValue', cdRegister);
  FExec.RegisterDelphiFunction(@GetHeaderValue, 'GetHeaderValue', cdRegister);
  FExec.RegisterDelphiFunction(@DoRequest, 'DoRequest', cdRegister);

  if not FExec.LoadData(VData) then begin // Load the data from the Data string.
    raise Exception.Create(SAS_ERR_UrlScriptByteCodeLoad);
  end;
  FpResultUrl := PPSVariantAString(FExec.GetVar2('ResultURL'));
  FpGetURLBase := PPSVariantAString(FExec.GetVar2('GetURLBase'));
  FpGetURLBase.Data := FURLBase;
  FpRequestHead := PPSVariantAString(FExec.GetVar2('RequestHead'));
  FpRequestHead.Data := FRequestHead;
  FpResponseHead := PPSVariantAString(FExec.GetVar2('ResponseHead'));
  FpResponseHead.Data := FLastResponseHead;
  FpScriptBuffer := PPSVariantAString(FExec.GetVar2('ScriptBuffer'));
  FpGetX := PPSVariantS32(FExec.GetVar2('GetX'));
  FpGetY := PPSVariantS32(FExec.GetVar2('GetY'));
  FpGetZ := PPSVariantS32(FExec.GetVar2('GetZ'));
  FpGetLlon := PPSVariantDouble(FExec.GetVar2('GetLlon'));
  FpGetTLat := PPSVariantDouble(FExec.GetVar2('GetTLat'));
  FpGetBLat := PPSVariantDouble(FExec.GetVar2('GetBLat'));
  FpGetRLon := PPSVariantDouble(FExec.GetVar2('GetRLon'));
  FpGetLmetr := PPSVariantDouble(FExec.GetVar2('GetLmetr'));
  FpGetTmetr := PPSVariantDouble(FExec.GetVar2('GetTmetr'));
  FpGetBmetr := PPSVariantDouble(FExec.GetVar2('GetBmetr'));
  FpGetRmetr := PPSVariantDouble(FExec.GetVar2('GetRmetr'));
  FpConverter := PPSVariantInterface(FExec.GetVar2('Converter'));
end;

destructor TUrlGenerator.Destroy;
begin
  FreeAndNil(FExec);
  FCoordConverter := nil;
  inherited;
end;

procedure TUrlGenerator.SetVar(AXY: TPoint; AZoom: Byte);
var
  XY: TPoint;
  Ll: TDoublePoint;
begin
  FpGetX.Data := AXY.X;
  FpGetY.Data := AXY.Y;
  FpGetZ.Data := AZoom + 1;
  Ll := FCoordConverter.Pos2LonLat(AXY, AZoom);
  FpGetLlon.Data := Ll.X;
  FpGetTLat.Data := Ll.Y;
  Ll := FCoordConverter.LonLat2Metr(LL);
  FpGetLMetr.Data := Ll.X;
  FpGetTMetr.Data := Ll.Y;
  XY := AXY;
  Inc(XY.X);
  Inc(XY.Y);
  Ll := FCoordConverter.Pos2LonLat(XY, AZoom);
  FpGetRLon.Data := Ll.X;
  FpGetBLat.Data := Ll.Y;
  Ll := FCoordConverter.LonLat2Metr(LL);
  FpGetRMetr.Data := Ll.X;
  FpGetBMetr.Data := Ll.Y;
  FpConverter.Data := FCoordConverter;
  FpGetURLBase.Data := FURLBase;
end;

function TUrlGenerator.GenLink(Ax, Ay: Integer; Azoom: byte): string;
begin
    Lock;
  try
    FpResultUrl.Data := '';
    SetVar(Point(Ax, Ay), Azoom);
    try
      FExec.RunScript; // Run the script.
    except
      on e: Exception do begin
        raise EUrlGeneratorScriptRunError.Create(e.Message);
      end;
    end;
    Result := FpResultUrl.Data;
  finally
    Unlock;
  end;
end;

procedure TUrlGenerator.GenRequest(Ax, Ay: Integer; Azoom: byte; out AUrl, AHeaders: string);
begin
    Lock;
  try
    FpResultUrl.Data := '';
    FpRequestHead.Data := FRequestHead;
    FpResponseHead.Data := FLastResponseHead;
    FpScriptBuffer.Data := FScriptBuffer;
    SetVar(Point(Ax, Ay), Azoom);
    try
      FExec.RunScript; // Run the script.
    except on E: Exception do
      raise EUrlGeneratorScriptRunError.Create(E.Message);
    end;
    AUrl := FpResultUrl.Data;
    AHeaders := FpRequestHead.Data;
    FScriptBuffer := FpScriptBuffer.Data;
  finally
    Unlock;
  end;
end;

procedure TUrlGenerator.LoadProjectionInfo(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
  VCoordConverter: ICoordConverter;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  VCoordConverter := GState.CoordConverterFactory.GetCoordConverterByConfig(VParams);
  FCoordConverter := VCoordConverter as ICoordConverterSimple;
end;

end.

