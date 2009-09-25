unit u_UrlGenerator;

interface
Uses
  Windows,
  SysUtils,
  DateUtils,
  uPSC_dll,
  uPSR_dll,
  uPSRuntime,
  uPSCompiler,
  u_CoordConverterAbstract;

type
  EUrlGeneratorScriptCompileError = class(Exception);
  EUrlGeneratorScriptRunError = class(Exception);
  TUrlGenerator = class
  private
    procedure SetGetURLBase(const Value: string);
    procedure SetdLpix(const d:integer);
    procedure SetdRpix(const d:integer);
    procedure SetdTpix(const d:integer);
    procedure SetdBpix(const d:integer);
  protected
    posxp:integer;
    posyp:integer;
    zoom:byte;
    FCoordConverter : ICoordConverter;
    FCS : TRTLCriticalSection;
    FExec: TPSExec;
    FpResultUrl : PPSVariantAString;
    FpGetURLBase : PPSVariantAString;
    FpGetX : PPSVariantS32;
    FpGetY : PPSVariantS32;
    FpGetZ : PPSVariantS32;
    FpGetLlon : PPSVariantExtended;
    FpGetTLat : PPSVariantExtended;
    FpGetBLat : PPSVariantExtended;
    FpGetRLon : PPSVariantExtended;
    FpGetLMetr: PPSVariantExtended;
    FpGetRMetr: PPSVariantExtended;
    FpGetTMetr: PPSVariantExtended;
    FpGetBMetr: PPSVariantExtended;
    FpdLpix : PPSVariantS32;
    FpdTpix : PPSVariantS32;
    FpdBpix : PPSVariantS32;
    FpdRpix : PPSVariantS32;
    FdLpix : integer;
    FdTpix : integer;
    FdBpix : integer;
    FdRpix : integer;
    FGetURLScript : string;
    FGetURLBase : String;
  public
    constructor Create(AGetURLScript : string; ACoordConverter : ICoordConverter);
    destructor Destroy; override;
    function GenLink(Ax, Ay : longint; Azoom : byte) : string;
    procedure SetVar;
    property dLpix : integer read FdLpix write SetdLpix;
    property dRpix : integer read FdRpix write SetdRpix;
    property dTpix : integer read FdTpix write SetdTpix;
    property dBpix : integer read FdBpix write SetdBpix;
    property GetURLBase : string read FGetURLBase write SetGetURLBase;
  end;

implementation

uses
  Math,
  t_GeoTypes;

function ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
var
  T : TPSType;
begin
  if Name = 'SYSTEM' then
  begin
    T := Sender.FindType('string');
    Sender.AddUsedVariable('ResultURL', t);
    Sender.AddUsedVariable('GetURLBase', t);
    T := Sender.FindType('integer');
    Sender.AddUsedVariable('GetX', t);
    Sender.AddUsedVariable('GetY', t);
    Sender.AddUsedVariable('GetZ', t);
    Sender.AddUsedVariable('dLpix', t);
    Sender.AddUsedVariable('dTpix', t);
    Sender.AddUsedVariable('dBpix', t);
    Sender.AddUsedVariable('dRpix', t);
    T := Sender.FindType('extended');
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
    Sender.AddDelphiFunction('function RoundEx(chislo: Extended; Precision: Integer): string');
    Sender.AddDelphiFunction('function IntPower(const Base: Extended; const Exponent: Integer): Extended register');
    Result := True;
  end else begin
    Result := False;
  end;
end;

function Rand(x:integer):integer;
begin
 Result:=Random(x);
end;

function GetUnixTime(x:integer):int64;
begin
 Result:=DateTimeToUnix(now);
end;

function RoundEx(chislo: Extended; Precision: Integer): string;
var VFormatSettings : TFormatSettings;
begin
  VFormatSettings.DecimalSeparator := '.';
  Result := FloatToStrF(chislo, ffFixed, Precision,Precision, VFormatSettings);
end;

{ TUrlGenerator }
constructor TUrlGenerator.Create(AGetURLScript : string; ACoordConverter : ICoordConverter);
var i:integer;
    Msg:string;
    VCompiler:TPSPascalCompiler;
    VData:string;
begin
  FGetURLScript:= AGetURLScript;
  FCoordConverter:= ACoordConverter;
  VCompiler:= TPSPascalCompiler.Create;       // create an instance of the compiler.
  VCompiler.OnExternalProc := DllExternalProc; // Добавляем стандартный обработчик внешних DLL(находится в модуле uPSC_dll)
  VCompiler.OnUses:= ScriptOnUses;            // assign the OnUses event.
  if not VCompiler.Compile(FGetURLScript) then begin  // Compile the Pascal script into bytecode.
    Msg := '';
    For i := 0 to VCompiler.MsgCount-1 do begin
     MSG := Msg + VCompiler.Msg[i].MessageToString+#13#10;
    end;
    raise EUrlGeneratorScriptCompileError.Create('Ошибка в скрипте при компиляции'#13#10 + Msg);
  end;
  VCompiler.GetOutput(VData); // Save the output of the compiler in the string Data.
  VCompiler.Free;            // After compiling the script, there is no further need for the compiler.
  FExec := TPSExec.Create;   // Create an instance of the executer.
  RegisterDLLRuntime(FExec);

  FExec.RegisterDelphiFunction(@RoundEx, 'RoundEx', cdRegister);
  FExec.RegisterDelphiFunction(@IntPower, 'IntPower', cdRegister);
  FExec.RegisterDelphiFunction(@Rand, 'Random', cdRegister);

  if not  FExec.LoadData(VData) then begin // Load the data from the Data string.
    raise Exception.Create('Ошибка при загрузке байткода');
  end;
  FpResultUrl := PPSVariantAString(FExec.GetVar2('ResultURL'));
  FpGetURLBase := PPSVariantAString(FExec.GetVar2('GetURLBase'));
  FpGetX := PPSVariantS32(FExec.GetVar2('GetX'));
  FpGetY := PPSVariantS32(FExec.GetVar2('GetY'));
  FpGetZ := PPSVariantS32(FExec.GetVar2('GetZ'));
  FpGetLlon := PPSVariantExtended(FExec.GetVar2('GetLlon'));
  FpGetTLat := PPSVariantExtended(FExec.GetVar2('GetTLat'));
  FpGetBLat := PPSVariantExtended(FExec.GetVar2('GetBLat'));
  FpGetRLon := PPSVariantExtended(FExec.GetVar2('GetRLon'));
  FpGetLmetr := PPSVariantExtended(FExec.GetVar2('GetLmetr'));
  FpGetTmetr := PPSVariantExtended(FExec.GetVar2('GetTmetr'));
  FpGetBmetr := PPSVariantExtended(FExec.GetVar2('GetBmetr'));
  FpGetRmetr := PPSVariantExtended(FExec.GetVar2('GetRmetr'));
  FpdLpix := PPSVariantS32(FExec.GetVar2('dLpix'));
  FpdRpix := PPSVariantS32(FExec.GetVar2('dRpix'));
  FpdTpix := PPSVariantS32(FExec.GetVar2('dTpix'));
  FpdBpix := PPSVariantS32(FExec.GetVar2('dBpix'));
  InitializeCriticalSection(FCS);
end;

destructor TUrlGenerator.Destroy;
begin
  DeleteCriticalSection(FCS);
  FreeAndNil(FExec);
  FCoordConverter := nil;
  inherited;
end;

procedure TUrlGenerator.SetVar;
var XY : TPoint;
    Ll : TExtendedPoint;
begin
    FpGetX.Data := posxp div 256;
    FpGetY.Data := posyp div 256;
    FpGetZ.Data := zoom + 1;
    XY.X := posxp-(posxp mod 256)+fpdLpix.Data;
    XY.Y := posyp-(posyp mod 256)+fpdTpix.Data;
    Ll := FCoordConverter.Pos2LonLat(XY, zoom);
    FpGetLlon.Data := Ll.X;
    FpGetTLat.Data := Ll.Y;
    Ll:= FCoordConverter.LonLat2Metr(LL);
    FpGetLMetr.Data := Ll.X;
    FpGetTMetr.Data := Ll.Y;
    XY.X := (posxp + 256-(posxp mod 256))+fpdRpix.Data;
    XY.Y := (posyp + 256-(posyp mod 256))+fpdBpix.Data;
    Ll := FCoordConverter.Pos2LonLat(XY, zoom);
    FpGetRLon.Data := Ll.X;
    FpGetBLat.Data := Ll.Y;
    Ll:=FCoordConverter.LonLat2Metr(LL);
    FpGetRMetr.Data := Ll.X;
    FpGetBMetr.Data := Ll.Y;
end;

function TUrlGenerator.GenLink(Ax, Ay: Integer; Azoom: byte): string;
begin
  EnterCriticalSection(FCS);
  try
    FpResultUrl.Data := '';
    posxp:=Ax;
    posYp:=Ay;
    zoom:=Azoom;
    SetVar;
    try
      FExec.RunScript; // Run the script.
    except
      on e : Exception do begin
        raise EUrlGeneratorScriptRunError.Create(e.Message);
      end;
    end;
    Result:=FpResultUrl.Data;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

procedure TUrlGenerator.SetdLpix(const d:integer);
begin
  FdLpix:=d;
  FpdLpix.Data:=d;
  SetVar;
end;

procedure TUrlGenerator.SetdRpix(const d:integer);
begin
  FdRpix:=d;
  FpdRpix.Data:=d;
  SetVar;
end;

procedure TUrlGenerator.SetdTpix(const d:integer);
begin
  FdTpix:=d;
  FpdTpix.Data:=d;
  SetVar;
end;

procedure TUrlGenerator.SetdBpix(const d:integer);
begin
  FdBpix:=d;
  FpdBpix.Data:=d;
  SetVar;
end;

procedure TUrlGenerator.SetGetURLBase(const Value: string);
begin
  FGetURLBase:=Value;
  FpGetURLBase.Data:=Value;
end;

end.
