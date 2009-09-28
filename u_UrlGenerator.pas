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
    FGetURLScript : string;
    FGetURLBase : String;
    procedure SetVar;
  public
    constructor Create(AGetURLScript : string; ACoordConverter : ICoordConverter);
    destructor Destroy; override;
    function GenLink(Ax, Ay : longint; Azoom : byte) : string;
    property GetURLBase : string read FGetURLBase write SetGetURLBase;
  end;

implementation

uses
  Math,
  u_GeoToStr,
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
    FpGetX.Data := posxp;
    FpGetY.Data := posyp;
    FpGetZ.Data := zoom + 1;
    XY.X := posxp;
    XY.Y := posyp;
    Ll := FCoordConverter.Pos2LonLat(XY, zoom);
    FpGetLlon.Data := Ll.X;
    FpGetTLat.Data := Ll.Y;
    Ll:= FCoordConverter.LonLat2Metr(LL);
    FpGetLMetr.Data := Ll.X;
    FpGetTMetr.Data := Ll.Y;
    XY.X := (posxp + 1);
    XY.Y := (posyp + 1);
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

procedure TUrlGenerator.SetGetURLBase(const Value: string);
begin
  FGetURLBase:=Value;
  FpGetURLBase.Data:=Value;
end;

end.
