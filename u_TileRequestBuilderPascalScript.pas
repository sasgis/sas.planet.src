unit u_TileRequestBuilderPascalScript;

interface

uses
  Windows,
  SysUtils,
  uPSC_dll,
  uPSR_dll,
  uPSRuntime,
  uPSCompiler,
  uPSUtils,
  i_JclNotify,
  i_ConfigDataProvider,
  i_CoordConverter,
  i_MapVersionInfo,
  i_LanguageManager,
  i_CoordConverterFactory,
  i_LastResponseInfo,
  i_TileRequestBuilderConfig,
  u_TileRequestBuilder;

type
  EPascalScriptCompileError = class(Exception);
  EPascalScriptRunError = class(Exception);

  TTileRequestBuilderPascalScript = class(TTileRequestBuilder)
  private
    FCoordConverter: ICoordConverterSimple;
    FPascalScript: string;
    FScriptBuffer: string;

    FLang: string;
    FLangManager: ILanguageManager;
    FLangListener: IJclListener;
    FLangChangeCount: Integer;

    FExec: TPSExec;
    FpResultUrl: PPSVariantAString;
    FpGetURLBase: PPSVariantAString;
    FpRequestHead: PPSVariantAString;
    FpResponseHead: PPSVariantAString;
    FpScriptBuffer: PPSVariantAString;
    FpVersion: PPSVariantAString;
    FpLang: PPSVariantAString;
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
    procedure PrepareCoordConverter(
      ACoordConverterFactory: ICoordConverterFactory;
      AConfig: IConfigDataProvider
    );
    procedure PreparePascalScript(AConfig: IConfigDataProvider);
    procedure SetVar(
      ALastResponseInfo: ILastResponseInfo;
      AVersionInfo: IMapVersionInfo;
      AXY: TPoint;
      AZoom: Byte
    );
    procedure OnLangChange(Sender: TObject);
  protected
    function  BuildRequestUrl(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo
    ): string; override;
    procedure BuildRequest(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo;
      ALastResponseInfo: ILastResponseInfo;
      out AUrl: string;
      out ARequestHeader: string
    ); override;
  public
    constructor Create(
      AConfig: ITileRequestBuilderConfig;
      AConfigData: IConfigDataProvider;
      ACoordConverterFactory: ICoordConverterFactory;
      ALangManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  Variants,
  u_NotifyEventListener,
  t_GeoTypes,
  u_GeoToStr,
  u_TileRequestBuilderHelpers,
  u_ResStrings;

const
  PascalScriptFileName = 'GetUrlScript.txt';

function ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean; forward;

{ TTileRequestBuilderPascalScript }

constructor TTileRequestBuilderPascalScript.Create(
  AConfig: ITileRequestBuilderConfig;
  AConfigData: IConfigDataProvider;
  ACoordConverterFactory: ICoordConverterFactory;
  ALangManager: ILanguageManager
);
begin
  inherited Create(AConfig);
  PrepareCoordConverter(ACoordConverterFactory, AConfigData);
  PreparePascalScript(AConfigData);

  FLangManager := ALangManager;
  FLangListener := TNotifyEventListener.Create(Self.OnLangChange);
  FLangManager.GetChangeNotifier.Add(FLangListener);

  OnLangChange(nil);
end;

destructor TTileRequestBuilderPascalScript.Destroy;
begin
  FLangManager.GetChangeNotifier.Remove(FLangListener);
  FLangManager := nil;
  FLangListener := nil;

  FreeAndNil(FExec);
  FCoordConverter := nil;
  inherited Destroy;
end;

procedure TTileRequestBuilderPascalScript.OnLangChange(Sender: TObject);
begin
  InterlockedIncrement(FLangChangeCount);
end;

function TTileRequestBuilderPascalScript.BuildRequestUrl(
  ATileXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
): string;
begin
  Lock;
  try
    SetVar(nil, AVersionInfo, ATileXY, AZoom);
    try
      FExec.RunScript;
    except on E: Exception do
      raise EPascalScriptRunError.Create(E.Message);
    end;
    Result := FpResultUrl.Data;
  finally
    Unlock;
  end;
end;

procedure TTileRequestBuilderPascalScript.BuildRequest(
  ATileXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo;
  ALastResponseInfo: ILastResponseInfo;
  out AUrl: string;
  out ARequestHeader: string
);
begin
  Lock;
  try
    SetVar(ALastResponseInfo, AVersionInfo, ATileXY, AZoom);
    try
      FExec.RunScript;
    except on E: Exception do
      raise EPascalScriptRunError.Create(E.Message);
    end;
    AUrl := FpResultUrl.Data;
    ARequestHeader := FpRequestHead.Data;
    FScriptBuffer := FpScriptBuffer.Data;
  finally
    Unlock;
  end;
end;

procedure TTileRequestBuilderPascalScript.PrepareCoordConverter(
  ACoordConverterFactory: ICoordConverterFactory;
  AConfig: IConfigDataProvider
);
var
  VParams: IConfigDataProvider;
  VCoordConverter: ICoordConverter;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  VCoordConverter := ACoordConverterFactory.GetCoordConverterByConfig(VParams);
  FCoordConverter := VCoordConverter as ICoordConverterSimple;
end;

procedure TTileRequestBuilderPascalScript.PreparePascalScript(AConfig: IConfigDataProvider);
var
  i: integer;
  VCompilerMsg: string;
  VCompiler: TPSPascalCompiler;
  VData: string;
begin
  FPascalScript := AConfig.ReadString(PascalScriptFileName, '');
  FScriptBuffer := '';

  VCompiler := TPSPascalCompiler.Create;
  try
    VCompiler.OnExternalProc := DllExternalProc;
    VCompiler.OnUses := ScriptOnUses;
    if not VCompiler.Compile(FPascalScript) then
    begin
      VCompilerMsg := '';
      for i := 0 to VCompiler.MsgCount - 1 do
        VCompilerMsg := VCompilerMsg + VCompiler.Msg[i].MessageToString + #13#10;
      raise EPascalScriptCompileError.CreateFmt(SAS_ERR_UrlScriptCompileError, [VCompilerMsg]);
    end;
    VCompiler.GetOutput(VData);
  finally
    VCompiler.Free;
  end;

  FExec := TPSExec.Create;
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
  
  if not FExec.LoadData(VData) then
    raise Exception.Create(SAS_ERR_UrlScriptByteCodeLoad);

  FpResultUrl := PPSVariantAString(FExec.GetVar2('ResultURL'));
  FpGetURLBase := PPSVariantAString(FExec.GetVar2('GetURLBase'));
  FpGetURLBase.Data := '';
  FpRequestHead := PPSVariantAString(FExec.GetVar2('RequestHead'));
  FpRequestHead.Data := '';
  FpResponseHead := PPSVariantAString(FExec.GetVar2('ResponseHead'));
  FpResponseHead.Data := '';
  FpVersion := PPSVariantAString(FExec.GetVar2('Version'));
  FpVersion.Data := '';
  FpLang := PPSVariantAString(FExec.GetVar2('Lang'));
  FpLang.Data := '';
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
    Sender.AddUsedVariable('Version', t);
    Sender.AddUsedVariable('Lang', t);
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

procedure TTileRequestBuilderPascalScript.SetVar(
  ALastResponseInfo: ILastResponseInfo;
  AVersionInfo: IMapVersionInfo;
  AXY: TPoint;
  AZoom: Byte
);
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
  FpResultUrl.Data := '';
  FConfig.LockRead;
  try
    FpGetURLBase.Data := FConfig.URLBase;
    FpRequestHead.Data := FConfig.RequestHeader;
  finally
    FConfig.UnlockRead;
  end;
  if ALastResponseInfo <> nil then begin
    FpResponseHead.Data := ALastResponseInfo.ResponseHead;
  end else begin
    FpResponseHead.Data := '';
  end;
  FpScriptBuffer.Data := FScriptBuffer;
  if AVersionInfo <> nil then begin
    FpVersion.Data := VarToStrDef(AVersionInfo.Version, '');
  end else begin
    FpVersion.Data := '';
  end;
  if InterlockedExchange(FLangChangeCount, 0) > 0 then begin
    FLang := FLangManager.GetCurrentLanguageCode;
  end;
  FpLang.Data := FLang;
end;

end.
