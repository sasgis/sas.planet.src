unit u_TileDownloadRequestBuilderFactoryPascalScript;

interface

uses
  SysUtils,
  uPSUtils,
  i_ConfigDataProvider,
  i_CoordConverter,
  i_LanguageManager,
  i_DownloadChecker,
  i_TileDownloaderConfig,
  i_TileDownloaderState,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory,
  u_TileDownloaderStateInternal;

type
  EPascalScriptCompileError = class(Exception);

  TTileDownloadRequestBuilderFactoryPascalScript = class(TInterfacedObject, ITileDownloadRequestBuilderFactory)
  private
    FState: ITileDownloaderStateChangeble;
    FStateInternal: ITileDownloaderStateInternal;
    FConfig: ITileDownloadRequestBuilderConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCheker: IDownloadChecker;
    FLangManager: ILanguageManager;
    FCompiledData: TbtString;
    function GetScriptText(AConfig: IConfigDataProvider): string;
    procedure PreparePascalScript(APascalScript: string);
  protected
    function GetState: ITileDownloaderStateChangeble;
    function BuildRequestBuilder: ITileDownloadRequestBuilder;
  public
    constructor Create(
      AZmpData: IConfigDataProvider;
      AConfig: ITileDownloadRequestBuilderConfig;
      ATileDownloaderConfig: ITileDownloaderConfig;
      ACheker: IDownloadChecker;
      ALangManager: ILanguageManager
    );
  end;

implementation

uses
  uPSC_dll,
  uPSCompiler,
  u_ResStrings,
  u_TileDownloadRequestBuilderPascalScript;

const
  PascalScriptFileName = 'GetUrlScript.txt';

{ TTileDownloadRequestBuilderFactoryPascalScript }

constructor TTileDownloadRequestBuilderFactoryPascalScript.Create(
  AZmpData: IConfigDataProvider;
  AConfig: ITileDownloadRequestBuilderConfig;
  ATileDownloaderConfig: ITileDownloaderConfig;
  ACheker: IDownloadChecker;
  ALangManager: ILanguageManager
);
var
  VScriptText: string;
  VState: TTileDownloaderStateInternal;
begin
  VState := TTileDownloaderStateInternal.Create;
  FStateInternal := VState;
  FState := VState;
  FConfig := AConfig;
  FCheker := ACheker;
  FLangManager := ALangManager;
  FTileDownloaderConfig := ATileDownloaderConfig;

  VScriptText := GetScriptText(AZmpData);
  if VScriptText = '' then begin
    FCompiledData := '';
    FStateInternal.Disable('Empty script');
  end else begin
    try
      PreparePascalScript(VScriptText);
    except
      on E:EPascalScriptCompileError do begin
        FStateInternal.Disable(E.Message);
        FCompiledData := '';
      end;
      on E: Exception do begin
        FStateInternal.Disable('Unknown script compile error: ' + E.Message);
        FCompiledData := '';
      end;
    end;
  end;
end;

function TTileDownloadRequestBuilderFactoryPascalScript.GetScriptText(
  AConfig: IConfigDataProvider): string;
begin
  Result := AConfig.ReadString(PascalScriptFileName, '');
end;

function TTileDownloadRequestBuilderFactoryPascalScript.GetState: ITileDownloaderStateChangeble;
begin
  Result := FState;
end;

function TTileDownloadRequestBuilderFactoryPascalScript.BuildRequestBuilder: ITileDownloadRequestBuilder;
begin
  Result := nil;
  if FStateInternal.Enabled then begin
    try
      Result :=
        TTileDownloadRequestBuilderPascalScript.Create(
          FCompiledData,
          FConfig,
          FTileDownloaderConfig,
          FCheker,
          FLangManager
        );
    except
      on E: Exception do begin
        FStateInternal.Disable('Request builder create error: ' + E.Message);
      end;
    end;
  end;
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
    Sender.AddDelphiFunction('function DoHttpRequest(const ARequestUrl, ARequestHeader, APostData: string; out AResponseHeader, AResponseData: string): Cardinal');
    Result := True;
  end else begin
    Result := False;
  end;
end;

procedure TTileDownloadRequestBuilderFactoryPascalScript.PreparePascalScript(
  APascalScript: string);
var
  i: integer;
  VCompilerMsg: string;
  VCompiler: TPSPascalCompiler;
begin
  VCompiler := TPSPascalCompiler.Create;
  try
    VCompiler.OnExternalProc := DllExternalProc;
    VCompiler.OnUses := ScriptOnUses;
    if not VCompiler.Compile(APascalScript) then
    begin
      VCompilerMsg := '';
      for i := 0 to VCompiler.MsgCount - 1 do
        VCompilerMsg := VCompilerMsg + VCompiler.Msg[i].MessageToString + #13#10;
      raise EPascalScriptCompileError.CreateFmt(SAS_ERR_UrlScriptCompileError, [VCompilerMsg]);
    end;
    VCompiler.GetOutput(FCompiledData);
  finally
    VCompiler.Free;
  end;
end;

end.
