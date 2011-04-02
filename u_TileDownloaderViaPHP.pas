unit u_TileDownloaderViaPHP;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_ConfigDataProvider,
  i_CoordConverter,
  i_TileDownlodSession,
  t_GeoTypes,
  PHP4Delphi;

type
  TCustomParams = record
    LLon  : Double;
    TLat  : Double;
    RLon  : Double;
    BLat  : Double;
    LMetr : Double;
    TMetr : Double;
    RMetr : Double;
    BMetr : Double;
  end;

  TPhpScript = class
    private
      FCoordConverter: ICoordConverterSimple;
      FEnableCoordConverter: Boolean;
      FURLBase: string;
      FDefUrlBase: string;
      FZMPFileName: string;
      FScriptPath: string;
      FScriptStr: string;
      FCookieStr: string;
      FEngine: TPHPEngine;
      FEngineInitialized: Boolean;
      procedure Initialize(AConfig: IConfigDataProvider);
      function GetCustomParams(AXY: TPoint; AZoom: Byte): TCustomParams;
    public
      constructor Create(AConfig: IConfigDataProvider; const ZmpFileName: string);
      destructor Destroy; override;
      function DownloadTile(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl, AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;
      property Enabled: Boolean read FEngineInitialized;
      property UrlBase: string write FURLBase;
      property DefUrlBase: string write FDefUrlBase;
  end;

const
  ZmpScriptName = 'downloader.php';

implementation

uses
  i_ProxySettings,
  u_GlobalState;

type
  TPHPReader = class
    private
      FTile: TPoint;
      FZoom: Byte;
      FOldTileSize: Integer;
      FCookieStr: string;
      FUrl: string;
      FDefUrl: string;
      FMIMEType: string;
      FBody: TMemoryStream;
      FPHP: TpsvPHP;
      FScript: string;
      procedure OnReadResult (Sender : TObject; Stream : TStream);
    public
      constructor Create(const AFileName: string; const AScript: string = '');
      destructor Destroy; override;
      function Exec (out ExecError: string): integer;
      procedure SetCustomParams(AParams: TCustomParams);
      procedure SetProxyConfig(AInetConfig: IInetConfig);

      property Tile: TPoint write FTile;
      property Zoom: Byte write FZoom;
      property FileBuf: TMemoryStream read FBody write FBody;
      property OldTileSize: Integer write FOldTileSize;
      property Cookie: string read FCookieStr write FCookieStr;
      property Url: string read FUrl write FUrl;
      property DefUrl: string write FDefUrl;
      property MIMEType: string read FMIMEType;
  end;

var
  PhpThreadSafe: TRTLCriticalSection;

{ TPhpScript }

constructor TPhpScript.Create(AConfig: IConfigDataProvider; const ZmpFileName: string);
begin
  inherited Create;
  FEngineInitialized := False;
  FURLBase := '';
  FDefUrlBase := '';
  FScriptPath := '';
  FCookieStr := '';
  FZMPFileName := ZmpFileName;
  FCoordConverter := nil;
  Initialize(AConfig);
end;

destructor TPhpScript.Destroy;
begin
  try
    if FEngineInitialized then
      FEngine.ShutdownEngine;
    FreeAndNil(FEngine);
  finally
    inherited Destroy;
  end;
end;

procedure TPhpScript.Initialize(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
  VCoordConverter: ICoordConverter;
  VMem: TMemoryStream;
begin
  try
    VParams := AConfig.GetSubItem('params.txt').GetSubItem('PHP');
    if VParams <> nil then
      if VParams.ReadBool('Use', False) then
        if FileExists(FZMPFileName + '\' + ZmpScriptName) then
        begin
          FScriptPath := FZMPFileName + '\' + ZmpScriptName;

          if VParams.ReadBool('Preload', False) then
          begin
              VMem := TMemoryStream.Create;
            try
              VMem.LoadFromFile(FScriptPath);
              if VMem.Size > 0 then
              begin
                SetLength(FScriptStr, VMem.Size);
                VMem.Position := 0;
                VMem.ReadBuffer(FScriptStr[1], VMem.Size);
              end;
            finally
              FreeAndNil(VMem);
            end;
          end;

          if FScriptPath <> '' then
          begin
            FEngine := TPHPEngine.Create(nil);
            FEngine.HandleErrors := VParams.ReadBool('Debug', False);
            FEngine.StartupEngine;
            FEngineInitialized := True;
          end;

          FEnableCoordConverter := VParams.ReadBool('CustomParams', False);

        end;

    if FEngineInitialized and FEnableCoordConverter then
    begin
      VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
      VCoordConverter := GState.CoordConverterFactory.GetCoordConverterByConfig(VParams);
      FCoordConverter := VCoordConverter as ICoordConverterSimple;
    end;

  except
    FEngineInitialized := False;
  end;
end;

function TPhpScript.DownloadTile(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl: string; out AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;
var
  VPhpReader: TPHPReader;
  VErrStr: string;
begin
    VPhpReader := TPHPReader.Create(FScriptPath, FScriptStr);
  try
    VPhpReader.Url := FURLBase;
    VPhpReader.DefUrl := FDefUrlBase;
    VPhpReader.Tile := ATile;
    VPhpReader.Zoom := AZoom + 1;

    if ACheckTileSize then
      VPhpReader.OldTileSize := AOldTileSize
    else
      VPhpReader.OldTileSize := -1;

    VPhpReader.FileBuf := fileBuf;

      EnterCriticalSection(PhpThreadSafe);
    try
      VPhpReader.Cookie := FCookieStr;
      if FEnableCoordConverter and Assigned(FCoordConverter) then
        VPhpReader.SetCustomParams(Self.GetCustomParams(ATile, AZoom));
    finally
      LeaveCriticalSection(PhpThreadSafe);
    end;

    VPhpReader.SetProxyConfig(GState.InetConfig);

    VErrStr := '';
    Result := TDownloadTileResult( VPhpReader.Exec(VErrStr) );
    if VErrStr <> '' then
      raise Exception.Create(VErrStr);

    if Result <> dtrUnknownError then
    begin
      AUrl := VPhpReader.FUrl;
      AContentType := VPhpReader.MIMEType;

        EnterCriticalSection(PhpThreadSafe);
      try
        FCookieStr := VPhpReader.Cookie;
      finally
        LeaveCriticalSection(PhpThreadSafe);
      end;
    end;

  finally
    FreeAndNil(VPhpReader);
  end;
end;

function TPhpScript.GetCustomParams(AXY: TPoint; AZoom: Byte): TCustomParams;
var
  XY: TPoint;
  LL: TDoublePoint;
begin
  ZeroMemory(@result, SizeOf(TCustomParams));
  if Assigned(FCoordConverter) then
  begin
    LL := FCoordConverter.Pos2LonLat(AXY, AZoom);
    Result.LLon := LL.X;
    Result.TLat := LL.Y;

    LL := FCoordConverter.LonLat2Metr(LL);
    Result.LMetr := LL.X;
    Result.TMetr := LL.Y;

    XY := AXY;
    Inc(XY.X);
    Inc(XY.Y);

    LL := FCoordConverter.Pos2LonLat(XY, AZoom);
    Result.RLon := LL.X;
    Result.BLat := LL.Y;

    LL := FCoordConverter.LonLat2Metr(LL);
    Result.RMetr := LL.X;
    Result.BMetr := LL.Y;
  end;
end;

{ TPHPReader }

constructor TPHPReader.Create(const AFileName: string; const AScript: string = '');
begin
  inherited Create;
  FScript := AScript;
  FBody := nil;
  FPHP    := TpsvPHP.Create(nil);
  FPHP.FileName := AFileName;
  FPHP.OnReadResult := Self.OnReadResult;
end;

destructor TPHPReader.Destroy;
begin
  try
    FPHP.Free;
  finally
    inherited Destroy;
  end;
end;

procedure TPHPReader.OnReadResult (Sender: TObject; Stream: TStream);
begin
  if Assigned(Stream) and Assigned(FBody) then
  if Stream.Size > 0 then
  begin
    FBody.Clear;
    FBody.LoadFromStream(Stream);
    if FBody.Size > 2 then    
      if Word (Pointer( LongWord(FBody.Memory) + (FBody.Size - 2) )^ ) = $0A0D then
        FBody.SetSize(FBody.Size - 2);
  end;
end;

function TPHPReader.Exec (out ExecError: string): integer;
begin
  ExecError := '';

  FPHP.Variables.Add.Name := 'ErrStr';
  FPHP.Variables.ByName('ErrStr').AsString := '';

  FPHP.Variables.Add.Name := 'DownloadResult';
  FPHP.Variables.ByName('DownloadResult').AsInteger := 9;

  FPHP.Variables.Add.Name := 'Url';
  FPHP.Variables.ByName('Url').AsString := FUrl;

  FPHP.Variables.Add.Name := 'DefUrl';
  FPHP.Variables.ByName('DefUrl').AsString := FDefUrl;

  FPHP.Variables.Add.Name := 'CookieStr';
  FPHP.Variables.ByName('CookieStr').AsString := FCookieStr;

  FPHP.Variables.Add.Name := 'MIMEType';
  FPHP.Variables.ByName('MIMEType').AsString := '';

  FPHP.Variables.Add.Name := 'X';
  FPHP.Variables.ByName('X').AsInteger := FTile.X;

  FPHP.Variables.Add.Name := 'Y';
  FPHP.Variables.ByName('Y').AsInteger := FTile.Y;

  FPHP.Variables.Add.Name := 'Z';
  FPHP.Variables.ByName('Z').AsInteger := FZoom;

  FPHP.Variables.Add.Name := 'OldTileSize';
  FPHP.Variables.ByName('OldTileSize').AsInteger := FOldTileSize;

  try
    if FScript = '' then
      FPHP.Execute
    else
      FPHP.RunCode(FScript);

    ExecError := FPHP.Variables.ByName('ErrStr').AsString;

    Result := FPHP.Variables.ByName('DownloadResult').AsInteger;

    FUrl := FPHP.Variables.ByName('Url').AsString;

    FCookieStr := FPHP.Variables.ByName('CookieStr').AsString;

    FMIMEType := FPHP.Variables.ByName('MIMEType').AsString;

  except
    Result := Integer(dtrUnknownError);
  end;
end;

procedure TPHPReader.SetProxyConfig(AInetConfig: IInetConfig);
var
  VProxyConfig: IProxyConfig;
  VUseIESettings: Boolean;
  VUseProxy: Boolean;
  VUseLogin: Boolean;
  VLogin: string;
  VPassword: string;
  VHost: string;
begin
  VProxyConfig := AInetConfig.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUseIESettings := VProxyConfig.GetUseIESettings;
    VUseProxy := VProxyConfig.GetUseProxy;
    VUseLogin := (not VUseIESettings) and VUseProxy and VProxyConfig.GetUseLogin;
    VLogin := VProxyConfig.GetLogin;
    VPassword := VProxyConfig.GetPassword;
    VHost := VProxyConfig.GetHost;
  finally
    VProxyConfig.UnlockRead;
  end;

  FPHP.Variables.Add.Name := 'ProxyUseIESettings';
  FPHP.Variables.ByName('ProxyUseIESettings').AsBoolean := VUseIESettings;

  FPHP.Variables.Add.Name := 'UseProxy';
  FPHP.Variables.ByName('UseProxy').AsBoolean := VUseProxy;

  FPHP.Variables.Add.Name := 'ProxyUseLogin';
  FPHP.Variables.ByName('ProxyUseLogin').AsBoolean := VUseLogin;

  FPHP.Variables.Add.Name := 'ProxyLogin';
  FPHP.Variables.ByName('ProxyLogin').AsString := VLogin;

  FPHP.Variables.Add.Name := 'ProxyPassword';
  FPHP.Variables.ByName('ProxyPassword').AsString := VPassword;

  FPHP.Variables.Add.Name := 'ProxyHost';
  FPHP.Variables.ByName('ProxyHost').AsString := VHost;
end;

procedure TPHPReader.SetCustomParams(AParams: TCustomParams);
begin
  FPHP.Variables.Add.Name := 'LLon';
  FPHP.Variables.ByName('LLon').AsFloat := AParams.LLon;

  FPHP.Variables.Add.Name := 'TLat';
  FPHP.Variables.ByName('TLat').AsFloat := AParams.TLat;

  FPHP.Variables.Add.Name := 'RLon';
  FPHP.Variables.ByName('RLon').AsFloat := AParams.RLon;

  FPHP.Variables.Add.Name := 'BLat';
  FPHP.Variables.ByName('BLat').AsFloat := AParams.BLat;

  FPHP.Variables.Add.Name := 'LMetr';
  FPHP.Variables.ByName('LMetr').AsFloat := AParams.LMetr;

  FPHP.Variables.Add.Name := 'TMetr';
  FPHP.Variables.ByName('TMetr').AsFloat := AParams.TMetr;

  FPHP.Variables.Add.Name := 'RMetr';
  FPHP.Variables.ByName('RMetr').AsFloat := AParams.RMetr;

  FPHP.Variables.Add.Name := 'BMetr';
  FPHP.Variables.ByName('BMetr').AsFloat := AParams.BMetr;
end;

initialization
InitializeCriticalSection(PhpThreadSafe);

finalization
DeleteCriticalSection(PhpThreadSafe);

end.
