unit u_TileDownloaderPhpBase;

interface

uses
  Windows,
  Classes,
  SysUtils,
  WinInet,
  i_ConfigDataProvider,
  i_ProxySettings,
  i_TileDownlodSession,
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

  TTileDownloaderPhpBase = class
    private
      FPHP: TpsvPHP;
      FScript: string;
      FBody: TMemoryStream;
      FTile: TPoint;
      FZoom: Byte;
      FOldTileSize: Integer;
      FUrl: string;
      FMIMEType: string;
      FLanguage: string;
      FCookieStr: string;
      procedure OnReadResult (Sender : TObject; Stream : TStream);
    public
      constructor Create(const AFileName: string; const AScript: string; AUseCustomParams: Boolean);
      destructor Destroy; override;
      function Exec (out ExecError: string): integer;
      procedure SetMainParams;
      procedure SetCustomParams(AParams: TCustomParams);
      procedure SetProxyConfig(AInetConfig: IInetConfig);

      property Tile: TPoint write FTile;
      property Zoom: Byte write FZoom;
      property FileBuf: TMemoryStream read FBody write FBody;
      property OldTileSize: Integer write FOldTileSize;
      property Url: string read FUrl write FUrl;
      property MIMEType: string read FMIMEType;
      property Language: string write FLanguage;
  end;

implementation

{ TTileDownloaderPhpBase }

constructor TTileDownloaderPhpBase.Create(const AFileName: string; const AScript: string; AUseCustomParams: Boolean);
begin
  inherited Create;

  FCookieStr := '';

  FBody := nil;
  FScript := AScript;

  FPHP := TpsvPHP.Create(nil);
  FPHP.FileName := AFileName;
  FPHP.OnReadResult := Self.OnReadResult;
  FPHP.MaxExecutionTime := 60; // секунд

  FPHP.Variables.Add.Name := 'ProxyUseIESettings';
  FPHP.Variables.Add.Name := 'UseProxy';
  FPHP.Variables.Add.Name := 'ProxyUseLogin';
  FPHP.Variables.Add.Name := 'ProxyLogin';
  FPHP.Variables.Add.Name := 'ProxyPassword';
  FPHP.Variables.Add.Name := 'ProxyHost';

  FPHP.Variables.Add.Name := 'ErrStr';
  FPHP.Variables.Add.Name := 'DownloadResult';
  FPHP.Variables.Add.Name := 'Url';
  FPHP.Variables.Add.Name := 'CookieStr';
  FPHP.Variables.Add.Name := 'MIMEType';
  FPHP.Variables.Add.Name := 'SasLangCodeStr';
  FPHP.Variables.Add.Name := 'X';
  FPHP.Variables.Add.Name := 'Y';
  FPHP.Variables.Add.Name := 'Z';
  FPHP.Variables.Add.Name := 'OldTileSize';

  if AUseCustomParams then
  begin
    FPHP.Variables.Add.Name := 'LLon';
    FPHP.Variables.Add.Name := 'TLat';
    FPHP.Variables.Add.Name := 'RLon';
    FPHP.Variables.Add.Name := 'BLat';
    FPHP.Variables.Add.Name := 'LMetr';
    FPHP.Variables.Add.Name := 'TMetr';
    FPHP.Variables.Add.Name := 'RMetr';
    FPHP.Variables.Add.Name := 'BMetr';
  end;
end;

destructor TTileDownloaderPhpBase.Destroy;
begin
  try
    FreeAndNil(FPHP);
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderPhpBase.OnReadResult (Sender: TObject; Stream: TStream);
begin
  if Assigned(Stream) and Assigned(FBody) then
    if Stream.Size > 0 then
    begin
      FBody.Clear;
      FBody.LoadFromStream(Stream);
      FBody.Position := 0;
    end;
end;

function TTileDownloaderPhpBase.Exec (out ExecError: string): integer;
begin
    ExecError := '';
  try
    if FScript <> '' then
      FPHP.RunCode(FScript)
    else
      FPHP.Execute;

    ExecError := FPHP.Variables.ByName('ErrStr').AsString;
    Result := FPHP.Variables.ByName('DownloadResult').AsInteger;
    FUrl := FPHP.Variables.ByName('Url').AsString;
    FCookieStr := FPHP.Variables.ByName('CookieStr').AsString;
    FMIMEType := FPHP.Variables.ByName('MIMEType').AsString;
  except
    Result := Integer(dtrUnknownError);
  end;
end;

procedure TTileDownloaderPhpBase.SetProxyConfig(AInetConfig: IInetConfig);

  procedure GetProxyData(var ProxyEnabled: boolean; var ProxyServer: string);
  var
    ProxyInfo: PInternetProxyInfo;
    Len: LongWord;
    i, j: integer;
  begin
    Len := 4096;
    ProxyEnabled := false;
    GetMem(ProxyInfo, Len);
    try
      if InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len)
      then
        if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
        begin
          ProxyServer := ProxyInfo^.lpszProxy;
          if ProxyServer <> '' then
          begin
            ProxyEnabled:= True;
            i := Pos('http=', ProxyServer);
            if (i > 0) then
            begin
              Delete(ProxyServer, 1, i + 5);
              j := Pos(';', ProxyServer);
              if (j > 0) then
                ProxyServer := Copy(ProxyServer, 1, j - 1);
            end;
          end;
        end
    finally
      FreeMem(ProxyInfo);
    end;
  end;

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

  if VUseIESettings then
  begin
    VLogin := '';
    VPassword := '';
    GetProxyData(VUseProxy, VHost);
  end;

  FPHP.Variables.ByName('ProxyUseIESettings').AsBoolean := VUseIESettings;
  FPHP.Variables.ByName('UseProxy').AsBoolean := VUseProxy;
  FPHP.Variables.ByName('ProxyUseLogin').AsBoolean := VUseLogin;
  FPHP.Variables.ByName('ProxyLogin').AsString := VLogin;
  FPHP.Variables.ByName('ProxyPassword').AsString := VPassword;
  FPHP.Variables.ByName('ProxyHost').AsString := VHost;
end;

procedure TTileDownloaderPhpBase.SetMainParams;
begin
  FPHP.Variables.ByName('ErrStr').AsString := '';
  FPHP.Variables.ByName('DownloadResult').AsInteger := 9;
  FPHP.Variables.ByName('Url').AsString := FUrl;
  FPHP.Variables.ByName('CookieStr').AsString := FCookieStr;
  FPHP.Variables.ByName('MIMEType').AsString := '';
  FPHP.Variables.ByName('SasLangCodeStr').AsString := FLanguage;
  FPHP.Variables.ByName('X').AsInteger := FTile.X;
  FPHP.Variables.ByName('Y').AsInteger := FTile.Y;
  FPHP.Variables.ByName('Z').AsInteger := FZoom;
  FPHP.Variables.ByName('OldTileSize').AsInteger := FOldTileSize;
end;

procedure TTileDownloaderPhpBase.SetCustomParams(AParams: TCustomParams);
begin
  FPHP.Variables.ByName('LLon').AsFloat := AParams.LLon;
  FPHP.Variables.ByName('TLat').AsFloat := AParams.TLat;
  FPHP.Variables.ByName('RLon').AsFloat := AParams.RLon;
  FPHP.Variables.ByName('BLat').AsFloat := AParams.BLat;
  FPHP.Variables.ByName('LMetr').AsFloat := AParams.LMetr;
  FPHP.Variables.ByName('TMetr').AsFloat := AParams.TMetr;
  FPHP.Variables.ByName('RMetr').AsFloat := AParams.RMetr;
  FPHP.Variables.ByName('BMetr').AsFloat := AParams.BMetr;
end;

end.
