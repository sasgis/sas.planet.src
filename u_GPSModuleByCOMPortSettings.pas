unit u_GPSModuleByCOMPortSettings;

interface

uses
  i_JclNotify,
  i_IGPSModuleByCOMPortSettings;

type
  TGPSModuleByCOMPortSettings = class(TInterfacedObject, IGPSModuleByCOMPortSettings)
  private
    FPort: Integer;
    FBaudRate: Integer;
    FConnectionTimeout: Integer;
    FDelay: Integer;
    FNMEALog: Boolean;
    FLogPath: WideString;
    FConfigChangeNotifier: IJclNotifier;
  protected
    function GetPort: Integer; safecall;
    function GetBaudRate: Integer; safecall;
    function GetConnectionTimeout: Integer; safecall;
    function GetDelay: Integer; safecall;
    function GetNMEALog: Boolean; safecall;
    function GetLogPath: WideString; safecall;

    function GetConfigChangeNotifier: IJclNotifier; safecall;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPort(AValue: Integer);
    procedure SetBaudRate(AValue: Integer);
    procedure SetConnectionTimeout(AValue: Integer);
    procedure SetDelay(AValue: Integer);
    procedure SetNMEALog(AValue: Boolean);
    procedure SetLogPath(AValue: WideString);
  end;

implementation

uses
  u_JclNotify;

{ TGPSModuleByCOMPortSettings }

constructor TGPSModuleByCOMPortSettings.Create;
begin
  FConfigChangeNotifier := TJclBaseNotifier.Create;
  FPort := 0;
  FBaudRate := 4800;
  FConnectionTimeout := 300;
  FDelay := 1000;
  FNMEALog := False;
end;

destructor TGPSModuleByCOMPortSettings.Destroy;
begin
  FConfigChangeNotifier := nil;
  FLogPath := '';
  inherited;
end;

function TGPSModuleByCOMPortSettings.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

function TGPSModuleByCOMPortSettings.GetConfigChangeNotifier: IJclNotifier;
begin
  Result := FConfigChangeNotifier;
end;

function TGPSModuleByCOMPortSettings.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TGPSModuleByCOMPortSettings.GetDelay: Integer;
begin
  Result := FDelay;
end;

function TGPSModuleByCOMPortSettings.GetLogPath: WideString;
begin
  Result := FLogPath;
end;

function TGPSModuleByCOMPortSettings.GetNMEALog: Boolean;
begin
  Result := FNMEALog;
end;

function TGPSModuleByCOMPortSettings.GetPort: Integer;
begin
  Result := FPort;
end;

procedure TGPSModuleByCOMPortSettings.SetBaudRate(AValue: Integer);
begin
  if FBaudRate <> AValue then begin
    FBaudRate := AValue;
    FConfigChangeNotifier.Notify(nil);
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetConnectionTimeout(AValue: Integer);
begin
  if FConnectionTimeout <> AValue then begin
    FConnectionTimeout := AValue;
    FConfigChangeNotifier.Notify(nil);
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetDelay(AValue: Integer);
begin
  if FDelay <> AValue then begin
    FDelay := AValue;
    FConfigChangeNotifier.Notify(nil);
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetLogPath(AValue: WideString);
begin
  if FLogPath <> AValue then begin
    FLogPath := AValue;
    FConfigChangeNotifier.Notify(nil);
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetNMEALog(AValue: Boolean);
begin
  if FNMEALog <> AValue then begin
    FNMEALog := AValue;
    FConfigChangeNotifier.Notify(nil);
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetPort(AValue: Integer);
begin
  if FPort <> AValue then begin
    FPort := AValue;
    FConfigChangeNotifier.Notify(nil);
  end;
end;

end.
