unit u_GPSModuleByZylGPS;

interface

uses
  SysUtils,
  ZylGPSReceiver,
  i_IGPSModuleByCOMPortSettings,
  u_GPSModuleAbstract;

type
  TConnectState = (csDisconnected, csConnecting, csConnected, csDisconnecting);

  TGPSModuleByZylGPS = class(TGPSModuleAbstract)
  protected
    FGPSReceiver: TZylGPSReceiver;
    FConnectState: TConnectState;
    FSettings: IGPSModuleByCOMPortConfigSatic;
    FFormatSettings: TFormatSettings;
    procedure GPSReceiver1SatellitesReceive(Sender: TObject);
    procedure GPSReceiverReceive(Sender: TObject; Buffer: string);
    procedure GPSReceiverDisconnect(Sender: TObject; const Port: TCommPort);
    procedure GPSReceiverConnect(Sender: TObject; const Port: TCommPort);
    procedure GPSReceiverTimeout(Sender: TObject);
  protected
    function GetSatActive(pcode:integer;NMEA:string):boolean;
    procedure _UpdateReceiverSettings;
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    function GetIsConnected: Boolean; override;
  public
    constructor Create(ASettings: IGPSModuleByCOMPortConfigSatic);
    destructor Destroy; override;
  end;
implementation

uses
  StrUtils,
  ZylCustomGPSReceiver,
  t_GeoTypes;

{ TGPSModuleByZylGPS }

const
  CMaxSatCount = 32;

constructor TGPSModuleByZylGPS.Create(ASettings: IGPSModuleByCOMPortConfigSatic);
begin
  inherited Create;
  FSettings := ASettings;

  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.DateSeparator := '.';
  FFormatSettings.ShortDateFormat := 'yyyy.MM.dd';
  FFormatSettings.TimeSeparator := ':';
  FFormatSettings.LongTimeFormat := 'HH-mm-ss';
  FFormatSettings.ShortTimeFormat := 'HH-mm-ss';
  FFormatSettings.ListSeparator := ';';
  FFormatSettings.TwoDigitYearCenturyWindow := 50;

  FGPSReceiver := TZylGPSReceiver.Create(nil);
  FGPSReceiver.Name := 'GPSReceiver';
  FGPSReceiver.CustomBaudRate := 0;
  FGPSReceiver.Commands := [GPAAM, GPBWC, GPGGA, GPGLL, GPMSS, GPRMB, GPRMC, GPGSA, GPGSV, GPVTG, GPZDA, GPWPL, GPRTE, GPXTE, AllNMEA];
  FGPSReceiver.IdleInterval := 20;
  FGPSReceiver.OnConnect := GPSReceiverConnect;
  FGPSReceiver.OnDisconnect := GPSReceiverDisconnect;
  FGPSReceiver.OnTimeout := GPSReceiverTimeout;
  FGPSReceiver.OnReceive := GPSReceiverReceive;
  FGPSReceiver.OnSatellitesReceive := GPSReceiver1SatellitesReceive;
  FGPSReceiver.NeedSynchronization := False;
  _UpdateReceiverSettings;
  FConnectState := csDisconnected;
end;

destructor TGPSModuleByZylGPS.Destroy;
begin
  Disconnect;
  FreeAndNil(FGPSReceiver);
  inherited;
end;

procedure TGPSModuleByZylGPS._UpdateReceiverSettings;
var
  VLogFile: string;
begin
  VLogFile := FSettings.LogPath + DateTimeToStr(Now, FFormatSettings) +'.nmea';
  FGPSReceiver.LogFile := VLogFile;
  FGPSReceiver.NMEALog := FSettings.NMEALog;
  FGPSReceiver.Delay := FSettings.Delay;
  FGPSReceiver.ConnectionTimeout := FSettings.ConnectionTimeout;
  FGPSReceiver.Port :=  FGPSReceiver.StringToCommPort('COM' + IntToStr(FSettings.Port));
  FGPSReceiver.BaudRate := FGPSReceiver.IntToBaudRate(FSettings.BaudRate);
end;

procedure TGPSModuleByZylGPS.Connect;
var
  VState: TConnectState;
begin
  inherited;
  Lock;
  try
    VState := FConnectState;
    if FConnectState = csDisconnected then begin
      FConnectState := csConnecting;
    end;
  finally
    UnLock;
  end;
  if VState = csDisconnected then begin
    _UpdateReceiverSettings;
    try
      FGPSReceiver.Open;
    except
      GetConnectErrorNotifier.Notify(nil);
      Lock;
      try
        if (FConnectState = csConnecting) or (FConnectState = csConnected) then begin
          FConnectState := csDisconnecting;
        end;
      finally
        UnLock;
      end;
      FGPSReceiver.Close;
      GPSReceiverDisconnect(FGPSReceiver, spNone);
    end;
  end;
end;

procedure TGPSModuleByZylGPS.GPSReceiverConnect(Sender: TObject;
  const Port: TCommPort);
var
  VState: TConnectState;
begin
  Lock;
  try
    VState := FConnectState;
    FConnectState := csConnected;
  finally
    UnLock;
  end;
  if VState = csConnecting then begin
    GetConnectNotifier.Notify(nil);
  end;
end;

procedure TGPSModuleByZylGPS.Disconnect;
var
  VState: TConnectState;
begin
  inherited;
  Lock;
  try
    VState := FConnectState;
    if FConnectState = csConnected then begin
      FConnectState := csDisconnecting;
    end;
  finally
    UnLock;
  end;
  if VState = csConnected then begin
    FGPSReceiver.Close;
  end;
end;

procedure TGPSModuleByZylGPS.GPSReceiverDisconnect(Sender: TObject;
  const Port: TCommPort);
var
  VState: TConnectState;
begin
  Lock;
  try
    VState := FConnectState;
    FConnectState := csDisconnected;
    _UpdateToEmptyPosition;
  finally
    UnLock;
  end;
  if VState = csDisconnecting then begin
    GetDisconnectNotifier.Notify(nil);
  end;
end;

procedure TGPSModuleByZylGPS.GPSReceiverTimeout(Sender: TObject);
begin
  GetTimeOutNotifier.Notify(nil);
  Disconnect;
  GPSReceiverDisconnect(FGPSReceiver, spNone);
end;

function TGPSModuleByZylGPS.GetIsConnected: Boolean;
begin
  Lock;
  try
    Result := FConnectState = csConnected;
  finally
    UnLock;
  end;
end;

function TGPSModuleByZylGPS.GetSatActive(pcode: integer;
  NMEA: string): boolean;
var str:string;
    i,j,count:integer;
begin
  i:=Pos('GPGSA',NMEA);
  if i<>0 then begin
    i:=PosEx(',',NMEA,i+6);
    i:=PosEx(',',NMEA,i+1);
    count:=0;
    str:='0';
    while (i<>0)and(str<>'')and(str<>',')and(count<12)and(strtoint(str)<>pcode) do begin
      j:=PosEx(',',NMEA,i+1);
      str:=copy(NMEA,i+1,j-i-1);
      inc(count);
      i:=j;
    end;
    if (i<>0)and(str<>'')and(str<>',')and(count<12) then begin
      result:=pcode=strtoint(str);
    end else begin
      result:=false;
    end;
  end else begin
    result:=false;
  end;
end;

procedure TGPSModuleByZylGPS.GPSReceiver1SatellitesReceive(Sender: TObject);
var
  VFixed: array [0..CMaxSatCount - 1] of Boolean;
  VSatCount: Integer;
  VFixCount: Integer;
  i: Integer;
  VSatellites: TSatellites;
  VSatellite: TSatellite;
begin
  Lock;
  try
    VSatellites := FGPSReceiver.GetSatellites;
    VSatCount := VSatellites.Count;
    VFixCount := 0;
    for i := 0 to VSatCount - 1 do begin
      VFixed[i] := GetSatActive(VSatellites[i].PseudoRandomCode, FGPSReceiver.GetRawData);
      if VFixed[i] then begin
        Inc(VFixCount);
      end;
    end;
    _UpdateSatellitesCount(VFixCount, VSatCount);
    for i := 0 to VSatCount - 1 do begin
      VSatellite := VSatellites[i];
      _UpdateSattelite(
        i,
        VSatellite.PseudoRandomCode,
        VSatellite.Elevation,
        VSatellite.Azimuth,
        VSatellite.SignalToNoiseRatio,
        VFixed[i]
      );
    end;
  finally
    UnLock;
  end;
end;

procedure TGPSModuleByZylGPS.GPSReceiverReceive(Sender: TObject;
  Buffer: string);
var
  VPoint: TDoublePoint;
begin
  Lock;
  try
    VPoint.X := FGPSReceiver.GetLongitudeAsDecimalDegrees;
    VPoint.Y := FGPSReceiver.GetLatitudeAsDecimalDegrees;
    _UpdatePosition(
      VPoint,
      FGPSReceiver.GetAltitude,
      FGPSReceiver.GetSpeed_KMH,
      FGPSReceiver.GetHeading,
      FGPSReceiver.GetUTCDateTime,
      FGPSReceiver.GetLocalDateTime,
      FGPSReceiver.IsFix,
      FGPSReceiver.GetHDOP,
      FGPSReceiver.GetVDOP,
      FGPSReceiver.GetPDOP
    );
  finally
    UnLock;
  end;
  GetDataReciveNotifier.Notify(nil);
end;

end.
