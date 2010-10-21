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
    FSettings: IGPSModuleByCOMPortSettings;
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
    constructor Create(ASettings: IGPSModuleByCOMPortSettings);
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

procedure TGPSModuleByZylGPS.Connect;
begin
  inherited;
  Lock;
  try
    if FConnectState = csDisconnected then begin
      _UpdateReceiverSettings;
      FConnectState := csConnecting;
      try
        FGPSReceiver.Open;
      except
        FConnectState := csDisconnected;
        GetConnectErrorNotifier.Notify(nil);
        FGPSReceiver.Close;
      end;
    end;
  finally
    UnLock;
  end;
end;

constructor TGPSModuleByZylGPS.Create(ASettings: IGPSModuleByCOMPortSettings);
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

procedure TGPSModuleByZylGPS.Disconnect;
begin
  inherited;
  Lock;
  try
    if FConnectState <> csDisconnected then begin
      FConnectState := csDisconnecting;
      FGPSReceiver.Close;
    end;
  finally
    UnLock;
  end;
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

procedure TGPSModuleByZylGPS.GPSReceiverConnect(Sender: TObject;
  const Port: TCommPort);
begin
  Lock;
  try
    FConnectState := csConnected;
  finally
    UnLock;
  end;
  GetConnectNotifier.Notify(nil);
end;

procedure TGPSModuleByZylGPS.GPSReceiverDisconnect(Sender: TObject;
  const Port: TCommPort);
begin
  Lock;
  try
    FConnectState := csDisconnected;
  finally
    UnLock;
  end;
  GetDisconnectNotifier.Notify(nil);
end;

procedure TGPSModuleByZylGPS.GPSReceiverReceive(Sender: TObject;
  Buffer: string);
var
  VPoint: TExtendedPoint;
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

procedure TGPSModuleByZylGPS.GPSReceiverTimeout(Sender: TObject);
begin
  Lock;
  try
    FConnectState := csDisconnecting;
    FGPSReceiver.Close;
  finally
    UnLock;
  end;
  GetTimeOutNotifier.Notify(nil);
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

end.
