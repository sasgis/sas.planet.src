unit u_GPSState;

interface

uses
  SysUtils,
  i_IJclListenerNotifierLinksList,
  i_IDatum,
  i_IGPSRecorder,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IGPSModule,
  i_IGPSModuleByCOM,
  i_IGPSModuleByCOMPortSettings,
  u_GPSModuleByCOMPortSettings,
  u_GPSLogWriterToPlt;

type
  TGPSpar = class
  private
    FDatum: IDatum;
    FLogPath: string;
    FGPSRecorder: IGPSRecorder;
    FSettings: IGPSModuleByCOMPortConfig;
    FGPSModule: IGPSModule;
    FGPSModuleByCOM: IGPSModuleByCOM;
    GPS_enab: Boolean;

    FLinksList: IJclListenerNotifierLinksList;

    FLogWriter: TPltLogWriter;

    procedure OnGpsConnect(Sender: TObject);
    procedure OnGpsDataReceive(Sender: TObject);
    procedure OnGpsDisconnect(Sender: TObject);
  public
    speed: Double;
    len: Double;
    sspeed: Double;
    allspeed: Double;
    sspeednumentr: integer;
    altitude: Double;
    maxspeed: Double;
    nap: integer;
    azimut: Double;
    Odometr: Double;
    Odometr2: Double;

    //Заисывать GPS трек в файл
    GPS_WriteLog: boolean;
    constructor Create(ALogPath: string);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual;
    procedure Connect;
    procedure Disconnect;

    property GPSRecorder: IGPSRecorder read FGPSRecorder;
    property GPSSettings: IGPSModuleByCOMPortConfig read FSettings;
    property GPSModule: IGPSModule read FGPSModule;
  end;

implementation

uses
  t_GeoTypes,
  i_GPS,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  u_Datum,
  u_GPSModuleByZylGPS,
  u_GPSRecorderStuped;

procedure TGPSpar.Connect;
begin
  GPS_enab := True;
  FGPSModuleByCOM.Connect(FSettings.GetStatic);
end;

constructor TGPSpar.Create(ALogPath: string);
begin
  FDatum := TDatum.Create(3395, 6378137, 6356752);
  FLogPath := ALogPath;
  FGPSRecorder := TGPSRecorderStuped.Create;
  FSettings := TGPSModuleByCOMPortSettings.Create(FLogPath);
  FLogWriter := TPltLogWriter.Create(FLogPath);
  FGPSModuleByCOM := TGPSModuleByZylGPS.Create;
  FGPSModule := FGPSModuleByCOM;
  FLinksList := TJclListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsConnect),
    FGPSModule.ConnectedNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsDataReceive),
    FGPSModule.DataReciveNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsDisconnect),
    FGPSModule.DisconnectedNotifier
  );
  GPS_enab := False;
  GPS_WriteLog := true;
  Odometr := 0;
  Odometr2 := 0;
end;

destructor TGPSpar.Destroy;
begin
  FLinksList := nil;
  FGPSRecorder := nil;
  FSettings := nil;
  FGPSModule := nil;
  FreeAndNil(FLogWriter);
  inherited;
end;

procedure TGPSpar.Disconnect;
begin
  GPS_enab := False;
  FGPSModuleByCOM.Disconnect;
end;

procedure TGPSpar.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  VConfigProvider := AConfigProvider.GetSubItem('GPS');
  if VConfigProvider <> nil then begin
    GPS_enab := VConfigProvider.ReadBool('enbl',  GPS_enab);

    GPS_WriteLog := VConfigProvider.ReadBool('log', GPS_WriteLog);
    Odometr := VConfigProvider.ReadFloat('Odometr', Odometr);
    Odometr2 := VConfigProvider.ReadFloat('Odometr2', Odometr2);
  end;
  FSettings.ReadConfig(VConfigProvider);
end;

procedure TGPSpar.OnGpsConnect;
begin
  allspeed:=0;
  sspeed:=0;
  speed:=0;
  maxspeed:=0;
  sspeednumentr:=0;

  if GPS_WriteLog then begin
    try
      FLogWriter.StartWrite;
    except
      GPS_WriteLog := false;
    end;
  end;
end;

procedure TGPSpar.OnGpsDataReceive;
var
  VPosition: IGPSPosition;
  VPointCurr: TDoublePoint;
  VPointPrev: TDoublePoint;
  VTrackPoint: TGPSTrackPoint;
  VDistToPrev: Double;
begin
  VPosition := FGPSModule.Position;
  if (VPosition.IsFix=0) then exit;
  VPointCurr := VPosition.Position;
  if (VPointCurr.x<>0)or(VPointCurr.y<>0) then begin
    VPointPrev := GPSRecorder.GetLastPoint;
    VTrackPoint.Point := VPointCurr;
    VTrackPoint.Speed := VPosition.Speed_KMH;
    GPSRecorder.AddPoint(VTrackPoint);
    speed:=VTrackPoint.Speed;
    if maxspeed < speed then begin
      maxspeed:=speed;
    end;
    inc(sspeednumentr);
    allspeed:=allspeed+speed;
    sspeed:=allspeed/sspeednumentr;
    altitude:=VPosition.Altitude;
    if (VPointPrev.x<>0)or(VPointPrev.y<>0) then begin
      VDistToPrev := FDatum.CalcDist(VPointPrev, VPointCurr);
      len:=len+VDistToPrev;
      Odometr:=Odometr+VDistToPrev;
      Odometr2:=Odometr2+VDistToPrev;
      azimut:=VPosition.Heading;
    end;
  end;
  if FLogWriter.Started then begin
    FLogWriter.AddPoint(VPosition);
  end;
end;

procedure TGPSpar.OnGpsDisconnect;
begin
  try
    FLogWriter.CloseLog;
  except
  end;
end;

procedure TGPSpar.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('GPS');
  VConfigProvider.WriteBool('enbl', GPS_enab);

  VConfigProvider.WriteBool('log',GPS_WriteLog);

  VConfigProvider.WriteFloat('Odometr', Odometr);
  VConfigProvider.WriteFloat('Odometr2', Odometr2);
  FSettings.WriteConfig(VConfigProvider);
end;

procedure TGPSpar.SendTerminateToThreads;
begin
  FLinksList.DeactivateLinks;
  FGPSModuleByCOM.Disconnect;
end;

procedure TGPSpar.StartThreads;
begin
  FLinksList.ActivateLinks;
  if GPS_enab then begin
    FGPSModuleByCOM.Connect(FSettings.GetStatic);
  end;
end;

end.

