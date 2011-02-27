unit u_GPSState;

interface

uses
  SysUtils,
  i_IJclListenerNotifierLinksList,
  i_IGPSRecorder,
  i_IGPSConfig,
  i_IGPSModule,
  i_IGPSModuleByCOM,
  u_GPSLogWriterToPlt;

type
  TGPSpar = class
  private
    FConfig: IGPSConfig;
    FLogPath: string;
    FGPSRecorder: IGPSRecorder;
    FGPSModule: IGPSModule;
    FGPSModuleByCOM: IGPSModuleByCOM;
    FLinksList: IJclListenerNotifierLinksList;

    FLogWriter: TPltLogWriter;

    procedure OnGpsConnect(Sender: TObject);
    procedure OnGpsDataReceive(Sender: TObject);
    procedure OnGpsDisconnect(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  public
    constructor Create(ALogPath: string; AConfig: IGPSConfig; AGPSRecorder: IGPSRecorder);
    destructor Destroy; override;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;

    property GPSModule: IGPSModule read FGPSModule;
  end;

implementation

uses
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  u_GPSModuleByZylGPS;

constructor TGPSpar.Create(ALogPath: string; AConfig: IGPSConfig; AGPSRecorder: IGPSRecorder);
begin
  FConfig := AConfig;
  FLogPath := ALogPath;
  FGPSRecorder := AGPSRecorder;
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

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

destructor TGPSpar.Destroy;
begin
  FLinksList := nil;
  FGPSRecorder := nil;
  FGPSModule := nil;
  FreeAndNil(FLogWriter);
  inherited;
end;

procedure TGPSpar.OnConfigChange(Sender: TObject);
begin
  if FConfig.GPSEnabled then begin
    if FGPSModuleByCOM.IsReadyToConnect then begin
      FGPSModuleByCOM.Connect(FConfig.ModuleConfig.GetStatic);
    end;
  end else begin
    FGPSModuleByCOM.Disconnect;
  end;
end;

procedure TGPSpar.OnGpsConnect;
begin
  if FConfig.WriteLog then begin
    try
      FLogWriter.StartWrite;
    except
      FConfig.WriteLog := false;
    end;
  end;
  FConfig.GPSEnabled := True;
  FGPSRecorder.LockWrite;
  try
    FGPSRecorder.ResetMaxSpeed;
    FGPSRecorder.ResetAvgSpeed;
  finally
    FGPSRecorder.UnlockWrite;
  end;
end;

procedure TGPSpar.OnGpsDataReceive;
begin
  FGPSRecorder.AddPoint(FGPSModule.Position);
  if FLogWriter.Started then begin
    FLogWriter.AddPoint(FGPSModule.Position);
  end;
end;

procedure TGPSpar.OnGpsDisconnect;
begin
  FConfig.GPSEnabled := False;
  FGPSRecorder.AddPoint(FGPSModule.Position);
  try
    FLogWriter.CloseLog;
  except
  end;
end;

procedure TGPSpar.SendTerminateToThreads;
begin
  FLinksList.DeactivateLinks;
  FGPSModuleByCOM.Disconnect;
end;

procedure TGPSpar.StartThreads;
begin
  FLinksList.ActivateLinks;
  OnConfigChange(nil);
end;

end.

