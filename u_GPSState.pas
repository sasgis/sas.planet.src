unit u_GPSState;

interface

uses
  SysUtils,
  i_IJclListenerNotifierLinksList,
  i_IGPSRecorder,
  i_IGPSConfig,
  i_IGPSModule,
  i_ITrackWriter,
  i_IGPSModuleByCOM;

type
  TGPSpar = class
  private
    FConfig: IGPSConfig;
    FLogWriter: ITrackWriter;
    FGPSRecorder: IGPSRecorder;
    FGPSModule: IGPSModule;
    FGPSModuleByCOM: IGPSModuleByCOM;
    FLinksList: IJclListenerNotifierLinksList;


    procedure OnGpsConnect(Sender: TObject);
    procedure OnGpsDataReceive(Sender: TObject);
    procedure OnGpsDisconnect(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  public
    constructor Create(ATrackWriter: ITrackWriter; AConfig: IGPSConfig; AGPSRecorder: IGPSRecorder);
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

constructor TGPSpar.Create(ATrackWriter: ITrackWriter; AConfig: IGPSConfig; AGPSRecorder: IGPSRecorder);
begin
  FConfig := AConfig;
  FLogWriter := ATrackWriter;
  FGPSRecorder := AGPSRecorder;
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
  FLogWriter := nil;
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
  FLogWriter.AddPoint(FGPSModule.Position);
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

