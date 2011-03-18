unit u_GPSState;

interface

uses
  SysUtils,
  SyncObjs,
  i_JclNotify,
  i_IJclListenerNotifierLinksList,
  i_IGPSRecorder,
  i_IGPSConfig,
  i_IGPSModule,
  i_ITrackWriter,
  i_IGPSModuleByCOM;

type
  TConnectState = (csDisconnected, csConnecting, csConnected, csDisconnecting, csTimeOut, csConnectError);

type
  TGPSpar = class
  private
    FConfig: IGPSConfig;
    FLogWriter: ITrackWriter;
    FGPSRecorder: IGPSRecorder;
    FGPSModule: IGPSModule;
    FGPSModuleByCOM: IGPSModuleByCOM;

    FCS: TCriticalSection;
    FLinksList: IJclListenerNotifierLinksList;
    FConnectingNotifier: IJclNotifier;
    FConnectedNotifier: IJclNotifier;
    FDisconnectingNotifier: IJclNotifier;
    FDisconnectedNotifier: IJclNotifier;
    FTimeOutNotifier: IJclNotifier;
    FConnectErrorNotifier: IJclNotifier;
    FStateFromModule: TConnectState;
    FStateReported: TConnectState;

    procedure OnTimer(Sender: TObject);
    procedure OnGpsConnecting(Sender: TObject);
    procedure OnGpsConnected(Sender: TObject);
    procedure OnGpsDataReceive(Sender: TObject);
    procedure OnGpsDisconnecting(Sender: TObject);
    procedure OnGpsDisconnected(Sender: TObject);
    procedure OnGpsTimeout(Sender: TObject);
    procedure OnGpsConnectError(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
  public
    constructor Create(ATrackWriter: ITrackWriter; AConfig: IGPSConfig; AGPSRecorder: IGPSRecorder; ATimerNoifier: IJclNotifier);
    destructor Destroy; override;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;

    property GPSModule: IGPSModule read FGPSModule;
    property ConnectingNotifier: IJclNotifier read FConnectingNotifier;
    property ConnectedNotifier: IJclNotifier read FConnectedNotifier;
    property DisconnectingNotifier: IJclNotifier read FDisconnectingNotifier;
    property DisconnectedNotifier: IJclNotifier read FDisconnectedNotifier;
    property TimeOutNotifier: IJclNotifier read FTimeOutNotifier;
    property ConnectErrorNotifier: IJclNotifier read FConnectErrorNotifier;
  end;

implementation

uses
  u_JclNotify,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  u_GPSModuleByZylGPS;

constructor TGPSpar.Create(
  ATrackWriter: ITrackWriter;
  AConfig: IGPSConfig;
  AGPSRecorder: IGPSRecorder;
  ATimerNoifier: IJclNotifier
);
begin
  FConfig := AConfig;
  FLogWriter := ATrackWriter;
  FGPSRecorder := AGPSRecorder;

  FGPSModuleByCOM := TGPSModuleByZylGPS.Create;
  FGPSModule := FGPSModuleByCOM;
  FLinksList := TJclListenerNotifierLinksList.Create;
  FCS := TCriticalSection.Create;
  FStateFromModule := csDisconnected;
  FStateReported := csDisconnected;

  FConnectingNotifier := TJclBaseNotifier.Create;
  FConnectedNotifier := TJclBaseNotifier.Create;
  FDisconnectingNotifier := TJclBaseNotifier.Create;
  FDisconnectedNotifier := TJclBaseNotifier.Create;
  FTimeOutNotifier := TJclBaseNotifier.Create;
  FConnectErrorNotifier := TJclBaseNotifier.Create;

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsConnecting),
    FGPSModule.ConnectingNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsConnected),
    FGPSModule.ConnectedNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsDataReceive),
    FGPSModule.DataReciveNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsDisconnecting),
    FGPSModule.DisconnectingNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsDisconnected),
    FGPSModule.DisconnectedNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsTimeout),
    FGPSModule.TimeOutNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsConnectError),
    FGPSModule.ConnectErrorNotifier
  );

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
end;

destructor TGPSpar.Destroy;
begin
  FreeAndNil(FCS);
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

procedure TGPSpar.OnGpsConnected;
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
  FCS.Acquire;
  try
    FStateFromModule := csConnected;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsConnectError(Sender: TObject);
begin
  FCS.Acquire;
  try
    FStateFromModule := csConnectError;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsConnecting(Sender: TObject);
begin
  FCS.Acquire;
  try
    FStateFromModule := csConnecting;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsDataReceive;
begin
  FGPSRecorder.AddPoint(FGPSModule.Position);
  FLogWriter.AddPoint(FGPSModule.Position);
end;

procedure TGPSpar.OnGpsDisconnected;
begin
  FConfig.GPSEnabled := False;
  FGPSRecorder.AddPoint(FGPSModule.Position);
  try
    FLogWriter.CloseLog;
  except
  end;
  FCS.Acquire;
  try
    FStateFromModule := csDisconnected;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsDisconnecting(Sender: TObject);
begin
  FCS.Acquire;
  try
    FStateFromModule := csDisconnecting;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsTimeout(Sender: TObject);
begin
  FCS.Acquire;
  try
    FStateFromModule := csTimeOut;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnTimer(Sender: TObject);
var
  VNeedNotify: Boolean;
  VStateToReport: TConnectState;
begin
  repeat
    FCS.Acquire;
    try
      case FStateReported of
        csDisconnected: begin
          case FStateFromModule of
            csDisconnected: VStateToReport := csDisconnected;
            csConnecting: VStateToReport := csConnecting;
            csConnected: VStateToReport := csConnecting;
            csDisconnecting: VStateToReport := csConnecting;
            csTimeOut: VStateToReport := csDisconnected;
            csConnectError: VStateToReport := csDisconnected;
          end;
        end;
        csConnecting:  begin
          case FStateFromModule of
            csDisconnected: VStateToReport := csDisconnecting;
            csConnecting: VStateToReport := csConnecting;
            csConnected: VStateToReport := csConnected;
            csDisconnecting: VStateToReport := csDisconnecting;
            csTimeOut: VStateToReport := csTimeOut;
            csConnectError: VStateToReport := csConnectError;
          end;
        end;
        csConnected:  begin
          case FStateFromModule of
            csDisconnected: VStateToReport := csDisconnecting;
            csConnecting: VStateToReport := csDisconnecting;
            csConnected: VStateToReport := csConnected;
            csDisconnecting: VStateToReport := csDisconnecting;
            csTimeOut: VStateToReport := csTimeOut;
            csConnectError: VStateToReport := csConnectError;
          end;
        end;
        csDisconnecting:  begin
          case FStateFromModule of
            csDisconnected: VStateToReport := csDisconnected;
            csConnecting: VStateToReport := csConnecting;
            csConnected: ;
            csDisconnecting: ;
            csTimeOut: ;
            csConnectError: ;
          end;
        end;
        csTimeOut:  begin
          case FStateFromModule of
            csDisconnected: ;
            csConnecting: ;
            csConnected: ;
            csDisconnecting: ;
            csTimeOut: ;
            csConnectError: ;
          end;
        end;
        csConnectError:  begin
          case FStateFromModule of
            csDisconnected: ;
            csConnecting: ;
            csConnected: ;
            csDisconnecting: ;
            csTimeOut: ;
            csConnectError: ;
          end;
        end;
      end;
      if FStateReported <> VStateToReport then begin
        VNeedNotify := True;
      end else begin
        VNeedNotify := False;
      end;
    finally
      FCS.Release;
    end;
    if VNeedNotify then begin
      case VStateToReport of
        csDisconnected: FDisconnectedNotifier.Notify(nil);
        csConnecting: FConnectingNotifier.Notify(nil);
        csConnected: FConnectedNotifier.Notify(nil);
        csDisconnecting: FDisconnectingNotifier.Notify(nil);
        csTimeOut: FTimeOutNotifier.Notify(nil);
        csConnectError: FConnectErrorNotifier.Notify(nil);
      end;
    end;
  until not VNeedNotify;
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

