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
  TModuleState = (msDisconnected, msConnecting, msConnected, msDisconnecting);

  TInternalState = (isDisconnected, isConnecting, isConnected, isDisconnecting, isTimeOut, isConnectError);

type
  TGPSpar = class
  private
    FConfig: IGPSConfig;
    FLogWriter: ITrackWriter;
    FGPSRecorder: IGPSRecorder;
    FGPSModuleByCOM: IGPSModuleByCOM;

    FCS: TCriticalSection;
    FLinksList: IJclListenerNotifierLinksList;
    FDataReciveNotifier: IJclNotifier;
    FConnectingNotifier: IJclNotifier;
    FConnectedNotifier: IJclNotifier;
    FDisconnectingNotifier: IJclNotifier;
    FDisconnectedNotifier: IJclNotifier;
    FTimeOutNotifier: IJclNotifier;
    FConnectErrorNotifier: IJclNotifier;

    FModuleState: TModuleState;
    FWasError: Boolean;
    FWasTimeOut: Boolean;
    FDataRecived: Boolean;
    FInternalState: TInternalState;

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

    property ConnectingNotifier: IJclNotifier read FConnectingNotifier;
    property ConnectedNotifier: IJclNotifier read FConnectedNotifier;
    property DisconnectingNotifier: IJclNotifier read FDisconnectingNotifier;
    property DisconnectedNotifier: IJclNotifier read FDisconnectedNotifier;
    property TimeOutNotifier: IJclNotifier read FTimeOutNotifier;
    property ConnectErrorNotifier: IJclNotifier read FConnectErrorNotifier;
    property DataReciveNotifier: IJclNotifier read FDataReciveNotifier;
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
  FLinksList := TJclListenerNotifierLinksList.Create;
  FCS := TCriticalSection.Create;
  FModuleState := msDisconnected;
  FWasError := False;
  FWasTimeOut := False;
  FDataRecived := False;
  FInternalState := isDisconnected;

  FConnectingNotifier := TJclBaseNotifier.Create;
  FConnectedNotifier := TJclBaseNotifier.Create;
  FDisconnectingNotifier := TJclBaseNotifier.Create;
  FDisconnectedNotifier := TJclBaseNotifier.Create;
  FTimeOutNotifier := TJclBaseNotifier.Create;
  FConnectErrorNotifier := TJclBaseNotifier.Create;
  FDataReciveNotifier := TJclBaseNotifier.Create;

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsConnecting),
    FGPSModuleByCOM.ConnectingNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsConnected),
    FGPSModuleByCOM.ConnectedNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsDataReceive),
    FGPSModuleByCOM.DataReciveNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsDisconnecting),
    FGPSModuleByCOM.DisconnectingNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsDisconnected),
    FGPSModuleByCOM.DisconnectedNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsTimeout),
    FGPSModuleByCOM.TimeOutNotifier
  );
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnGpsConnectError),
    FGPSModuleByCOM.ConnectErrorNotifier
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
  FGPSModuleByCOM := nil;
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
    if FGPSModuleByCOM <> nil then begin
      FGPSModuleByCOM.Disconnect;
    end;
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
    FModuleState := msConnected;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsConnectError(Sender: TObject);
begin
  FCS.Acquire;
  try
    FWasError := True;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsConnecting(Sender: TObject);
begin
  FCS.Acquire;
  try
    FModuleState := msConnecting;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsDataReceive;
begin
  FGPSRecorder.AddPoint(FGPSModuleByCOM.Position);
  FLogWriter.AddPoint(FGPSModuleByCOM.Position);
  FCS.Acquire;
  try
    FDataRecived := True;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsDisconnected;
begin
  FConfig.GPSEnabled := False;
  FGPSRecorder.AddPoint(FGPSModuleByCOM.Position);
  try
    FLogWriter.CloseLog;
  except
  end;
  FCS.Acquire;
  try
    FModuleState := msDisconnected;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsDisconnecting(Sender: TObject);
begin
  FCS.Acquire;
  try
    FModuleState := msDisconnecting;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnGpsTimeout(Sender: TObject);
begin
  FCS.Acquire;
  try
    FWasTimeOut := True;
  finally
    FCS.Release;
  end;
end;

procedure TGPSpar.OnTimer(Sender: TObject);
var
  VNeedNotify: Boolean;
  VInternalStateNew: TInternalState;
  VInternalStatePrev: TInternalState;
  VDataRecived: Boolean;
begin
  VInternalStatePrev := isDisconnected;
  VInternalStateNew := isDisconnected;
  repeat
    VDataRecived := False;
    FCS.Acquire;
    try
      if FWasError then begin
        if FInternalState = isDisconnected then begin
          VInternalStateNew := isConnecting;
        end else begin
          VInternalStateNew := isConnectError;
          FWasError := False;
        end;
      end else if FWasTimeOut then begin
        VInternalStateNew := isTimeOut;
        FWasTimeOut := False;
      end else begin
        case FInternalState of
          isDisconnected: begin
            case FModuleState of
              msDisconnected: VInternalStateNew := isDisconnected;
              msConnecting: VInternalStateNew := isConnecting;
              msConnected: VInternalStateNew := isConnecting;
              msDisconnecting: VInternalStateNew := isConnecting;
            end;
          end;
          isConnecting:  begin
            case FModuleState of
              msDisconnected: VInternalStateNew := isDisconnecting;
              msConnecting: VInternalStateNew := isConnecting;
              msConnected: VInternalStateNew := isConnected;
              msDisconnecting: VInternalStateNew := isDisconnecting;
            end;
          end;
          isConnected:  begin
            case FModuleState of
              msDisconnected: VInternalStateNew := isDisconnecting;
              msConnecting: VInternalStateNew := isDisconnecting;
              msConnected: VInternalStateNew := isConnected;
              msDisconnecting: VInternalStateNew := isDisconnecting;
            end;
          end;
          isDisconnecting:  begin
            case FModuleState of
              msDisconnected: VInternalStateNew := isDisconnected;
              msConnecting: VInternalStateNew := isConnecting;
              msConnected: VInternalStateNew := isConnecting;
              msDisconnecting: VInternalStateNew := isDisconnecting;
            end;
          end;
          isTimeOut:  begin
            VInternalStateNew := VInternalStatePrev;
          end;
          isConnectError:  begin
            VInternalStateNew := VInternalStatePrev;
          end;
        end;
      end;
      if VInternalStateNew in [isConnectError, isTimeOut] then begin
        if not(FInternalState in [isConnectError, isTimeOut]) then begin
          VInternalStatePrev := FInternalState;
        end;
      end;
      if FInternalState <> VInternalStateNew then begin
        FInternalState := VInternalStateNew;
        VNeedNotify := True;
      end else begin
        VNeedNotify := False;
      end;
      if (FInternalState = isConnected) and FDataRecived then begin
        VDataRecived := True;
        FDataRecived := False;
      end else if FInternalState = isDisconnecting then begin
        VDataRecived := True;
        FDataRecived := False;
      end;
    finally
      FCS.Release;
    end;
    if VNeedNotify then begin
      case VInternalStateNew of
        isDisconnected: FDisconnectedNotifier.Notify(nil);
        isConnecting: FConnectingNotifier.Notify(nil);
        isConnected: FConnectedNotifier.Notify(nil);
        isDisconnecting: FDisconnectingNotifier.Notify(nil);
        isTimeOut: FTimeOutNotifier.Notify(nil);
        isConnectError: FConnectErrorNotifier.Notify(nil);
      end;
    end;
    if VDataRecived then begin
      FDataReciveNotifier.Notify(nil);
    end;
  until not VNeedNotify;
end;

procedure TGPSpar.SendTerminateToThreads;
begin
  FLinksList.DeactivateLinks;
  if FGPSModuleByCOM <> nil then begin
    FGPSModuleByCOM.Disconnect;
  end;
end;

procedure TGPSpar.StartThreads;
begin
  FLinksList.ActivateLinks;
  OnConfigChange(nil);
end;

end.

