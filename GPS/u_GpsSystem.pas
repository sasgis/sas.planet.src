{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GpsSystem;

interface

uses
  Windows,
  SysUtils,
  i_Listener,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_ListenerNotifierLinksList,
  i_GPSRecorder,
  i_GPSConfig,
  i_InternalPerformanceCounter,
  i_GPS,
  i_GPSModule,
  i_GPSModuleByCOMFactory,
  i_GPSModuleByCOM,
  u_BaseInterfacedObject;

type
  TModuleState = (msDisconnected, msConnecting, msConnected, msDisconnecting);

  TInternalState = (isDisconnected, isConnecting, isConnected, isDisconnecting, isTimeOut, isConnectError);

type
  TGpsSystem = class(TBaseInterfacedObject, IGPSModule)
  private
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;
    FConfig: IGPSConfig;
    FGPSRecorder: IGPSRecorderInternal;
    FGpsTrackRecorder: IGpsTrackRecorderInternal;
    FGPSModuleFactory: IGPSModuleByCOMFactory;
    FGPSModuleByCOM: IGPSModuleByCOM;

    FCS: IReadWriteSync;
    FAppStartedListener: IListener;
    FAppClosingListener: IListener;
    FLinksList: IListenerNotifierLinksList;
    FDataReciveNotifier: INotifierInternal;
    FConnectingNotifier: INotifierInternal;
    FConnectedNotifier: INotifierInternal;
    FDisconnectingNotifier: INotifierInternal;
    FDisconnectedNotifier: INotifierInternal;
    FTimeOutNotifier: INotifierInternal;
    FConnectErrorNotifier: INotifierInternal;

    FModuleState: TModuleState;
    FWasError: Boolean;
    FWasTimeOut: Boolean;
    FDataRecived: Boolean;
    FInternalState: TInternalState;
    FLastDataReceiveTick: Cardinal;
    FDataReceiveCounter: IInternalPerformanceCounter;

    procedure StartThreads;
    procedure SendTerminateToThreads;

    procedure OnAppStarted;
    procedure OnAppClosing;

    procedure OnTimer;
    procedure OnGpsConnecting;
    procedure OnGpsConnected;
    procedure OnGpsDataReceive;
    procedure OnGpsDisconnecting;
    procedure OnGpsDisconnected;
    procedure OnGpsTimeout;
    procedure OnGpsConnectError;
    procedure OnConfigChange;

    procedure CreateModuleAndLinks;
  private
    function GetPosition: IGPSPosition; safecall;
    function GetConnectedNotifier: INotifier; safecall;
    function GetConnectErrorNotifier: INotifier; safecall;
    function GetConnectingNotifier: INotifier; safecall;
    function GetDataReciveNotifier: INotifier; safecall;
    function GetDisconnectedNotifier: INotifier; safecall;
    function GetDisconnectingNotifier: INotifier; safecall;
    function GetTimeOutNotifier: INotifier; safecall;
    function GetGPSUnitInfo: String;
    procedure ApplyUTCDateTime;
    procedure ResetDGPS;
    procedure ResetUnitInfo;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AGPSModuleFactory: IGPSModuleByCOMFactory;
      const AConfig: IGPSConfig;
      const AGPSRecorder: IGPSRecorderInternal;
      const AGpsTrackRecorder: IGpsTrackRecorderInternal;
      const ATimerNoifier: INotifierTime;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Notifier,
  u_ListenerNotifierLinksList,
  u_Synchronizer,
  u_ListenerTime,
  u_ListenerByEvent;

procedure TGpsSystem.AfterConstruction;
begin
  inherited;
  FAppStartedNotifier.Add(FAppStartedListener);
  if FAppStartedNotifier.IsExecuted then begin
    StartThreads;
  end;
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    SendTerminateToThreads;
  end;
end;

procedure TGpsSystem.ApplyUTCDateTime;
begin
  inherited;
  FGPSModuleByCOM.ApplyUTCDateTime;
end;

constructor TGpsSystem.Create(
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AGPSModuleFactory: IGPSModuleByCOMFactory;
  const AConfig: IGPSConfig;
  const AGPSRecorder: IGPSRecorderInternal;
  const AGpsTrackRecorder: IGpsTrackRecorderInternal;
  const ATimerNoifier: INotifierTime;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FGpsTrackRecorder := AGpsTrackRecorder;
  FGPSModuleFactory := AGPSModuleFactory;

  FDataReceiveCounter := APerfCounterList.CreateAndAddNewCounter('GPS_Process');
  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FLinksList := TListenerNotifierLinksList.Create;
  FCS := MakeSyncRW_Var(Self, False);
  FModuleState := msDisconnected;
  FWasError := False;
  FWasTimeOut := False;
  FDataRecived := False;
  FInternalState := isDisconnected;
  FLastDataReceiveTick := 0;

  FConnectingNotifier := TNotifierBase.Create;
  FConnectedNotifier := TNotifierBase.Create;
  FDisconnectingNotifier := TNotifierBase.Create;
  FDisconnectedNotifier := TNotifierBase.Create;
  FTimeOutNotifier := TNotifierBase.Create;
  FConnectErrorNotifier := TNotifierBase.Create;
  FDataReciveNotifier := TNotifierBase.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  FLinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 500),
    ATimerNoifier
  );

  CreateModuleAndLinks;
end;

destructor TGpsSystem.Destroy;
begin
  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
  end;
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
  end;
  FLinksList := nil;
  FGPSRecorder := nil;
  FGpsTrackRecorder := nil;
  FGPSModuleByCOM := nil;
  FCS := nil;
  inherited;
end;

function TGpsSystem.GetConnectedNotifier: INotifier;
begin
  Result := FConnectedNotifier;
end;

function TGpsSystem.GetConnectErrorNotifier: INotifier;
begin
  Result := FConnectErrorNotifier;
end;

function TGpsSystem.GetConnectingNotifier: INotifier;
begin
  Result := FConnectingNotifier;
end;

function TGpsSystem.GetDataReciveNotifier: INotifier;
begin
  Result := FDataReciveNotifier;
end;

function TGpsSystem.GetDisconnectedNotifier: INotifier;
begin
  Result := FDisconnectedNotifier;
end;

function TGpsSystem.GetDisconnectingNotifier: INotifier;
begin
  Result := FDisconnectingNotifier;
end;

function TGpsSystem.GetGPSUnitInfo: String;
begin
  Result := FGPSModuleByCOM.GPSUnitInfo;
end;

function TGpsSystem.GetPosition: IGPSPosition;
begin
  Result := FGPSRecorder.CurrentPosition;
end;

function TGpsSystem.GetTimeOutNotifier: INotifier;
begin
  Result := FTimeOutNotifier;
end;

procedure TGpsSystem.CreateModuleAndLinks;
begin
  if FGPSModuleFactory <> nil then begin
    FGPSModuleByCOM := FGPSModuleFactory.CreateGPSModule;

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGpsConnecting),
      FGPSModuleByCOM.ConnectingNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGpsConnected),
      FGPSModuleByCOM.ConnectedNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGpsDataReceive),
      FGPSModuleByCOM.DataReciveNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGpsDisconnecting),
      FGPSModuleByCOM.DisconnectingNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGpsDisconnected),
      FGPSModuleByCOM.DisconnectedNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGpsTimeout),
      FGPSModuleByCOM.TimeOutNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGpsConnectError),
      FGPSModuleByCOM.ConnectErrorNotifier
    );
  end;
end;

procedure TGpsSystem.OnAppClosing;
begin
  SendTerminateToThreads;
end;

procedure TGpsSystem.OnAppStarted;
begin
  StartThreads;
end;

procedure TGpsSystem.OnConfigChange;
begin
  if FGPSModuleByCOM <> nil then begin
    if FConfig.GPSEnabled then begin
      if FGPSModuleByCOM.IsReadyToConnect then begin
        FGPSModuleByCOM.Connect(FConfig.ModuleConfig.GetStatic, FConfig);
      end;
    end else begin
      FGPSModuleByCOM.Disconnect;
    end;
  end;
end;

procedure TGpsSystem.OnGpsConnected;
begin
  FConfig.GPSEnabled := True;
  FCS.BeginWrite;
  try
    FModuleState := msConnected;
    FLastDataReceiveTick := GetTickCount;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGpsSystem.OnGpsConnectError;
begin
  FCS.BeginWrite;
  try
    FWasError := True;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGpsSystem.OnGpsConnecting;
begin
  FCS.BeginWrite;
  try
    FModuleState := msConnecting;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGpsSystem.OnGpsDataReceive;
var
  VPosition: IGPSPosition;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FDataReceiveCounter.StartOperation;
  try
    VPosition := FGPSModuleByCOM.Position;
    FGPSRecorder.AddPoint(VPosition);
    FGpsTrackRecorder.AddPoint(VPosition);
    FCS.BeginWrite;
    try
      FDataRecived := True;
      FLastDataReceiveTick := GetTickCount;
    finally
      FCS.EndWrite;
    end;
  finally
    FDataReceiveCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TGpsSystem.OnGpsDisconnected;
var
  VPosition: IGPSPosition;
begin
  FConfig.GPSEnabled := False;
  VPosition := FGPSModuleByCOM.Position;
  FGPSRecorder.AddPoint(VPosition);
  FGpsTrackRecorder.AddPoint(VPosition);
  FCS.BeginWrite;
  try
    FModuleState := msDisconnected;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGpsSystem.OnGpsDisconnecting;
begin
  FCS.BeginWrite;
  try
    FModuleState := msDisconnecting;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGpsSystem.OnGpsTimeout;
begin
  FCS.BeginWrite;
  try
    FWasTimeOut := True;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGpsSystem.OnTimer;
var
  VNeedNotify: Boolean;
  VInternalStateNew: TInternalState;
  VInternalStatePrev: TInternalState;
  VDataRecived: Boolean;
  VNotDataTimeout: Integer;
  VCurrTick: Cardinal;
  VTickDelta: Integer;
begin
  VNotDataTimeout := FConfig.NoDataTimeOut;
  VInternalStatePrev := isDisconnected;
  VInternalStateNew := isDisconnected;
  repeat
    VDataRecived := False;
    VCurrTick := GetTickCount;
    FCS.BeginWrite;
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
              msDisconnected: begin
                VInternalStateNew := isDisconnected;
              end;
              msConnecting: begin
                VInternalStateNew := isConnecting;
              end;
              msConnected: begin
                VInternalStateNew := isConnecting;
              end;
              msDisconnecting: begin
                VInternalStateNew := isConnecting;
              end;
            end;
          end;
          isConnecting: begin
            case FModuleState of
              msDisconnected: begin
                VInternalStateNew := isDisconnecting;
              end;
              msConnecting: begin
                VInternalStateNew := isConnecting;
              end;
              msConnected: begin
                VInternalStateNew := isConnected;
              end;
              msDisconnecting: begin
                VInternalStateNew := isDisconnecting;
              end;
            end;
          end;
          isConnected: begin
            case FModuleState of
              msDisconnected: begin
                VInternalStateNew := isDisconnecting;
              end;
              msConnecting: begin
                VInternalStateNew := isDisconnecting;
              end;
              msConnected: begin
                VInternalStateNew := isConnected;
              end;
              msDisconnecting: begin
                VInternalStateNew := isDisconnecting;
              end;
            end;
          end;
          isDisconnecting: begin
            case FModuleState of
              msDisconnected: begin
                VInternalStateNew := isDisconnected;
              end;
              msConnecting: begin
                VInternalStateNew := isConnecting;
              end;
              msConnected: begin
                VInternalStateNew := isConnecting;
              end;
              msDisconnecting: begin
                VInternalStateNew := isDisconnecting;
              end;
            end;
          end;
          isTimeOut: begin
            VInternalStateNew := VInternalStatePrev;
          end;
          isConnectError: begin
            VInternalStateNew := VInternalStatePrev;
          end;
        end;
      end;
      if VInternalStateNew in [isConnectError, isTimeOut] then begin
        if not (FInternalState in [isConnectError, isTimeOut]) then begin
          VInternalStatePrev := FInternalState;
        end;
      end;
      if FInternalState <> VInternalStateNew then begin
        FInternalState := VInternalStateNew;
        VNeedNotify := True;
      end else begin
        VNeedNotify := False;
      end;
      if (FInternalState = isConnected) then begin
        if FDataRecived then begin
          VDataRecived := True;
          FDataRecived := False;
        end else begin
          if FLastDataReceiveTick > 0 then begin
            if FLastDataReceiveTick > VCurrTick then begin
              FLastDataReceiveTick := VCurrTick;
            end else begin
              VTickDelta := VCurrTick - FLastDataReceiveTick;
              if VTickDelta > VNotDataTimeout then begin
                FGPSRecorder.AddEmptyPoint;
                FGpsTrackRecorder.AddEmptyPoint;
                VDataRecived := True;
              end;
            end;
          end;
        end;
      end else if FInternalState = isDisconnecting then begin
        VDataRecived := True;
        FDataRecived := False;
      end;
    finally
      FCS.EndWrite;
    end;
    if VNeedNotify then begin
      case VInternalStateNew of
        isDisconnected: begin
          FDisconnectedNotifier.Notify(nil);
        end;
        isConnecting: begin
          FConnectingNotifier.Notify(nil);
        end;
        isConnected: begin
          FConnectedNotifier.Notify(nil);
        end;
        isDisconnecting: begin
          FDisconnectingNotifier.Notify(nil);
        end;
        isTimeOut: begin
          FTimeOutNotifier.Notify(nil);
        end;
        isConnectError: begin
          FConnectErrorNotifier.Notify(nil);
        end;
      end;
    end;
    if VDataRecived then begin
      FDataReciveNotifier.Notify(nil);
    end;
  until not VNeedNotify;
end;

procedure TGpsSystem.ResetDGPS;
begin
  inherited;
  FGPSModuleByCOM.ResetDGPS;
end;

procedure TGpsSystem.ResetUnitInfo;
begin
  inherited;
  FGPSModuleByCOM.ResetUnitInfo;
end;

procedure TGpsSystem.SendTerminateToThreads;
begin
  FLinksList.DeactivateLinks;
  if FGPSModuleByCOM <> nil then begin
    FGPSModuleByCOM.Disconnect;
  end;
end;

procedure TGpsSystem.StartThreads;
begin
  FLinksList.ActivateLinks;
  OnConfigChange;
end;

end.
