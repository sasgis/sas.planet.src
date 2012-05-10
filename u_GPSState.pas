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

unit u_GPSState;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  i_GPSRecorder,
  i_GPSConfig,
  i_InternalPerformanceCounter,
  i_GPSModuleByCOMFactory,
  i_GPSModuleByCOM;

type
  TModuleState = (msDisconnected, msConnecting, msConnected, msDisconnecting);

  TInternalState = (isDisconnected, isConnecting, isConnected, isDisconnecting, isTimeOut, isConnectError);

type
  TGPSpar = class
  private
    FConfig: IGPSConfig;
    FGPSRecorder: IGPSRecorder;
    FGPSModuleFactory: IGPSModuleByCOMFactory;
    FGPSModuleByCOM: IGPSModuleByCOM;

    FCS: IReadWriteSync;
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
    FLastDataReceiveTick: Cardinal;
    FDataReceiveCounter: IInternalPerformanceCounter;

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
  public
    constructor Create(
      const AGPSModuleFactory: IGPSModuleByCOMFactory;
      const AConfig: IGPSConfig;
      const AGPSRecorder: IGPSRecorder;
      const ATimerNoifier: IJclNotifier;
      const APerfCounterList: IInternalPerformanceCounterList
    );
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
  i_GPS,
  u_JclNotify,
  u_JclListenerNotifierLinksList,
  u_Synchronizer,
  u_NotifyEventListener;

constructor TGPSpar.Create(
  const AGPSModuleFactory: IGPSModuleByCOMFactory;
  const AConfig: IGPSConfig;
  const AGPSRecorder: IGPSRecorder;
  const ATimerNoifier: IJclNotifier;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FGPSModuleFactory := AGPSModuleFactory;

  FDataReceiveCounter := APerfCounterList.CreateAndAddNewCounter('GPS_Process');
  FLinksList := TJclListenerNotifierLinksList.Create;
  FCS := MakeSyncObj(Self, TRUE);
  FModuleState := msDisconnected;
  FWasError := False;
  FWasTimeOut := False;
  FDataRecived := False;
  FInternalState := isDisconnected;
  FLastDataReceiveTick := 0;

  FConnectingNotifier := TJclBaseNotifier.Create;
  FConnectedNotifier := TJclBaseNotifier.Create;
  FDisconnectingNotifier := TJclBaseNotifier.Create;
  FDisconnectedNotifier := TJclBaseNotifier.Create;
  FTimeOutNotifier := TJclBaseNotifier.Create;
  FConnectErrorNotifier := TJclBaseNotifier.Create;
  FDataReciveNotifier := TJclBaseNotifier.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );

  CreateModuleAndLinks;
end;

destructor TGPSpar.Destroy;
begin
  FLinksList := nil;
  FGPSRecorder := nil;
  FGPSModuleByCOM := nil;
  FCS := nil;
  inherited;
end;

procedure TGPSpar.CreateModuleAndLinks;
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

procedure TGPSpar.OnConfigChange;
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

procedure TGPSpar.OnGpsConnected;
begin
  FConfig.GPSEnabled := True;
  FGPSRecorder.LockWrite;
  try
    FGPSRecorder.ResetMaxSpeed;
    FGPSRecorder.ResetAvgSpeed;
  finally
    FGPSRecorder.UnlockWrite;
  end;
  FCS.BeginWrite;
  try
    FModuleState := msConnected;
    FLastDataReceiveTick := GetTickCount;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGPSpar.OnGpsConnectError;
begin
  FCS.BeginWrite;
  try
    FWasError := True;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGPSpar.OnGpsConnecting;
begin
  FCS.BeginWrite;
  try
    FModuleState := msConnecting;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGPSpar.OnGpsDataReceive;
var
  VPosition: IGPSPosition;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FDataReceiveCounter.StartOperation;
  try
    VPosition := FGPSModuleByCOM.Position;
    FGPSRecorder.AddPoint(VPosition);
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

procedure TGPSpar.OnGpsDisconnected;
begin
  FConfig.GPSEnabled := False;
  FGPSRecorder.AddPoint(FGPSModuleByCOM.Position);
  FCS.BeginWrite;
  try
    FModuleState := msDisconnected;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGPSpar.OnGpsDisconnecting;
begin
  FCS.BeginWrite;
  try
    FModuleState := msDisconnecting;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGPSpar.OnGpsTimeout;
begin
  FCS.BeginWrite;
  try
    FWasTimeOut := True;
  finally
    FCS.EndWrite;
  end;
end;

procedure TGPSpar.OnTimer;
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
  OnConfigChange;
end;

end.
