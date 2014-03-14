{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_ListenerByEvent;

interface

uses
  i_NotifierTime,
  i_Listener,
  i_ListenerTime,
  i_SimpleFlag,
  u_BaseInterfacedObject;

type
  TNotifyListenerNoMmgEvent = procedure of object;
  TNotifyListenerEvent = procedure(const AMsg: IInterface) of object;

  TNotifyEventListener = class(TBaseInterfacedObject, IListener, IListenerDisconnectable)
  private
    FDisconnectFlag: ISimpleFlag;
    FEvent: TNotifyListenerEvent;
  private
    procedure Notification(const AMsg: IInterface);
  private
    procedure Disconnect;
  public
    constructor Create(AEvent: TNotifyListenerEvent);
  end;

  TNotifyNoMmgEventListener = class(TBaseInterfacedObject, IListener)
  private
    FEvent: TNotifyListenerNoMmgEvent;
  private
    procedure Notification(const AMsg: IInterface);
  public
    constructor Create(AEvent: TNotifyListenerNoMmgEvent);
  end;

  TNotifyEventListenerSync = class(TBaseInterfacedObject, IListener)
  private
    FTimerNoifier: INotifierTime;
    FTimerListener: IListenerTime;

    FNeedNotifyFlag: ISimpleFlag;
    FEvent: TNotifyListenerNoMmgEvent;
    procedure OnTimer;
  private
    procedure Notification(const AMsg: IInterface);
  public
    constructor Create(
      const ATimerNoifier: INotifierTime;
      const ACheckTime: Cardinal;
      AEvent: TNotifyListenerNoMmgEvent
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerTime,
  u_SimpleFlagWithInterlock;

{ TSimpleEventListener }

constructor TNotifyEventListener.Create(AEvent: TNotifyListenerEvent);
begin
  inherited Create;
  FEvent := AEvent;
  FDisconnectFlag := TSimpleFlagWithInterlock.Create;
  Assert(Assigned(FEvent));
end;

procedure TNotifyEventListener.Disconnect;
begin
  FDisconnectFlag.SetFlag;
end;

procedure TNotifyEventListener.Notification(const AMsg: IInterface);
begin
  inherited;
  if not FDisconnectFlag.CheckFlag then begin
    FEvent(AMsg);
  end;
end;

{ TNotifyEventListenerSync }

constructor TNotifyEventListenerSync.Create(
  const ATimerNoifier: INotifierTime;
  const ACheckTime: Cardinal;
  AEvent: TNotifyListenerNoMmgEvent
);
begin
  inherited Create;
  FTimerNoifier := ATimerNoifier;
  FEvent := AEvent;
  Assert(Assigned(FEvent));
  Assert(Assigned(FTimerNoifier));
  FNeedNotifyFlag := TSimpleFlagWithInterlock.Create;
  FTimerListener := TListenerTimeCheck.Create(Self.OnTimer, ACheckTime);
  FTimerNoifier.Add(FTimerListener);
end;

procedure TNotifyEventListenerSync.OnTimer;
begin
  if FNeedNotifyFlag.CheckFlagAndReset then begin
    FEvent;
  end;
end;

destructor TNotifyEventListenerSync.Destroy;
begin
  if Assigned(FTimerNoifier) and Assigned(FTimerListener) then begin
    FTimerNoifier.Remove(FTimerListener);
    FTimerNoifier := nil;
    FTimerListener := nil;
  end;
  inherited;
end;

procedure TNotifyEventListenerSync.Notification(const AMsg: IInterface);
begin
  inherited;
  FNeedNotifyFlag.SetFlag;
end;

{ TNotifyNoMmgEventListener }

constructor TNotifyNoMmgEventListener.Create(AEvent: TNotifyListenerNoMmgEvent);
begin
  inherited Create;
  FEvent := AEvent;
  Assert(Assigned(FEvent));
end;

procedure TNotifyNoMmgEventListener.Notification(const AMsg: IInterface);
begin
  FEvent;
end;

end.
