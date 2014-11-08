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

unit u_WindowLayerBasic;

interface

uses
  i_Listener,
  i_NotifierOperation,
  i_SimpleFlag,
  i_ListenerNotifierLinksList,
  u_BaseInterfacedObject;

type
  TWindowLayerAbstract = class(TBaseInterfacedObject)
  private
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;

    FViewUpdateLockCounter: ICounter;
    FAppStartedListener: IListener;
    FAppClosingListener: IListener;
    FLinksList: IListenerNotifierLinksList;
    procedure OnAppStarted;
    procedure OnAppClosing;
  protected
    procedure ViewUpdateLock;
    procedure ViewUpdateUnlock;
    procedure DoViewUpdate; virtual;

    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;

    property LinksList: IListenerNotifierLinksList read FLinksList;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock,
  u_ListenerNotifierLinksList;

{ TWindowLayerAbstract }

constructor TWindowLayerAbstract.Create(
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation
);
begin
  inherited Create;
  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;

  FViewUpdateLockCounter := TCounterInterlock.Create;
  FLinksList := TListenerNotifierLinksList.Create;
  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
end;

destructor TWindowLayerAbstract.Destroy;
begin
  FLinksList := nil;
  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
  end;
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
  end;
  inherited;
end;

procedure TWindowLayerAbstract.DoViewUpdate;
begin
  // Do nothing
end;

procedure TWindowLayerAbstract.AfterConstruction;
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

procedure TWindowLayerAbstract.OnAppClosing;
begin
  SendTerminateToThreads;
end;

procedure TWindowLayerAbstract.OnAppStarted;
begin
  StartThreads;
end;

procedure TWindowLayerAbstract.SendTerminateToThreads;
begin
  FLinksList.DeactivateLinks;
end;

procedure TWindowLayerAbstract.StartThreads;
begin
  FLinksList.ActivateLinks;
end;

procedure TWindowLayerAbstract.ViewUpdateLock;
begin
  FViewUpdateLockCounter.Inc;
end;

procedure TWindowLayerAbstract.ViewUpdateUnlock;
var
  VLockCount: Integer;
begin
  VLockCount := FViewUpdateLockCounter.Dec;
  Assert(VLockCount >= 0);
  if VLockCount = 0 then begin
    DoViewUpdate;
  end;
end;

end.
