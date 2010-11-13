unit u_NotifyEventListener;

interface

uses
  Classes,
  i_JclNotify,
  u_JclNotify;

type
  TNotifyEventListenerBase = class(TJclBaseListener)
  private
    FEvent: TNotifyEvent;
  protected
    procedure DoEvent; virtual;
  public
    constructor Create(AEvent: TNotifyEvent);
  end;

  TNotifyEventListener = class(TNotifyEventListenerBase)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

  TNotifyEventListenerSync = class(TNotifyEventListenerBase)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

implementation

{ TSimpleEventListenerBase }

constructor TNotifyEventListenerBase.Create(AEvent: TNotifyEvent);
begin
  FEvent := AEvent;
end;

procedure TNotifyEventListenerBase.DoEvent;
begin
  if Assigned(FEvent) then begin
    FEvent(nil);
  end;
end;

{ TSimpleEventListener }

procedure TNotifyEventListener.Notification(msg: IJclNotificationMessage);
begin
  inherited;
  DoEvent;
end;

{ TNotifyEventListenerSync }

procedure TNotifyEventListenerSync.Notification(msg: IJclNotificationMessage);
begin
  inherited;
  TThread.Synchronize(nil, DoEvent);
end;

end.
