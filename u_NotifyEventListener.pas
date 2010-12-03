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
    FSender: TObject;
  protected
    procedure DoEvent; virtual;
  public
    constructor Create(AEvent: TNotifyEvent; ASender: TObject = nil);
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

constructor TNotifyEventListenerBase.Create(AEvent: TNotifyEvent; ASender: TObject);
begin
  FSender := ASender;
  FEvent := AEvent;
end;

procedure TNotifyEventListenerBase.DoEvent;
begin
  if Assigned(FEvent) then begin
    FEvent(FSender);
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
