unit u_NotifyEventListener;

interface

uses
  Classes,
  i_JclNotify,
  u_JclNotify;

type
  TNotifyEventListener = class(TJclBaseListener)
  private
    FEvent: TNotifyEvent;
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  public
    constructor Create(AEvent: TNotifyEvent);
  end;

implementation

{ TSimpleEventListener }

constructor TNotifyEventListener.Create(AEvent: TNotifyEvent);
begin
  FEvent := AEvent;
end;

procedure TNotifyEventListener.Notification(msg: IJclNotificationMessage);
begin
  inherited;
  if Assigned(FEvent) then begin
    FEvent(nil);
  end;
end;

end.
