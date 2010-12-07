unit u_NotifyWithGUIDEvent;

interface

uses
  i_JclNotify,
  u_JclNotify;

type
  TNotifyWithGUIDEvent = procedure(AGUID: TGUID) of object;

type
  INotificationMessageWithGUID = interface(IJclNotificationMessage)
    ['{1C88C4F8-5990-42DA-A841-CB9597C97AB6}']
    function GetGUID: TGUID;
  end;

type
  TNotificationMessageWithGUID = class(TJclBaseNotificationMessage, INotificationMessageWithGUID)
  private
    FGUID: TGUID;
  protected
    function GetGUID: TGUID;
  public
    constructor Create(AGUID: TGUID);
  end;

type
  TNotifyWithGUIDEventListener =  class(TJclBaseListener)
  private
    FEvent: TNotifyWithGUIDEvent;
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  public
    constructor Create(AEvent: TNotifyWithGUIDEvent);
  end;


implementation

{ TNotificationMessageWithGUID }

constructor TNotificationMessageWithGUID.Create(AGUID: TGUID);
begin
  FGUID := AGUID;
end;

function TNotificationMessageWithGUID.GetGUID: TGUID;
begin
  Result := FGUID;
end;

{ TNotifyWithGUIDEventListener }

constructor TNotifyWithGUIDEventListener.Create(AEvent: TNotifyWithGUIDEvent);
begin
  FEvent := AEvent;
end;

procedure TNotifyWithGUIDEventListener.Notification(
  msg: IJclNotificationMessage);
begin
  FEvent(INotificationMessageWithGUID(msg).GetGUID);
end;

end.
