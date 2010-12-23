unit u_NotifyEventPosChangeListener;

interface

uses
  i_JclNotify,
  u_JclNotify,
  i_ILocalCoordConverter,
  i_IPosChangeMessage;

type
  TPosChangeNotifyEvent = procedure(ANewConverter: ILocalCoordConverter) of object;

  TPosChangeNotifyEventListener = class(TJclBaseListener)
  private
    FEvent: TPosChangeNotifyEvent;
  protected
    procedure DoEvent(AMessage: IPosChangeMessage); virtual;
    procedure Notification(msg: IJclNotificationMessage); override;
  public
    constructor Create(AEvent: TPosChangeNotifyEvent);
  end;

implementation

{ TPosChangeNotifyEventListener }

constructor TPosChangeNotifyEventListener.Create(AEvent: TPosChangeNotifyEvent);
begin
  FEvent := AEvent;
end;

procedure TPosChangeNotifyEventListener.DoEvent(AMessage: IPosChangeMessage);
begin
  if Assigned(FEvent) then begin
    FEvent(AMessage.GetVisualCoordConverter);
  end;
end;

procedure TPosChangeNotifyEventListener.Notification(
  msg: IJclNotificationMessage);
begin
  inherited;
  DoEvent(IPosChangeMessage(msg));
end;

end.
