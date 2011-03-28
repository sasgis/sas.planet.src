unit u_PosChangeListener;

interface

uses
  i_JclNotify,
  u_JclNotify,
  i_PosChangeMessage;

type
  TPosChangeEvent = procedure(AMessage: IPosChangeMessage) of object; 

type
  TPosChangeListener = class(TJclBaseListener)
  private
    FEvent: TPosChangeEvent;
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  public
    constructor Create(AEvent: TPosChangeEvent);
  end;

implementation

{ TPosChangeListener }

constructor TPosChangeListener.Create(AEvent: TPosChangeEvent);
begin
  FEvent := AEvent;
end;

procedure TPosChangeListener.Notification(msg: IJclNotificationMessage);
var
  VMessage: IPosChangeMessage;
begin
  if Assigned(FEvent) then begin
    if msg.QueryInterface(IPosChangeMessage, VMessage) = S_OK then begin
      FEvent(VMessage);
    end;
  end;
end;

end.
