unit u_HybrChangeMessage;

interface

uses
  JclNotify,
  i_IHybrChangeMessage,
  UMapType;

type
  THybrChangeMessage = class(TJclBaseNotificationMessage, IHybrChangeMessage)
  private
    FAction: THybrChangeAction;
    FMap: TMapType;
    function GetAction: THybrChangeAction; stdcall;
    function GetMap: TMapType; stdcall;
  public
    constructor Create(AMap: TMapType; AAction: THybrChangeAction);
  end;

implementation

{ THybrChangeMessage }

constructor THybrChangeMessage.Create(AMap: TMapType;
  AAction: THybrChangeAction);
begin
  FAction := AAction;
  FMap := AMap;
end;

function THybrChangeMessage.GetAction: THybrChangeAction;
begin
  Result := FAction;
end;

function THybrChangeMessage.GetMap: TMapType;
begin
  Result := FMap;
end;

end.

