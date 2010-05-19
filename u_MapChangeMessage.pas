unit u_MapChangeMessage;

interface

uses
  JclNotify,
  i_IMapChangeMessage,
  UMapType;

type
  TMapChangeMessage = class(TJclBaseNotificationMessage, IMapChangeMessage)
  private
    FSourceMap: TMapType;
    FNewMap: TMapType;
    function GetSorurceMap: TMapType; stdcall;
    function GetNewMap: TMapType; stdcall;
  public
    constructor Create(ASourceMap, ANewMap: TMapType);
  end;

implementation

{ TMapChangeMessage }

constructor TMapChangeMessage.Create(ASourceMap,
  ANewMap: TMapType);
begin
  FSourceMap := ASourceMap;
  FNewMap := ANewMap;
end;

function TMapChangeMessage.GetNewMap: TMapType;
begin
  Result := FNewMap;
end;

function TMapChangeMessage.GetSorurceMap: TMapType;
begin
  Result := FSourceMap;
end;

end.
 