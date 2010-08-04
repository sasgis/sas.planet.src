unit u_PosChangeMessage;

interface

uses
  Types,
  u_JclNotify,
  UMapType,
  i_IPosChangeMessage;

type
  TPosChangeMessage = class(TJclBaseNotificationMessage, IPosChangeMessage)
  private
    FMap: TMapType;
    FZoom: Byte;
    FMapPixel: TPoint;
    function GetMap: TMapType; stdcall;
    function GetZoom: Byte; stdcall;
    function GetMapPixel: TPoint; stdcall;
  public
    constructor Create(AMap: TMapType; AZoom: Byte; AMapPixel: TPoint);
  end;

implementation

{ TPosChangeMessage }

constructor TPosChangeMessage.Create(AMap: TMapType; AZoom: Byte;
  AMapPixel: TPoint);
begin
  FMap := AMap;
  FZoom := AZoom;
  FMapPixel := AMapPixel;
end;

function TPosChangeMessage.GetMap: TMapType;
begin
  Result := FMap;
end;

function TPosChangeMessage.GetMapPixel: TPoint;
begin
  Result := FMapPixel;
end;

function TPosChangeMessage.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
 