unit u_PosChangeMessage;

interface

uses
  Types,
  u_JclNotify,
  i_ICoordConverter,
  UMapType,
  i_IPosChangeMessage;

type
  TPosChangeMessage = class(TJclBaseNotificationMessage, IPosChangeMessage)
  private
    FMap: TMapType;
    FZoom: Byte;
    FMapPixel: TPoint;
    FViewSize: TPoint;
    FCoordConverter: ICoordConverter;
    function GetMap: TMapType; stdcall;
    function GetZoom: Byte; stdcall;
    function GetMapPixel: TPoint; stdcall;
    function GetViewSize: TPoint; stdcall;
    function GetCoordConverter: ICoordConverter; stdcall;
  public
    constructor Create(
      AViewSize: TPoint;
      ACoordConverter: ICoordConverter;
      AMap: TMapType;
      AZoom: Byte;
      AMapPixel: TPoint
    );
  end;

implementation

{ TPosChangeMessage }

constructor TPosChangeMessage.Create(
  AViewSize: TPoint;
  ACoordConverter: ICoordConverter;
  AMap: TMapType; AZoom: Byte;
  AMapPixel: TPoint);
begin
  FViewSize := AViewSize;
  FCoordConverter := ACoordConverter;
  FMap := AMap;
  FZoom := AZoom;
  FMapPixel := AMapPixel;
end;

function TPosChangeMessage.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TPosChangeMessage.GetMap: TMapType;
begin
  Result := FMap;
end;

function TPosChangeMessage.GetMapPixel: TPoint;
begin
  Result := FMapPixel;
end;

function TPosChangeMessage.GetViewSize: TPoint;
begin
  Result := FViewSize;
end;

function TPosChangeMessage.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
 