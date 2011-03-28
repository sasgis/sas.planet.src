unit u_PosChangeMessage;

interface

uses
  Types,
  u_JclNotify,
  i_LocalCoordConverter,
  i_IPosChangeMessage;

type
  TPosChangeMessage = class(TJclBaseNotificationMessage, IPosChangeMessage)
  private
    FVisualCoordConverter: ILocalCoordConverter;
    function GetVisualCoordConverter: ILocalCoordConverter; stdcall;
  public
    constructor Create(
      AVisuzlCoordConverter: ILocalCoordConverter
    );
  end;

implementation

{ TPosChangeMessage }

constructor TPosChangeMessage.Create(
  AVisuzlCoordConverter: ILocalCoordConverter
);
begin
  FVisualCoordConverter := AVisuzlCoordConverter;
end;

function TPosChangeMessage.GetVisualCoordConverter: ILocalCoordConverter;
begin
  Result := FVisualCoordConverter;
end;

end.
 