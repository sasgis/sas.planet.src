unit u_MapViewGotoOnFMain;

interface

uses
  t_GeoTypes,
  i_IMapViewGoto;

type
  TToPosEvent = procedure (ALonLat: TDoublePoint; AZoom: byte; AShowMarker: boolean) of object;

type
  TMapViewGotoOnFMain = class(TInterfacedObject, IMapViewGoto)
  private
    FToPosEvent: TToPosEvent;
  protected
    procedure GotoPos(ALonLat: TDoublePoint; AZoom: Byte);
  public
    constructor Create(AToPosEvent: TToPosEvent);
  end;

implementation

{ TMapViewGotoOnFMain }

constructor TMapViewGotoOnFMain.Create(AToPosEvent: TToPosEvent);
begin
  inherited Create;
  FToPosEvent := AToPosEvent;
end;

procedure TMapViewGotoOnFMain.GotoPos(ALonLat: TDoublePoint; AZoom: Byte);
begin
  FToPosEvent(ALonLat, AZoom, True);
end;

end.
