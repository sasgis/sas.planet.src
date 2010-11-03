unit u_MapViewGotoOnFMain;

interface

uses
  t_GeoTypes,
  i_IMapViewGoto;

type
  TMapViewGotoOnFMain = class(TInterfacedObject, IMapViewGoto)
  private
    procedure GotoPos(ALonLat: TDoublePoint; AZoom: Byte);
  end;

implementation

uses
  Unit1;

{ TMapViewGotoOnFMain }

procedure TMapViewGotoOnFMain.GotoPos(ALonLat: TDoublePoint; AZoom: Byte);
var
  VPoint: TDoublePoint;
begin
  VPoint.X := ALonLat.X;
  VPoint.Y := ALonLat.Y;
  Fmain.topos(VPoint, AZoom, True);
end;

end.
