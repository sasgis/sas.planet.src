unit u_MapMainLayer;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TMapMainLayer = class(TMapLayerBasic)
  protected
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    destructor Destroy; override;
  end;

implementation

{ TMapMainLayer }

constructor TMapMainLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin

end;

destructor TMapMainLayer.Destroy;
begin

  inherited;
end;

procedure TMapMainLayer.DoRedraw;
begin
  inherited;

end;

end.
 