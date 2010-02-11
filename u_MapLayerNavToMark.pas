unit u_MapLayerNavToMark;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TNavToMarkLayer =  class(TMapLayerBasic)
  protected
    FMarkPoint: TExtendedPoint;
    FBitmapSize: TPoint;
    FId: integer;
    procedure DoRedraw; override;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetScreenCenterInBitmapPixels: TPoint; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    procedure StartNav(APoint: TExtendedPoint; Aid: integer);
    function GetDistToMark: Double;
    property ID: Integer read FId;
  end;

implementation

{ TNavToMarkLayer }

constructor TNavToMarkLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin

end;

procedure TNavToMarkLayer.DoRedraw;
begin
  inherited;

end;

function TNavToMarkLayer.GetBitmapSizeInPixel: TPoint;
begin

end;

function TNavToMarkLayer.GetDistToMark: Double;
begin

end;

function TNavToMarkLayer.GetScreenCenterInBitmapPixels: TPoint;
begin

end;

procedure TNavToMarkLayer.StartNav(APoint: TExtendedPoint; Aid: integer);
begin

end;

end.
