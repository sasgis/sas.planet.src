unit u_CenterScale;

interface

uses
  Types,
  GR32_Image,
  u_WindowLayerBasic;

type
  TCenterScale = class(TWindowLayerBasic)
  protected
    FRadius: Integer;
    function GetBitmapSizeInPixel: TPoint; override;
    function GetFreezePointInVisualPixel: TPoint; override;
    function GetFreezePointInBitmapPixel: TPoint; override;
  public
    constructor Create(AParentMap: TImage32);
    procedure Redraw; override;
  end;

implementation

uses
  Graphics,
  GR32,
  SysUtils;

{ TCenterScale }

constructor TCenterScale.Create(AParentMap: TImage32);
begin
  inherited Create(AParentMap);
  FRadius := 120;
end;

function TCenterScale.GetBitmapSizeInPixel: TPoint;
begin
  Result := Point(290, 290);
end;


function TCenterScale.GetFreezePointInBitmapPixel: TPoint;
var
  VBitmapSize: TPoint;
begin
  VBitmapSize := GetBitmapSizeInPixel;
  Result := Point(VBitmapSize.X div 2, VBitmapSize.Y div 2);
end;

function TCenterScale.GetFreezePointInVisualPixel: TPoint;
var
  VVisibleSize: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  Result := Point(VVisibleSize.X div 2, VVisibleSize.Y div 2);
end;

procedure TCenterScale.Redraw;
var
  VRect: TRect;
  VHalfSize: TPoint;
  i: integer;
  r: Double;
  xy, xy1: TPoint;
  VSize: TPoint;
begin
  inherited;
  VSize := GetBitmapSizeInPixel;
  VHalfSize := Point(VSize.X div 2, VSize.Y div 2);

  i:=0;
  FLayer.Bitmap.Clear(clBlack);
  FLayer.Bitmap.Canvas.Pen.Color := clRed;
  FLayer.Bitmap.Font.Size := 6;
  While i<360 do begin
    FLayer.Bitmap.Font.Size:=6;
    if (i mod 90) = 0 then begin
      r:=0;
      FLayer.Bitmap.Font.Size:=10;
    end else if (i mod 45) = 0 then begin
      r:=80;
      FLayer.Bitmap.Font.Size:=8;
    end else begin
      r:=110;
    end;
    xy.x := round(VHalfSize.X + FRadius * cos(i*(Pi/180)));
    xy.y := round(VHalfSize.Y + FRadius * sin(i*(Pi/180)));
    xy1.x := round(VHalfSize.X + r * cos(i*(Pi/180)));
    xy1.y := round(VHalfSize.Y + r * sin(i*(Pi/180)));
    FLayer.Bitmap.LineFS(xy.x,xy.y,xy1.x,xy1.y, SetAlpha(clRed32,180));
    if (i mod 15) = 0 then begin
      xy1.x := round(VHalfSize.X + 132* cos(i*(Pi/180)))-FLayer.Bitmap.TextWidth(inttostr((i+90)mod 360)+'°')div 2;
      xy1.y := round(VHalfSize.X + 132* sin(i*(Pi/180)))-2-FLayer.Bitmap.Font.size div 2;
      FLayer.Bitmap.RenderText(xy1.x+1,xy1.y+1,inttostr((i+90)mod 360)+'°',3,SetAlpha(clWhite32,250) );
      FLayer.Bitmap.RenderText(xy1.x,xy1.y,inttostr((i+90)mod 360)+'°',3,SetAlpha(clBlue32,250) );
    end;
    inc(i,5);
  end;
end;
end.
