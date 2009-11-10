unit u_CenterScale;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Layers;

type
  TCenterScale = class
  private
    FRadius: Integer;
    FSize: TPoint;
    FParentMap: TImage32;
    LayerMapScale: TBitmapLayer;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create(AParentMap: TImage32);
    procedure Resize;
    procedure Show;
    procedure Hide;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

implementation

uses
  Types,
  Graphics,
  SysUtils;

{ TCenterScale }

constructor TCenterScale.Create(AParentMap: TImage32);
var
  VMapCenter: TPoint;
  VRect: TRect;
  VHalfSize: TPoint;
  i: integer;
  r: Double;
  xy, xy1: TPoint;
begin
  FParentMap := AParentMap;
  FRadius := 120;
  FSize := Point(290, 290);
  VHalfSize := Point(FSize.X div 2, FSize.Y div 2);
  VMapCenter := Point(FParentMap.Width div 2, FParentMap.Height div 2);

  VRect.Left := VMapCenter.X - VHalfSize.X;
  VRect.Top := VMapCenter.Y - VHalfSize.Y;
  VRect.Right := VMapCenter.X + VHalfSize.X;
  VRect.Bottom := VMapCenter.Y + VHalfSize.Y;

  LayerMapScale := TBitmapLayer.Create(FParentMap.Layers);
  LayerMapScale.location := floatrect(VRect);
  LayerMapScale.Bitmap.Width := FSize.x;
  LayerMapScale.Bitmap.Height := FSize.Y;
  LayerMapScale.Bitmap.DrawMode := dmBlend;
  LayerMapScale.Bitmap.CombineMode := cmMerge;
  LayerMapScale.bitmap.Font.Charset := RUSSIAN_CHARSET;
  i:=0;
  LayerMapScale.Bitmap.Clear(clBlack);
  LayerMapScale.Bitmap.Canvas.Pen.Color := clRed;
  LayerMapScale.Bitmap.Font.Size := 6;
  While i<360 do begin
    LayerMapScale.Bitmap.Font.Size:=6;
    if (i mod 90) = 0 then begin
      r:=0;
      LayerMapScale.Bitmap.Font.Size:=10;
    end else if (i mod 45) = 0 then begin
      r:=80;
      LayerMapScale.Bitmap.Font.Size:=8;
    end else begin
      r:=110;
    end;
    xy.x := round(VHalfSize.X + FRadius * cos(i*(Pi/180)));
    xy.y := round(VHalfSize.Y + FRadius * sin(i*(Pi/180)));
    xy1.x := round(VHalfSize.X + r * cos(i*(Pi/180)));
    xy1.y := round(VHalfSize.Y + r * sin(i*(Pi/180)));
    LayerMapScale.Bitmap.LineFS(xy.x,xy.y,xy1.x,xy1.y,SetAlpha(clRed32,180));
    if (i mod 15) = 0 then begin
      xy1.x := round(VHalfSize.X + 132* cos(i*(Pi/180)))-LayerMapScale.Bitmap.TextWidth(inttostr((i+90)mod 360)+'°')div 2;
      xy1.y := round(VHalfSize.X + 132* sin(i*(Pi/180)))-2-LayerMapScale.Bitmap.Font.size div 2;
      LayerMapScale.Bitmap.RenderText(xy1.x+1,xy1.y+1,inttostr((i+90)mod 360)+'°',3,SetAlpha(clWhite32,250) );
      LayerMapScale.Bitmap.RenderText(xy1.x,xy1.y,inttostr((i+90)mod 360)+'°',3,SetAlpha(clBlue32,250) );
    end;
    inc(i,5);
  end;
end;

function TCenterScale.GetVisible: Boolean;
begin
  Result := LayerMapScale.Visible;
end;

procedure TCenterScale.Hide;
begin
  LayerMapScale.Visible := False;
  LayerMapScale.SendToBack;
end;

procedure TCenterScale.Resize;
var
  VMapCenter: TPoint;
  VRect: TRect;
  VHalfSize: TPoint;
begin
  VHalfSize := Point(FSize.X div 2, FSize.Y div 2);
  VMapCenter := Point(FParentMap.Width div 2, FParentMap.Height div 2);

  VRect.Left := VMapCenter.X - VHalfSize.X;
  VRect.Top := VMapCenter.Y - VHalfSize.Y;
  VRect.Right := VMapCenter.X + VHalfSize.X;
  VRect.Bottom := VMapCenter.Y + VHalfSize.Y;

  LayerMapScale.location := floatrect(VRect);
end;

procedure TCenterScale.SetVisible(const Value: Boolean);
begin
  if Value then begin
    Show;
  end else begin
    Hide;
  end;
end;

procedure TCenterScale.Show;
begin
  LayerMapScale.Visible := True;
  LayerMapScale.BringToFront;
end;

end.
