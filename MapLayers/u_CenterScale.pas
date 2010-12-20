unit u_CenterScale;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState,
  u_WindowLayerBasic;

type
  TCenterScale = class(TWindowLayerBasicFixedSizeWithBitmap)
  protected
    FRadius: Integer;
    FFontSize: Integer;
    FDigitsOffset: Integer;
    FSize: TPoint;
    function GetBitmapSizeInPixel: TPoint; override;
    procedure DrawScale;
    function GetMapLayerLocationRect: TFloatRect; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
  end;

implementation

uses
  Graphics,
  SysUtils;

{ TCenterScale }

constructor TCenterScale.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
var
	 textWdth: integer;
begin
  inherited;
  FRadius := 115;
  FDigitsOffset := 20;
  FFontSize := 12;
  textWdth := FLayer.Bitmap.TextWidth('270°');
  FSize := Point((FRadius * 2) + (FDigitsOffset * 2) + (textWdth * 2), (FRadius * 2) + (FDigitsOffset * 2) + (textWdth * 2));
  FLayer.Bitmap.SetSize(FSize.X, FSize.Y);
  DrawScale;
end;

function TCenterScale.GetBitmapSizeInPixel: TPoint;
begin
  Result := FSize;
end;

function TCenterScale.GetMapLayerLocationRect: TFloatRect;
begin
  Result.Left := FMapViewSize.X / 2 - FSize.X / 2;
  Result.Top := FMapViewSize.Y / 2 - FSize.Y / 2;
  Result.Right := Result.Left + FSize.X;
  Result.Bottom := Result.Top + FSize.Y;
end;

procedure TCenterScale.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    Visible := VConfigProvider.ReadBool('showscale',false);
  end;
end;

procedure TCenterScale.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  VConfigProvider.WriteBool('showscale', Visible);
end;

procedure TCenterScale.DrawScale;
var
  VHalfSize: TPoint;
  i: integer;
  r: Double;
  xy, xy1: TPoint;
  VSize: TPoint;
begin
  inherited;
  VSize := GetBitmapSizeInPixel;
  VHalfSize := Point(VSize.X div 2, VSize.Y div 2);

  i := 0;
  FLayer.Bitmap.Clear(clBlack);
  FLayer.Bitmap.Font.Size := FFontSize - 3;
  While i < 360 do begin
    FLayer.Bitmap.Font.Size := FFontSize - 3;
    if (i mod 90) = 0 then begin
      r := 0;
      FLayer.Bitmap.Font.Size := FFontSize;
    end else if (i mod 45) = 0 then begin
      r := FRadius - 40;
      FLayer.Bitmap.Font.Size := FFontSize - 1;
    end else begin
      r := FRadius - 10;
    end;
    xy.x := round(VHalfSize.X + FRadius * cos(i * (Pi / 180)));
    xy.y := round(VHalfSize.Y + FRadius * sin(i * (Pi / 180)));
    xy1.x := round(VHalfSize.X + r * cos(i * (Pi / 180)));
    xy1.y := round(VHalfSize.Y + r * sin(i * (Pi / 180)));
    FLayer.Bitmap.LineFS(xy.x, xy.y, xy1.x, xy1.y, SetAlpha(clRed32, 180));
    if (i mod 15) = 0 then begin
      xy1.x := round(VHalfSize.X + (FRadius + FDigitsOffset) * cos(i * (Pi / 180))) - FLayer.Bitmap.TextWidth(inttostr((i + 90) mod 360) + '°') div 2;
      xy1.y := round(VHalfSize.X + (FRadius + FDigitsOffset) * sin(i * (Pi / 180))) - 2 - FLayer.Bitmap.Font.size div 2;
      FLayer.Bitmap.RenderText(xy1.x + 1, xy1.y + 1, inttostr((i + 90) mod 360) + '°', 3, SetAlpha(clWhite32, 150));
      FLayer.Bitmap.RenderText(xy1.x, xy1.y, inttostr((i + 90) mod 360) + '°', 3, SetAlpha(clBlue32, 210));
    end;
    inc(i, 5);
  end;
end;

end.
