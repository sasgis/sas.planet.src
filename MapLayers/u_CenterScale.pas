unit u_CenterScale;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState,
  u_WindowLayerWithPos;

type
  TCenterScale = class(TWindowLayerFixedSizeWithBitmap)
  private
    FRadius: Integer;
    FFontSize: Integer;
    FDigitsOffset: Integer;
    procedure DrawScale;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

implementation

uses
  Graphics,
  SysUtils;

{ TCenterScale }

constructor TCenterScale.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
var
 textWdth: integer;
 VSize: TPoint;
begin
  inherited;
  FRadius := 115;
  FDigitsOffset := 20;
  FFontSize := 12;
  textWdth := FLayer.Bitmap.TextWidth('270°');
  VSize := Point((FRadius * 2) + (FDigitsOffset * 2) + (textWdth * 2), (FRadius * 2) + (FDigitsOffset * 2) + (textWdth * 2));
  FLayer.Bitmap.SetSize(VSize.X, VSize.Y);
  DoUpdateLayerSize(VSize);
  DrawScale;
end;

function TCenterScale.GetMapLayerLocationRect: TFloatRect;
var
  VSize: TPoint;
  VViewSize: TPoint;
begin
  VSize := LayerSize;
  VViewSize := FVisualCoordConverter.GetLocalRectSize;
  Result.Left := VViewSize.X / 2 - VSize.X / 2;
  Result.Top := VViewSize.Y / 2 - VSize.Y / 2;
  Result.Right := Result.Left + VSize.X;
  Result.Bottom := Result.Top + VSize.Y;
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
  VSize := LayerSize;
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
