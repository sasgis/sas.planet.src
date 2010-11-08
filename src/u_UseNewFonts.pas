unit u_UseNewFonts;
// Код взят из блога GunSmoker-а
// http://www.gunsmoker.ru/2010/11/windows-xp-windows-vista7.html
interface

uses
  Graphics;

function GUIFont: TFont;
function MonoFont: TFont;

implementation

uses
  Windows,
  SysUtils;

var 
  FGUIFont: TFont;
  FMonoFont: TFont;

function GUIFont: TFont;
begin
  Result := FGUIFont;
end;

function MonoFont: TFont;
begin
  Result := FMonoFont;
end;

procedure InitDefFontData;
var
  Metrics: TNonClientMetrics;
begin
  FGUIFont := TFont.Create;
  FMonoFont := TFont.Create;

  FillChar(Metrics, SizeOf(Metrics), 0);
  Metrics.cbSize := SizeOf(Metrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, Metrics.cbSize, @Metrics, 0) then
  begin
    FGUIFont.Handle := CreateFontIndirect(Metrics.lfMessageFont);

    DefFontData.Height := FGUIFont.Height;
    DefFontData.Orientation := FGUIFont.Orientation;
    DefFontData.Pitch := FGUIFont.Pitch;
    DefFontData.Style := FGUIFont.Style;
    DefFontData.Charset := FGUIFont.Charset;
    DefFontData.Name := UTF8Encode(FGUIFont.Name);
  end;
  FMonoFont.Handle := GetStockObject(ANSI_FIXED_FONT);
end;

initialization
  InitDefFontData;

finalization
  FreeAndNil(FMonoFont);
  FreeAndNil(FGUIFont);

end.
