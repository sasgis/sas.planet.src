unit u_TesterCoordConvertAbstract;

interface

uses
  i_ICoordConverter,
  u_CoordConverterAbstract;

type
  TTesterCoordConverterAbstract = class
  protected
    FConverter: ICoordConverter;
    FEpsilon: Extended;
    function CheckExtended(E1, E2: Extended): Boolean;
  public
    constructor Create(AConverter: ICoordConverter);
    destructor Destroy; override;
    procedure Check_TilesAtZoom; virtual;

    procedure Check_TilePos2PixelPos; virtual;
    procedure Check_TilePos2PixelRect; virtual;
    procedure Check_TilePos2Relative; virtual;
    procedure Check_TilePos2RelativeRect; virtual;

    procedure Check_PixelPos2TilePos; virtual;
    procedure Check_PixelPos2Relative; virtual;
    procedure Check_PixelRect2TileRect; virtual;
    procedure Check_PixelRect2RelativeRect; virtual;

    procedure Check_Relative2Pixel; virtual;
    procedure Check_Relative2Tile; virtual;
    procedure Check_RelativeRect2PixelRect; virtual;
    procedure Check_RelativeRect2TileRect; virtual;

    procedure Check_TilePos2LonLat; virtual;

    procedure Check_Relative2LonLat2Relative; virtual;
    procedure Check_TilePos2LonLat2TilePos; virtual;

    procedure Check_Monotonic_Relative2LonLat; virtual;

    procedure CheckConverter; virtual;
  end;
implementation

uses
  Types,
  SysUtils,
  t_GeoTypes;

{ TTesterCoordConverterAbstract }

procedure TTesterCoordConverterAbstract.CheckConverter;
begin
  try
    Check_TilesAtZoom;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции TilesAtZoom:' + E.Message);
    end;
  end;

  try
    Check_TilePos2PixelPos;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции TilePos2PixelPos:' + E.Message);
    end;
  end;

  try
    Check_TilePos2PixelRect;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции TilePos2PixelRect:' + E.Message);
    end;
  end;

  try
    Check_TilePos2Relative;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции TilePos2Relative:' + E.Message);
    end;
  end;

  try
    Check_TilePos2RelativeRect;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции TilePos2RelativeRect:' + E.Message);
    end;
  end;

  try
    Check_PixelPos2TilePos;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции PixelPos2TilePos:' + E.Message);
    end;
  end;

  try
    Check_PixelPos2Relative;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции PixelPos2Relative:' + E.Message);
    end;
  end;

  try
    Check_PixelRect2TileRect;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции PixelRect2TileRect:' + E.Message);
    end;
  end;

  try
    Check_PixelRect2RelativeRect;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции PixelRect2RelativeRect:' + E.Message);
    end;
  end;

  try
    Check_Relative2Pixel;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции Relative2Pixel:' + E.Message);
    end;
  end;

  try
    Check_Relative2Tile;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции Relative2Tile:' + E.Message);
    end;
  end;

  try
    Check_RelativeRect2PixelRect;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции RelativeRect2PixelRect:' + E.Message);
    end;
  end;

  try
    Check_RelativeRect2TileRect;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции RelativeRect2TileRect:' + E.Message);
    end;
  end;

  try
    Check_Monotonic_Relative2LonLat;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании монотонности проецирования ' + E.Message);
    end;
  end;


  try
    Check_TilePos2LonLat;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании функции TilePos2LonLat:' + E.Message);
    end;
  end;

  try
    Check_TilePos2LonLat2TilePos;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании двойного преобразования TilePos2LonLat2TilePos:' + E.Message);
    end;
  end;

  try
    Check_Relative2LonLat2Relative;
  except
    on E: Exception do begin
      raise Exception.Create('Ошибка при тестировании двойного преобразования Relative2LonLat2Relative:' + E.Message);
    end;
  end;
end;

function TTesterCoordConverterAbstract.CheckExtended(E1,
  E2: Extended): Boolean;
begin
  Result := abs(E1-E2) < FEpsilon;
end;


procedure TTesterCoordConverterAbstract.Check_Monotonic_Relative2LonLat;
var
  VDelta: Double;
  VSource: TExtendedPoint;
  VTarget: TExtendedPoint;
  VSourceLast: TExtendedPoint;
  VTargetLast: TExtendedPoint;
  VEpsilon: Double;
  VStart: Double;
  VFinish: Double;
begin
  VDelta := abs(1 / FConverter.PixelsAtZoom(22));
  VStart := 0;
  VFinish := VDelta * 100000;
  VSource.X := 0.5;
  VSource.Y := VStart;
  VSourceLast := VSource;
  VTargetLast := FConverter.Relative2LonLat(VSourceLast);
  VSource.Y := VSource.Y + VDelta;

  while (VSource.Y < VFinish) do begin
    VTarget := FConverter.Relative2LonLat(VSource);
    VEpsilon := VTargetLast.Y - VTarget.Y;
    if VEpsilon < 0 then begin
      raise Exception.Create('Функция проецирования не монотонна в точке ' + FloatToStr(VSource.Y) + ' Eps=' + FloatToStr(VEpsilon));
    end;
    VSourceLast := VSource;
    VTargetLast := VTarget;
    VSource.Y := VSource.Y + VDelta;
  end;

end;

procedure TTesterCoordConverterAbstract.Check_PixelPos2Relative;
var
  Res: TExtendedPoint;
begin
  Res := FConverter.PixelPos2Relative(Point(0, 128), 0);
  if not CheckExtended(Res.X, 0) then
    raise Exception.Create('z = 0 Ошибка в кординате X');
  if not CheckExtended(Res.Y, 0.5) then
    raise Exception.Create('z = 0 Ошибка в кординате Y');

  Res := FConverter.PixelPos2Relative(Point(255, 256), 0);
  if not CheckExtended(Res.X, 1 - 1/256) then
    raise Exception.Create('z = 0 Ошибка в кординате X');
  if not CheckExtended(Res.Y, 1) then
    raise Exception.Create('z = 0 Ошибка в кординате Y');

  Res := FConverter.PixelPos2Relative(Point(0, 1 shl 30), 23);
  if not CheckExtended(Res.X, 0) then
    raise Exception.Create('z = 0 Ошибка в кординате X');
  if not CheckExtended(Res.Y, 0.5) then
    raise Exception.Create('z = 0 Ошибка в кординате Y');

  Res := FConverter.PixelPos2Relative(Point(2147483392 + 255, 1 shl 31), 23);
  if not CheckExtended(Res.X, 1 - 1/(1 shl 30 + (1 shl 30 - 1))) then
    raise Exception.Create('z = 0 Ошибка в кординате X');
  if not CheckExtended(Res.Y, 1) then
    raise Exception.Create('z = 0 Ошибка в кординате Y');

end;

procedure TTesterCoordConverterAbstract.Check_PixelPos2TilePos;
var
  Res: TPoint;
begin
  Res := FConverter.PixelPos2TilePos(Point(0,0), 0);
  if Res.X <> 0 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 0 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Res := FConverter.PixelPos2TilePos(Point(156,73), 0);
  if Res.X <> 0 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 0 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Res := FConverter.PixelPos2TilePos(Point(255,255), 0);
  if Res.X <> 0 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 0 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Res := FConverter.PixelPos2TilePos(Point(255,255), 23);
  if Res.X <> 0 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 0 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Res := FConverter.PixelPos2TilePos(Point(2147483392,2147483392 + 255), 23);
  if Res.X <> 1 shl 23 - 1 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 1 shl 23 - 1 then
    raise Exception.Create('Z = 0. Ошибка в y координате');
end;

procedure TTesterCoordConverterAbstract.Check_PixelRect2RelativeRect;
var
  Res: TExtendedRect;
begin
  Res := FConverter.PixelRect2RelativeRect(Rect(0,0,0,0),0);
  if not CheckExtended(Res.Left, 0) then
    raise Exception.Create('Z = 0. Ошибка в Left');
  if not CheckExtended(Res.Top, 0) then
    raise Exception.Create('Z = 0. Ошибка в Top');
  if not CheckExtended(Res.Right, 1/256) then
    raise Exception.Create('Z = 0. Ошибка в Right');
  if not CheckExtended(Res.Bottom, 1/256) then
    raise Exception.Create('Z = 0. Ошибка в Bottom');

  Res := FConverter.PixelRect2RelativeRect(Rect(0,0,255,255), 0);
  if not CheckExtended(Res.Left, 0) then
    raise Exception.Create('Z = 0. Ошибка в Left');
  if not CheckExtended(Res.Top, 0) then
    raise Exception.Create('Z = 0. Ошибка в Top');
  if not CheckExtended(Res.Right, 1) then
    raise Exception.Create('Z = 0. Ошибка в Right');
  if not CheckExtended(Res.Bottom, 1) then
    raise Exception.Create('Z = 0. Ошибка в Bottom');

  Res := FConverter.PixelRect2RelativeRect(Rect(0, 1 shl 30, 255, 2147483392 + 255),23);
  if not CheckExtended(Res.Left, 0) then
    raise Exception.Create('Z = 23. Ошибка в Left');
  if not CheckExtended(Res.Top, 0.5) then
    raise Exception.Create('Z = 23. Ошибка в Top');
  if not CheckExtended(Res.Right, 1/(1 shl 23)) then
    raise Exception.Create('Z = 23. Ошибка в Right');
  if not CheckExtended(Res.Bottom, 1) then
    raise Exception.Create('Z = 23. Ошибка в Bottom');
end;

procedure TTesterCoordConverterAbstract.Check_PixelRect2TileRect;
var
  Res: TRect;
begin
  Res := FConverter.PixelRect2TileRect(Rect(0, 0, 255, 255), 0);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Left прямоугольника');
  if Res.Top <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Top прямоугольника');
  if Res.Right <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Right прямоугольника');
  if Res.Bottom <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Bottom прямоугольника');

  Res := FConverter.PixelRect2TileRect(Rect(0, 0, 255, 255), 1);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 1. Ошибка в Left прямоугольника');
  if Res.Top <> 0 then
    raise Exception.Create('Z = 1. Ошибка в Top прямоугольника');
  if Res.Right <> 0 then
    raise Exception.Create('Z = 1. Ошибка в Right прямоугольника');
  if Res.Bottom <> 0 then
    raise Exception.Create('Z = 1. Ошибка в Bottom прямоугольника');

  Res := FConverter.PixelRect2TileRect(Rect(0, 0, 511, 255), 1);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 1. Ошибка в Left прямоугольника');
  if Res.Top <> 0 then
    raise Exception.Create('Z = 1. Ошибка в Top прямоугольника');
  if Res.Right <> 1 then
    raise Exception.Create('Z = 1. Ошибка в Right прямоугольника');
  if Res.Bottom <> 0 then
    raise Exception.Create('Z = 1. Ошибка в Bottom прямоугольника');

  Res := FConverter.PixelRect2TileRect(Rect(0, 0, 511, 255), 23);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 23. Ошибка в Left прямоугольника');
  if Res.Top <> 0 then
    raise Exception.Create('Z = 23. Ошибка в Top прямоугольника');
  if Res.Right <> 1 then
    raise Exception.Create('Z = 23. Ошибка в Right прямоугольника');
  if Res.Bottom <> 0 then
    raise Exception.Create('Z = 23. Ошибка в Bottom прямоугольника');

  Res := FConverter.PixelRect2TileRect(Rect(2147483392, 2147483392 + 255, 2147483392,2147483392 + 255), 23);
  if Res.Left <> 8388607 then
    raise Exception.Create('Z = 23. Ошибка в Left прямоугольника');
  if Res.Top <> 8388607 then
    raise Exception.Create('Z = 23. Ошибка в Top прямоугольника');
  if Res.Right <> 8388607 then
    raise Exception.Create('Z = 23. Ошибка в Right прямоугольника');
  if Res.Bottom <> 8388607 then
    raise Exception.Create('Z = 23. Ошибка в Bottom прямоугольника');

  Res := FConverter.PixelRect2TileRect(Rect(0, 0, 2147483392,2147483392 + 255), 23);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 23. Ошибка в Left прямоугольника');
  if Res.Top <> 0 then
    raise Exception.Create('Z = 23. Ошибка в Top прямоугольника');
  if Res.Right <> 8388607 then
    raise Exception.Create('Z = 23. Ошибка в Right прямоугольника');
  if Res.Bottom <> 8388607 then
    raise Exception.Create('Z = 23. Ошибка в Bottom прямоугольника');
end;

procedure TTesterCoordConverterAbstract.Check_Relative2LonLat2Relative;
var
  Source, Temp, Res: TExtendedPoint;
  Delta: Extended;
  MaxDelta: Extended;
const
  Step: Double = 0.00001;
begin
  Source.X := 1/256;
  Source.Y := 0;
  MaxDelta := 0;
  while Source.Y <= 1 do begin
    Temp := FConverter.Relative2LonLat(Source);
    Res := FConverter.LonLat2Relative(Temp);
    Delta := Source.Y - Res.Y;
    if abs(Delta) > abs(MaxDelta) then
      MaxDelta := Delta;
    if abs(Delta)> 1e-8 then
      raise Exception.Create('Слишком большая погрешность');
    Source.Y := Source.Y + Step;
  end;
  if abs(MaxDelta)> 2.3283064365e-10 then
    raise Exception.Create('Слишком большая погрешность ' + FloatToStr(MaxDelta));
end;

procedure TTesterCoordConverterAbstract.Check_Relative2Pixel;
var
  Res: TPoint;
  Source: TExtendedPoint;
begin
  Source.X := 1/256;
  Source.Y := 1/500;
  Res := FConverter.Relative2Pixel(Source, 0);
  if Res.X <> 1 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 0 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Source.X := 1;
  Source.Y := 1;
  Res := FConverter.Relative2Pixel(Source, 0);
  if Res.X <> 256 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 256 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Source.X := 1;
  Source.Y := 1;
  Res := FConverter.Relative2Pixel(Source, 23);
  if Res.X <> 1 shl 31 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 1 shl 31 then
    raise Exception.Create('Z = 0. Ошибка в y координате');
end;

procedure TTesterCoordConverterAbstract.Check_Relative2Tile;
var
  Res: TPoint;
  Source: TExtendedPoint;
begin
  Source.X := 1/256;
  Source.Y := 1/500;
  Res := FConverter.Relative2Tile(Source, 0);
  if Res.X <> 0 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 0 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Source.X := 1;
  Source.Y := 1;
  Res := FConverter.Relative2Tile(Source, 0);
  if Res.X <> 1 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 1 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Source.X := 1;
  Source.Y := 1;
  Res := FConverter.Relative2Tile(Source, 23);
  if Res.X <> 1 shl 23 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 1 shl 23 then
    raise Exception.Create('Z = 0. Ошибка в y координате');
end;

procedure TTesterCoordConverterAbstract.Check_RelativeRect2PixelRect;
var
  Res: TRect;
  Source: TExtendedRect;
begin
  Source.Left := 0;
  Source.Top := 1/256;
  Source.Right := 1;
  Source.Bottom := 511/512;
  Res := FConverter.RelativeRect2PixelRect(Source, 0);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Left прямоугольника');
  if Res.Top <> 1 then
    raise Exception.Create('Z = 0. Ошибка в Top прямоугольника');
  if Res.Right <> 255 then
    raise Exception.Create('Z = 0. Ошибка в Right прямоугольника');
  if Res.Bottom <> 255 then
    raise Exception.Create('Z = 0. Ошибка в Bottom прямоугольника');

  Source.Left := 0;
  Source.Top := 1/256;
  Source.Right := 1;
  Source.Bottom := 511/512;
  Res := FConverter.RelativeRect2PixelRect(Source, 23);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 23. Ошибка в Left прямоугольника');
  if Res.Top <> 1 shl 23 then
    raise Exception.Create('Z = 23. Ошибка в Top прямоугольника');
  if Res.Right <> 2147483647 then
    raise Exception.Create('Z = 23. Ошибка в Right прямоугольника');
  if Res.Bottom <> 511 shl 22 - 1 then
    raise Exception.Create('Z = 23. Ошибка в Bottom прямоугольника');
end;

procedure TTesterCoordConverterAbstract.Check_RelativeRect2TileRect;
var
  Res: TRect;
  Source: TExtendedRect;
begin
  Source.Left := 0;
  Source.Top := 1/256;
  Source.Right := 1;
  Source.Bottom := 511/512;
  Res := FConverter.RelativeRect2TileRect(Source, 0);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Left прямоугольника');
  if Res.Top <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Top прямоугольника');
  if Res.Right <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Right прямоугольника');
  if Res.Bottom <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Bottom прямоугольника');

  Source.Left := 0;
  Source.Top := 1/256;
  Source.Right := 1;
  Source.Bottom := 511/512;
  Res := FConverter.RelativeRect2TileRect(Source, 23);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 23. Ошибка в Left прямоугольника');
  if Res.Top <> 1 shl 15 then
    raise Exception.Create('Z = 23. Ошибка в Top прямоугольника');
  if Res.Right <> 1 shl 23 - 1 then
    raise Exception.Create('Z = 23. Ошибка в Right прямоугольника');
  if Res.Bottom <> 511 shl 14 - 1 then
    raise Exception.Create('Z = 23. Ошибка в Bottom прямоугольника');
end;

procedure TTesterCoordConverterAbstract.Check_TilePos2LonLat;
var
  Res: TExtendedPoint;
  Res1: TExtendedPoint;
begin
  Res := FConverter.TilePos2LonLat(Point(0, 0), 8);
  Res1 := FConverter.Pos2LonLat(Point(0, 0), 8);
  if not CheckExtended(Res.X, Res1.X) then
    raise Exception.Create('Ошибка на Z=0');
  if not CheckExtended(Res.Y, Res1.Y) then
    raise Exception.Create('Ошибка на Z=0');

  Res := FConverter.TilePos2LonLat(Point(1, 2), 8);
  Res1 := FConverter.Pos2LonLat(Point(1, 2), 8);
  if not CheckExtended(Res.X, Res1.X) then
    raise Exception.Create('Ошибка на Z=0');
  if not CheckExtended(Res.Y, Res1.Y) then
    raise Exception.Create('Ошибка на Z=0');

  Res := FConverter.TilePos2LonLat(Point(256, 256), 8);
  Res1 := FConverter.Pos2LonLat(Point(256, 256), 8);
  if not CheckExtended(Res.X, Res1.X) then
    raise Exception.Create('Ошибка на Z=0');
  if not CheckExtended(Res.Y, Res1.Y) then
    raise Exception.Create('Ошибка на Z=0');

  Res := FConverter.TilePos2LonLat(Point(255, 255), 8);
  Res1 := FConverter.Pos2LonLat(Point(255, 255), 8);
  if not CheckExtended(Res.X, Res1.X) then
    raise Exception.Create('Ошибка на Z=0');
  if not CheckExtended(Res.Y, Res1.Y) then
    raise Exception.Create('Ошибка на Z=0');

  Res := FConverter.TilePos2LonLat(Point(13123, 2231), 23);
  Res1 := FConverter.Pos2LonLat(Point(13123, 2231), 23);
  if not CheckExtended(Res.X, Res1.X) then
    raise Exception.Create('Ошибка на Z=0');
  if not CheckExtended(Res.Y, Res1.Y) then
    raise Exception.Create('Ошибка на Z=0');
end;

procedure TTesterCoordConverterAbstract.Check_TilePos2LonLat2TilePos;
var
  Source, Res: TPoint;
  Zoom: byte;
  VTemp: TExtendedPoint;
  VTempOld: TExtendedPoint;
  ResOld: TPoint;
begin
  Source := Point(10, 99);
  Zoom := 8;
  VTemp := FConverter.TilePos2LonLat(Source, Zoom);
  VTempOld := FConverter.Pos2LonLat(Source, Zoom);
  ResOld := FConverter.LonLat2Pos(VTempOld, Zoom);

  Res := FConverter.LonLat2TilePos(VTemp, Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 0. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 0. Ошибка в x координате Y');

  Zoom := 10;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 10. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 10. Ошибка в x координате Y');

  Zoom := 14;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 14. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 14. Ошибка в x координате Y');

  Zoom := 20;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 20. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 20. Ошибка в x координате Y');

  Zoom := 23;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 23. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 23. Ошибка в x координате Y');

  Source := Point(1024, 768);
  Zoom := 10;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 10. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 10. Ошибка в x координате Y');

  Zoom := 11;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 10. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 10. Ошибка в x координате Y');

  Zoom := 14;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 14. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 14. Ошибка в x координате Y');

  Zoom := 20;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 20. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 20. Ошибка в x координате Y');

  Zoom := 22;
  Res := FConverter.LonLat2TilePos(FConverter.TilePos2LonLat(Source, Zoom), Zoom);
  if Res.X <> Source.X then
    raise Exception.Create('Z = 23. Ошибка в x координате X');
  if Res.Y <> Source.Y then
    raise Exception.Create('Z = 23. Ошибка в x координате Y');
end;

procedure TTesterCoordConverterAbstract.Check_TilePos2PixelPos;
var
  Res: TPoint;
begin
  Res := FConverter.TilePos2PixelPos(Point(0,0), 0);
  if Res.X <> 0 then
    raise Exception.Create('Z = 0. Ошибка в x координате');
  if Res.Y <> 0 then
    raise Exception.Create('Z = 0. Ошибка в y координате');

  Res := FConverter.TilePos2PixelPos(Point(0,1), 1);
  if Res.X <> 0 then
    raise Exception.Create('Z = 1. Ошибка в x координате');
  if Res.Y <> 256 then
    raise Exception.Create('Z = 1. Ошибка в y координате');

  Res := FConverter.TilePos2PixelPos(Point(1,1), 1);
  if Res.X <> 256 then
    raise Exception.Create('Z = 1. Ошибка в x координате');
  if Res.Y <> 256 then
    raise Exception.Create('Z = 1. Ошибка в y координате');

  Res := FConverter.TilePos2PixelPos(Point(1,1), 23);
  if Res.X <> 256 then
    raise Exception.Create('Z = 23. Ошибка в x координате');
  if Res.Y <> 256 then
    raise Exception.Create('Z = 23. Ошибка в y координате');

  Res := FConverter.TilePos2PixelPos(Point(1 shl 23 - 1, 1 shl 23 - 1), 23);
  if Res.X <> 2147483392 then
    raise Exception.Create('Z = 23. Ошибка в x координате');
  if Res.Y <> 2147483392 then
    raise Exception.Create('Z = 23. Ошибка в y координате');
end;

procedure TTesterCoordConverterAbstract.Check_TilePos2PixelRect;
var
  Res: TRect;
begin
  Res := FConverter.TilePos2PixelRect(Point(0, 0), 0);
  if Res.Left <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Left прямоугольника');
  if Res.Top <> 0 then
    raise Exception.Create('Z = 0. Ошибка в Top прямоугольника');
  if Res.Right <> 255 then
    raise Exception.Create('Z = 0. Ошибка в Right прямоугольника');
  if Res.Bottom <> 255 then
    raise Exception.Create('Z = 0. Ошибка в Bottom прямоугольника');

  Res := FConverter.TilePos2PixelRect(Point(1, 0), 1);
  if Res.Left <> 256 then
    raise Exception.Create('Z = 1. Ошибка в Left прямоугольника');
  if Res.Top <> 0 then
    raise Exception.Create('Z = 1. Ошибка в Top прямоугольника');
  if Res.Right <> 511 then
    raise Exception.Create('Z = 1. Ошибка в Right прямоугольника');
  if Res.Bottom <> 255 then
    raise Exception.Create('Z = 1. Ошибка в Bottom прямоугольника');

  Res := FConverter.TilePos2PixelRect(Point(FConverter.TilesAtZoom(23) - 1, FConverter.TilesAtZoom(23) - 1), 23);
  if Res.Left <> 2147483392 then
    raise Exception.Create('Z = 23. Ошибка в Left прямоугольника');
  if Res.Top <> 2147483392 then
    raise Exception.Create('Z = 23. Ошибка в Top прямоугольника');
  if Res.Right <> 2147483647 then
    raise Exception.Create('Z = 23. Ошибка в Right прямоугольника');
  if Res.Bottom <> 2147483647 then
    raise Exception.Create('Z = 23. Ошибка в Bottom прямоугольника');
end;

procedure TTesterCoordConverterAbstract.Check_TilePos2Relative;
var
  Res: TExtendedPoint;
begin
  Res := FConverter.TilePos2Relative(Point(0, 0), 0);
  if not CheckExtended(Res.X, 0) then
    raise Exception.Create('На зуме 0 относительные координаты единственного тайла должны быть (0;0)');
  if not CheckExtended(Res.Y, 0) then
    raise Exception.Create('На зуме 0 относительные координаты единственного тайла должны быть (0;0)');

  Res := FConverter.TilePos2Relative(Point(0, 0), 1);
  if not CheckExtended(Res.X, 0) then
    raise Exception.Create('На зуме 1 относительные координаты тайла (0;0) должны быть (0;0)');
  if not CheckExtended(Res.Y, 0) then
    raise Exception.Create('На зуме 1 относительные координаты тайла (0;0) должны быть (0;0)');

  Res := FConverter.TilePos2Relative(Point(1, 1), 1);
  if not CheckExtended(Res.X, 0.5) then
    raise Exception.Create('На зуме 1 относительные координаты тайла (1;1) должны быть (0.5;0.5)');
  if not CheckExtended(Res.Y, 0.5) then
    raise Exception.Create('На зуме 1 относительные координаты тайла (1;1) должны быть (0.5;0.5)');

  Res := FConverter.TilePos2Relative(Point(2, 2), 1);
  if not CheckExtended(Res.X, 1) then
    raise Exception.Create('На зуме 1 относительные координаты тайла (2;2) должны быть (1;1)');
  if not CheckExtended(Res.Y, 1) then
    raise Exception.Create('На зуме 1 относительные координаты тайла (2;2) должны быть (1;1)');

  Res := FConverter.TilePos2Relative(Point(0, 0), 23);
  if not CheckExtended(Res.X, 0) then
    raise Exception.Create('На зуме 23 относительные координаты тайла (0;0) должны быть (0;0)');
  if not CheckExtended(Res.Y, 0) then
    raise Exception.Create('На зуме 23 относительные координаты тайла (0;0) должны быть (0;0)');

  Res := FConverter.TilePos2Relative(Point(1, 1), 23);
  if not CheckExtended(Res.X, 1.1920928955e-07) then
    raise Exception.Create('На зуме 23 относительные координаты тайла (1;1) должны быть (1.1920928955e-07;1.1920928955e-07)');
  if not CheckExtended(Res.Y, 1.1920928955e-07) then
    raise Exception.Create('На зуме 23 относительные координаты тайла (1;1) должны быть (1.1920928955e-07;1.1920928955e-07)');

  Res := FConverter.TilePos2Relative(Point(1 shl 23, 1 shl 23), 23);
  if not CheckExtended(Res.X, 1) then
    raise Exception.Create('На зуме 23 относительные координаты тайла (Max;Max) должны быть (1;1)');
  if not CheckExtended(Res.Y, 1) then
    raise Exception.Create('На зуме 23 относительные координаты тайла (Max;Max) должны быть (1;1)');
end;

procedure TTesterCoordConverterAbstract.Check_TilePos2RelativeRect;
var
  Res: TExtendedRect;
begin
  Res := FConverter.TilePos2RelativeRect(Point(0,0),0);
  if not CheckExtended(Res.Left, 0) then
    raise Exception.Create('Z = 0. Ошибка в Left');
  if not CheckExtended(Res.Top, 0) then
    raise Exception.Create('Z = 0. Ошибка в Top');
  if not CheckExtended(Res.Right, 1) then
    raise Exception.Create('Z = 0. Ошибка в Right');
  if not CheckExtended(Res.Bottom, 1) then
    raise Exception.Create('Z = 0. Ошибка в Bottom');

  Res := FConverter.TilePos2RelativeRect(Point(1,1),1);
  if not CheckExtended(Res.Left, 0.5) then
    raise Exception.Create('Z = 0. Ошибка в Left');
  if not CheckExtended(Res.Top, 0.5) then
    raise Exception.Create('Z = 0. Ошибка в Top');
  if not CheckExtended(Res.Right, 1) then
    raise Exception.Create('Z = 0. Ошибка в Right');
  if not CheckExtended(Res.Bottom, 1) then
    raise Exception.Create('Z = 0. Ошибка в Bottom');

  Res := FConverter.TilePos2RelativeRect(Point(1 shl 23 - 1, 1 shl 23 - 1), 23);
  if not CheckExtended(Res.Left, 1 - 1.1920928955e-07) then
    raise Exception.Create('Z = 0. Ошибка в Left');
  if not CheckExtended(Res.Top, 1 - 1.1920928955e-07) then
    raise Exception.Create('Z = 0. Ошибка в Top');
  if not CheckExtended(Res.Right, 1) then
    raise Exception.Create('Z = 0. Ошибка в Right');
  if not CheckExtended(Res.Bottom, 1) then
    raise Exception.Create('Z = 0. Ошибка в Bottom');
end;

procedure TTesterCoordConverterAbstract.Check_TilesAtZoom;
var
  Res: Integer;
begin
  Res := FConverter.TilesAtZoom(0);
  if Res <> 1 then
    raise Exception.Create('На зуме 0 должен быть 1 тайл');

  Res := FConverter.TilesAtZoom(1);
  if Res <> 2 then
    raise Exception.Create('На зуме 1 должно быть 2 тайла');

  Res := FConverter.TilesAtZoom(23);
  if Res <> 8388608 then
    raise Exception.Create('На зуме 23 должно быть 8388608 тайлов');
end;

constructor TTesterCoordConverterAbstract.Create(
  AConverter: ICoordConverter);
begin
  FConverter := AConverter;
  FEpsilon := 1/(1 shl 30 + (1 shl 30 - 1));
end;

destructor TTesterCoordConverterAbstract.Destroy;
begin
  FConverter := nil;
  inherited;
end;

end.
