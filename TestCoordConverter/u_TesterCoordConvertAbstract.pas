unit u_TesterCoordConvertAbstract;

interface

uses
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

end;

function TTesterCoordConverterAbstract.CheckExtended(E1,
  E2: Extended): Boolean;
begin
  Result := abs(E1-E2) < FEpsilon;
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
