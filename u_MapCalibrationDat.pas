unit u_MapCalibrationDat;

interface

uses
  Types,
  i_ICoordConverter,
  i_IMapCalibration;

type
  TMapCalibrationDat = class(TInterfacedObject, IMapCalibration)
  public
    // Имя для вывода в листбоксе для выбора при экспорте.
    function GetName: WideString; safecall;
    // Более детальное описание привязки
    function GetDescription: WideString; safecall;
    // Генерирует привязку для склеенной карты.
    procedure SaveCalibrationInfo(AFileName: WideString; xy1, xy2: TPoint; Azoom: byte; AConverter: ICoordConverter); safecall;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  u_GeoToStr;

{ TMapCalibrationDat }

function TMapCalibrationDat.GetDescription: WideString;
begin
  Result := 'Привязка *.dat для программы Radio Mobile';
end;

function TMapCalibrationDat.GetName: WideString;
begin
  Result := '.dat';
end;

procedure TMapCalibrationDat.SaveCalibrationInfo(AFileName: WideString;
  xy1, xy2: TPoint; Azoom: byte; AConverter: ICoordConverter);
var
  f:TextFile;
  LL1,LL2:TExtendedPoint;
begin
  assignfile(f, ChangeFileExt(AFileName, '.dat'));
  rewrite(f);
  writeln(f,'2');
  LL1:=AConverter.PixelPos2LonLat(xy1, Azoom);
  LL2:=AConverter.PixelPos2LonLat(xy2, Azoom);
  writeln(f,R2StrPoint(LL1.x)+','+R2StrPoint(LL1.y));
  writeln(f,R2StrPoint(LL2.x)+','+R2StrPoint(LL1.y));
  writeln(f,R2StrPoint(LL2.x)+','+R2StrPoint(LL2.y));
  writeln(f,R2StrPoint(LL1.x)+','+R2StrPoint(LL2.y));
  writeln(f,'(SASPlanet)');
  closefile(f);
end;

end.
 