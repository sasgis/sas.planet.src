{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapCalibrationDat;

interface

uses
  Types,
  i_CoordConverter,
  i_MapCalibration;

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
  f: TextFile;
  LL1, LL2: TDoublePoint;
begin
  assignfile(f, ChangeFileExt(AFileName, '.dat'));
  rewrite(f);
  writeln(f, '2');
  LL1 := AConverter.PixelPos2LonLat(xy1, Azoom);
  LL2 := AConverter.PixelPos2LonLat(xy2, Azoom);
  writeln(f, R2StrPoint(LL1.x) + ',' + R2StrPoint(LL1.y));
  writeln(f, R2StrPoint(LL2.x) + ',' + R2StrPoint(LL1.y));
  writeln(f, R2StrPoint(LL2.x) + ',' + R2StrPoint(LL2.y));
  writeln(f, R2StrPoint(LL1.x) + ',' + R2StrPoint(LL2.y));
  writeln(f, '(SASPlanet)');
  closefile(f);
end;

end.
 