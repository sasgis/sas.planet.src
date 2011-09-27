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

unit u_MapCalibrationKml;

interface

uses
  Types,
  i_CoordConverter,
  i_MapCalibration;

type
  TMapCalibrationKml = class(TInterfacedObject, IMapCalibration)
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

{ TMapCalibrationKml }

function TMapCalibrationKml.GetDescription: WideString;
begin
  Result := 'Привязка *.kml для программы Google Earth';
end;

function TMapCalibrationKml.GetName: WideString;
begin
  Result := '.kml';
end;

procedure TMapCalibrationKml.SaveCalibrationInfo(AFileName: WideString;
  xy1, xy2: TPoint; Azoom: byte; AConverter: ICoordConverter);
var
  f: TextFile;
  LL1, LL2: TDoublePoint;
  str: UTF8String;
  VFileName: String;
begin
  assignfile(f, ChangeFileExt(AFileName, '.kml'));
  rewrite(f);
  VFileName := ExtractFileName(AFileName);
  str := ansiToUTF8('<?xml version="1.0" encoding="UTF-8"?>' + #13#10);
  str := str + ansiToUTF8('<kml><GroundOverlay><name>' + VFileName + '</name><color>88ffffff</color><Icon>' + #13#10);
  str := str + ansiToUTF8('<href>' + VFileName + '</href>' + #13#10);
  str := str + ansiToUTF8('<viewBoundScale>0.75</viewBoundScale></Icon><LatLonBox>' + #13#10);
  LL1 := AConverter.PixelPos2LonLat(xy1, Azoom);
  LL2 := AConverter.PixelPos2LonLat(xy2, Azoom);
  str := str + ansiToUTF8('<north>' + R2StrPoint(LL1.y) + '</north>' + #13#10);
  str := str + ansiToUTF8('<south>' + R2StrPoint(LL2.y) + '</south>' + #13#10);
  str := str + ansiToUTF8('<east>' + R2StrPoint(LL2.x) + '</east>' + #13#10);
  str := str + ansiToUTF8('<west>' + R2StrPoint(LL1.x) + '</west>' + #13#10);
  str := str + ansiToUTF8('</LatLonBox></GroundOverlay></kml>');
  writeln(f, str);
  closefile(f);
end;

end.
 