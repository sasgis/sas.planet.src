{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GeoCoderByPolishMap;

interface

uses
  Classes,
  sysutils,
  i_GeoCoder,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_ValueToStringConverter,
  u_GeoCoderLocalBasic;

type
  EGeoCoderERR = class(Exception);
  EDirNotExist = class(EGeoCoderERR);

  TGeoCoderByPolishMap = class(TGeoCoderLocalBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FLock: IReadWriteSync;
  procedure SearchInMapFile(
   const ACancelNotifier: INotifierOperation;
   AOperationID: Integer;
   const AFile : string ;
   const ASearch : widestring;
   const AList : IInterfaceListSimple;
   var Acnt : integer
   );
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
    constructor Create(
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  StrUtils,
  ALFcnString,
  ALStringList,
  t_GeoTypes,
  u_InterfaceListSimple,
  u_ResStrings,
  u_Synchronizer;

{ TGeoCoderByPolishMap }

function getType(
  const ASection: AnsiString;
  const AType:integer
  ):string;
begin
result := '';
if ASection = 'POI' then begin
case AType of
$0100: result := 'Мегаполис (свыше 10 млн.)';
$0200: result := 'Мегаполис (5-10 млн.)';
$0300: result := 'Крупный город (2-5 млн.)';
$0400: result := 'Крупный город (1-2 млн.)';
$0500: result := 'Крупный город (0.5-1 млн.)';
$0600: result := 'Город (200-500 тыс.)';
$0700: result := 'Город (100-200 тыс.)';
$0800: result := 'Город (50-100 тыс.)';
$0900: result := 'Город (20-50 тыс.)';
$0a00: result := 'Город (10-20 тыс.)';
$0b00: result := 'Населённый пункт (5-10 тыс.)';
$0c00: result := 'Населённый пункт (2-5 тыс.)';
$0d00: result := 'Населённый пункт (1-2 тыс.)';
$0e00: result := 'Поселок (500-1000)';
$0f00: result := 'Поселок (200-500)';
$1000: result := 'Поселок (100-200)';
$1100: result := 'Поселок (менее 100)';
$1200: result := 'Пристань с инфраструктурой';
$1400: result := 'Название крупного государства';
$1500: result := 'Название малого государства';
$1e00: result := 'Название области, провинции, штата';
$1f00: result := 'Название района, округа';
$2800: result := 'Надпись';
$1c00: result := 'Водная преграда';
$1c01: result := 'Обломки кораблекрушения';
$1c02: result := 'Затопленные обломки, опасные';
$1c03: result := 'Затопленные обломки, не опасные';
$1c04: result := 'Обломки, очищенные драгой';
$1c05: result := 'Преграда, видимая при высокой воде';
$1c06: result := 'Преграда на уровне воды';
$1c07: result := 'Преграда ниже уровня воды';
$1c08: result := 'Преграда, очищенная драгой';
$1c09: result := 'Рифы на уровне воды';
$1c0a: result := 'Подводные рифы';
$1c0b: result := 'Мель';
$1d00: result := 'Расписание приливов';
$2000: result := 'Съезд с шоссе';
$2100: result := 'Съезд с шоссе с инфраструктурой';
$210F: result := 'Съезд к сервису';
$2200: result := 'Съезд к туалету';
$2300: result := 'Съезд к магазину';
$2400: result := 'Съезд к весовой станции';
$2500: result := 'Съезд платный';
$2600: result := 'Съезд к справочной';
$2700: result := 'Съезд с шоссе';
$2900: result := 'Прочие организации';
$2a00: result := 'Предприятие питания';
$2a01: result := 'Ресторан (американская кухня)';
$2a02: result := 'Ресторан (азиатская кухня)';
$2a03: result := 'Ресторан (шашлык)';
$2a04: result := 'Ресторан (китайская кухня)';
$2a05: result := 'Ресторан (деликатесы, торты, пирожные)';
$2a06: result := 'Ресторан (интернациональная кухня)';
$2a07: result := 'Ресторан быстрого питания';
$2a08: result := 'Ресторан (итальянская кухня)';
$2a09: result := 'Ресторан (мексиканская кухня)';
$2a0a: result := 'Пиццерия';
$2a0b: result := 'Ресторан (морепродукты)';
$2a0c: result := 'Ресторан (гриль)';
$2a0d: result := 'Общепит (кондитерские изделия)';
$2a0e: result := 'Кафе';
$2a0f: result := 'Ресторан (французская кухня)';
$2a10: result := 'Ресторан (немецкая кухня)';
$2a11: result := 'Ресторан (британская островная кухня)';
$2a12: result := 'Специальные пищевые продукты';
$2b00: result := 'Гостиница';
$2b01: result := 'Отель или мотель';
$2b02: result := 'Отель с завтраком';
$2b03: result := 'Кемпинг';
$2b04: result := 'Курортный отель, дом отдыха';
$2c00: result := 'Объект культуры, досуга';
$2c01: result := 'ПКиО';
$2c02: result := 'Музей';
$2c03: result := 'Библиотека';
$2c04: result := 'Достопримечательность';
$2c05: result := 'Школа';
$2c06: result := 'Парк/Сад';
$2c07: result := 'Зоопарк/Аквариум';
$2c08: result := 'Стадион';
$2c09: result := 'Зал';
$2c0a: result := 'Дегустация вин';
$2c0b: result := 'Храм/Мечеть/Синагога';
$2d00: result := 'Развлекательное заведение';
$2d01: result := 'Театр';
$2d02: result := 'Бар/Ночной клуб';
$2d03: result := 'Кинотеатр';
$2d04: result := 'Казино';
$2d05: result := 'Гольф-клуб';
$2d06: result := 'Лыжный центр/курорт';
$2d07: result := 'Боулинг-центр';
$2d08: result := 'Каток';
$2d09: result := 'Бассейн';
$2d0a: result := 'Спортзал/Фитнес-центр';
$2d0b: result := 'Спортивный аэродром';
$2e00: result := 'Торговый объект';
$2e01: result := 'Универмаг';
$2e02: result := 'Продовольственный магазин';
$2e03: result := 'Торговая фирма';
$2e04: result := 'Торговый центр';
$2e05: result := 'Аптека';
$2e06: result := 'Товары повседневного спроса';
$2e07: result := 'Одежда';
$2e08: result := 'Товары для дома и сада';
$2e09: result := 'Мебель';
$2e0a: result := 'Специализированный магазин';
$2e0b: result := 'Компьютеры/ПО';
$2f00: result := 'Услуги';
$2f01: result := 'АЗС';
$2f02: result := 'Аренда автомобилей';
$2f03: result := 'Автосервис';
$2f04: result := 'Аэровокзал';
$2f05: result := 'Почтовое отделение';
$2f06: result := 'Банк';
$2f07: result := 'Автомагазин';
$2f08: result := 'Станция/остановка наземного транспорта';
$2f09: result := 'Ремонт лодок, катеров, яхт';
$2f0a: result := 'Аварийная служба, техпомощь';
$2f0b: result := 'Автостоянка';
$2f0c: result := 'Зона отдыха, информация для туристов';
$2f0d: result := 'Автоклуб';
$2f0e: result := 'Автомойка';
$2f0f: result := 'Дилер фирмы Garmin';
$2f10: result := 'Служба быта (прачечная, химчистка)';
$2f11: result := 'Бизнес-сервис';
$2f12: result := 'Пункт связи';
$2f13: result := 'Бюро ремонта';
$2f14: result := 'Собес';
$2f15: result := 'Коммунальные службы';
$2f16: result := 'Стоянка грузовиков';
$2f17: result := 'Остановка общественного транспорта';
$3000: result := 'Государственная или экстренная служба';
$3001: result := 'Отделение милиции';
$3002: result := 'Больница';
$3003: result := 'Мэрия';
$3004: result := 'Суд';
$3005: result := 'Помещение для проведения общественных мероприятий';
$3006: result := 'Пограничный пункт';
$3007: result := 'Государственное учреждение';
$3008: result := 'Пожарная часть';
$4000: result := 'Гольф-клуб';
$4100: result := 'Место для рыбалки';
$4200: result := 'Обломки, развалины';
$4300: result := 'Пристань для яхт';
$4400: result := 'АЗС';
$4500: result := 'Ресторан';
$4600: result := 'Бар';
$4700: result := 'Лодочный причал';
$4800: result := 'Кемпинг';
$4900: result := 'Парк';
$4a00: result := 'Место для пикника';
$4b00: result := 'Медпункт';
$4c00: result := 'Справочная';
$4d00: result := 'Автостоянка';
$4e00: result := 'Туалет';
$4f00: result := 'Душ';
$5000: result := 'Питьевая вода';
$5100: result := 'Телефон';
$5200: result := 'Красивый вид';
$5300: result := 'Лыжная база';
$5400: result := 'Место для купания';
$5500: result := 'Дамба, плотина';
$5600: result := 'Запретная зона';
$5700: result := 'Опасная зона';
$5800: result := 'Ограниченный доступ';
$5900: result := 'Аэропорт';
$5901: result := 'Крупный аэропорт';
$5902: result := 'Средний аэропорт';
$5903: result := 'Малый аэропорт';
$5904: result := 'Вертолетная площадка';
$5905: result := 'Аэропорт';
$5a00: result := 'Километровый столб';
$5b00: result := 'Колокол';
$5c00: result := 'Место для дайвинга';
$5d00: result := 'Дневной знак (зёленый квадрат)';
$5e00: result := 'Дневной знак (красный треугольник)';
$6000: result := 'Громкоговоритель';
$6100: result := 'Дом';
$6200: result := 'Отметка глубины';
$6300: result := 'Отметка высоты';
$6400: result := 'Искусственное сооружение';
$6401: result := 'Мост';
$6402: result := 'Здание';
$6403: result := 'Кладбище';
$6404: result := 'Храм/Мечеть/Синагога';
$6405: result := 'Общественное здание';
$6406: result := 'Перекресток, переправа, перевал';
$6407: result := 'Плотина';
$6408: result := 'Больница';
$6409: result := 'Плотина, набережная';
$640a: result := 'Указатель';
$640b: result := 'Военный объект';
$640c: result := 'Шахта, рудник';
$640d: result := 'Месторождение нефти';
$640e: result := 'Парк';
$640f: result := 'Почта';
$6410: result := 'Школа';
$6411: result := 'Башня, вышка';
$6412: result := 'Начало тропы';
$6413: result := 'Начало/окончание тоннеля';
$6414: result := 'Питьевая вода, родник, колодец';
$6415: result := 'Заброшенное жилье';
$6416: result := 'Пристройка';
$6500: result := 'Объект гидрографии';
$6501: result := 'Арройо, высохшее русло';
$6502: result := 'Песчаная отмель';
$6503: result := 'Залив';
$6504: result := 'Излучина реки';
$6505: result := 'Искусственный канал';
$6506: result := 'Пролив';
$6507: result := 'Бухта';
$6508: result := 'Водопад';
$6509: result := 'Гейзер';
$650a: result := 'Ледник';
$650b: result := 'Гавань';
$650c: result := 'Остров';
$650d: result := 'Озеро';
$650e: result := 'Пороги';
$650f: result := 'Водохранилище';
$6510: result := 'Море';
$6511: result := 'Родник';
$6512: result := 'Ручей';
$6513: result := 'Болото';
$6600: result := 'Природный наземный объект';
$6601: result := 'Арка';
$6602: result := 'Район, область';
$6603: result := 'Котловина';
$6604: result := 'Берег';
$6605: result := 'Карниз, уступ';
$6606: result := 'Мыс';
$6607: result := 'Утес';
$6608: result := 'Кратер';
$6609: result := 'Плато';
$660a: result := 'Лес';
$660b: result := 'Ущелье, пропасть';
$660c: result := 'Узкий проход';
$660d: result := 'Перешеек';
$660e: result := 'Лава';
$660f: result := 'Столб, колонна';
$6610: result := 'Равнина';
$6611: result := 'Гряда';
$6612: result := 'Заповедник';
$6613: result := 'Хребет';
$6614: result := 'Скала';
$6615: result := 'Склон';
$6616: result := 'Вершина холма или горы';
$6617: result := 'Долина';
$6618: result := 'Лес';
$1600: result := 'Маяк';
$1601: result := 'Туманный ревун';
$1602: result := 'Радиомаяк';
$1603: result := 'Радиобуй';
$1604: result := 'Дневной маяк (красный треугольник)';
$1605: result := 'Дневной маяк (зелёный квадрат)';
$1606: result := 'Дневной маяк (белый ромб)';
$1607: result := 'Несветящийся маяк белый';
$1608: result := 'Несветящийся маяк красный';
$1609: result := 'Несветящийся маяк зелёный';
$160a: result := 'Несветящийся маяк черный';
$160b: result := 'Несветящийся маяк желтый';
$160c: result := 'Несветящийся маяк оражевый';
$160d: result := 'Несветящийся маяк многоцветный';
$160e: result := 'Светящийся маяк';
$160f: result := 'Светящийся маяк белый';
$1610: result := 'Светящийся маяк красный';
$1611: result := 'Светящийся маяк зелёный';
$1612: result := 'Светящийся маяк желтый';
$1613: result := 'Светящийся маяк оранжевый';
$1614: result := 'Светящийся маяк фиолетовый';
$1615: result := 'Светящийся маяк синий';
$1616: result := 'Светящийся маяк многоцветный';
 else result := '{Poi=0x'+IntToHex(AType,4)+'}';
end;
   end else
if ASection = 'POLYGON' then begin
 case AType of
 $01: result := 'Городcкая застройка (>200 тж)';
 $02: result := 'Городcкая застройка (<200 тж)';
 $03: result := 'Застройка сельского типа';
 $04: result := 'Военная база';
 $05: result := 'Автостоянка';
 $06: result := 'Гаражи';
 $07: result := 'Аэропорт';
 $08: result := 'Место для торговли';
 $09: result := 'Пристань';
 $0a: result := 'Территория университета или колледжа';
 $0b: result := 'Больница';
 $0c: result := 'Промышленная зона';
 $0d: result := 'Резервация, заповедник';
 $0e: result := 'Взлетно-посадочная полоса';
 $13: result := 'Здание, искусственное сооружение';
 $14: result := 'Национальный парк';
 $15: result := 'Национальный парк';
 $16: result := 'Национальный парк';
 $17: result := 'Городской парк';
 $18: result := 'Поле для гольфа';
 $19: result := 'Спортивный комплекс';
 $1a: result := 'Кладбище';
 $1e: result := 'Государственный парк';
 $1f: result := 'Государственный парк';
 $20: result := 'Государственный парк';
 $28: result := 'Море/океан';
 $29: result := 'Водоём';
 $32: result := 'Море';
 $3b: result := 'Водоём';
 $3c: result := 'Озеро большое (250-600 км2)';
 $3d: result := 'Озеро большое (77-250 км2)';
 $3e: result := 'Озеро среднее (25-77 км2)';
 $3f: result := 'Озеро среднее (11-25 км2)';
 $40: result := 'Озеро малое (0.25-11 км2)';
 $41: result := 'Озеро малое (<0.25 км2)';
 $42: result := 'Озеро крупное (>3.3 т.км2)';
 $43: result := 'Озеро крупное (1.1-3.3 т.км2)';
 $44: result := 'Озеро большое (0.6-1.1 т.км2)';
 $45: result := 'Водоём';
 $46: result := 'Река крупная (>1 км)';
 $47: result := 'Река большая (200 м-1 км)';
 $48: result := 'Река средняя (40-200 м)';
 $49: result := 'Река малая (<40 м)';
 $4a: result := 'Область выделения карты';
 $4b: result := 'Область покрытия карты';
 $4c: result := 'Пересыхающая река или озеро';
 $4d: result := 'Ледник';
 $4e: result := 'Фруктовый сад или огород';
 $4f: result := 'Кустарник';
 $50: result := 'Лес';
 $51: result := 'Болото';
 $52: result := 'Тундра';
 $53: result := 'Отмель';
 else result := '{Polygon=0x'+IntToHex(AType,4)+'}';
 end end else
if ASection = 'POLYLINE' then begin
 case AType of
 $00: result := 'Дорога';
 $01: result := 'Автомагистраль';
 $02: result := 'Шоссе основное';
 $03: result := 'Прочие загородные дороги';
 $04: result := 'Городская магистраль';
 $05: result := 'Улица крупная';
 $06: result := 'Улица малая';
 $07: result := 'Переулок, внутриквартальный проезд';
 $08: result := 'Наклонный съезд с путепровода';
 $09: result := 'Наклонный съезд с путепровода скоростной';
 $0a: result := 'Грунтовая дорога';
 $0b: result := 'Соединительное шоссе';
 $0c: result := 'Круговое движение';
 $16: result := 'Аллея, тропа';
 $14: result := 'Железная дорога';
 $1a: result := 'Паром';
 $1b: result := 'Паром';
 $19: result := 'Граница часового пояса';
 $1e: result := 'Международная граница';
 $1c: result := 'Граница области';
 $1d: result := 'Граница района, округа';
 $15: result := 'Береговая линия';
 $18: result := 'Ручей';
 $1f: result := 'Река';
 $26: result := 'Пересыхающая река, ручей или канава';
 $20: result := 'Горизонталь вспомогательная';
 $21: result := 'Горизонталь основная';
 $22: result := 'Горизонталь утолщённая';
 $23: result := 'Изобата вспомогательная';
 $24: result := 'Изобата основная';
 $25: result := 'Изобата утолщённая';
 $27: result := 'Взлетно-посадочная полоса';
 $28: result := 'Трубопровод';
 $29: result := 'Линия электропередачи';
 $2a: result := 'Морская граница';
 $2b: result := 'Морская опасность';
 else result := '{PolyLine=0x'+IntToHex(AType,4)+'}';
 end;

end;
end;

function ItemExist(
  const AValue: IGeoCodePlacemark;
  const AList: IInterfaceListSimple
):boolean;
var
  i: Integer;
  VPlacemark: IGeoCodePlacemark;
  j : integer;
  str1,str2 : string;
begin
  Result := false;
  for i := 0 to AList.Count - 1 do begin
    VPlacemark := IGeoCodePlacemark(AList.Items[i]);
    j:= posex(')',VPlacemark.Name);
    str1 := copy(VPlacemark.Name,j,length(VPlacemark.Name)-(j+1));
    j:= posex(')',AValue.Name);
    str2 := copy(AValue.Name,j,length(AValue.Name)-(j+1));
    if str1=str2 then begin
      if
        abs(VPlacemark.GetPoint.Point.x-AValue.GetPoint.Point.x) +
        abs(VPlacemark.GetPoint.Point.Y-AValue.GetPoint.Point.Y) < 0.05
      then begin
        Result := true;
        Break;
      end;
    end;
  end;
end;

function GetParam(
  const AName: AnsiString;
  const AStr: AnsiString;
  const ADef: AnsiString = ''
):AnsiString;
var
  i, j: integer;
begin
  result := ADef;
  i := ALPosEx(AName, AStr);
  if (i = 0) then exit;
  j := ALPosEx(#$A, AStr, i+1);
  if (j = 0) then j := length(AStr) + 1;
  if (j > 1) and (AStr[j - 1] = #$D) then dec(j); // учёт второго варианта конца строки
  result := Copy(AStr, i + length(AName) , j - (i + length(AName)));
end;

procedure TGeoCoderByPolishMap.SearchInMapFile(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AFile : string ;
  const ASearch : widestring;
  const AList : IInterfaceListSimple;
  var Acnt : integer
  );
var
 VFormatSettings : TALFormatSettings;
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 slat, slon: AnsiString;
 sname, sdesc, sfulldesc : string;
 VLinkErr : boolean;
 Vi, i, j, k, l: integer;
 VStr: AnsiString;
 vStr2: AnsiString;
 VSearch : AnsiString;
 VCityList : TALStringList;

 V_SectionType,
 V_Label,
 V_CityName,
 V_RegionName,
 V_StreetDesc,
 V_HouseNumber,
 V_CountryName,
 V_WebPage,
 V_Phone : AnsiString;
 V_Type : integer;
 Skip: boolean;
 VStream: TFileStream;
 V_EndOfLine : AnsiString;
 VValueConverter: IValueToStringConverter;
begin
 VFormatSettings.DecimalSeparator := '.';
 VSearch := AnsiString(AnsiUpperCase(ASearch));
 FLock.BeginRead;
 try
  VStream := TFileStream.Create(AFile, fmOpenRead);
   try
    SetLength(Vstr,VStream.Size);
    i := VStream.Size;
    VStream.ReadBuffer(VStr[1],VStream.Size);
   finally
   VStream.Free;
   end;
  finally
  FLock.EndRead;
 end;
  if i < 10 then exit; // файл слишком маленький
  i := ALPosEx(#$A, VStr, 2); // вдруг в самом начале файла пустая строка и лишь с #$A? её пропустим и найдём конец следующей
  if (i > 1) and (VStr[i - 1] = #$D) then V_EndOfLine := #$D#$A else V_EndOfLine := #$A;
  VCityList := TALStringList.Create;
  try
    Vstr := AlUpperCase(Vstr);
    i := ALPosEx('[CITIES]', VStr);
    if i > 0 then begin
      k :=  ALPosEx('[END', VStr, i);
      VStr2 := copy(VStr, i, k - i); // вырежем весь первый блок
      while (ALPosEx('CITY', VStr2, i) > 0) do begin
        j := i;
        i := ALPosEx('CITY', VStr2, j);
        i := ALPosEx('=', VStr2, i);
        j := ALPosEx(V_EndOfLine, VStr2, i);
        VCityList.add(Copy(VStr2, i + 1, j - (i + 1)));
      end;// заполнили массив городов, если они заданы в начале файла
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    end;
    Vi := i + 1;
    // ищем вхождение, затем бежим назад до начала блока
    while true do begin
      VLinkErr := false;
      Vi := ALPosEx(VSearch, VStr, Vi) + 1;
      if Vi = 1 then break; // больше не нашли?
      j := Vi - 1;
      while (j > 0) and (Vstr[j] <> #$A) do dec(j); // начало строки с найденными данными
      if Vi < j + 7 then continue; // нашлось в имени тега, пропустим
      if copy(Vstr, j + 1, 6) <> 'LABEL=' then continue; // найденные данные не в теге Label?
      k := ALPosEx(#$A'[END]', VStr, Vi); // конец блока найденных данных.
      l := j;
      while (l > 0) and (copy(Vstr, l, 2) <> #$A'[') do dec(l); // начало блока с найденными данными
      VStr2 := copy(VStr, l + 1, k - l); // вырежем весь блок с найденными данными

      V_Label := '';
      V_CityName := '';
      V_RegionName := '';
      V_StreetDesc := '';
      V_HouseNumber := '';
      V_CountryName := '';
      V_WebPage := '';
      V_Phone :='';

      i := ALPosEx(']', VStr2);
      if i>0 then begin
       if i<k then begin
         V_SectionType := Copy(Vstr2, 2, i - 2);
        end else
         V_SectionType := '';
      end;

      V_Label := GetParam(#$A'LABEL=',vStr2);
      V_Label := ALStringReplace(V_Label,'~[0X1F]',' ', [rfReplaceAll]);
      i := ALStrToIntDef(GetParam(#$A'CITYIDX=',vStr2),-1);
      if (i > 0) and (i < VCityList.Count) then
          V_CityName := VCityList.Strings[i - 1] else  V_CityName := '';
      V_CityName := GetParam(#$A'CITYNAME=',vStr2,V_CityName);
      V_RegionName := GetParam(#$A'REGIONNAME=',vStr2);
      V_CountryName := GetParam(#$A'COUNTRYNAME=',vStr2);
      V_Type := ALStrToIntDef(GetParam(#$A'TYPE=',VStr2), -1);
      V_StreetDesc := GetParam(#$A'STREETDESC=',vStr2);
      V_HouseNumber := GetParam(#$A'HOUSENUMBER=',vStr2);
      V_WebPage := GetParam(#$A'WEBPAGE=',vStr2,'');
      V_Phone := GetParam(#$A'PHONE=',vStr2,'');

      i := ALPosEx('DATA', VStr2);
      i := ALPosEx('(', VStr2, i);
      j := ALPosEx(',', VStr2, i);
      slat := Copy(Vstr2, i + 1, j - (i + 1));
      i := ALPosEx(')', VStr2, i);
      slon := Copy(Vstr2, j + 1, i - (j + 1));

      if (slat='') or (slon='') then VLinkErr := true;
      if V_Label='' then VLinkErr := true;
      if Acnt mod 5 =0 then
       if ACancelNotifier.IsOperationCanceled(AOperationID) then
         Exit;

      if VLinkErr <> true then begin
       try
         VPoint.Y := ALStrToFloat(slat, VFormatSettings);
         VPoint.X := ALStrToFloat(slon, VFormatSettings);
       except
         raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
       end;
        sdesc := '';
        sname := string(V_Label);
        if V_CountryName <> '' then sdesc := sdesc + string(V_CountryName) + ', ';
        if V_RegionName <> '' then sdesc := sdesc + string(V_RegionName);
        if sdesc <> '' then sdesc := sdesc + #$D#$A;

        if V_HouseNumber <>'' then
          if sname='' then  sname := string(V_StreetDesc)+' №'+string(V_HouseNumber)
            else sdesc := string(V_StreetDesc)+' №'+string(V_HouseNumber) + #$D#$A + sdesc;
        if V_Type<> -1 then sdesc := getType(V_SectionType,V_Type)+ #$D#$A + sdesc;
        if V_CityName <> '' then sdesc := sdesc +  string(V_CityName) + #$D#$A;
        if V_Phone<>'' then sdesc := sdesc +'Phone '+ string(V_Phone) + #$D#$A;
        VValueConverter := FValueToStringConverterConfig.GetStatic;
        sdesc := sdesc + '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
        sdesc := sdesc + #$D#$A + ExtractFileName(AFile);
        sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
        if V_WebPage<>'' then sfulldesc := sfulldesc + '<br><a href=' + string(V_WebPage) + '>' + string(V_WebPage) + '</a>';
        VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);

        // если закометировать условие то не будет производиться фильтрация одинаковых элементов
        skip := ItemExist(Vplace,AList);
        if not skip then
         begin
          inc(Acnt);
          AList.Add(VPlace);
         end;
      end;
    end;
  finally
    VCityList.Free;
  end;
end;

constructor TGeoCoderByPolishMap.Create(
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(APlacemarkFactory);
  if not DirectoryExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\mp')) then
    raise EDirNotExist.Create('not found .\userdata\mp\! skip GeoCoderByPolishMap');
  FLock := MakeSyncRW_Std(Self, False);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function TGeoCoderByPolishMap.DoSearch(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
  ): IInterfaceListSimple;
var
VList: IInterfaceListSimple;
vpath : string;
Vcnt : integer;
VFolder: string;
SearchRec: TSearchRec;
MySearch : string;
begin
 Vcnt := 1;
 MySearch := ASearch;
 while PosEx('  ',MySearch)>0 do MySearch := ReplaceStr(MySearch,'  ',' ');
 VList := TInterfaceListSimple.Create;
 VFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\mp\');
 if FindFirst(VFolder + '*.mp', faAnyFile, SearchRec) = 0 then begin
  repeat
    if (SearchRec.Attr and faDirectory) = faDirectory then begin
      continue;
    end;
    vpath:= VFolder+SearchRec.Name;
    SearchInMapFile(ACancelNotifier, AOperationID, Vpath , MySearch, vlist , Vcnt);
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
  until FindNext(SearchRec) <> 0;
 end;
 Result := VList;
end;
end.
