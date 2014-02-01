{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_GeoCoderByNavitel;

interface

uses
  Classes,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_DownloadRequest,
  i_DownloadResult,
  u_GeoCoderBasic;

type
  TGeoCoderByNavitel = class(TGeoCoderBasic)
  protected
    function PrepareRequest(
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; override;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
  end;

implementation

uses
  SysUtils,
  ALString,
  RegExprUtils,
  t_GeoTypes,
  i_GeoCoder,
  i_VectorDataItemSimple,
  i_CoordConverter,
  u_InterfaceListSimple,
  u_ResStrings,
  u_GeoToStrFunc;

{ TGeoCoderByNavitel }

function NavitelType(VType: integer): string;
begin
case VType of
  1: Result := '';
  2: Result := 'ул. ';
  3: Result := 'дорога ';
  5: Result := 'пер. ';
  6: Result := 'пр-д ';
  7: Result := 'пр-кт ';
  8: Result := 'шоссе ';
  9: Result := 'набережная ';
  10: Result := 'Набережная ';
  11: Result := 'автодорога ';
  13: Result := 'магистраль ';
  18: Result := 'Магистраль ';
  20: Result := 'Дорога ';
  24: Result := 'Шоссе ';
  27: Result := 'Автострада ';
  30: Result := 'с. ';
  31: Result := 'п. ';
  32: Result := 'д. ';
  33: Result := 'г. ';
  34: Result := 'р-н ';
  35: Result := 'р.п. ';
  36: Result := '';
  37: Result := 'волость ';
  38: Result := 'х. ';
  39: Result := 'к.п. ';
  40: Result := 'пгт. ';
  41: Result := 'с. ';
  42: Result := 'ж/р ';
  43: Result := '';
  44: Result := 'с. ';
  45: Result := '';
  46: Result := 'мрн ';
  47: Result := 'с. ';
  48: Result := 'а. ';
  49: Result := 'у. ';
  50: Result := '';
  51: Result := 'ж/с ';
  52: Result := 'р.п. ';
  53: Result := 'с.п. ';
  54: Result := 'р. ';
  55: Result := 'в. ';
  56: Result := 'м. ';
  57: Result := 'д.п. ';
  60: Result := 'у. ';
  81: Result := 'ул. ';
  80: Result := 'ул. ';
  82: Result := 'ул. ';
  86: Result := 'ул. ';
  88: Result := 'ул. ';
  29: Result := 'ул. ';
  15: Result := 'пр-кт ';
  84: Result := 'пр-кт ';
  85: Result := 'пр-кт ';
  12: Result := 'пр-д ';
  87: Result := 'пр-д ';
  83: Result := 'Набережная ';
  58: Result := 'Респ. ';
  59: Result := 'обл. ';
  61: Result := 'пгт. ';
  62: Result := 'с.о. ';
  63: Result := 'п. ';
  64: Result := '';
  65: Result := 'а. ';
  66: Result := 'край ';
  67: Result := 'терр. ';
  68: Result := 'з. ';
  69: Result := 'а.о. ';
  70: Result := '';
  71: Result := 'г. ';
  72: Result := 'г. ';
  73: Result := '';
  74: Result := '';
  75: Result := 'п.р. ';
  76: Result := 'о. ';
  77: Result := 'д.п. ';
  78: Result := 'а.обл. ';
  79: Result := 'п/з ';
  89: Result := 'Автобусная станция ';
  90: Result := 'Остановка автобуса ';
  91: Result := 'Остановка трамвая ';
  92: Result := 'Остановка троллейбуса ';
  93: Result := 'Станция метро ';
  94: Result := 'Железнодорожная станция или платформа ';
  95: Result := 'Железнодорожный вокзал ';
  96: Result := 'Станция монорельсовой дороги ';
  97: Result := 'Остановка фуникулёра ';
  98: Result := 'Порт ';
  99: Result := 'Пристань ';
  100: Result := 'Причал парома ';
  101: Result := 'Православный храм ';
  102: Result := 'Католический храм ';
  103: Result := 'Протестантский храм ';
  104: Result := 'Мечеть ';
  105: Result := 'Синагога ';
  106: Result := 'Пагода ';
  107: Result := 'Светофор ';
  108: Result := 'Пост ГАИ ';
  109: Result := 'Ж/д переезд со шлагбаумом ';
  110: Result := 'Радар ';
  111: Result := 'АГЗС ';
  112: Result := 'Шиномонтаж ';
  113: Result := 'Сход-развал ';
  114: Result := 'Ж/д переезд без шлагбаума ';
  115: Result := 'Дошкольное учреждение ';
  116: Result := 'Средняя школа ';
  117: Result := 'Профессиональное училище ';
  118: Result := 'Высшее учебное заведение ';
  119: Result := 'Специальное учебное заведение ';
  120: Result := 'Банкомат ';
  121: Result := 'Парикмахерская ';
  122: Result := 'Баня: Result := сауна ';
  123: Result := 'Химчистка: Result := прачечная ';
  124: Result := 'Фотография ';
  125: Result := 'Ателье ';
  126: Result := 'Ремонт бытовой техники ';
  127: Result := 'Ремонт компьютеров ';
  128: Result := 'Ремонт одежды ';
  129: Result := 'Ремонт обуви ';
  130: Result := 'Ремонт мебели ';
  131: Result := 'Юридическая контора ';
  132: Result := 'Нотариус ';
  187: Result := 'Школа ';
  133: Result := 'Страховая компания ';
  135: Result := 'Справочная служба ';
  137: Result := 'Типография ';
  139: Result := 'Авиакасса: Result := ж/д касса ';
  141: Result := 'Интернет-кафе ';
  143: Result := 'Обувной магазин ';
  145: Result := 'Магазин бытовой химии ';
  134: Result := 'Агентство недвижимости ';
  136: Result := 'Редакция ';
  138: Result := 'Турагентство ';
  140: Result := 'Бесплатная точка доступа Wi-Fi ';
  142: Result := 'Рынок ';
  144: Result := 'Промтоварный магазин ';
  146: Result := 'Магазин бытовой техники ';
  147: Result := 'Магазин мобильной техники ';
  148: Result := 'Спортивные и туристические товары ';
  149: Result := 'Оружейный магазин ';
  150: Result := 'Магазин подарков ';
  151: Result := 'Цветочный магазин ';
  152: Result := 'Ювелирный магазин ';
  153: Result := 'Зоомагазин ';
  154: Result := 'Стоматология ';
  155: Result := 'Ветеринарная клиника ';
  156: Result := 'Цирк ';
  157: Result := 'Пристань с инфраструктурой ';
  158: Result := 'Предприятие питания ';
  159: Result := 'Ресторан (американская кухня) ';
  160: Result := 'Ресторан (азиатская кухня) ';
  161: Result := 'Ресторан (шашлык) ';
  162: Result := 'Ресторан (китайская кухня) ';
  163: Result := 'Ресторан (деликатесы: Result := торты: Result := пирожные) ';
  164: Result := 'Ресторан (интернациональная кухня) ';
  165: Result := 'Ресторан быстрого питания ';
  166: Result := 'Ресторан (итальянская кухня) ';
  167: Result := 'Ресторан (мексиканская кухня) ';
  168: Result := 'Пиццерия ';
  169: Result := 'Ресторан (морепродукты) ';
  170: Result := 'Ресторан (гриль) ';
  171: Result := 'Общепит (кондитерские изделия) ';
  172: Result := 'Кафе ';
  173: Result := 'Ресторан (французская кухня) ';
  174: Result := 'Ресторан (немецкая кухня) ';
  175: Result := 'Ресторан (британская островная кухня) ';
  176: Result := 'Специальные пищевые продукты ';
  177: Result := 'Гостиница ';
  178: Result := 'Отель или мотель ';
  179: Result := 'Отель с завтраком ';
  180: Result := 'Кемпинг ';
  181: Result := 'Курортный отель: Result := дом отдыха ';
  182: Result := 'Объект культуры: Result := досуга ';
  183: Result := 'ПКиО ';
  184: Result := 'Музей ';
  185: Result := 'Библиотека ';
  186: Result := 'Достопримечательность ';
  188: Result := 'Парк/Сад ';
  190: Result := 'Стадион ';
  192: Result := 'Дегустация вин ';
  194: Result := 'Развлекательное заведение ';
  196: Result := 'Бар/Ночной клуб ';
  198: Result := 'Казино ';
  200: Result := 'Лыжный центр/курорт ';
  202: Result := 'Каток ';
  189: Result := 'Зоопарк/Аквариум ';
  191: Result := 'Зал ';
  193: Result := 'Храм/Мечеть/Синагога ';
  195: Result := 'Театр ';
  197: Result := 'Кинотеатр ';
  199: Result := 'Гольф-клуб ';
  201: Result := 'Боулинг-центр ';
  203: Result := 'Бассейн ';
  204: Result := 'Спортзал/Фитнес-центр ';
  205: Result := 'Спортивный аэродром ';
  206: Result := 'Торговый объект ';
  207: Result := 'Универмаг ';
  208: Result := 'Продовольственный магазин ';
  209: Result := 'Торговая фирма ';
  210: Result := 'Торговый центр ';
  211: Result := 'Аптека ';
  212: Result := 'Товары повседневного спроса ';
  213: Result := 'Одежда ';
  214: Result := 'Товары для дома и сада ';
  215: Result := 'Мебель ';
  216: Result := 'Специализированный магазин ';
  217: Result := 'Компьютеры/ПО ';
  218: Result := 'Услуги ';
  219: Result := 'АЗС ';
  220: Result := 'Аренда автомобилей ';
  221: Result := 'Автосервис ';
  222: Result := 'Аэровокзал ';
  223: Result := 'Почтовое отделение ';
  224: Result := 'Банк ';
  225: Result := 'Автомагазин ';
  226: Result := 'Станция/остановка наземного транспорта ';
  227: Result := 'Ремонт лодок: Result := катеров: Result := яхт ';
  228: Result := 'Аварийная служба: Result := техпомощь ';
  229: Result := 'Автостоянка ';
  230: Result := 'Зона отдыха: Result := информация для туристов ';
  231: Result := 'Автоклуб ';
  232: Result := 'Автомойка ';
  233: Result := 'Дилер фирмы Garmin ';
  234: Result := 'Служба быта (прачечная: Result := химчистка) ';
  235: Result := 'Бизнес-сервис ';
  236: Result := 'Пункт связи ';
  237: Result := 'Бюро ремонта ';
  238: Result := 'Собес ';
  239: Result := 'Коммунальные службы ';
  240: Result := 'Стоянка грузовиков ';
  241: Result := 'Остановка общественного транспорта ';
  242: Result := 'Государственная или экстренная служба ';
  243: Result := 'Отделение милиции ';
  244: Result := 'Больница ';
  245: Result := 'Мэрия ';
  246: Result := 'Суд ';
  247: Result := 'Помещение для проведения общественных мероприятий ';
  248: Result := 'Пограничный пункт ';
  249: Result := 'Государственное учреждение ';
  251: Result := 'Гольф-клуб ';
  253: Result := 'Обломки: Result := развалины ';
  255: Result := 'АЗС ';
  257: Result := 'Бар ';
  259: Result := 'Кемпинг ';
  261: Result := 'Место для пикника ';
  263: Result := 'Справочная ';
  265: Result := 'Туалет ';
  267: Result := 'Питьевая вода ';
  269: Result := 'Красивый вид ';
  250: Result := 'Пожарная часть ';
  252: Result := 'Место для рыбалки ';
  254: Result := 'Пристань для яхт ';
  256: Result := 'Ресторан ';
  258: Result := 'Лодочный причал ';
  260: Result := 'Парк ';
  262: Result := 'Медпункт ';
  264: Result := 'Автостоянка ';
  266: Result := 'Душ ';
  268: Result := 'Телефон ';
  270: Result := 'Лыжная база ';
  271: Result := 'Место для купания ';
  272: Result := 'Дамба: Result := плотина ';
  273: Result := 'Запретная зона ';
  274: Result := 'Опасная зона ';
  275: Result := 'Ограниченный доступ ';
  276: Result := 'Аэропорт ';
  277: Result := 'Крупный аэропорт ';
  278: Result := 'Средний аэропорт ';
  279: Result := 'Малый аэропорт ';
  280: Result := 'Вертолетная площадка ';
  281: Result := 'Аэропорт ';
  282: Result := 'Километровый столб ';
  283: Result := 'Колокол ';
  284: Result := 'Место для дайвинга ';
  285: Result := 'Дневной знак (зёленый квадрат) ';
  286: Result := 'Дневной знак (красный треугольник) ';
  287: Result := 'Громкоговоритель ';
  288: Result := 'Дом ';
  289: Result := 'Отметка глубины ';
  290: Result := 'Отметка высоты ';
  291: Result := 'Искусственное сооружение ';
  292: Result := 'Мост ';
  293: Result := 'Здание ';
  294: Result := 'Кладбище ';
  295: Result := 'Храм/Мечеть/Синагога ';
  296: Result := 'Общественное здание ';
  297: Result := 'Перекресток: Result := переправа: Result := перевал ';
  298: Result := 'Плотина ';
  299: Result := 'Больница ';
  300: Result := 'Плотина: Result := набережная ';
  301: Result := 'Указатель ';
  302: Result := 'Военный объект ';
  303: Result := 'Шахта: Result := рудник ';
  304: Result := 'Месторождение нефти ';
  305: Result := 'Парк ';
  306: Result := 'Почта ';
  307: Result := 'Школа ';
  308: Result := 'Башня: Result := вышка ';
  309: Result := 'Начало тропы ';
  310: Result := 'Начало/окончание тоннеля ';
  311: Result := 'Питьевая вода: Result := родник: Result := колодец ';
  312: Result := 'Заброшенное жилье ';
  313: Result := 'Пристройка ';
  314: Result := 'Объект гидрографии ';
  315: Result := 'Арройо: Result := высохшее русло ';
  316: Result := 'Песчаная отмель ';
  317: Result := 'Залив ';
  318: Result := 'Излучина реки ';
  319: Result := 'Искусственный канал ';
  320: Result := 'Пролив ';
  321: Result := 'Бухта ';
  322: Result := 'Водопад ';
  323: Result := 'Гейзер ';
  324: Result := 'Ледник ';
  325: Result := 'Гавань ';
  326: Result := 'Остров ';
  327: Result := 'Озеро ';
  328: Result := 'Пороги ';
  329: Result := 'Водохранилище ';
  330: Result := 'Море ';
  331: Result := 'Родник ';
  332: Result := 'Ручей ';
  333: Result := 'Болото ';
  334: Result := 'Природный наземный объект ';
  335: Result := 'Арка ';
  336: Result := 'Район: Result := область ';
  337: Result := 'Котловина ';
  338: Result := 'Берег ';
  339: Result := 'Карниз: Result := уступ ';
  340: Result := 'Мыс ';
  341: Result := 'Утес ';
  342: Result := 'Кратер ';
  343: Result := 'Плато ';
  344: Result := 'Лес ';
  345: Result := 'Ущелье: Result := пропасть ';
  346: Result := 'Узкий проход ';
  347: Result := 'Перешеек ';
  348: Result := 'Лава ';
  349: Result := 'Столб: Result := колонна ';
  350: Result := 'Равнина ';
  351: Result := 'Гряда ';
  352: Result := 'Заповедник ';
  353: Result := 'Хребет ';
  354: Result := 'Скала ';
  355: Result := 'Склон ';
  356: Result := 'Вершина холма или горы ';
  357: Result := 'Долина ';
  358: Result := 'Лес ';
  359: Result := 'Маяк ';
  360: Result := 'Туманный ревун ';
  361: Result := 'Радиомаяк ';
  362: Result := 'Радиобуй ';
  363: Result := 'Дневной маяк (красный треугольник) ';
  364: Result := 'Дневной маяк (зелёный квадрат) ';
  365: Result := 'Дневной маяк (белый ромб) ';
  366: Result := 'Несветящийся маяк белый ';
  367: Result := 'Несветящийся маяк красный ';
  368: Result := 'Несветящийся маяк зелёный ';
  369: Result := 'Несветящийся маяк черный ';
  370: Result := 'Несветящийся маяк желтый ';
  371: Result := 'Несветящийся маяк оражевый ';
  372: Result := 'Несветящийся маяк многоцветный ';
  373: Result := 'Светящийся маяк ';
  374: Result := 'Светящийся маяк белый ';
  375: Result := 'Светящийся маяк красный ';
  376: Result := 'Светящийся маяк зелёный ';
  377: Result := 'Светящийся маяк желтый ';
  378: Result := 'Светящийся маяк оранжевый ';
  380: Result := 'Светящийся маяк синий ';
  379: Result := 'Светящийся маяк фиолетовый ';
  381: Result := 'Светящийся маяк многоцветный ';
  382: Result := 'Военная база ';
  383: Result := 'Автостоянка ';
  384: Result := 'Гаражи ';
  385: Result := 'Аэропорт ';
  386: Result := 'Место для торговли ';
  387: Result := 'Пристань ';
  388: Result := 'Территория университета или колледжа ';
  389: Result := 'Больница ';
  390: Result := 'Промышленная зона ';
  391: Result := 'Резервация: Result := заповедник ';
  392: Result := 'Взлетно-посадочная полоса ';
  393: Result := 'Здание: Result := искусственное сооружение ';
  394: Result := 'Национальный парк ';
  395: Result := 'Национальный парк ';
  396: Result := 'Национальный парк ';
  397: Result := 'Городской парк ';
  398: Result := 'Поле для гольфа ';
  399: Result := 'Спортивный комплекс ';
  400: Result := 'Кладбище ';
  401: Result := 'Государственный парк ';
  402: Result := 'Государственный парк ';
  403: Result := 'Государственный парк ';
  404: Result := 'Автономный округ ';
  405: Result := 'Округ ';
  406: Result := 'Кожуун ';
  407: Result := 'Территория ';
  408: Result := 'Курортный поселок ';
  409: Result := 'Почтовое отделение ';
  410: Result := 'Сельская администрация ';
  411: Result := 'Сельское муниципальное образование ';
  412: Result := 'Сумон ';
  413: Result := 'Территория ';
  414: Result := 'Арбан ';
  415: Result := 'Волость ';
  416: Result := 'Городок ';
  417: Result := 'Железнодорожная будка ';
  418: Result := 'Железнодорожная казарма ';
  419: Result := 'Железнодорожная платформа ';
  420: Result := 'Железнодорожный пост ';
  421: Result := 'Казарма ';
  422: Result := 'Кордон ';
  423: Result := 'Леспромхоз ';
  424: Result := 'Почтовое отделение ';
  425: Result := 'Погост ';
  426: Result := 'Автодорога '
  else  Result := '';
  end;
end;

function TGeoCoderByNavitel.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  VLatStr, VLonStr: AnsiString;
  VSName, VFullDesc: string;
  VDescStr: AnsiString;
  VNavitel_ID, VNavitel_Type, VPlace_Id: AnsiString;
  i, j, Vii, Vjj: integer;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItemPoint;
  VList: IInterfaceListSimple;
  VFormatSettings: TALFormatSettings;
  vCurPos: integer;
  vCurChar: AnsiString;
  vBrLevel: integer;
  VBuffer: AnsiString;
  VStr: AnsiString;
  VDesc: string;
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
begin
  VFullDesc := '';
  VDescStr := '';
  vBrLevel := 1;
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  SetLength(Vstr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, Vstr[1], AResult.Data.Size);

  VStr := ALStringReplace(VStr, #$0A, '', [rfReplaceAll]);
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceListSimple.Create;

  vCurPos := 1;
  while (vCurPos<length(VStr)) do begin
    inc (vCurPos);
    vCurChar := copy(VStr, vCurPos, 1);
    VBuffer := VBuffer + vCurChar;
    if vCurChar='[' then Inc(vBrLevel);
    if vCurChar=']' then begin
      dec(vBrLevel);
      if vBrLevel=1 then  begin
        //[848692, ["РњРѕСЃРєРІР°"], 72, 857666, null],
        //[817088, ["РќРѕРІР°СЏ РњРѕСЃРєРІР°"], 32, null, ["РЁРєРѕС‚РѕРІСЃРєРёР№ СЂ-РЅ", "РџСЂРёРјРѕСЂСЃРєРёР№ РєСЂР°Р№", "Р РѕСЃСЃРёСЏ"]],
        VDescStr := '';
        VSName := '';
        VFullDesc := '';
        i := ALPosEx('[', VBuffer, 1);
        j := ALPosEx(',', VBuffer, 1);
        VNavitel_ID := Copy(VBuffer, i + 1, j - (i + 1));
        j := 1;
        i := ALPosEx('[', VBuffer, 1);
        if i>0  then begin
          j := ALPosEx(',', VBuffer, i + 1);
          VRequest :=
            PrepareRequestByURL(
              'http://maps.navitel.su/webmaps/searchTwoStepInfo?id=' + (Copy(VBuffer, i + 1, j - (i + 1)))
            );
          VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
          if Supports(VResult, IDownloadResultOk, VResultOk) then begin
            SetLength(VDescStr, VResultOk.Data.Size);
            Move(VResultOk.Data.Buffer^, VDescStr[1], VResultOk.Data.Size);
            Vii := 1;
            Vjj := ALPosEx(',', VDescStr, Vii + 1 );
            VLonStr := Copy(VDescStr, Vii + 1, Vjj - (Vii + 1));
            Vii := Vjj;
            Vjj := ALPosEx(',', VDescStr, Vii + 1 );
            VLatStr := Copy(VDescStr, Vii + 1, Vjj - (Vii + 1));
            VFullDesc := '';
          end else begin
            Exit;
          end;
        end;
        i:= j + 1;
        j := ALPosEx(']', VBuffer, i);
        VSName := Utf8ToAnsi(Copy(VBuffer, i + 3, j - (i + 4)));
        j := ALPosEx(',', VBuffer, j + 1);
        i := j + 1;
        j := ALPosEx(',', VBuffer, j + 1);
        VNavitel_Type := Copy(VBuffer, i + 1, j - (i + 1));
        VSName := NavitelType(ALStrToInt(VNavitel_Type )) + VSName;
        i := j + 1;
        j := ALPosEx(',', VBuffer, i + 1);
        VPlace_Id := Copy(VBuffer, i + 1, j - (i + 1));
        if VPlace_Id <> 'null' then begin
          VRequest := PrepareRequestByURL('http://maps.navitel.su/webmaps/searchById?id=' + (VPlace_Id));
          VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
          //http://maps.navitel.su/webmaps/searchById?id=812207
          if Supports(VResult, IDownloadResultOk, VResultOk) then begin
            SetLength(VDescStr, VResultOk.Data.Size);
            Move(VResultOk.Data.Buffer^, VDescStr[1], VResultOk.Data.Size);
            VDescStr := RegExprReplaceMatchSubStr(VDescStr, '[0-9]','');
            VDescStr := ALStringReplace(VDescStr, #$0A, '', [rfReplaceAll]);
            VDescStr := ALStringReplace(VDescStr, #$0D, '', [rfReplaceAll]);
            VDescStr := ALStringReplace(VDescStr, '[', '', [rfReplaceAll]);
            VDescStr := ALStringReplace(VDescStr, ']', '', [rfReplaceAll]);
            VDescStr := ALStringReplace(VDescStr, 'null', '', [rfReplaceAll]);
            VDescStr := ALStringReplace(VDescStr, ', ', '', [rfReplaceAll]);
            VDescStr := ALStringReplace(VDescStr, '""', '","', [rfReplaceAll]);
            VDesc := Utf8ToAnsi(VDescStr);
          end else begin
            Exit;
          end;
        end else begin
           i := ALPosEx('[', VBuffer, j + 1);
           if i > j + 1 then begin
             j := ALPosEx(']', VBuffer, i);
             VDesc := Utf8ToAnsi(Copy(VBuffer, i + 1, j - (i + 1)));
          end;
        end;

        try
          VPoint.Y := ALStrToFloat(VLatStr, VFormatSettings);
          VPoint.X := ALStrToFloat(VLonStr, VFormatSettings);
        except
          raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [VLatStr, VLonStr]);
        end;
        VPlace := PlacemarkFactory.Build(VPoint, VSName, VDesc, VFullDesc, 4);
        VList.Add(VPlace);

        VBuffer := '';
      end;
    end;
  end;

  Result := VList;
end;

function TGeoCoderByNavitel.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
begin

  VSearch := ASearch;
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);

  //http://maps.navitel.su/webmaps/searchTwoStep?s=%D0%BD%D0%BE%D0%B2%D0%BE%D1%82%D0%B8%D1%82%D0%B0%D1%80%D0%BE%D0%B2%D1%81%D0%BA%D0%B0%D1%8F&lon=38.9739197086479&lat=45.2394838066316&z=11
  //http://maps.navitel.su/webmaps/searchTwoStepInfo?id=842798

  //http://maps.navitel.su/webmaps/searchTwoStep?s=%D0%BC%D0%BE%D1%81%D0%BA%D0%B2%D0%B0&lon=37.6&lat=55.8&z=6
  //http://maps.navitel.su/webmaps/searchTwoStepInfo?id=848692
  Result := PrepareRequestByURL(
   'http://maps.navitel.su/webmaps/searchTwoStep?s=' + URLEncode(AnsiToUtf8(VSearch)) +
   '&lon=' + R2AnsiStrPoint(ALocalConverter.GetCenterLonLat.x) + '&lat=' + R2AnsiStrPoint(ALocalConverter.GetCenterLonLat.y) +
   '&z=' + ALIntToStr(VZoom));
end;
end.

