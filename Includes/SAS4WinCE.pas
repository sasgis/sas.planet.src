(******************************************************************************
Модуль:      SAS4WinCE
Назначение:  SASPlanet
Автор:       Dima2000
Версия:      v19
Дата:        06.06.2012

Модуль   содержит  класс  TSAS4WinCE,  реализующий экспорт тайлов из SASPlanet
сразу  в    формат   пакованного   кэша   для   программ  v_max  (SAS4WinCE  и
SAS4Andriod, http://4pna.com/forumdisplay.php?f=114).
Возможно   использование   модуля   и  в  других продуктах, могущих передавать
классу  тайлы   с   координатами   аналогично   SASPlanet,   на   это  никаких
ограничений не накладывается.
В   результате  работы  класса  создаются  файлы *.d00 (возможно и до *.d99) и
*.inx,  полностью   готовые   для   записи  на  SD  карту  и  использования  в
SAS4WinCE и SAS4Andriod.
Имя   создаваемых   файлов,   а   так   же  папка  где их создавать передаются
классу  снаружи  в  момент  начала  экспорта.  В  процессе работы класса в его
свойствах (property)  хранится  информация  об  параметрах  создаваемых файлов
(переданная  конструктору,   для  контроля)  и текущая статистика обработанных
тайлов. Подробнее смотри объявление класса.


*******************************************************************************
Типовая рекомендуемая последовательность вызовов:
  .Create(
    <Имя создаваемых файлов, очень желательно с путём к ним>,
    -1,
    <Комментарий к файлам, может быть пустым>
  );
  for ... do // Для каждого тайла
  begin
    .Add(
      z,x,y,
      tile,     // Указатель на буфер с бинарными данными тайла
      tilesize, // Объём данных в буфере
      <Расширение исходного файла с тайлом, если известно>
    );
  end;
  .SaveINX(); // Для формирования и записи индекса
  .Destroy;
При  желании  можно  повторять  связку  из  кучи  .Add  и  одного .SaveINX для
формирования  нескольких  увеличивающихся индексов с общими файлами данных. Не
знаю зачем нужно, но можно.
Можно  вызывать .SaveINX много раз для сохранения одинакового индекса в разные
файлы.
Можно   вызывать  .Create  с  пустым  именем  файла.  При  этом  файлы  данных
создаваться не будут, но их размеры считаться будут как обычно, и индекс можно
будет записать правильным. Возможность скорее для отладки.
Можно  (и нужно) вызывать и SaveINX с пустым именем файла. Тогда имя файла или
возьмётся  из  .Create,  или  индекс не будет записываться (если и там пусто).
Последнее тоже скорее для отладки.
Можно  вызвать  кучу  .Add, потом .SaveINX(''), а потом прочитать получившийся
размер  и решить что делать дальше. Так можно даже добивать тайлы до заданного
суммарного размера файлов и прочее.
Можно  вызвать  конструктор  с  пустым  именем  файла,  .SaveINX тоже с пустым
именем,  проверить  поулчившиеся размеры файлов, уничтожить объект и повторить
всё заново, возможно с другими параметрами.

*******************************************************************************
В файлы .d?? может сохраняться 12..15+ байтовая структура для каждого тайла:
recovery info = record
  z: byte;       // Один байт
  x: int24;      // Лишь три байта
  y: int24;      // Лишь три байта
  len: int32;    // Длина файла, 4 байта
  ext_len: byte; // Длина расширения файла в байтах
  ext: char[];   // Расширение файла (обрезается до первых 255 символов)
end;
Данная структура нужна исключительно для восстановления файла индекса по файлу
данных  или  для обратной распаковки файлов данных. В остальных случаях она не
требуется.    SAS4WinCE/SAS4Android    её    никак  не  использует.  Но  может
пригодиться в будущем для новых применений кэша в данном формате.

*******************************************************************************
История изменений.
Обозначения:
[+] - добавление функционала
[-] - удаление функционала
[!] - исправление ошибок
[*] - исправление (не ошибок), улучшение
{!} - отладочные куски, можно безопасно удалить
*******************************************************************************
Дата    Версия  Автор   Изменения
*******************************************************************************
28.04.2012  v0.4
Dima2000:
[*] Первый рабочий вариант в отдельном unit.

28.04.2012  v0.5
Dima2000:
[+] Сделан     контроль    повторного    запуска   экспорта   и   недопустимых
    последовательностей вызова процедур.
[!] По окончании экспорта не освобождалась память служебных таблиц (Tx и Ty).
[*] Добавлен контроль передачи пустого имени файла.

28.04.2012  v0.6
Dima2000:
[+] Добавлена выдача обнаруженных ошибок.
[*] Убрано дополнение расширения тайла до 4-х байтов.

28.04.2012  v0.7
Dima2000:
[+] Добавлена возможность задать имя файла индекса отличного от файла данных.
[+] Соответственно реализована возможность не писать на диск файлы данных или
    и   индекса  тоже  (указанием   пустого  имени  файла).  Размеры  при этом
    считаются правильно.

28.04.2012  v8
Dima2000:
[+] Добавлена процедура останова экспорта.
[+] Добавлена структура с текущей информацией о процессе экспорта.
[*] Реализован переменный размер таблицы Z. Но место занимает всегда на все 24
    элемента.
[*] Сменена нумерация версий на прямую.

30.04.2012  v9
Dima2000:
[*] Начата переделка в класс.
[*] Написан интерфейс класса.
[+] Реализовано добавление произвольной строки комментария к файлу индекса и к
    файлам данных.

01.05.2012  v10
Dima2000:
[*] Продолжение работы над переделкой в класс. Изменений много. К счастью не в
    логике работы, а скорее косметических.
[*] Вроде  бы класс доделал, компилится, даже работать должен.  Осталось много
    костылей от процедурности.

01.05.2012  v11
Dima2000:
[*] Продолжение работы над переделкой в класс. Уборка костылей.
[*] Наконец заработало в виде класса.

01.05.2012  v12
Dima2000:
[*] Перевод системы ошибок на генерацию исключений.

01.05.2012  v13
Dima2000:
[*] Увеличил нижний предел размера файлов данных с 1КБ до 1МБ.
[*] Переделал приём тайла на из TMemoryStream.
[*] Добавлена   возможность   разрешить   дублирование  координат  в  .SaveINX
    параметром вызова (по умолчанию генерить исключение).
[*] Расставил const для параметров функций.

01.05.2012  v14
Dima2000:
[*] Добавил свой класс для исключений.

01.05.2012  v15
Dima2000:
[*] Снова переделал контроль ошибок, критичные генерят исключения, некритичные
    возвращают false и можно продолжать процесс, причина пока не сообщается.
[*] Для  критичных  ошибок  сделал  все  исключения  потомками базового класса
    исключений.

02.05.2012  v16
Dima2000:
[*] Добавил выдачу названия класса, можно использовать для показа в диалоге SAS.
[!] В деструкторе не закрывался файл индекса, если он вдруг остался открытым.
[!] Добавлена проверка (и класс исключений) на максимальное количество тайлов
    в .Add и на размер индекса в .SaveINX - из-за ограничения индекса в 2ГБ.
zed:
[*] Смена источника тайла в .Add с TMemoryStream на указатель на буфер + размер.

02.05.2012  v17
Dima2000:
[*] Отформатировал  согласно стандартам ... Выглядит ужасно.
    В  коде  изменений  вроде как нет, идентификаторы к стандарту не приводил,
    стандарты дело хорошее, но не до фанатизма.

18.05.2012  v18
Dima2000:
[+] Добавил метод .AddExistTile для реиндексации и прочих вкусностей.
[!] Исправил глюк с многотомными файлами данных с комментариями.

06.06.2012  v19
Dima2000:
[!] Переделал метод .SaveINX для упорядоченной записи таблиц в индекс. Иначе
    под андроидом клиент v_max-а не работает. Заодно теперь должна быть и
    двоичная идентичность индексных файлов с родным упаковщиком (если не писать
    дополнительной инфы конечно).

*******************************************************************************
ToDo:
+1. Вместо кодов ошибок генерить исключения.
+2. Вместо string принимать тайл в TMemoryStream.
-3. Выяснить можно ли  (и как)  скрыть объявления типов из interface, ведь они
нужны  лишь  для  самого  этого  класса  и  всё. Оказалось проблема в моей 5-й
дельфи, она не позволяет, нужна 7-я.
-4.  Ошибки  выделения  памяти, записи в файлы и прочие подобные проверять нет
смысла  -  исключения  и  так  сгенерятся,  ну  малопонятные,  ну и что. Потом
когда-нибудь добавим, может быть.
+5. Говорят надо свой класс ошибок сделать. Как и зачем?
+6.  Добавить в описание список не смертельных исключений и рекомендации что с
ними делать.
-7. Проверить нужна ли буферизация вывода в файлы (с буфером порядка 1МБ). Или
существенно  скорости  не  добавит. Да ну её в пень, пусть винда буферизирует,
Blockwrite в боевых условиях пишет приличные куски данных.
+8.  Добавить  механизм разрешения дублирования координат тайлов в .SaveINX().
Вероятно параметром вызова, чтобы не генерилось исключение.
+9.  Проверять  на  превышение  или  количества  тайлов  (130млн), или размера
индекса, на 2ГБ. И генерить исключение.
-10.   Оказывается   перед   началом  экспорта  известно  точное  максимальное
количество тайлов, может его передать в конструктор? Память выделять не будем,
но  вот  проверить  его  можно  сразу  будет, а не ждать сутки пока что-нибудь
переполнится. Нет, проще проверить снаружи перед вызовом конструктора.
******************************************************************************)

unit SAS4WinCE;

interface

uses
  SysUtils, Classes;

type
  {Базовый класс исключений}
  ESAS4WinCE = class(Exception);
  {Слишком большой или слишком маленький лимит размера файлов (в .Create)}
  ESAS4WinCElimit = class(ESAS4WinCE);
  {Слишком длинный комментарий (для указанного лимита размера файлов) (в .Create)}
  ESAS4WinCEbigComment = class(ESAS4WinCE);
  {Более ста файлов данных формировать нельзя (в .Add)}
  ESAS4WinCEover100dat = class(ESAS4WinCE);
  {А вот не будем экспортировать нулевое количество тайлов, не будем (в .SaveINX)}
  ESAS4WinCEnoTiles = class(ESAS4WinCE);
  {Найдены  тайлы  с  одинаковыми  координатами z,x,y
  (может когда-то это и не ошибка,  вдруг  они  с разным расширением, но прогу
  v_max  может  проглючить),  поведение  регулируется параметром при вызове
  (в .SaveINX)}
  ESAS4WinCEdupZXY = class(ESAS4WinCE);
  {Слишком  много тайлов, индекс не должен превышать 2ГБ, это не более ~130млн
  тайлов (в .Add и в .SaveINX)}
  ESAS4WinCEbigIndex = class(ESAS4WinCE);
  {Внутренняя ошибка сортировки, создавать индекс бессмысленно (в .SaveINX)}
  ESAS4WinCEsortingError = class(ESAS4WinCE);

  TTileInfo = packed record
    dzxy: int64;    // d<<56+z<<48+x<<24+y
    ptr: integer;   // Смещение в файле данных
    size: integer;  // Длина файла
  end;

  TSAS4WinCE = class
  private
    {Количество принятых тайлов}
    fTilesNum: integer;
    {Количество задействованных файлов данных}
    fDataNum: integer;
    {Размер всех предыдущих файлов данных}
    fDataPrev: int64;
    {Общий размер всех файлов данных включая и формируемый}
    fDataLen: int64;
    {Максимально допустимый размер файлов данных}
    fMaxSize: integer;
    {Размер файла индекса (заполняется методом .SaveINX)}
    fIndexLen: integer;
    {Имя (без расширения!) (можно с путём) создаваемых файлов}
    fFileName: string;
    {Максимально допустимый размер файлов данных}
    fLimit: integer;
    FD: file;
    {Добавляемый к файлам комментарий}
    fComment: string;
    {Записывать ли в файлы данных дополнительную информацию об тайле
    (структуру recovery info, 12-15+ байтов) и копирайт в файлы данных и индекса}
    bWriteTileInfo: boolean;
    {Было ли задано имя файла данных}
    bWriteFD: boolean;
    {Открыт ли файл данных}
    bOpenFD: boolean;
    {Открыт ли файл индекса}
    bOpenFI: boolean;
    {Массив инфы о тайлах}
    Ttiles: array of TTileInfo;
    {Для операций над массивом и ускорения доступа}
    t: TTileInfo;
{!} fSwap: int64;             // Сколько всего обменов произвела сортировка
    fMaxTx, fMaxTy: integer;  // Максимальный достигнутый размер таблиц X и Y

    {Сформировать индекс и записать его в указанный файл}
    procedure CreateINX(const fname: string; const bAllowDups: boolean);
    {Отсортировать буфер. Применена битовая сортировка с 56-ю проходами по полям z,x,y}
    procedure SortAll(const l,r: integer; const m: int64);
    {Закрыть открытый файл данных, предварительно дописав туда комментарий}
    procedure CloseFD();

  public

    {Строка с названием модуля (для отображения в диалоге SAS?)}
    class function Name(): string;

    {Выдать строку с информацией о версии}
    class function Version(): string;

    {Начинает процесс экспорта тайлов в файл}
    constructor Create(
      {Имя файлов (без расширения), куда экспортировать, с путём}
      const fname: string;
      {Максимально допустимый размер файлов данных, если < 0, то взять 1ГБ}
      const maxsize: integer = -1;
      {Добавляемый в конец файлов комментарий}
      const cmt: string = '';
      {Записывать     в    файлы   данных   recovery  info. И копирайт в файлы
      данных  и  файл  индекса.  Копирайт  является  также  сигнатурой наличия
      recovery info в файлах данных, это будет использовано позже}
      const info: boolean = true
    );

    {Закрывает открытые файлы и освобождает память}
    destructor Destroy; override;

    {Добавляет тайл}
    function Add(
      {Координаты тайла}
      const z, x, y: integer;
      {Указатель на буфер с данными тайла}
      const ptile: pointer;
      {Объём данных в буфере}
      const tilesize: integer;
      {Расширение исходного файла с тайлом}
      const fext: string=''
    {Вернёт true если тайл обработан}
    ): boolean;

    {Добавляет лишь информацию о тайле}
    function AddExistTile(
      {Координаты тайла}
      const z, x, y: integer;
      {Номер файла данных с тайлом}
      const d: byte;
      {Смещение тайла в файле данных}
      const ptr: integer;
      {Размер тайла}
      const tilesize: integer
    {Вернёт true если тайл обработан}
    ): boolean;

    {Завершить передачу тайлов, сформировать индекс, записать его в файл}
    function SaveINX(
      {Имя  файла  (без  расширения)  для  записи  индекса,  с  путём,
      если не указано, берётся имя из конструктора}
      const fname: string = '';
      {Допускать тайлы с одинаковыми координатами}
      const bAllowDups: boolean = false
    {Вернёт true если индекс успешно записан}
    ): boolean;

    {Путь и имя (без расширения) создаваемых файлов данных (переданное .Create)}
    property FileName: string  read fFileName;
    {Общее количество принятых тайлов (модифицируется методом .Add)}
    property TilesNum: integer read fTilesNum;
    {Номер текущего файла данных (модифицируется методом .Add)}
    property DataNum: integer  read fDataNum;
    {Общий размер всех файлов данных
    (до вызова .SaveINX - без одного комментария, в последнем файле данных)
    (модифицируется методами .Add и .SaveINX)}
    property DataLen: int64    read fDataLen;
    {Размер файла индекса (заполняется методом .SaveINX после завершения экспорта)}
    property IndexLen: integer read fIndexLen;
    {Добавляемый к файлам комментарий (переданный .Create)}
    property Comment: string   read fComment;
    {Записывать    в   файлы  данных  recovery info. И копирайт в файлы данных
    и  файл  индекса.   Копирайт  является  также  сигнатурой наличия recovery
    info в файлах данных, это будет использовано позже}
    property WriteTileInfo: boolean  read bWriteTileInfo;
    {Максимально допустимый размер файлов данных (переданное .Create)}
    property MaxFileSize: integer  read fMaxSize;
{!} property Swaps: int64      read fSwap;  // Сколько всего обменов произвела сортировка
    property MaxTx: integer    read fMaxTx; // Максимальный размер таблиц X
    property MaxTy: integer    read fMaxTy; // Максимальный размер таблиц Y
  end;


implementation

const
  unit_name = 'Packed cache for SAS4WinCE/SAS4Android';
  unit_ver = 'v19';
  copyright =
    #13#10'*****   Export from SAS.Planeta to SAS4WinCE  '
    + unit_ver
    + '   *****'#13#10#13#10#0;
  {На сколько увеличивать размер буфера при переполнении (в записях по 16Б)}
  TilesSizeInc = 100000;
  {Маска битов для сортировки}
  mask56 = $FFFFFFFFFFFFFF;
  {Максимально допустимый размер файла индекса}
  IndexMaxSize = 2147000000;


{Получить строку с версий класса}
class function TSAS4WinCE.Version(): string;
begin
  Result := unit_ver;
end;

{Получить строку с именем/описанием класса}
class function TSAS4WinCE.Name(): string;
begin
  Result := unit_name;
end;


{Закрыть открытый файл данных, предварительно дописав туда комментарий}
procedure TSAS4WinCE.CloseFD();
var
  s: string;
begin
  if not bOpenFD then
    {Нет открытого файла данных}
    Exit;
  if length(Comment) > 0 then
  begin
    {Добавим  два  перевода  строки  для  визуального отделения комментария от
    бинарных данных и \0 в конец чтобы строка стала ASCIIZ}
    s := #13#10#13#10 + Comment + #0;
    Blockwrite(FD, s[1], length(s));
    {Увеличим общий объём всех файлов данных на длину добавленной строки}
    Inc(fDataLen, length(s));
    {Запомним сколько байтов было накоплено в файлах данных ранее}
    fDataPrev := fDataLen;
  end;
  CloseFile(FD);
  bOpenFD := false;
end;


{Добавляет тайл}
function TSAS4WinCE.Add(
  {Координаты тайла}
  const z, x, y: integer;
  {Указатель на буфер с данными тайла}
  const ptile: pointer;
  {Объём данных в буфере}
  const tilesize: integer;
  {Расширение исходного файла с тайлом}
  const fext: string=''
{Вернёт true если тайл обработан}
): boolean;
var
  n: integer;
  s: string;
begin
  Result := false;
  if fTilesNum > 130000000 then
    {Слишком много тайлов для индекса 2ГБ}
    raise ESAS4WinCEbigIndex.Create('Too many tiles!');
  if tilesize = 0 then
    {Тайлы нулевой длины не принимаем}
    Exit;{raise ESAS4WinCE.Create('Size of tile is 0.');}
  if tilesize > fLimit then
    {Тайлы длиннее предела не принимаем}
    Exit;{raise ESAS4WinCE.Create('Size of tile is too big.');}
  if not z in [1..24] then
    {Зум может быть только 1..24}
    Exit;{raise ESAS4WinCE.Create('Incorrect zoom.');}
  {Расчитаем максимально допустимые координаты}
  n := ((1 shl z) shr 1) - 1;
  if (x < 0) or (x > n) then
    {Тайлы с некорректными координатами не принимаем}
    Exit;{raise ESAS4WinCE.Create('Incorrect x.');}
  if (y < 0) or (y > n) then
    {Тайлы с некорректными координатами не принимаем}
    Exit;{raise ESAS4WinCE.Create('Incorrect y.');}
  if fDataLen - fDataPrev + int64(tilesize) > fLimit then
  begin
    {Если не влезет в ограничение, то перейти к следующему файлу данных}
    if fDataNum >= 99 then
      {Больше ста файлов данных создавать нельзя}
      raise ESAS4WinCEover100dat.Create('Over 100 data files is not allowed.');
    Inc(fDataNum);
    {Запомним сколько байтов было накоплено в файлах данных ранее}
    fDataPrev := fDataLen;
    if bWriteFD then
    begin
      {Если пишем в файлы, то закрываем открытый файл данных и открываем новый}
      CloseFD();
      AssignFile(FD, Format('%s.d%2.2d', [fFileName, fDataNum]));
      Rewrite(FD, 1);
      bOpenFD := true;
    end;
    if bWriteTileInfo then
    begin
      {Если   пишем   recovery  info,  то  в  начало  файла  данных записываем
      копирайт,  который    одновременно    будет  являться сигнатурой наличия
      recovery info в файлах данных}
      s := copyright;
      Inc(fDataLen, length(s));
      if bWriteFD then
        {Физическую запись производим только если разрешено}
        Blockwrite(FD, s[1], length(s));
    end;
  end;
  {Сохраним информацию о файле}
  if bWriteTileInfo then
  begin
    n := length(fext);
    {Ограничение длины строки с расширением}
    if n > 255 then
      n := 255;
    if bWriteFD then
    begin
      {Физическую запись производим только если разрешено}
      Blockwrite(FD, z, 1);
      Blockwrite(FD, x, 3);
      Blockwrite(FD, y, 3);
      Blockwrite(FD, tilesize, 4);
      Blockwrite(FD, n, 1);
      if n > 0 then
        Blockwrite(FD, fext[1], n);
    end;
    {Увеличим счётчик размера файлов данных на объём записанной информации}
    Inc(fDataLen, 1+3+3+4+1 + n);
  end;
  if bWriteFD then
    {Физическую запись тайла производим только если разрешено}
    Blockwrite(FD, ptile^, tilesize);
  if fTilesNum >= Length(Ttiles) then
    {Мало памяти, надо ещё увеличить буфер}
    SetLength(Ttiles, Length(Ttiles) + TilesSizeInc);
  t.dzxy := (int64(fDataNum) shl 56) + (int64(z) shl 48) + (int64(x) shl 24) + y;
  {Смещение в текущем файле данных}
  t.ptr := fDataLen - fDataPrev;
  t.size := tilesize;
  Ttiles[fTilesNum] := t;
  Inc(fTilesNum);
  Inc(fDataLen, tilesize);
  {Если дошли сюда без исключений, значит всё хорошо}
  Result := true;
end;


{Добавляет лишь информацию о тайле}
function TSAS4WinCE.AddExistTile(
  {Координаты тайла}
  const z, x, y: integer;
  {Номер файла данных с тайлом}
  const d: byte;
  {Смещение тайла в файле данных}
  const ptr: integer;
  {Размер тайла}
  const tilesize: integer
{Вернёт true если тайл обработан}
): boolean;
var
  n: integer;
begin
  Result := false;
  if fTilesNum > 130000000 then
    {Слишком много тайлов для индекса 2ГБ}
    raise ESAS4WinCEbigIndex.Create('Too many tiles!');
  if tilesize = 0 then
    {Тайлы нулевой длины не принимаем}
    Exit;{raise ESAS4WinCE.Create('Size of tile is 0.');}
  if tilesize > fLimit then
    {Тайлы длиннее предела не принимаем}
    Exit;{raise ESAS4WinCE.Create('Size of tile is too big.');}
  if not z in [1..24] then
    {Зум может быть только 1..24}
    Exit;{raise ESAS4WinCE.Create('Incorrect zoom.');}
  {Расчитаем максимально допустимые координаты}
  n := ((1 shl z) shr 1) - 1;
  if (x < 0) or (x > n) then
    {Тайлы с некорректными координатами не принимаем}
    Exit;{raise ESAS4WinCE.Create('Incorrect x.');}
  if (y < 0) or (y > n) then
    {Тайлы с некорректными координатами не принимаем}
    Exit;{raise ESAS4WinCE.Create('Incorrect y.');}
  if d > 99 then
    {Больше ста файлов данных создавать нельзя}
    raise ESAS4WinCEover100dat.Create('Over 100 data files is not allowed.');

  if d <> fDataNum then begin
    fDataPrev := fDataLen;
    fDataNum := d;
  end;
  if fTilesNum >= Length(Ttiles) then
    {Мало памяти, надо ещё увеличить буфер}
    SetLength(Ttiles, Length(Ttiles) + TilesSizeInc);
  t.dzxy := (int64(fDataNum) shl 56) + (int64(z) shl 48) + (int64(x) shl 24) + y;
  {Смещение в текущем файле данных}
  t.ptr := ptr;
  t.size := tilesize;
  Ttiles[fTilesNum] := t;
  Inc(fTilesNum);
  fDataLen := fDataPrev + ptr + tilesize;
  {Если дошли сюда без исключений, значит всё хорошо}
  Result := true;
end;


{Отсортировать буфер.
Применена битовая сортировка с 56-ю проходами для сортировки по полям z,x,y}
procedure TSAS4WinCE.SortAll(const l,r: integer; const m: int64);
var
  i, j: integer;
begin
  if (m = 0) or (l >= r) then Exit;
  i := l; j := r;
  while i <= j do
  begin
    while i <= j do
    begin
      if (Ttiles[i].dzxy and m) > 0 then break; //Найдено a=1
      Inc(i);
    end;
    while i <= j do
    begin
      if (Ttiles[j].dzxy and m) = 0 then break; //Найдено b=0
      Dec(j);
    end;
    if i < j then
    begin //Найдено несоответствие, обменяем
      t := Ttiles[i];
      Ttiles[i] := Ttiles[j];
      Ttiles[j] := t;
      {Найденные ячейки уже правильные, перейдём дальше}
      Inc(i);
      Dec(j);
{!}   Inc(fSwap);
    end;
  end;
  {Сначала сортируется правый подмассив для исключения оптимизатором рекурсии
  для левого подмассива, который в почти сортированном массиве обычно больше}
  if (m > 1) and (j + 1 < r) then
    {Сортируем правый подмассив}
    SortAll(j + 1, r, m shr 1);
  if (m > 1) and (l < i - 1) then
    {Сортируем левый подмассив}
    SortAll(l, i - 1, m shr 1);
end;


{Сформировать индекс и записать его в указанный файл}
procedure TSAS4WinCE.CreateINX(
  {Файл с путём (без расширения) для записи индекса}
  const fname: string;
  {Допускать ли тайлы с одинаковыми координатами}
  const bAllowDups: boolean
);
type
  TTnumY = packed record
    x: integer;     // Значение
    n: integer;     // Количество Y для данных Z,X
  end;
  TTzx = packed record
    x: integer;
    ptr: integer;
  end;
  TTy = packed record
    y: integer;
    d: integer;
    ptr: integer;
    size: integer;
  end;
var
  {Для хранения значений Z}
  Tz: array[0..23] of integer;
  {Для подсчёта сколько разных Y для каждой пары Z,X}
  TnumY: array of array of TTnumY;
  {Для ускорения записи таблиц X}
  Tx: array of TTzx;
  {Для ускорения записи таблиц Y}
  Ty: array of TTy;
  p: TTileInfo;
  z, x, nx, ny, pz, px, iz, ix, iy, it: integer;
  s: string;
  bWr: boolean;
begin
  {Писать будем только в непустой файл}
  bWr := (length(fname) > 0);
  fMaxTx := 0; fMaxTy := 0;
  pz := 0; px := -1; p.dzxy := -1;
  iz := -1; ix := -1;
  {Первый проход по массиву - для подсчётов длин таблиц}
  for it := 0 to fTilesNum - 1 do begin
    t := Ttiles[it];
    if (t.dzxy and mask56) < p.dzxy then
      {Найдена ошибка сортировки!}
      raise ESAS4WinCEsortingError.Create('Found sorting error!');
    if (not bAllowDups) and ((t.dzxy and mask56) = p.dzxy) then
      {Найдены тайлы с одинаковыми координатами z,x,y
      (это в общем-то не обязательно ошибка, может они с разным расширением)}
      raise ESAS4WinCEdupZXY.Create('Duplicate z,x,y in tiles.');
    p.dzxy := t.dzxy and mask56;
    z := (t.dzxy shr 48) and $FF;
    x := (t.dzxy shr 24) and $FFFFFF;
    if pz <> z then begin
      {Обнаружен новый Z}
      Inc(iz);
      SetLength(TnumY, iz + 1);
      Tz[iz] := z;
      px := -1; ix := -1;
    end;
    if px <> x then begin
      {Обнаружен новый X}
      Inc(ix);
      SetLength(TnumY[iz], ix + 1);
      TnumY[iz, ix].x := x;
      TnumY[iz, ix].n := 0;
    end;
    Inc(TnumY[iz, ix].n);
    pz := z; px := x;
  end;
  {Сформировать копирайт, тут для учёта его размера между таблицами Z и X}
  s := '';
  if bWriteTileInfo then begin
    s := #13#10 + copyright + #0#0#0#0;
    {В файле индекса длину текста выравниваем на 4 байта}
    s := Copy(s, 1, length(s) and -4);
  end;
  z := Length(TnumY);
  if bWr then begin
    {Если файл индекса пишется, то откроем его, запишем размер таблицы Z}
    AssignFile(FD, fname + '.inx');
    Rewrite(FD, 1);
    bOpenFI := true;
    Blockwrite(FD, z, 4);
  end;
  {Смещение начала таблиц X}
  fIndexLen := 4 + z * SizeOf(TTzx) + length(s);
  {Формируем таблицу Z}
  for iz := 0 to High(TnumY) do begin
    if bWr then begin
      Blockwrite(FD, Tz[iz], 4);
      Blockwrite(FD, fIndexLen, 4);
    end;
    nx := Length(TnumY[iz]);
    if nx > fMaxTx then fMaxTx := nx;
    {Продвинем указатель на размер таблицы X для данного Z}
    Inc(fIndexLen, 4 + nx * SizeOf(TTzx));
  end;
  {Выделим память под самую большую таблицу X}
  SetLength(Tx, fMaxTx);
  {После таблицы Z запишем копирайт}
  if bWr and (length(s) > 0) then
    Blockwrite(FD, s[1], length(s));
  {Теперь будем писать все таблицы X}
  for iz := 0 to High(TnumY) do begin
    nx := Length(TnumY[iz]);
    for ix := 0 to nx - 1 do begin
      Tx[ix].x := TnumY[iz, ix].x;
      Tx[ix].ptr := fIndexLen;
      ny := TnumY[iz, ix].n;
      if ny > fMaxTy then fMaxTy := ny;
      {Продвинем указатель на размер таблицы Y для данных Z,X}
      Inc(fIndexLen, 4 + ny * SizeOf(TTy));
    end;
    if bWr then begin
      Blockwrite(FD, nx, 4);
      Blockwrite(FD, Tx[0], nx * SizeOf(TTzx));
    end;
  end;
  if bWr then begin
    it := -1;
    {Выделим память под самую большую таблицу Y}
    SetLength(Ty, fMaxTy);
    {Второй проход по массиву - теперь будем писать все таблицы Y}
    for iz := 0 to High(TnumY) do begin
      nx := Length(TnumY[iz]);
      for ix := 0 to nx - 1 do begin
        ny := TnumY[iz, ix].n;
        for iy := 0 to ny - 1 do begin
          Inc(it);
          t := Ttiles[it];
          Ty[iy].y := t.dzxy and $FFFFFF;
          Ty[iy].d := t.dzxy shr 56;
          Ty[iy].ptr := t.ptr;
          Ty[iy].size := t.size;
        end;
        Blockwrite(FD, ny, 4);
        Blockwrite(FD, Ty[0], ny * SizeOf(TTy));
      end;
    end;
  end;
  if bWr then begin
    {Добавим в конец файла индекса пользовательский комментарий}
    if length(comment)>0 then begin
      {Добавим  два  перевода  строки  для  визуального отделения комментария от
      бинарных данных и \0 в конец чтобы строка стала ASCIIZ}
      s := #13#10#13#10 + Comment + #0;
      if fIndexLen + length(s) > IndexMaxSize then
        {Слишком много тайлов для индекса 2ГБ}
        raise ESAS4WinCEbigIndex.Create('Too many tiles!');
      Blockwrite(FD, s[1], length(s));
      {Увеличим объём файла индекса на длину добавленной строки}
      Inc(fIndexLen, length(s));
    end;
    CloseFile(FD);
    bOpenFI := false;
  end;
  SetLength(Tx, 0);
  SetLength(Ty, 0);
end;


{Начинает процесс экспорта тайлов в файл}
constructor TSAS4WinCE.Create(
  {Имя файлов (без расширения), куда экспортировать, с путём}
  const fname: string;
  {Максимально допустимый размер файлов данных, если < 0, то взять 1ГБ}
  const maxsize: integer = -1;
  {Добавляемый в конец файлов комментарий}
  const cmt: string = '';
  {Записывать    в   файлы  данных  recovery info. И копирайт в файлы данных и
  файл индекса.  Копирайт  является  также  сигнатурой наличия recovery info в
  файлах данных, это будет использовано позже}
  const info: boolean = true);
var
  maxlen: integer;
begin
  inherited Create;
  maxlen := maxsize;
  if maxlen >= (int64(2) shl 31) - 2 then
    {Такие большие файлы создавать не умеем}
    raise ESAS4WinCElimit.Create('Limit file size is too big!');
  {При отрицательном значении используем значение по умолчанию 1ГБ}
  if maxlen < 0 then maxlen := 1000000000;
  if maxlen < 1000000 then
    {Нельзя создавать слишком маленькие файлы данных, там же копирайт и комменты!}
    raise ESAS4WinCElimit.Create('Limit file size is too small!');
  if length(cmt) > maxlen div 2 then
    {А вот не дам добавлять огромные комментарии в половину файла данных!}
    raise ESAS4WinCEbigComment.Create('Comment is too big!');
  bWriteFD := false;
  if length(fname) > 0 then
    {Если не задано имя файла для записи,
    то не создаём файлы данных, лишь внутренние операции}
    bWriteFD := true;
{Дальше инициализация внутренних переменных класса}
  fTilesNum := 0; // Нет тайлов
  fMaxSize := maxlen;
  fComment := cmt;
  bWriteTileInfo := info;
  bOpenFD := false; // Нет открытого файла данных
  fDataNum := -1; // Нет открытого файла данных
  fDataLen := 0; // Пока не записано ни байта
  fDataPrev := -fMaxSize; // Начать новый файл данных
  fFileName := fname;
  bOpenFI := false; // Файл индекса не открыт
  SetLength(Ttiles, TilesSizeInc); // Выделить немного памяти заранее
  fLimit := fMaxSize - 1*1024; // Запас в 1КБ
  if length(Comment) > 0 then
    {Уменьшим предел на длину комментария если он есть}
    Dec(fLimit, length(Comment));
  if bWriteTileInfo then
    {А также на длину копирайта если таковой пишется
    и на длину расширения, и на размер recovery info}
    Dec(fLimit, length(copyright) + 300);
  fMaxTx := 0; fMaxTy := 0;
  fSwap := 0; {!}
end;


{Закрывает открытые файлы и освобождает память}
destructor TSAS4WinCE.Destroy;
begin
  if bOpenFD or bOpenFI then
    {Закрыть открытый файл данных или индекса}
    CloseFile(FD);
  {Освободить всю занятую таблицами память}
  SetLength(Ttiles, 0);
  inherited Destroy;
end;


{Завершить передачу тайлов, сформировать индекс, записать его в файл}
function TSAS4WinCE.SaveINX(
  {Имя  файла  (без  расширения)  для  записи  индекса,  с  путём,
  если не указано, берётся имя из конструктора}
  const fname: string = '';
  {Допускать тайлы с одинаковыми координатами}
  const bAllowDups: boolean = false
{Вернёт true если индекс успешно записан}
): boolean;
var
  fn: string;
begin
  if bOpenFI then
    {Закроем открытый файл индекса}
    CloseFile(FD);
  bOpenFI := false;
  if fTilesNum = 0 then
    {А вот не будем экспортировать нулевое количество тайлов, не будем}
    raise ESAS4WinCEnoTiles.Create('Nothing to export.');
  {Предварительно закроем файл данных}
  CloseFD();
  fSwap := 0;
  {Отсортировать буфер, применена битовая сортировка
  с 56-ю проходами для сортировки по полям z,x,y}
  SortAll(0, fTilesNum-1, mask56 xor (mask56 shr 1));
  fn := fname;
  if length(fn) = 0 then
    {Если не задано имя файла индекса, то используем имя файла данных}
    fn := fFileName;
  {Создадим файл индекса}
  CreateINX(fn, bAllowDups);
  {Если дошли сюда без исключений, значит всё хорошо}
  Result := true;
end;


end.
