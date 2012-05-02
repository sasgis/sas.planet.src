(****************************************************************************************************************************************************

Типовая последовательность вызовов: .Create, для каждого тайла .Add, .SaveINX для формирования и записи индекса, Destroy.
При желании можно повторять связку из кучи .Add и одного .SaveINX для формирования нескольких увеличивающихся индексов с общими файлами данных. Не знаю зачем нужно, но можно.
Также можно вызывать .SaveINX много раз для сохранения одинакового индекса в разные файлы.
Можно вызывать .Create с пустым именем файла. При этом файлы данных создаваться не будут, но их размеры считаться будут как обычно, и индекс можно будет записать правильным.
Можно вызывать и SaveINX с пустым именем файла. Тогда имя файла или возьмётся из .Create, или индекс не будет записываться (если и там пусто).
Можно вызвать кучу .Add, потом .SaveINX(''), а потом прочитать получившийся размер и решить что делать дальше. Так можно даже добивать тайлы до заданного суммарного размера файлов и прочее.
*****************************************************************************************************************************************************
В файлы .d?? может сохраняться следующая 12..15+ байтовая структура для каждого тайла:
        z:byte;         //Один байт
        x:int24;        //Лишь три байта
        y:int24;        //Лишь три байта
        len:int32;      //Длина файла, 4 байта
        ext_len:byte;   //Длина расширения файла в байтах
        ext:char[];     //Расширение файла
Данная структура нужна исключительно для восстановления файла индекса по файлу данных или для обратной распаковки файлов данных.
В остальных случаях она не требуется. SAS4WinCE её никак не использует.
Но может пригодиться в будущем для новых применений кэша в данном формате.
*****************************************************************************************************************************************************
Комментарием {!} помечены отладочные куски, их можно безопасно удалить.
*****************************************************************************************************************************************************
Дата            Версия  Автор           Изменения
*****************************************************************************************************************************************************
28.04.2012      v0.4    Dima2000        1.      Первый рабочий вариант в отдельном unit.
28.04.2012      v0.5    Dima2000        1. Add: Сделан контроль повторного запуска экспорта и недопустимых последовательностей вызова процедур.
                                        2. Fix: По окончании экспорта не освобождалась память служебных таблиц (Tx и Ty).
                                        3. Add: Добавлен контроль передачи пустого имени файла.
28.04.2012      v0.6    Dima2000        1. Add: Добавлена выдача обнаруженных ошибок.
                                        2. Fix: Убрано дополнение расширения тайла до 4-х байтов.
28.04.2012      v0.7    Dima2000        1. Add: Добавлена возможность задать имя файла индекса отличного от файла данных.
                                        2. Add: Соответственно реализована возможность не писать на диск файлы данных или и индекса тоже (указанием пустого имени файла). Размеры при этом считаются правильно.
28.04.2012      v8      Dima2000        1. Add: Добавлена процедура останова экспорта.
                                        2. Add: Добавлена структура с текущей информацией о процессе экспорта.
                                        3. Fix: Реализован переменный размер таблицы Z. Но место занимает всегда на все 24 элемента.
                                        4.      Сменена нумерация версий на прямую.
30.04.2012      v9      Dima2000        1.      Начата переделка в класс.
                                        2. Add: Написан интерфейс класса.
                                        3. Add: Реализовано добавление произвольной строки комментария к файлу индекса и к файлам данных.
01.05.2012      v10     Dima2000        1.      Продолжение работы над переделкой в класс. Изменений много. К счастью не в логике работы, а скорее косметических.
                                        2.      Вроде бы класс доделал, компилится, даже работать должен. Осталось много костылей от процедурности.
01.05.2012      v11     Dima2000        1.      Продолжение работы над переделкой в класс. Уборка костылей.
                                        2.      Наконец заработало в виде класса.
01.05.2012      v12     Dima2000        1. Fix  Перевод системы ошибок на генерацию исключений.
01.05.2012      v13     Dima2000        1. Fix  Увеличил нижний предел размера файлов данных с 1КБ до 1МБ.
                                        2. Fix  Переделал приём тайла на из TMemoryStream.
                                        3. Add  Добавлена возможность разрешить дублирование координат в .SaveINX параметром вызова (по умолчанию генерить исключение).
                                        4. Fix  Расставил const для параметров функций.
01.05.2012      v14     Dima2000        1. Add  Добавил свой класс для исключений.
01.05.2012      v15     Dima2000        1.      Снова переделал контроль ошибок, критичные генерят исключения, некритичные возвращают false и можно продолжать процесс, причина пока не сообщается.
                                        2. Add  Для критичных ошибок сделал все исключения потомками базового класса исключений.
******************************************************************************************************************************************************
ToDo:
+1. Вместо кодов ошибок генерить исключения.
+2. Вместо string принимать тайл в TMemoryStream.
-3. Выяснить можно ли (и как) скрыть объявления типов из interface, ведь они нужны лишь для самого этого класса и всё. Оказалось проблема в моей 5-й дельфи, она не позволяет, нужна 7-я.
-4. Ошибки выделения памяти, записи в файлы и прочие подобные проверять нет смысла - исключения и так сгенерятся, ну малопонятные, ну и что. Потом когда-нибудь добавим, может быть.
+5. Говорят надо свой класс ошибок сделать. Как и зачем?
+6. Добавить в описание список не смертельных исключений и рекомендации что с ними делать.
7. Проверить нужна ли буферизация вывода в файлы (с буфером порядка 1МБ). Или существенно скорости не добавит.
+8. Добавить механизм разрешения дублирования координат тайлов в .SaveINX(). Вероятно параметром вызова, чтобы не генерилось исключение.
*****************************************************************************************************************************************************)
unit SAS4WinCE;

interface

uses
        SysUtils, Classes;

type
        ESAS4WinCE = class(Exception); //Базовый класс исключений
        ESAS4WinCElimit = class(ESAS4WinCE); //Слишком большой или маленький лимит размера файлов (в .Create)
        ESAS4WinCEbigComment = class(ESAS4WinCE); //Слишком длинный комментарий (для указанного лимита размера файлов) (в .Create)
        ESAS4WinCEover100dat = class(ESAS4WinCE); //Более ста файлов данных формировать нельзя (в .Add)
        ESAS4WinCEsortingError = class(ESAS4WinCE); //Внутренняя ошибка сортировки, создавать индекс бессмысленно (в .SaveINX)
        ESAS4WinCEnoTiles = class(ESAS4WinCE); //А вот не будем экспортировать нулевое количество тайлов, не будем (в .SaveINX)
        ESAS4WinCEdupZXY = class(ESAS4WinCE); //Найдены тайлы с одинаковыми координатами z,x,y (может когда-то это и не ошибка, вдруг они с разным расширением, но прогу v_max может проглючить), поведение регулируется параметром при вызове (в .SaveINX)

        TTileInfo = packed record
                dzxy:int64; //d<<56+z<<48+x<<24+y
                ptr:integer; //Смещение в файле данных
                size:integer; //Длина файла
        end;
        TTableZX = packed record
                n:integer; //Значение
                ptr:integer; //Смещение таблицы в файле индекса
        end;
        TTableY = packed record
                n:integer; //Значение
                d:integer; //Номер файла данных
                ptr:integer; //Смещение тайла в файле данных
                size:integer; //Длина файла тайла
        end;

        TSAS4WinCE = class
        private
                fTilesNum:integer;
                fDataNum:integer; //Количество задействованных файлов данных
                fDataPrev:int64; //Размер всех предыдущих файлов данных
                fDataLen:int64; //Общий размер всех файлов данных включая и текущий формируемый
                fMaxSize:integer; //Максимально допустимый размер файлов данных
                fIndexLen:integer; //Размер файла индекса (заполняется после завершения экспорта)
                fFileName:string; //Имя (без расширения!) (можно с путём) создаваемых файлов
                fLimit:integer; //Максимально допустимый размер файлов данных (с учётом запаса и строк)
                FD:File;
                fComment:string; //Добавляемый к файлам комментарий
                bWriteTileInfo:boolean; //Записывать ли в файлы данных дополнительную информацию об тайле (12-15+ байтов) и копирайт в файлы данных и файл индекса
                bWriteFD:boolean; //Было ли задано имя файла данных
                bOpenFD:boolean; //Открыт ли файл данных
                bOpenFI:boolean; //Открыт ли файл индекса
                Ttiles:array of TTileInfo; //Массив инфы о тайлах
                t:TTileInfo; //Для операций над массивом и ускорения доступа
                Tz:array[1..24] of TTableZX; //Таблица Z
                Tx:array of TTableZX; //Таблица X[z]
                Ty:array of TTableY; //Таблица Y[z,x]
{!}             fSwap:int64; //Сколько всего обменов произвела сортировка
{!}             fMaxTx,fMaxTy:integer; //Максимальный размер таблиц X и Y
                procedure CreateINX(const fname:string; const bAllowDups:boolean); //Записать индекс в файл fname
                procedure SortAll(const l,r:integer; const m:int64); //Отсортировать буфер, применена битовая сортировка с 56-ю проходами для сортировки по полям z,x,y
                procedure CloseFD(); //Закрыть открытый файл данных, предварительно дописав туда комментарий
        public
                constructor Create(const fname:string; const maxsize:integer=-1; const cmt:string=''; const info:boolean=true); //Начинает процесс экспорта тайлов в файл fname (без расширения!); maxsize - максимально допустимый размер файлов данных (если <0, то взять значение по умолчанию); cmt - однократно добавляемый в конец файлов комментарий; info - записывать ли в файлы данных дополнительную информацию об тайле (12-15+ байтов) и копирайт в файлы данных и файл индекса. Копирайт является также сигнатурой наличия дополнительной инфы в файлах данных!
                destructor Destroy(); override; //Закрывает открытые файлы и освобождает память
                function Add(const z,x,y:integer; const ptile:pointer; const tilesize: integer; const fext:string=''):boolean; //Добавляет тайл: z,x,y его координаты; ftile содержимое; fext расширение исходного файла с тайлом. Вернёт true если тайл обработан.
                function SaveINX(const fname:string=''; const bAllowDups:boolean=false):boolean; //Завершить передачу тайлов, сформировать индекс, записать его в файл fname (без расширения!) (если не указан, то берётся имя файла данных); разрешение дубликатов с одинаковыми координатами. Вернёт true если всё прошло удачно.
                class function GetVersion():string; //Выдать строку с информацией о версии.
                class function GetCopyright():string; //Выдать строку с информацией о классе.
                property FileName:string        read fFileName; //Путь и имя (без расширения) создаваемых файлов данных (переданное .Create)
                property TilesNum:integer       read fTilesNum; //Общее количество принятых тайлов (модифицируется методом .Add)
                property DataNum:integer        read fDataNum; //Номер текущего файла данных (модифицируется методом .Add)
                property DataLen:int64          read fDataLen; //Общий размер всех файлов данных (до вызова .SaveINX - без одного комментария, в последнем файле данных) (модифицируется методами .Add и .SaveINX)
                property IndexLen:integer       read fIndexLen; //Размер файла индекса (заполняется методом .SaveINX после завершения экспорта)
                property Comment:string         read fComment; //Добавляемый к файлам комментарий (переданный .Create)
                property WriteTileInfo:boolean  read bWriteTileInfo; //Записывать ли в файлы данных дополнительную информацию об тайле (12-15+ байтов) и копирайт в файлы данных и файл индекса (переданное .Create)
                property MaxFileSize:integer    read fMaxSize; //Максимально допустимый размер файлов данных (переданное .Create)
{!}             property Swaps:int64            read fSwap; //Сколько всего обменов произвела сортировка
{!}             property MaxTx:integer          read fMaxTx; //Максимальный размер таблиц X
{!}             property MaxTy:integer          read fMaxTy; //Максимальный размер таблиц Y
        end;


implementation

const
        version='v15';
        copyright=#13#10'*****   Export from SAS.Planeta to SAS4WinCE  '+version+'   *****'#13#10#13#10#0;
        TilesSizeInc=100000; //На сколько увеличивать размер буфера при переполнении (в записях по 16Б)
        mask56=$FFFFFFFFFFFFFF; //Маска битов для сортировки


class function TSAS4WinCE.GetVersion():string;
begin
        Result:=version;
end;

class function TSAS4WinCE.GetCopyright():string;
begin
        Result:=copyright;
end;


procedure TSAS4WinCE.CloseFD(); //Закрыть открытый файл данных, предварительно дописав туда комментарий
var     s:string;
begin
        if not bOpenFD then Exit; //Нет открытого файла данных
        if length(Comment)>0 then begin
                s:=#13#10#13#10+Comment+#0; //Два перевода строки для визуального отделения комментария от бинарных данных и \0 в конец чтоб строка стала ASCIIZ
                Blockwrite(FD,s[1],length(s)); //Добавим к концу файла два перевода строки и пользовательскую строку
                Inc(fDataLen,length(s)); //Увеличим общий объём всех файлов данных на длину добавленной строки
        end;
        CloseFile(FD);
        bOpenFD:=false;
end;


function TSAS4WinCE.Add(const z,x,y:integer; const ptile:pointer; const tilesize: integer; const fext:string=''):boolean; //Добавляет тайл: z,x,y его координаты; ftile содержимое; fext расширение исходного файла с тайлом. Вернёт true если тайл обработан.
var     n,l:integer;
        s:string;
begin
        Result:=false; l:=tilesize;
        if l=0 then Exit;{raise ESAS4WinCE.Create('Size of tile is 0.');} //Тайлы нулевой длины не принимаем
        if l>fLimit then Exit;{raise ESAS4WinCE.Create('Size of tile is too big.');} //Тайлы длиннее предела не принимаем
        if not z in [1..24] then Exit;{raise ESAS4WinCE.Create('Incorrect zoom.');} //Зум может быть только 1..24
        n:=((1 shl z) shr 1)-1; //Максимально допустимые координаты
        if (x<0) or (x>n) then Exit;{raise ESAS4WinCE.Create('Incorrect x.');} //Тайлы с некорректными координатами не принимаем
        if (y<0) or (y>n) then Exit;{raise ESAS4WinCE.Create('Incorrect y.');} //Тайлы с некорректными координатами не принимаем
        if fDataLen-fDataPrev+int64(l)>fLimit then begin //Если не влезет в ограничение, то перейти к следующему файлу данных
                if fDataNum>=99 then raise ESAS4WinCEover100dat.Create('Over 100 data files is not allowed.'); //Больше ста файлов данных создавать нельзя
                Inc(fDataNum);
                fDataPrev:=fDataLen; //Если есть открытый файл данных, то запомним сколько было накоплено байтов в файлах данных
                if bWriteFD then begin
                        CloseFD();
                        AssignFile(FD,Format('%s.d%2.2d',[fFileName,fDataNum])); Rewrite(FD,1); bOpenFD:=true;
                end;
                if bWriteTileInfo then begin
                        s:=copyright;
                        Inc(fDataLen,length(s));
                        if bWriteFD then Blockwrite(FD,s[1],length(s));
                end;
        end;
        if bWriteTileInfo then begin //Сохранить информацию о файле
                n:=length(fext); if n>255 then n:=255;
                if bWriteFD then begin
                        Blockwrite(FD,z,1);
                        Blockwrite(FD,x,3);
                        Blockwrite(FD,y,3);
                        Blockwrite(FD,l,4);
                        Blockwrite(FD,n,1);
                        if n>0 then Blockwrite(FD,fext[1],n);
                end;
                Inc(fDataLen,1+3+3+4+1+n);
        end;
        if bWriteFD then Blockwrite(FD,ptile^,l);
        if fTilesNum>High(Ttiles) then SetLength(Ttiles,High(Ttiles)+1+TilesSizeInc); //Мало памяти, надо ещё увеличить буфер
        t.dzxy:=(int64(fDataNum) shl 56)+(int64(z) shl 48)+(int64(x) shl 24)+y;
        t.ptr:=fDataLen-fDataPrev;
        t.size:=l;
        Ttiles[fTilesNum]:=t;
        Inc(fTilesNum);
        Inc(fDataLen,l);
        Result:=true;
end;


procedure TSAS4WinCE.SortAll(const l,r:integer; const m:int64); //Отсортировать буфер, применена битовая сортировка с 56-ю проходами для сортировки по полям z,x,y
var     i,j:integer;
begin
        if (m=0) or (l>=r) then Exit;
        i:=l; j:=r;
        while i<=j do begin
                while i<=j do begin
                        if (Ttiles[i].dzxy and m)>0 then break; //Найдено a=1
                        Inc(i);
                end;
                while i<=j do begin
                        if (Ttiles[j].dzxy and m)=0 then break; //Найдено b=0
                        Dec(j);
                end;
                if i<j then begin //Найдено несоответствие, обменяем
                        t:=Ttiles[i];
                        Ttiles[i]:=Ttiles[j];
                        Ttiles[j]:=t;
                        Inc(i); Dec(j); //Найденные ячейки уже правильные, перейдём дальше
{!}                     Inc(fSwap);
                end;
        end;
        if (m>1) and (j+1<r) then SortAll(j+1,r,m shr 1); //Сортируем правый подмассив
        if (m>1) and (l<i-1) then SortAll(l,i-1,m shr 1); //Сортируем левый подмассив
end;


procedure TSAS4WinCE.CreateINX(const fname:string; const bAllowDups:boolean); //Записать индекс в файл fname
var     i,z,x,y,pz,px,nz,nx,ny:integer;
        s:string;
        p:TTileInfo;
        bWr:boolean;
begin
        bWr:=false; if length(fname)>0 then bWr:=true; //Писать будем только в непустой файл
        s:=''; if bWriteTileInfo then s:=#13#10+copyright+#0#0#0#0;
        if bWr then begin
                AssignFile(FD,fname+'.inx'); Rewrite(FD,1); bOpenFI:=true;
                for i:=1 to 24 do begin Tz[i].n:=0; Tz[i].ptr:=0; end;
                z:=0; Blockwrite(FD,z,4); Blockwrite(FD,Tz,24*SizeOf(TTableZX)); //Начальная талица Z, всегда 24 нулевых элемента
                if length(s)>0 then Blockwrite(FD,s[1],length(s) and -4); //После таблицы Z запишем копирайт, в файле индекса длину копирайта выравниваем на 4 байта
        end;
        fIndexLen:=4+24*SizeOf(TTableZX)+(length(s) and -4); //Размер таблицы Z и копирайта
        pz:=0; px:=-1; nz:=0; nx:=0; ny:=0; p.dzxy:=-1;
{!}     fMaxTx:=0; fMaxTy:=0;
        for i:=0 to fTilesNum do begin //Последний раз фиктивный, только для сохранения накопленных таблиц
                t.dzxy:=-1; if i<fTilesNum then t:=Ttiles[i]; //Фиктивный раз тайл не читаем
                z:=((t.dzxy shr 48) and $FF);
                x:=((t.dzxy shr 24) and $FFFFFF);
                y:=((t.dzxy shr  0) and $FFFFFF);
                if (t.dzxy and mask56)<p.dzxy then raise ESAS4WinCEsortingError.Create('Found sorting error!'); //Найдена ошибка сортировки!
                if (not bAllowDups) and ((t.dzxy and mask56)=p.dzxy) then raise ESAS4WinCEdupZXY.Create('Duplicate z,x,y in tiles.'); //Найдены тайлы с одинаковыми координатами z,x,y (это в общем-то не обязательно ошибка, может они с разным расширением)
                p.dzxy:=t.dzxy and mask56;
                if (z<>pz) or (x<>px) then begin //Обнаружен новый Z или X, пора начать новую таблицу Y
                        if ny>0 then begin //Были Y, сохраним таблицу Y
                                if nx>High(Tx) then SetLength(Tx,High(Tx)+1+10000); //Слишком много X, не влезают в таблицу, увеличим размер таблички
                                Tx[nx].n:=px; Tx[nx].ptr:=fIndexLen; //Указатель на начало таблицы Y
                                if bWr then begin
                                        Blockwrite(FD,ny,4); Blockwrite(FD,Ty[0],ny*SizeOf(TTableY));
                                end;
{!}                             if ny>fMaxTy then fMaxTy:=ny;
                                Inc(nx); Inc(fIndexLen,4+ny*SizeOf(TTableY));
                        end;
                        ny:=0; //Начали новую таблицу Y
                end;
                if z<>pz then begin //Обнаружен новый Z, значит пора записать таблицу X и начать новую
                        if nx>0 then begin
                                Inc(nz);
                                Tz[nz].n:=pz; Tz[nz].ptr:=fIndexLen; //Указатель на начало таблицы X
                                if bWr then begin
                                        Blockwrite(FD,nx,4); Blockwrite(FD,Tx[0],nx*SizeOf(TTableZX));
                                end;
{!}                             if nx>fMaxTx then fMaxTx:=nx;
                                Inc(fIndexLen,4+nx*SizeOf(TTableZX));
                        end;
                        nx:=0; //Начали новую таблицу X
                end;
                if ny>High(Ty) then SetLength(Ty,High(Ty)+1+10000); //Слишком много Y, не влезают в таблицу, увеличим размер таблички
                Ty[ny].n:=y;
                Ty[ny].d:=t.dzxy shr 56;
                Ty[ny].ptr:=t.ptr;
                Ty[ny].size:=t.size;
                Inc(ny);
                pz:=z; px:=x;
        end;
        if bWr then begin
                if length(comment)>0 then begin //Добавим в конец файла индекса пользовательский комментарий
                        s:=#13#10#13#10+Comment+#0; //Два перевода строки для визуального отделения комментария от бинарных данных и \0 в конец чтоб строка стала ASCIIZ
                        Blockwrite(FD,s[1],length(s)); //Добавим к концу файла два перевода строки и пользовательскую строку
                        Inc(fIndexLen,length(s)); //Увеличим объём файла индекса на длину добавленной строки
                end;
                Seek(FD,0);
                Blockwrite(FD,nz,4);
                Blockwrite(FD,Tz[1],24*SizeOf(TTableZX));
                CloseFile(FD);
                bOpenFI:=false;
        end;
        SetLength(Tx,0); SetLength(Ty,0); //Освободить память
end;


constructor TSAS4WinCE.Create(const fname:string; const maxsize:integer=-1; const cmt:string=''; const info:boolean=true); //Начинает процесс экспорта тайлов в файл fname (без расширения!); maxsize - максимально допустимый размер файлов данных (если <0, то взять значение по умолчанию); cmt - однократно добавляемый в конец файлов комментарий; info - записывать ли в файлы данных дополнительную информацию об тайле (12-15+ байтов) и копирайт в файлы данных и файл индекса. Копирайт является также сигнатурой наличия дополнительной инфы в файлах данных!
var     maxlen:integer;
begin
        inherited Create;
        maxlen:=maxsize;
        if maxlen>=(int64(2) shl 31)-2 then raise ESAS4WinCElimit.Create('Limit file size is too big!'); //Такие большие файлы создавать не умеем
        if maxlen<0 then maxlen:=1000000000;
        if maxlen<1000000 then raise ESAS4WinCElimit.Create('Limit file size is too small!'); //Нельзя создавать слишком маленькие файлы данных, там же копирайт и комменты!
        if length(cmt)>maxlen div 2 then raise ESAS4WinCEbigComment.Create('Comment is too big!'); //А вот не дам добавлять комментарии в половину файла данных!
        bWriteFD:=false; if length(fname)>0 then bWriteFD:=true; //Если не задано имя файла для записи, то не создаём файлы данных, лишь внутренние операции
        fTilesNum:=0; //Нет тайлов
        fMaxSize:=maxlen;
        fComment:=cmt;
        bWriteTileInfo:=info;
        bOpenFD:=false; //Нет открытого файла данных
        fDataNum:=-1; //Нет открытого файла данных
        fDataLen:=0; //Пока не записано ни байта
        fDataPrev:=-fMaxSize; //Начать новый файл данных
        fFileName:=fname;
        bOpenFI:=false; //Файл индекса не открыт
        SetLength(Ttiles,TilesSizeInc); //Выделить немного памяти заранее
        fLimit:=fMaxSize-10; //Запас
        if length(Comment)>0 then Dec(fLimit,length(Comment)); //Уменьшим предел на длину комментария если он есть
        if bWriteTileInfo then Dec(fLimit,length(copyright)+300); //И на длину копирайта если таковой пишется и длинного расширения с recovery инфой
        fSwap:=0; fMaxTx:=0; fMaxTy:=0;
end;


destructor TSAS4WinCE.Destroy(); //Закрывает открытые файлы и освобождает память
begin
        CloseFD();
        SetLength(Ttiles,0); //Освободить память
        inherited Destroy;
end;


function TSAS4WinCE.SaveINX(const fname:string=''; const bAllowDups:boolean=false):boolean; //Завершить передачу тайлов, сформировать индекс, записать его в файл fname (без расширения!) (если не указан, то берётся имя файла данных); разрешение дубликатов с одинаковыми координатами. Вернёт true если всё прошло удачно.
var     fn:string;
begin
        if bOpenFI then CloseFile(FD); //Закроем открытый файл индекса
        bOpenFI:=false;
        if fTilesNum=0 then raise ESAS4WinCEnoTiles.Create('Nothing to export.'); //А вот не будем экспортировать нулевое количество тайлов, не будем
        CloseFD();
        fSwap:=0;
        SortAll(0,fTilesNum-1,mask56 xor (mask56 shr 1)); //Отсортировать буфер, применена битовая сортировка с 56-ю проходами для сортировки по полям z,x,y
        fn:=fname; if length(fn)=0 then fn:=fFileName; //Если не задано имя файла индекса, то используем имя файла данных
        CreateINX(fn,bAllowDups); //Создать файл индекса
        Result:=true;
end;


end.
