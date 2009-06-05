unit BMSearch;

(* -------------------------------------------------------------------

Поиск строки методом Boyer-Moore.

Это - один из самых быстрых алгоритмов поиска строки.
See a description in:

R. Boyer и S. Moore.
Быстрый алгоритм поиска строки.
Communications of the ACM 20, 1977, страницы 762-772
------------------------------------------------------------------- *)

interface

type
{$IFDEF WINDOWS}

  size_t = Word;
{$ELSE}

  size_t = LongInt;
{$ENDIF}

type

  TTranslationTable = array[char] of char; { таблица перевода }

  TSearchBM = class(TObject)
  private
    FTranslate: TTranslationTable; { таблица перевода }
    FJumpTable: array[char] of Byte; { таблица переходов }
    FShift_1: integer;
    FPattern: pchar;
    FPatternLen: size_t;

  public
    procedure Prepare(Pattern: pchar; PatternLen: size_t; IgnoreCase: Boolean);
    procedure PrepareStr(const Pattern: string; IgnoreCase: Boolean);

    function Search(Text: pchar; TextLen: size_t): pchar;
    function Pos(const S: string): integer;
  end;

implementation

uses SysUtils;

(* -------------------------------------------------------------------

Игнорируем регистр таблицы перевода
------------------------------------------------------------------- *)

procedure CreateTranslationTable(var T: TTranslationTable; IgnoreCase: Boolean);
var

  c: char;
begin

  for c := #0 to #255 do
    T[c] := c;

  if not IgnoreCase then
    exit;

  for c := 'a' to 'z' do
    T[c] := UpCase(c);

  { Связываем все нижние символы с их эквивалентом верхнего регистра }

  T['Б'] := 'A';
  T['А'] := 'A';
  T['Д'] := 'A';
  T['В'] := 'A';

  T['б'] := 'A';
  T['а'] := 'A';
  T['д'] := 'A';
  T['в'] := 'A';

  T['Й'] := 'E';
  T['И'] := 'E';
  T['Л'] := 'E';
  T['К'] := 'E';

  T['й'] := 'E';
  T['и'] := 'E';
  T['л'] := 'E';
  T['к'] := 'E';

  T['Н'] := 'I';
  T['М'] := 'I';
  T['П'] := 'I';
  T['О'] := 'I';

  T['н'] := 'I';
  T['м'] := 'I';
  T['п'] := 'I';
  T['о'] := 'I';

  T['У'] := 'O';
  T['Т'] := 'O';
  T['Ц'] := 'O';
  T['Ф'] := 'O';

  T['у'] := 'O';
  T['т'] := 'O';
  T['ц'] := 'O';
  T['ф'] := 'O';

  T['Ъ'] := 'U';
  T['Щ'] := 'U';
  T['Ь'] := 'U';
  T['Ы'] := 'U';

  T['ъ'] := 'U';
  T['щ'] := 'U';
  T['ь'] := 'U';
  T['ы'] := 'U';

  T['с'] := 'С';
end;

(* -------------------------------------------------------------------

Подготовка таблицы переходов
------------------------------------------------------------------- *)

procedure TSearchBM.Prepare(Pattern: pchar; PatternLen: size_t;

  IgnoreCase: Boolean);
var

  i: integer;
  c, lastc: char;
begin

  FPattern := Pattern;
  FPatternLen := PatternLen;

  if FPatternLen < 1 then
    FPatternLen := strlen(FPattern);

  { Данный алгоритм базируется на наборе из 256 символов }

  if FPatternLen > 256 then
    exit;

  { 1. Подготовка таблицы перевода }

  CreateTranslationTable(FTranslate, IgnoreCase);

  { 2. Подготовка таблицы переходов }

  for c := #0 to #255 do
    FJumpTable[c] := FPatternLen;

  for i := FPatternLen - 1 downto 0 do
  begin
    c := FTranslate[FPattern[i]];
    if FJumpTable[c] >= FPatternLen - 1 then
      FJumpTable[c] := FPatternLen - 1 - i;
  end;

  FShift_1 := FPatternLen - 1;
  lastc := FTranslate[Pattern[FPatternLen - 1]];

  for i := FPatternLen - 2 downto 0 do
    if FTranslate[FPattern[i]] = lastc then
    begin
      FShift_1 := FPatternLen - 1 - i;
      break;
    end;

  if FShift_1 = 0 then
    FShift_1 := 1;
end;

procedure TSearchBM.PrepareStr(const Pattern: string; IgnoreCase: Boolean);
var

  str: pchar;
begin

  if Pattern <> '' then
  begin
{$IFDEF Windows}

    str := @Pattern[1];
{$ELSE}

    str := pchar(Pattern);
{$ENDIF}

    Prepare(str, Length(Pattern), IgnoreCase);
  end;
end;

{ Поиск последнего символа & просмотр справа налево }

function TSearchBM.Search(Text: pchar; TextLen: size_t): pchar;
var

  shift, m1, j: integer;
  jumps: size_t;
begin

  result := nil;
  if FPatternLen > 256 then
    exit;

  if TextLen < 1 then
    TextLen := strlen(Text);

  m1 := FPatternLen - 1;
  shift := 0;
  jumps := 0;

  { Поиск последнего символа }

  while jumps <= TextLen do
  begin
    Inc(Text, shift);
    shift := FJumpTable[FTranslate[Text^]];
    while shift <> 0 do
    begin
      Inc(jumps, shift);
      if jumps > TextLen then
        exit;

      Inc(Text, shift);
      shift := FJumpTable[FTranslate[Text^]];
    end;

    { Сравниваем справа налево FPatternLen - 1 символов }

    if jumps >= m1 then
    begin
      j := 0;
      while FTranslate[FPattern[m1 - j]] = FTranslate[(Text - j)^] do
      begin
        Inc(j);
        if j = FPatternLen then
        begin
          result := Text - m1;
          exit;
        end;
      end;
    end;

    shift := FShift_1;
    Inc(jumps, shift);
  end;
end;

function TSearchBM.Pos(const S: string): integer;
var

  str, p: pchar;
begin

  result := 0;
  if S <> '' then
  begin
{$IFDEF Windows}

    str := @S[1];
{$ELSE}

    str := pchar(S);
{$ENDIF}

    p := Search(str, Length(S));
    if p <> nil then
      result := 1 + p - str;
  end;
end;

end.