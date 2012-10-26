unit u_WideStrings_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  SysUtils,
  TestFramework;

type
  TestTWideStringList = class(TTestCase)
  published
    procedure TestAdd;
  end;

implementation

uses
  cUnicode,
  WideStrings;

procedure TestTWideStringList.TestAdd;
var
  VList: TWideStringList;
  VIndex: Integer;
begin
  VList := TWideStringList.Create;
  try
    VList.Add(WideString('Test')+ WideLineSeparator);
    Check(WideCompareStr(VList[0], 'Test') <> 0, 'Плохо обработался разделитель строк.');
    Check((VList[0])[1] = WideChar('T'), 'Потерялся первый символ.');
    Check((VList[0])[5] = WideLineSeparator, 'Потерялся последний символ.');
    VList[0] := 'Test';
    VList.Values['Test'] := 'Proba';
    Check(WideCompareStr(VList.Values['Test'], 'Proba') = 0, 'Не нашлось значение по ключу');
    VList.AddObject('IntTest', TObject(10));
    VIndex := VList.IndexOf('IntTest');
    Check(VIndex >= 0, 'Не нашлось по ключу');
    Check(Integer(VList.Objects[VIndex]) = 10, 'По ключу получен не тот объект, который записали');
    VList.Delete(1);
    Check(VList.Count = 2,'После удаления должно было остаться 2 строки');
  finally
    FreeAndNil(VList);
  end;
end;


initialization
  RegisterTest(TestTWideStringList.Suite);
end.
