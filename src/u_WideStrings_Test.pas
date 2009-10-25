unit u_WideStrings_Test;

interface

implementation

uses
  cUnicodeChar,
  u_WideStrings, SysUtils;

procedure TestAdd;
var
  VList: TWideStringList;
  VIndex: Integer;
begin
  VList := TWideStringList.Create;
  try
    VList.Add(WideString('Test')+ WideLineSeparator);
    if WideCompareStr(VList[0], 'Test') = 0 then begin
      raise Exception.Create('Потерялся последний символ.');
    end;
    if (VList[0])[1] <> WideChar('T') then begin
      raise Exception.Create('Потерялся первый символ.');
    end;
    if (VList[0])[5] <> WideLineSeparator then begin
      raise Exception.Create('Потерялся последний символ.');
    end;
    VList[0] := 'Test';
    VList.Values['Test'] := 'Proba';
    if WideCompareStr(VList.Values['Test'], 'Proba') <> 0 then begin
      raise Exception.Create('Ошибка работы объекта');
    end;
    VList.AddObject('IntTest', TObject(10));
    VIndex := VList.IndexOf('IntTest');
    if VIndex < 0 then begin
      raise Exception.Create('Ошибка работы объекта');
    end;
    if Integer(VList.Objects[VIndex]) <> 10 then begin
      raise Exception.Create('Ошибка работы объекта');
    end;
    VList.Delete(1);
    if VList.Count <> 2 then begin
      raise Exception.Create('Ошибка работы объекта');
    end;
  finally
    FreeAndNil(VList);
  end;
end;


procedure Test;
begin
  TestAdd;
end;

initialization
  test;
  Writeln('u_WideStrings_Test passed');
end.
