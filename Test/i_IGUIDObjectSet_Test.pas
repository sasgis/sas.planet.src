unit i_IGUIDObjectSet_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  ActiveX,
  TestFramework,
  i_GUIDSet;

type
  // Test methods for interface IGUIDObjectSet

  TestIGUIDObjectSet = class(TTestCase)
  protected
    FGUIDList: IGUIDObjectSet;
    FG1: TObject;
    FG2: TObject;
    FG3: TObject;
    FG4: TObject;
    FG5: TObject;
    FG6: TObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestIsExists;
    procedure TestGetByGUID;
    procedure TestReplace;
    procedure TestRemove;
    procedure TestClear;
    procedure TestGetGUIDEnum;
  end;

implementation

uses
  SysUtils;

type
  TSimple = class
  protected
    FGUID: TGUID;
  public
    constructor Create(AGUID: TGUID);
    function GetGUID(): TGUID;
  end;

{ TSimple }

constructor TSimple.Create(AGUID: TGUID);
begin
  FGUID := AGUID;
end;

function TSimple.GetGUID: TGUID;
begin
  Result := FGUID;
end;

const
  G1: TGUID = '{357CDEAB-14FE-449E-B282-A5B96094BE81}';
  G2: TGUID = '{357CDEAB-14FE-449E-B282-A5B96094BE82}';
  G3: TGUID = '{357CDEAB-14FE-449E-B282-A5B96094BE83}';
  G4: TGUID = '{357CDEAB-14FE-449E-B282-A5B96094BE84}';
  G5: TGUID = '{86EA601F-EA2D-4E26-BDBE-1C4F65444CA5}';
  G6: TGUID = '{F81CCA0A-D467-4962-A9F7-2A50B4BFDD46}';
  G7: TGUID = '{F81CCA0A-D467-4962-A9F7-2A50B4BFDD47}';


{ TestIGUIDObjectSet }

procedure TestIGUIDObjectSet.SetUp;
begin
  inherited;
  FG1 := TSimple.Create(G1);
  FG2 := TSimple.Create(G2);
  FG3 := TSimple.Create(G3);
  FG4 := TSimple.Create(G4);
  FG5 := TSimple.Create(G5);
  FG6 := TSimple.Create(G6);
end;

procedure TestIGUIDObjectSet.TearDown;
begin
  FG1.Free;
  FG2.Free;
  FG3.Free;
  FG4.Free;
  FG5.Free;
  FG6.Free;
  FGUIDList := nil;
end;

procedure TestIGUIDObjectSet.TestAdd;
var
  VResult: TObject;
  VSource: TObject;
begin
  VResult := FGUIDList.Add(G1, FG1);
  Check(FGUIDList.Count = 1, 'После добавления должно быть элементов: 1');
  Check(VResult = FG1, 'После добавления функция Add должна вернуть добавленный объект');

  FGUIDList.Add(G2, FG2);
  Check(FGUIDList.Count = 2, 'После добавления должно быть элементов: 2');

  FGUIDList.Add(G3, FG3);
  Check(FGUIDList.Count = 3, 'После добавления должно быть элементов: 3');

  FGUIDList.Add(G4, FG4);
  Check(FGUIDList.Count = 4, 'После добавления должно быть элементов: 4');

  FGUIDList.Add(G5, FG5);
  Check(FGUIDList.Count = 5, 'После добавления должно быть элементов: 5');

  FGUIDList.Add(G6, FG6);
  Check(FGUIDList.Count = 6, 'После добавления должно быть элементов: 6');

  VSource := TSimple.Create(G2);
  try
    VResult := FGUIDList.Add(G1, VSource);
    Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
    Check(VResult = FG1, 'Должно вернуть старый объект');
  finally
    VSource.Free;
  end;
  VSource := TSimple.Create(G3);
  try
    FGUIDList.Add(G2, VSource);
    Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  finally
    VSource.Free;
  end;
  VSource := TSimple.Create(G4);
  try
    FGUIDList.Add(G3, VSource);
    Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  finally
    VSource.Free;
  end;
  VSource := TSimple.Create(G5);
  try
    FGUIDList.Add(G4, VSource);
    Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  finally
    VSource.Free;
  end;
  VSource := TSimple.Create(G6);
  try
    FGUIDList.Add(G5, VSource);
    Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  finally
    VSource.Free;
  end;
  VSource := TSimple.Create(G1);
  try
    FGUIDList.Add(G6, VSource);
    Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  finally
    VSource.Free;
  end;
end;

procedure TestIGUIDObjectSet.TestIsExists;
begin
  FGUIDList.Add(G6, FG6);
  FGUIDList.Add(G1, FG1);
  FGUIDList.Add(G5, FG5);
  FGUIDList.Add(G2, FG2);
  FGUIDList.Add(G4, FG4);
  FGUIDList.Add(G3, FG3);

  Check(FGUIDList.IsExists(G1), 'Ошбка проверки наличия элемента G1');
  Check(FGUIDList.IsExists(G2), 'Ошбка проверки наличия элемента G2');
  Check(FGUIDList.IsExists(G3), 'Ошбка проверки наличия элемента G3');
  Check(FGUIDList.IsExists(G4), 'Ошбка проверки наличия элемента G4');
  Check(FGUIDList.IsExists(G5), 'Ошбка проверки наличия элемента G5');
  Check(FGUIDList.IsExists(G6), 'Ошбка проверки наличия элемента G6');
  Check(not FGUIDList.IsExists(G7), 'Ошбка проверки наличия элемента G7');
end;

procedure TestIGUIDObjectSet.TestGetByGUID;
var
  VI: TSimple;
begin
  FGUIDList.Add(G6, FG6);
  FGUIDList.Add(G1, FG1);
  FGUIDList.Add(G5, FG5);
  FGUIDList.Add(G2, FG2);
  FGUIDList.Add(G4, FG4);
  FGUIDList.Add(G3, FG3);

  VI := TSimple(FGUIDList.GetByGUID(G1));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G1), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G2));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G2), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G3));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G3), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G4));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G4), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G5));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G5), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G6));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G6), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G7));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');

end;

procedure TestIGUIDObjectSet.TestReplace;
var
  VI: TSimple;
begin
  FGUIDList.Add(G6, FG6);
  FGUIDList.Add(G1, FG1);
  FGUIDList.Add(G5, FG5);
  FGUIDList.Add(G2, FG2);
  FGUIDList.Add(G4, FG4);
  FGUIDList.Add(G3, FG3);

  FGUIDList.Replace(G1, FG2);
  FGUIDList.Replace(G2, FG3);
  FGUIDList.Replace(G3, FG4);
  FGUIDList.Replace(G4, FG5);
  FGUIDList.Replace(G5, FG6);
  FGUIDList.Replace(G6, FG1);

  VI := TSimple(FGUIDList.GetByGUID(G1));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G2), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G2));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G3), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G3));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G4), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G4));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G5), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G5));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G6), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G6));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G1), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G7));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');
end;

procedure TestIGUIDObjectSet.TestRemove;
var
  VI: TSimple;
begin

  FGUIDList.Add(G6, FG6);
  FGUIDList.Add(G1, FG1);
  FGUIDList.Add(G5, FG5);
  FGUIDList.Add(G2, FG2);
  FGUIDList.Add(G4, FG4);
  FGUIDList.Add(G3, FG3);

  FGUIDList.Remove(G1);
  FGUIDList.Remove(G3);
  FGUIDList.Remove(G5);

  VI := TSimple(FGUIDList.GetByGUID(G1));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');

  VI := TSimple(FGUIDList.GetByGUID(G2));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G2), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G3));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');

  VI := TSimple(FGUIDList.GetByGUID(G4));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G4), 'Найден ошибочный элемент');

  VI := TSimple(FGUIDList.GetByGUID(G5));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');

  VI := TSimple(FGUIDList.GetByGUID(G6));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G6), 'Найден ошибочный элемент');

end;

procedure TestIGUIDObjectSet.TestClear;
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  I: Cardinal;
begin
  FGUIDList.Add(G6, FG6);
  FGUIDList.Add(G1, FG1);
  FGUIDList.Add(G5, FG5);
  FGUIDList.Add(G2, FG2);
  FGUIDList.Add(G4, FG4);
  FGUIDList.Add(G3, FG3);

  FGUIDList.Clear;
  Check(FGUIDList.Count = 0, 'Список не пустой после очистки');

  VEnum := FGUIDList.GetGUIDEnum;
  Check(VEnum <> nil, 'Итератор не получен');

  Check(VEnum.Next(1, VGUID, I) = S_FALSE, 'Лишний элемент в итераторе');
end;

procedure TestIGUIDObjectSet.TestGetGUIDEnum;
var
  VGUID: TGUID;
  I: Cardinal;
  VEnum: IEnumGUID;
begin
  FGUIDList.Add(G6, FG6);
  FGUIDList.Add(G1, FG1);
  FGUIDList.Add(G5, FG5);
  FGUIDList.Add(G2, FG2);
  FGUIDList.Add(G4, FG4);
  FGUIDList.Add(G3, FG3);

  VEnum := FGUIDList.GetGUIDEnum;
  Check(VEnum <> nil, 'Итератор не получен');

  Check(VEnum.Next(1, VGUID, I) = S_OK, 'Ошибка получения GUID');
  Check(IsEqualGUID(VGUID, G1), 'Ошибочный элемент.');
  Check(VEnum.Next(1, VGUID, I) = S_OK, 'Ошибка получения GUID');
  Check(IsEqualGUID(VGUID, G2), 'Ошибочный элемент.');
  Check(VEnum.Next(1, VGUID, I) = S_OK, 'Ошибка получения GUID');
  Check(IsEqualGUID(VGUID, G3), 'Ошибочный элемент.');
  Check(VEnum.Next(1, VGUID, I) = S_OK, 'Ошибка получения GUID');
  Check(IsEqualGUID(VGUID, G4), 'Ошибочный элемент.');
  Check(VEnum.Next(1, VGUID, I) = S_OK, 'Ошибка получения GUID');
  Check(IsEqualGUID(VGUID, G5), 'Ошибочный элемент.');
  Check(VEnum.Next(1, VGUID, I) = S_OK, 'Ошибка получения GUID');
  Check(IsEqualGUID(VGUID, G6), 'Ошибочный элемент.');
  Check(VEnum.Next(1, VGUID, I) = S_FALSE, 'Лишний элемент в итераторе');
end;

end.
