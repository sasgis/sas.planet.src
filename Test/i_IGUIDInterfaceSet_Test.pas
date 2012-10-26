unit i_IGUIDInterfaceSet_Test;

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
  // Test methods for interface IGUIDInterfaceSet

  TestIGUIDInterfaceSet = class(TTestCase)
  protected
    FGUIDList: IGUIDInterfaceSet;
  public
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
  ISimple = interface(IInterface)
    function GetGUID(): TGUID;
    function GetCounter(): integer;
    property Counter: integer read GetCounter;
  end;

  TSimple = class(TInterfacedObject, ISimple, IInterface)
  protected
    FGUID: TGUID;
  public
    constructor Create(AGUID: TGUID);
    function GetGUID(): TGUID;
    function GetCounter(): integer;
  end;

{ TSimple }

constructor TSimple.Create(AGUID: TGUID);
begin
  FGUID := AGUID;
end;

function TSimple.GetCounter: integer;
begin
  Result := FRefCount;
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


{ TestIGUIDInterfaceSet }

procedure TestIGUIDInterfaceSet.TearDown;
begin
  FGUIDList := nil;
end;

procedure TestIGUIDInterfaceSet.TestAdd;
var
  VSource: IInterface;
  VResult: IInterface;
begin
  VSource := TSimple.Create(G1);
  VResult := FGUIDList.Add(G1, VSource);
  Check(FGUIDList.Count = 1, 'После добавления должно быть элементов: 1');
  Check(VResult = VSource, 'После добавления функция Add должна вернуть добавленный объект');
  FGUIDList.Add(G2, TSimple.Create(G2));
  Check(FGUIDList.Count = 2, 'После добавления должно быть элементов: 2');
  FGUIDList.Add(G3, TSimple.Create(G3));
  Check(FGUIDList.Count = 3, 'После добавления должно быть элементов: 3');
  FGUIDList.Add(G4, TSimple.Create(G4));
  Check(FGUIDList.Count = 4, 'После добавления должно быть элементов: 4');
  FGUIDList.Add(G5, TSimple.Create(G5));
  Check(FGUIDList.Count = 5, 'После добавления должно быть элементов: 5');
  FGUIDList.Add(G6, TSimple.Create(G6));
  Check(FGUIDList.Count = 6, 'После добавления должно быть элементов: 6');

  VResult := FGUIDList.Add(G1, TSimple.Create(G2));
  Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  Check(VResult = VSource, 'Должно вернуть старый объект');

  FGUIDList.Add(G2, TSimple.Create(G3));
  Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  FGUIDList.Add(G3, TSimple.Create(G4));
  Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  FGUIDList.Add(G4, TSimple.Create(G5));
  Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  FGUIDList.Add(G5, TSimple.Create(G6));
  Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
  FGUIDList.Add(G6, TSimple.Create(G1));
  Check(FGUIDList.Count = 6, 'После добавления неунинкального количество меняться не должно');
end;

procedure TestIGUIDInterfaceSet.TestIsExists;
begin
  FGUIDList.Add(G6, TSimple.Create(G6));
  FGUIDList.Add(G1, TSimple.Create(G1));
  FGUIDList.Add(G5, TSimple.Create(G5));
  FGUIDList.Add(G2, TSimple.Create(G2));
  FGUIDList.Add(G4, TSimple.Create(G4));
  FGUIDList.Add(G3, TSimple.Create(G3));

  Check(FGUIDList.IsExists(G1), 'Ошбка проверки наличия элемента G1');
  Check(FGUIDList.IsExists(G2), 'Ошбка проверки наличия элемента G2');
  Check(FGUIDList.IsExists(G3), 'Ошбка проверки наличия элемента G3');
  Check(FGUIDList.IsExists(G4), 'Ошбка проверки наличия элемента G4');
  Check(FGUIDList.IsExists(G5), 'Ошбка проверки наличия элемента G5');
  Check(FGUIDList.IsExists(G6), 'Ошбка проверки наличия элемента G6');
  Check(not FGUIDList.IsExists(G7), 'Ошбка проверки наличия элемента G7');
end;

procedure TestIGUIDInterfaceSet.TestGetByGUID;
var
  VI: ISimple;
begin
  FGUIDList.Add(G6, TSimple.Create(G6));
  FGUIDList.Add(G1, TSimple.Create(G1));
  FGUIDList.Add(G5, TSimple.Create(G5));
  FGUIDList.Add(G2, TSimple.Create(G2));
  FGUIDList.Add(G4, TSimple.Create(G4));
  FGUIDList.Add(G3, TSimple.Create(G3));

  VI := ISimple(FGUIDList.GetByGUID(G1));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G1), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G2));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G2), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G3));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G3), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G4));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G4), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G5));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G5), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G6));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G6), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G7));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');

end;

procedure TestIGUIDInterfaceSet.TestReplace;
var
  VI: ISimple;
begin
  FGUIDList.Add(G6, TSimple.Create(G6));
  FGUIDList.Add(G1, TSimple.Create(G1));
  FGUIDList.Add(G5, TSimple.Create(G5));
  FGUIDList.Add(G2, TSimple.Create(G2));
  FGUIDList.Add(G4, TSimple.Create(G4));
  FGUIDList.Add(G3, TSimple.Create(G3));

  FGUIDList.Replace(G1, TSimple.Create(G2));
  FGUIDList.Replace(G2, TSimple.Create(G3));
  FGUIDList.Replace(G3, TSimple.Create(G4));
  FGUIDList.Replace(G4, TSimple.Create(G5));
  FGUIDList.Replace(G5, TSimple.Create(G6));
  FGUIDList.Replace(G6, TSimple.Create(G1));

  VI := ISimple(FGUIDList.GetByGUID(G1));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G2), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G2));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G3), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G3));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G4), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G4));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G5), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G5));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G6), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G6));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G1), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G7));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');
end;

procedure TestIGUIDInterfaceSet.TestRemove;
var
  VI: ISimple;
begin

  FGUIDList.Add(G6, TSimple.Create(G6));
  FGUIDList.Add(G1, TSimple.Create(G1));
  FGUIDList.Add(G5, TSimple.Create(G5));
  FGUIDList.Add(G2, TSimple.Create(G2));
  FGUIDList.Add(G4, TSimple.Create(G4));
  FGUIDList.Add(G3, TSimple.Create(G3));

  FGUIDList.Remove(G1);
  FGUIDList.Remove(G3);
  FGUIDList.Remove(G5);

  VI := ISimple(FGUIDList.GetByGUID(G1));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');

  VI := ISimple(FGUIDList.GetByGUID(G2));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G2), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G3));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');

  VI := ISimple(FGUIDList.GetByGUID(G4));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G4), 'Найден ошибочный элемент');

  VI := ISimple(FGUIDList.GetByGUID(G5));
  Check(VI = nil, 'Найден элемент, которого быть не должно было');

  VI := ISimple(FGUIDList.GetByGUID(G6));
  Check(VI <> nil, 'Элемент не найден');
  Check(IsEqualGUID(VI.GetGUID, G6), 'Найден ошибочный элемент');

end;

procedure TestIGUIDInterfaceSet.TestClear;
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  I: Cardinal;
begin
  FGUIDList.Add(G6, TSimple.Create(G6));
  FGUIDList.Add(G1, TSimple.Create(G1));
  FGUIDList.Add(G5, TSimple.Create(G5));
  FGUIDList.Add(G2, TSimple.Create(G2));
  FGUIDList.Add(G4, TSimple.Create(G4));
  FGUIDList.Add(G3, TSimple.Create(G3));

  FGUIDList.Clear;
  Check(FGUIDList.Count = 0, 'Список не пустой после очистки');

  VEnum := FGUIDList.GetGUIDEnum;
  Check(VEnum <> nil, 'Итератор не получен');

  Check(VEnum.Next(1, VGUID, I) = S_FALSE, 'Лишний элемент в итераторе');
end;

procedure TestIGUIDInterfaceSet.TestGetGUIDEnum;
var
  VGUID: TGUID;
  I: Cardinal;
  VEnum: IEnumGUID;
begin
  FGUIDList.Add(G6, TSimple.Create(G6));
  FGUIDList.Add(G1, TSimple.Create(G1));
  FGUIDList.Add(G5, TSimple.Create(G5));
  FGUIDList.Add(G2, TSimple.Create(G2));
  FGUIDList.Add(G4, TSimple.Create(G4));
  FGUIDList.Add(G3, TSimple.Create(G3));

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
