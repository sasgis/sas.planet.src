unit u_HashCacheWithQueuesAbstract_Test;

interface

uses
  TestFramework,
  i_HashInterfaceCache,
  u_BaseInterfacedObject;

type
  ISimple = interface
    ['{B16B5C6C-1003-4C46-ACB7-757AC08422E0}']
    function GetKey: UInt64;
    property Key: UInt64 read GetKey;

    function GetOpId: Integer;
    property OpId: Integer read GetOpId;
  end;

  TSimple = class(TInterfacedObject, ISimple)
  private
    FKey: UInt64;
    FOpId: Integer;
  private
    function GetKey: UInt64;
    function GetOpId: Integer;
  public
    constructor Create(
      AKey: UInt64;
      AOpId: Integer
    );
  end;

  IHashCacheWithQueuesTest = interface
    ['{BEE066BD-EA83-4F7E-8F7C-C380DF0470F5}']
    function GetByKey(AKey: UInt64): ISimple;
  end;

  THashCacheWithQueuesTest = class(TBaseInterfacedObject, IHashCacheWithQueuesTest)
  private
    FOpId: Integer;
    FCache: IHashInterfaceCache;
  private
    function CreateByKey(
      const AKey: UInt64;
      const AData: Pointer
    ): IInterface;
  private
    function GetByKey(AKey: UInt64): ISimple;
  public
    constructor Create(
      AFirstUseCount: Integer;
      AMultiUseCount: Integer;
      AFirstOutCount: Integer
    );
  end;

  TestTHashCacheWithQueuesTestAs2q = class(TTestCase)
  private
    FCache: IHashCacheWithQueuesTest;
  protected
    procedure SetUp; override;
  published
    procedure TestSimpleAddAndGet;
    procedure TestAdvAddAndGet;
  end;

  TestTHashCacheWithQueuesTestAsLRU = class(TTestCase)
  private
    FCache: IHashCacheWithQueuesTest;
  protected
    procedure SetUp; override;
  published
    procedure TestSimpleAddAndGet;
    procedure TestAdvAddAndGet;
  end;

  TestTHashCacheWithQueuesTestAsNoFirst = class(TTestCase)
  private
    FCache: IHashCacheWithQueuesTest;
  protected
    procedure SetUp; override;
  published
    procedure TestSimpleAddAndGet;
    procedure TestAdvAddAndGet;
  end;

  TestTHashCacheWithQueuesTestAsOnElement = class(TTestCase)
  private
    FCache: IHashCacheWithQueuesTest;
  protected
    procedure SetUp; override;
  published
    procedure TestSimpleAddAndGet;
    procedure TestAdvAddAndGet;
  end;

implementation

uses
  u_HashInterfaceCache2Q;

{ TSimple }

constructor TSimple.Create(
  AKey: UInt64;
  AOpId: Integer
);
begin
  FKey := AKey;
  FOpId := AOpId;
end;

function TSimple.GetKey: UInt64;
begin
  Result := FKey;
end;

function TSimple.GetOpId: Integer;
begin
  Result := FOpId;
end;

{ THashCacheWithQueuesTest }

constructor THashCacheWithQueuesTest.Create(
  AFirstUseCount: Integer;
  AMultiUseCount: Integer;
  AFirstOutCount: Integer
);
begin
  inherited Create;
  FOpId := 0;
  FCache :=
    THashInterfaceCache2Q.Create(
      Self.CreateByKey,
      6,
      AFirstUseCount,
      AMultiUseCount,
      AFirstOutCount
    );
end;

function THashCacheWithQueuesTest.CreateByKey(
  const AKey: UInt64;
  const AData: Pointer
): IInterface;
var
  VItem: ISimple;
begin
  inherited;
  VItem := TSimple.Create(AKey, FOpId);
  Result := VItem;
  Inc(FOpId);
end;

function THashCacheWithQueuesTest.GetByKey(AKey: UInt64): ISimple;
var
  VResult: IInterface;
begin
  VResult := FCache.GetOrCreateItem(AKey, nil);
  Result := VResult as ISimple;
end;

{ TestTHashCacheWithQueuesTestAs2q }

procedure TestTHashCacheWithQueuesTestAs2q.SetUp;
begin
  inherited;
  FCache := THashCacheWithQueuesTest.Create(2, 2, 4);
end;

procedure TestTHashCacheWithQueuesTestAs2q.TestAdvAddAndGet;
var
  VKey: UInt64;
  VItem1: ISimple;
  VItem2: ISimple;
  VItem3: ISimple;
  VItem4: ISimple;
  VItem5: ISimple;
  VItem6: ISimple;
  VItem7: ISimple;
  VItem8: ISimple;
begin
  VKey := 0;
  VItem1 := FCache.GetByKey(VKey);

  VKey := 1;
  VItem2 := FCache.GetByKey(VKey);
  CheckNotNull(VItem2);
  CheckEquals(VKey, VItem2.Key);

  VKey := 0;
  VItem3 := FCache.GetByKey(VKey);
  CheckNotNull(VItem3);
  CheckEquals(VKey, VItem3.Key);
  CheckEquals(VItem3.OpId, VItem1.OpId);

  VKey := $100 + 1;
  VItem4 := FCache.GetByKey(VKey);
  CheckNotNull(VItem4);
  CheckEquals(VKey, VItem4.Key);

  VKey := $100 + 1;
  VItem5 := FCache.GetByKey(VKey);
  CheckNotNull(VItem5);
  CheckEquals(VKey, VItem5.Key);
  CheckEquals(VItem4.OpId, VItem5.OpId);

  VKey := 2;
  FCache.GetByKey(VKey);
  VKey := 3;
  FCache.GetByKey(VKey);

  VKey := 0;
  VItem6 := FCache.GetByKey(VKey);
  CheckNotNull(VItem6);
  CheckEquals(VKey, VItem6.Key);
  CheckNotEquals(VItem1.OpId, VItem6.OpId);

  VKey := 4;
  FCache.GetByKey(VKey);
  VKey := 5;
  FCache.GetByKey(VKey);

  VKey := 0;
  VItem7 := FCache.GetByKey(VKey);
  CheckNotNull(VItem7);
  CheckEquals(VKey, VItem7.Key);
  CheckEquals(VItem7.OpId, VItem6.OpId);

  VKey := $100 + 1;
  VItem8 := FCache.GetByKey(VKey);
  CheckNotNull(VItem8);
  CheckEquals(VKey, VItem8.Key);
  CheckNotEquals(VItem4.OpId, VItem8.OpId);


end;

procedure TestTHashCacheWithQueuesTestAs2q.TestSimpleAddAndGet;
var
  VKey: UInt64;
  VItem1: ISimple;
  VItem2: ISimple;
begin
  VKey := 0;
  VItem1 := FCache.GetByKey(VKey);
  CheckNotNull(VItem1);
  CheckEquals(VKey, VItem1.Key);

  VItem2 := FCache.GetByKey(VKey);
  CheckNotNull(VItem2);
  CheckEquals(VKey, VItem2.Key);
  CheckEquals(VItem2.OpId, VItem1.OpId);
end;

{ TestTHashCacheWithQueuesTestAsLRU }

procedure TestTHashCacheWithQueuesTestAsLRU.SetUp;
begin
  inherited;
  FCache := THashCacheWithQueuesTest.Create(0, 2, 0);
end;

procedure TestTHashCacheWithQueuesTestAsLRU.TestAdvAddAndGet;
var
  VKey: UInt64;
  VItem1: ISimple;
  VItem2: ISimple;
  VItem3: ISimple;
  VItem4: ISimple;
  VItem5: ISimple;
  VItem6: ISimple;
  VItem7: ISimple;
  VItem8: ISimple;
begin
  VKey := 0;
  VItem1 := FCache.GetByKey(VKey);

  VKey := 1;
  VItem2 := FCache.GetByKey(VKey);
  CheckNotNull(VItem2);
  CheckEquals(VKey, VItem2.Key);

  VKey := 0;
  VItem3 := FCache.GetByKey(VKey);
  CheckNotNull(VItem3);
  CheckEquals(VKey, VItem3.Key);
  CheckEquals(VItem3.OpId, VItem1.OpId);

  VKey := $100 + 1;
  VItem4 := FCache.GetByKey(VKey);
  CheckNotNull(VItem4);
  CheckEquals(VKey, VItem4.Key);

  VKey := $100 + 1;
  VItem5 := FCache.GetByKey(VKey);
  CheckNotNull(VItem5);
  CheckEquals(VKey, VItem5.Key);
  CheckEquals(VItem4.OpId, VItem5.OpId);

  VKey := 0;
  VItem6 := FCache.GetByKey(VKey);
  CheckNotNull(VItem6);
  CheckEquals(VKey, VItem6.Key);
  CheckEquals(VItem1.OpId, VItem6.OpId);

  VKey := 4;
  FCache.GetByKey(VKey);
  VKey := 5;
  FCache.GetByKey(VKey);

  VKey := 0;
  VItem7 := FCache.GetByKey(VKey);
  CheckNotNull(VItem7);
  CheckEquals(VKey, VItem7.Key);
  CheckNotEquals(VItem7.OpId, VItem6.OpId);

  VKey := $100 + 1;
  VItem8 := FCache.GetByKey(VKey);
  CheckNotNull(VItem8);
  CheckEquals(VKey, VItem8.Key);
  CheckNotEquals(VItem4.OpId, VItem8.OpId);
end;

procedure TestTHashCacheWithQueuesTestAsLRU.TestSimpleAddAndGet;
var
  VKey: UInt64;
  VItem1: ISimple;
  VItem2: ISimple;
begin
  VKey := 0;
  VItem1 := FCache.GetByKey(VKey);
  CheckNotNull(VItem1);
  CheckEquals(VKey, VItem1.Key);

  VItem2 := FCache.GetByKey(VKey);
  CheckNotNull(VItem2);
  CheckEquals(VKey, VItem2.Key);
  CheckEquals(VItem2.OpId, VItem1.OpId);
end;

{ TestTHashCacheWithQueuesTestAsNoFirst }

procedure TestTHashCacheWithQueuesTestAsNoFirst.SetUp;
begin
  inherited;
  FCache := THashCacheWithQueuesTest.Create(0, 2, 4);
end;

procedure TestTHashCacheWithQueuesTestAsNoFirst.TestAdvAddAndGet;
var
  VKey: UInt64;
  VItem1: ISimple;
  VItem2: ISimple;
  VItem3: ISimple;
  VItem4: ISimple;
  VItem5: ISimple;
  VItem6: ISimple;
  VItem7: ISimple;
  VItem8: ISimple;
begin
  VKey := 0;
  VItem1 := FCache.GetByKey(VKey);

  VKey := 1;
  VItem2 := FCache.GetByKey(VKey);
  CheckNotNull(VItem2);
  CheckEquals(VKey, VItem2.Key);

  VKey := 0;
  VItem3 := FCache.GetByKey(VKey);
  CheckNotNull(VItem3);
  CheckEquals(VKey, VItem3.Key);
  CheckNotEquals(VItem3.OpId, VItem1.OpId);

  VKey := $100 + 1;
  VItem4 := FCache.GetByKey(VKey);
  CheckNotNull(VItem4);
  CheckEquals(VKey, VItem4.Key);

  VKey := $100 + 1;
  VItem5 := FCache.GetByKey(VKey);
  CheckNotNull(VItem5);
  CheckEquals(VKey, VItem5.Key);
  CheckNotEquals(VItem4.OpId, VItem5.OpId);

  VKey := 0;
  VItem6 := FCache.GetByKey(VKey);
  CheckNotNull(VItem6);
  CheckEquals(VKey, VItem6.Key);
  CheckEquals(VItem3.OpId, VItem6.OpId);

  VKey := 4;
  FCache.GetByKey(VKey);
  VKey := 5;
  FCache.GetByKey(VKey);

  VKey := 0;
  VItem7 := FCache.GetByKey(VKey);
  CheckNotNull(VItem7);
  CheckEquals(VKey, VItem7.Key);
  CheckEquals(VItem7.OpId, VItem3.OpId);

  VKey := $100 + 1;
  VItem8 := FCache.GetByKey(VKey);
  CheckNotNull(VItem8);
  CheckEquals(VKey, VItem8.Key);
  CheckEquals(VItem5.OpId, VItem8.OpId);
end;

procedure TestTHashCacheWithQueuesTestAsNoFirst.TestSimpleAddAndGet;
var
  VKey: UInt64;
  VItem1: ISimple;
  VItem2: ISimple;
  VItem3: ISimple;
begin
  VKey := 0;
  VItem1 := FCache.GetByKey(VKey);
  CheckNotNull(VItem1);
  CheckEquals(VKey, VItem1.Key);

  VItem2 := FCache.GetByKey(VKey);
  CheckNotNull(VItem2);
  CheckEquals(VKey, VItem2.Key);
  CheckNotEquals(VItem2.OpId, VItem1.OpId);

  VItem3 := FCache.GetByKey(VKey);
  CheckNotNull(VItem3);
  CheckEquals(VKey, VItem3.Key);
  CheckEquals(VItem2.OpId, VItem3.OpId);
end;

{ TestTHashCacheWithQueuesTestAsOnElement }

procedure TestTHashCacheWithQueuesTestAsOnElement.SetUp;
begin
  inherited;
  FCache := THashCacheWithQueuesTest.Create(1, 1, 1);
end;

procedure TestTHashCacheWithQueuesTestAsOnElement.TestAdvAddAndGet;
var
  VKey: UInt64;
  VItem1: ISimple;
  VItem2: ISimple;
  VItem3: ISimple;
  VItem4: ISimple;
  VItem5: ISimple;
  VItem6: ISimple;
  VItem7: ISimple;
  VItem8: ISimple;
begin
  VKey := 0;
  VItem1 := FCache.GetByKey(VKey);

  VKey := 9;
  FCache.GetByKey(VKey);

  VKey := 1;
  VItem2 := FCache.GetByKey(VKey);
  CheckNotNull(VItem2);
  CheckEquals(VKey, VItem2.Key);

  VKey := 0;
  VItem3 := FCache.GetByKey(VKey);
  CheckNotNull(VItem3);
  CheckEquals(VKey, VItem3.Key);
  CheckNotEquals(VItem3.OpId, VItem1.OpId);

  VKey := $100 + 1;
  VItem4 := FCache.GetByKey(VKey);
  CheckNotNull(VItem4);
  CheckEquals(VKey, VItem4.Key);

  VKey := $100 + 1;
  VItem5 := FCache.GetByKey(VKey);
  CheckNotNull(VItem5);
  CheckEquals(VKey, VItem5.Key);
  CheckEquals(VItem4.OpId, VItem5.OpId);

  VKey := 0;
  VItem6 := FCache.GetByKey(VKey);
  CheckNotNull(VItem6);
  CheckEquals(VKey, VItem6.Key);
  CheckEquals(VItem3.OpId, VItem6.OpId);

  VKey := 4;
  FCache.GetByKey(VKey);
  VKey := 5;
  FCache.GetByKey(VKey);

  VKey := 0;
  VItem7 := FCache.GetByKey(VKey);
  CheckNotNull(VItem7);
  CheckEquals(VKey, VItem7.Key);
  CheckEquals(VItem7.OpId, VItem3.OpId);

  VKey := $100 + 1;
  VItem8 := FCache.GetByKey(VKey);
  CheckNotNull(VItem8);
  CheckEquals(VKey, VItem8.Key);
  CheckNotEquals(VItem5.OpId, VItem8.OpId);
end;

procedure TestTHashCacheWithQueuesTestAsOnElement.TestSimpleAddAndGet;
var
  VKey: UInt64;
  VItem1: ISimple;
  VItem2: ISimple;
  VItem3: ISimple;
begin
  VKey := 0;
  VItem1 := FCache.GetByKey(VKey);
  CheckNotNull(VItem1);
  CheckEquals(VKey, VItem1.Key);

  VItem2 := FCache.GetByKey(VKey);
  CheckNotNull(VItem2);
  CheckEquals(VKey, VItem2.Key);
  CheckEquals(VItem2.OpId, VItem1.OpId);

  VItem3 := FCache.GetByKey(VKey);
  CheckNotNull(VItem3);
  CheckEquals(VKey, VItem3.Key);
  CheckEquals(VItem2.OpId, VItem3.OpId);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTHashCacheWithQueuesTestAs2q.Suite);
  RegisterTest(TestTHashCacheWithQueuesTestAsLRU.Suite);
  RegisterTest(TestTHashCacheWithQueuesTestAsNoFirst.Suite);
  RegisterTest(TestTHashCacheWithQueuesTestAsOnElement.Suite);
end.
