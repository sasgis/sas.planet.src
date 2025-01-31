unit u_WikimapiaPlainTextParser_Test;

interface

uses
  TestFramework,
  i_BinaryData,
  i_VectorDataLoader;

type
  TestWikimapiaPlainTextParser = class(TTestCase)
  private
    FParser: IVectorDataLoader;
    FContext: TVectorLoadContext;

    function GetTestDataBase: IBinaryData;
  protected
    procedure SetUp; override;
  published
    procedure TestBase;
  end;

implementation

uses
  i_HashFunction,
  i_GeometryLonLatFactory,
  i_InternalPerformanceCounter,
  i_VectorDataFactory,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_ImportConfig,
  i_HtmlToHintTextConverter,
  u_BinaryData,
  u_VectorDataFactorySimple,
  u_VectorItemSubsetBuilder,
  u_InternalPerformanceCounterFake,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_HtmlToHintTextConverterStuped,
  u_GeometryLonLatFactory,
  u_WikimapiaPlainTextParser;

{ TestWikimapiaPlainTextParser }

procedure TestWikimapiaPlainTextParser.SetUp;
var
  VHashFunction: IHashFunction;
  VVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  VVectorDataFactory: IVectorDataFactory;
  VGeometryFactory: IGeometryLonLatFactory;
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  VPerfCounterList := TInternalPerformanceCounterFake.Create;

  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      VPerfCounterList
    );

  VVectorSubsetBuilderFactory := TVectorItemSubsetBuilderFactory.Create(VHashFunction);
  VVectorDataFactory := TVectorDataFactorySimple.Create(VHashFunction);
  VGeometryFactory := TGeometryLonLatFactory.Create(VPerfCounterList, VHashFunction);

  FParser :=
    TWikimapiaPlainTextParser.Create(
      VVectorSubsetBuilderFactory,
      VVectorDataFactory,
      VGeometryFactory
    );

  FContext.Init;
  FContext.MainInfoFactory :=
    TVectorDataItemMainInfoFactory.Create(
      VHashFunction,
      THtmlToHintTextConverterStuped.Create as IHtmlToHintTextConverter
    );
end;

procedure TestWikimapiaPlainTextParser.TestBase;
var
  VResult: IVectorItemSubset;
begin
  VResult := FParser.Load(FContext, Self.GetTestDataBase);

  CheckNotNull(VResult);
  CheckEquals(5, VResult.Count);
end;

function TestWikimapiaPlainTextParser.GetTestDataBase: IBinaryData;
begin
  Result := TBinaryData.CreateByAnsiString(UTF8Encode(
    '0302122133120|0|7' + #10 +
    '' + #10 +
    '16851244|241|277944283,277948468,539024529,539026298|13|164,46534,46535|!Рябиновая ул., 1' + #31 + ' Rabinavaja vulica, 1|1|qgm_t@_q}xeBInCbHD?zDcY??_J' + #10 +
    '30249213|212|277812122,277815887,538990746,538992764|13|164,46534|!Пригородная ул., 17' + #31 + ' Pryharadnaja vulica, 17|1|ois~s@w_wxeBDlEhCAD`EmK@?k@aJ??cAx@?CkAhFC?kE' + #10 +
    '35992798|210|277850326,277854205,539013056,539015319|13|164,46534,46535| Biarezinskaja vulica, 15' + #31 + '!Березинская ул., 15|1|qyz~s@ul{xeBcQbF~DjDlCw@xBhBhGkByBgBjDcA' + #10 +
    '16898499|137|277991518,277993472,539020061,539021171|14|164,46534,46535|!Гиацинтовая ул., 13' + #31 + ' Hijacyntavaja vulica, 13|1|}ev_t@}p|xeBOpEuJGLsE' + #10 +
    '16851049|110|277946267,277947884,539022183,539022996|14|164,46534,46535|!Рябиновая ул., 3' + #31 + ' Rabinavaja vulica, 3|1|cum_t@u||xeBA~C`I??}C'// + #10
  ));
end;

initialization
  RegisterTest(TestWikimapiaPlainTextParser.Suite);

end.
