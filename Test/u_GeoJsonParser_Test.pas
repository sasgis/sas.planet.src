unit u_GeoJsonParser_Test;

interface

{$IF CompilerVersion >= 36.0} // Delphi 12.0 and up
  {$DEFINE HAS_MULTILINE_STRING}
{$ENDIF}

uses
  TestFramework,
  i_BinaryData,
  i_ProjConverter,
  i_VectorDataLoader;

type
  TestGeoJsonParser = class(TTestCase)
  private
    FParser: IVectorDataLoader;
    FContext: TVectorLoadContext;
    FProjConverterFactory: IProjConverterFactory;

    function GetTestDataBase: IBinaryData;
    function GetTestDataMulti: IBinaryData;
    function GetTestDataGeometryCollection: IBinaryData;
    function GetTestDataCRS3857: IBinaryData;
  protected
    procedure SetUp; override;
  published
    procedure TestBase;
    procedure TestMulti;
    procedure TestGeometryCollection;
    procedure TestCRS3857;
  end;

implementation

uses
  Math,
  SysUtils,
  i_HashFunction,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_InternalPerformanceCounter,
  i_VectorDataFactory,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_ImportConfig,
  i_HtmlToHintTextConverter,
  u_BinaryData,
  u_ProjConverterFactory,
  u_VectorDataItemBase,
  u_VectorDataFactorySimple,
  u_VectorItemSubsetBuilder,
  u_InternalPerformanceCounterFake,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_HtmlToHintTextConverterStuped,
  u_GeometryLonLatFactory,
  u_GeoJsonParser;

{ TestGeoJsonParser }

procedure TestGeoJsonParser.SetUp;
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
  FProjConverterFactory := TProjConverterFactory.Create;

  FParser :=
    TGeoJsonParser.Create(
      VVectorSubsetBuilderFactory,
      VVectorDataFactory,
      VGeometryFactory,
      FProjConverterFactory
    );

  FContext.Init;
  FContext.MainInfoFactory :=
    TVectorDataItemMainInfoFactory.Create(
      VHashFunction,
      THtmlToHintTextConverterStuped.Create as IHtmlToHintTextConverter
    );
end;

procedure TestGeoJsonParser.TestBase;
var
  VResult: IVectorItemSubset;
begin
  // Simple features: Point, Line, Polygon
  VResult := FParser.Load(FContext, Self.GetTestDataBase);

  CheckNotNull(VResult);
  // todo: check result
end;

procedure TestGeoJsonParser.TestMulti;
var
  VResult: IVectorItemSubset;
begin
  // MultiPoint, MultiLine, MultiPolygon
  VResult := FParser.Load(FContext, Self.GetTestDataMulti);

  CheckNotNull(VResult);
  // todo: check result
end;

procedure TestGeoJsonParser.TestGeometryCollection;
var
  VResult: IVectorItemSubset;
begin
  // Feature Collection with Geometry Collection
  VResult := FParser.Load(FContext, Self.GetTestDataGeometryCollection);

  CheckNotNull(VResult);
  // todo: check result
end;

procedure TestGeoJsonParser.TestCRS3857;
var
  VResult: IVectorItemSubset;
  VPoint: IGeometryLonLatPoint;
begin
  // Feature Collection with CRS EPSG:3857
  VResult := FParser.Load(FContext, Self.GetTestDataCRS3857);

  CheckNotNull(VResult);
  CheckEquals(1, VResult.Count);

  CheckTrue(
    Supports(VResult.Items[0].Geometry, IGeometryLonLatPoint, VPoint)
  );

  CheckEquals(40.3550894, RoundTo(VPoint.Point.X, -7));
  CheckEquals(57.6611737, RoundTo(VPoint.Point.Y, -7));
end;

function TestGeoJsonParser.GetTestDataBase: IBinaryData;
begin
  Result := TBinaryData.CreateByAnsiString(UTF8Encode(
    {$IFDEF HAS_MULTILINE_STRING}
    '''
    {
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "geometry": {
            "type": "Point",
            "coordinates": [102.0, 0.5]
          },
          "properties": {
            "prop0": "value0"
          }
        },
        {
          "type": "Feature",
          "geometry": {
            "type": "LineString",
            "coordinates": [
              [102.0, 0.0],
              [103.0, 1.0],
              [104.0, 0.0],
              [105.0, 99.0]
            ]
          },
          "properties": {
            "prop0": "value0",
            "prop1": 0.0
          }
        },
        {
          "type": "Feature",
          "geometry": {
            "type": "Polygon",
            "coordinates": [
              [
                [100.0, 0.0],
                [101.0, 0.0],
                [101.0, 1.0],
                [100.0, 1.0],
                [100.0, 0.0]
              ]
            ]
          },
          "properties": {
            "prop0": "value0",
            "prop1": { "this": "that" }
          }
        }
      ]
    }
    '''
    {$ELSE}
    '{"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"Point","coordinates":[102.0,0.5]},' +
    '"properties":{"prop0":"value0"}},{"type":"Feature","geometry":{"type":"LineString","coordinates":[[102.0,0.0],' +
    '[103.0,1.0],[104.0,0.0],[105.0,99.0]]},"properties":{"prop0":"value0","prop1":0.0}},{"type":"Feature","geometry":' +
    '{"type":"Polygon","coordinates":[[[100.0,0.0],[101.0,0.0],[101.0,1.0],[100.0,1.0],[100.0,0.0]]]},"properties":' +
    '{"prop0":"value0","prop1":{"this":"that"}}}]}'
    {$ENDIF}
  ));
end;

function TestGeoJsonParser.GetTestDataMulti: IBinaryData;
begin
  Result := TBinaryData.CreateByAnsiString(UTF8Encode(
    {$IFDEF HAS_MULTILINE_STRING}
    '''
    {
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "geometry": {
            "type": "MultiPoint",
            "coordinates": [
              [10.0, 40.0],
              [40.0, 30.0],
              [20.0, 20.0],
              [30.0, 10.0]
            ]
          }
        },
        {
          "type": "Feature",
          "geometry": {
            "type": "MultiLineString",
            "coordinates": [
                [
                    [10.0, 10.0],
                    [20.0, 20.0],
                    [10.0, 40.0]
                ],
                [
                    [40.0, 40.0],
                    [30.0, 30.0],
                    [40.0, 20.0],
                    [30.0, 10.0]
                ]
            ]
          }
        },
        {
          "type": "Feature",
          "geometry": {
            "type": "MultiPolygon",
            "coordinates": [
                [
                    [
                        [40.0, 40.0],
                        [20.0, 45.0],
                        [45.0, 30.0],
                        [40.0, 40.0]
                    ]
                ],
                [
                    [
                        [20.0, 35.0],
                        [10.0, 30.0],
                        [10.0, 10.0],
                        [30.0, 5.0],
                        [45.0, 20.0],
                        [20.0, 35.0]
                    ],
                    [
                        [30.0, 20.0],
                        [20.0, 15.0],
                        [20.0, 25.0],
                        [30.0, 20.0]
                    ]
                ]
            ]
          }
        }
      ]
    }
    '''
    {$ELSE}
    '{"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"MultiPoint","coordinates":' +
    '[[10.0,40.0],[40.0,30.0],[20.0,20.0],[30.0,10.0]]}},{"type":"Feature","geometry":{"type":"MultiLineString",' +
    '"coordinates":[[[10.0,10.0],[20.0,20.0],[10.0,40.0]],[[40.0,40.0],[30.0,30.0],[40.0,20.0],[30.0,10.0]]]}},' +
    '{"type":"Feature","geometry":{"type":"MultiPolygon","coordinates":[[[[40.0,40.0],[20.0,45.0],[45.0,30.0],[40.0,40.0]]],' +
    '[[[20.0,35.0],[10.0,30.0],[10.0,10.0],[30.0,5.0],[45.0,20.0],[20.0,35.0]],[[30.0,20.0],[20.0,15.0],[20.0,25.0],[30.0,20.0]]]]}}]}'
    {$ENDIF}
  ));
end;

function TestGeoJsonParser.GetTestDataGeometryCollection: IBinaryData;
begin
  Result := TBinaryData.CreateByAnsiString(UTF8Encode(
    {$IFDEF HAS_MULTILINE_STRING}
    '''
    {
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "geometry": {
            "type": "GeometryCollection",
            "geometries": [
              {
                "type": "Point",
                "coordinates": [40.0, 10.0]
              },
              {
                "type": "LineString",
                "coordinates": [
                  [10.0, 10.0],
                  [20.0, 20.0],
                  [10.0, 40.0]
                ]
              },
              {
                "type": "Polygon",
                "coordinates": [
                  [
                    [40.0, 40.0],
                    [20.0, 45.0],
                    [45.0, 30.0],
                    [40.0, 40.0]
                  ]
                ]
              }
            ]
          }
        }
      ]
    }
    '''
    {$ELSE}
    '{"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":' +
    '[{"type":"Point","coordinates":[40.0,10.0]},{"type":"LineString","coordinates":[[10.0,10.0],[20.0,20.0],[10.0,40.0]]},' +
    '{"type":"Polygon","coordinates":[[[40.0,40.0],[20.0,45.0],[45.0,30.0],[40.0,40.0]]]}]}}]}'
    {$ENDIF}
  ));
end;

function TestGeoJsonParser.GetTestDataCRS3857: IBinaryData;
begin
  Result := TBinaryData.CreateByAnsiString(UTF8Encode(
    {$IFDEF HAS_MULTILINE_STRING}
    '''
    {
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "geometry": {
            "type": "Point",
            "coordinates": [4492308.0, 7896475.0],
            "crs": {
              "type": "name",
              "properties": {
                "name": "EPSG:3857"
              }
            }
          }
        }
      ]
    }
    '''
    {$ELSE}
    '{"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"Point","coordinates":[4492308.0,7896475.0],' +
    '"crs":{"type":"name","properties":{"name":"EPSG:3857"}}}}]}'
    {$ENDIF}
  ));
end;

initialization
  RegisterTest(TestGeoJsonParser.Suite);

end.
