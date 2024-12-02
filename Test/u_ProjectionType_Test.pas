unit u_ProjectionType_Test;

interface

uses
  TestFramework,
  i_ProjectionType,
  i_ProjectionTypeFactory;

type
  TestProjectionType = class(TTestCase)
  private
    FProjectionTypeFactory: IProjectionTypeFactory;
    procedure DoTest(const AProjectionType: IProjectionType);
  protected
    procedure SetUp; override;
  published
    procedure TestGeographic;
    procedure TestMercatorOnSphere;
    procedure TestMercatorOnEllipsoid;
  end;

implementation

uses
  SysUtils,
  c_CoordConverter,
  t_GeoTypes,
  i_HashFunction,
  i_DatumFactory,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_DatumFactory,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_InternalPerformanceCounterFake,

  u_ProjectionTypeGeographic,
  u_ProjectionTypeMercatorOnSphere,
  u_ProjectionTypeMercatorOnEllipsoid,

  u_ProjectionTypeFactorySimple;

{ TestProjectionType }

procedure TestProjectionType.SetUp;
var
  VHashFunction: IHashFunction;
  VDatumFactory: IDatumFactory;
begin
  inherited;

  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );

  VDatumFactory := TDatumFactory.Create(VHashFunction);

  FProjectionTypeFactory :=
    TProjectionTypeFactorySimple.Create(
      VHashFunction,
      VDatumFactory
    );
end;

procedure TestProjectionType.DoTest(const AProjectionType: IProjectionType);

  function DoublePointToString(const P: TDoublePoint): string;
  begin
    Result := 'X: ' + R2StrPoint(P.X) + '; Y: ' + R2StrPoint(P.Y);
  end;

const
  CTestPoints: array [0..4] of TDoublePoint = (
    (X: 1; Y: 1), (X: -1; Y: 1), (X: 1; Y: -1), (X: -1; Y: -1), (X: 0; Y: 0)
  );
var
  I: Integer;
  VLonLat: TDoublePoint;
  VRelative: TDoublePoint;
begin
  for I := 0 to Length(CTestPoints) - 1 do begin
    VLonLat.X := 360 * CTestPoints[I].X;
    VLonLat.Y := 360 * CTestPoints[I].Y;

    AProjectionType.ValidateLonLatPos(VLonLat);

    CheckTrue(
      AProjectionType.CheckLonLatPos(VLonLat),
      'Case #' + IntToStr(I+1) + ' LonLat(' + DoublePointToString(VLonLat) + ')'
    );

    VRelative := AProjectionType.LonLat2Relative(VLonLat);

    CheckTrue(
      AProjectionType.CheckRelativePos(VRelative),
      'Case #' + IntToStr(I+1) + ' Relative(' + DoublePointToString(VRelative) + ')'
    );
  end;
end;

procedure TestProjectionType.TestGeographic;
begin
  DoTest(FProjectionTypeFactory.GetByCode(CGELonLatProjectionEPSG));
end;

procedure TestProjectionType.TestMercatorOnEllipsoid;
begin
  DoTest(FProjectionTypeFactory.GetByCode(CYandexProjectionEPSG));
end;

procedure TestProjectionType.TestMercatorOnSphere;
begin
  DoTest(FProjectionTypeFactory.GetByCode(CGoogleProjectionEPSG));
end;

initialization
  RegisterTest(TestProjectionType.Suite);

end.
