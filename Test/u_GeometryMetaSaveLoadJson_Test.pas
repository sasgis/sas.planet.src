unit u_GeometryMetaSaveLoadJson_Test;

interface

uses
  TestFramework,
  Classes,
  SysUtils,
  DateUtils,
  Diagnostics,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_GeometryToStream,
  i_GeometryFromStream,
  i_DoublePointsMeta,
  u_GeometryMetaToStreamJson,
  u_GeometryMetaFromStreamJson;

type
  TestGeometryMetaSaveLoadJson = class(TTestCase)
  private
    FGeometryFactory: IGeometryLonLatFactory;

    FToStream: IGeometryMetaToStream;
    FFromStream: IGeometryMetaFromStream;

    function CreateTestSingleLine(const APointsCount: Integer; const AWithElevation: Boolean; const AWithTimeStamp: Boolean): IGeometryLonLatLine;
    function CreateTestMultilineLine: IGeometryLonLatLine;

    procedure TestLine(const ALine: IGeometryLonLatLine);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyMeta;
    procedure TestSingleLineWithMeta;
    procedure TestSingleLineWithoutMeta;
    procedure TestMultiLine;
    procedure TestPerformance;
  end;

implementation

uses
  t_GeoTypes,
  i_HashFunction,
  i_DoublePoints,
  i_DoublePointsAggregator,
  i_InternalPerformanceCounter,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_InternalPerformanceCounterFake,
  u_GeoFunc,
  u_GeometryFunc,
  u_GeometryLonLatFactory,
  u_DoublePointsAggregator;

{ TestGeometryMetaSaveLoadJson }

procedure TestGeometryMetaSaveLoadJson.SetUp;
var
  VHashFunction: IHashFunction;
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  inherited;

  VPerfCounterList := TInternalPerformanceCounterFake.Create;

  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      VPerfCounterList
    );

  FGeometryFactory := TGeometryLonLatFactory.Create(VPerfCounterList, VHashFunction);

  FToStream := TGeometryMetaToStreamJson.Create;
  FFromStream := TGeometryMetaFromStreamJson.Create;
end;

procedure TestGeometryMetaSaveLoadJson.TearDown;
begin
  FFromStream := nil;
  FToStream := nil;
  FGeometryFactory := nil;
  inherited;
end;

function TestGeometryMetaSaveLoadJson.CreateTestSingleLine(
  const APointsCount: Integer;
  const AWithElevation: Boolean;
  const AWithTimeStamp: Boolean
): IGeometryLonLatLine;
var
  I: Integer;
  VPoints: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VItem: TDoublePointsMetaItem;
  VTimestamp: TDateTime;
begin
  VPoints := TDoublePointsAggregator.Create;

  VPoint.X := 0;
  VPoint.Y := 0;

  VItem.IsElevationOk := AWithElevation;
  VItem.IsTimeStampOk := AWithTimeStamp;

  VTimeStamp := Now;

  for I := 0 to APointsCount - 1 do begin
    VItem.Elevation := I * 0.5;
    VItem.TimeStamp := IncMinute(VTimeStamp, I);

    VPoints.Add(VPoint, @VItem);
  end;

  Result := FGeometryFactory.CreateLonLatLine(VPoints.Points, VPoints.Meta, VPoints.Count);
end;

function TestGeometryMetaSaveLoadJson.CreateTestMultilineLine: IGeometryLonLatLine;
var
  I: Integer;
  VLines: array[0..4] of IGeometryLonLatLine;
  VBuilder: IGeometryLonLatLineBuilder;
begin
  I := 0;

  VLines[I] := CreateTestSingleLine(10, False, False); Inc(I);
  VLines[I] := CreateTestSingleLine(20, False, True);  Inc(I);
  VLines[I] := CreateTestSingleLine(30, True,  False); Inc(I);
  VLines[I] := CreateTestSingleLine(40, False, False); Inc(I);
  VLines[I] := CreateTestSingleLine(50, True,  True);

  VBuilder := FGeometryFactory.MakeLineBuilder;
  for I := 0 to High(VLines) do begin
    VBuilder.AddLine(VLines[I]);
  end;

  Result := VBuilder.MakeStaticAndClear;
end;

procedure TestGeometryMetaSaveLoadJson.TestLine(const ALine: IGeometryLonLatLine);

  function _GetLinePoints(out AHasMeta: Boolean): IDoublePoints;
  var
    VLine: IGeometryLonLatSingleLine;
    VLines: TArrayOfGeometryLonLatSingleLine;
    VLinePoints: IDoublePointsAggregator;
  begin
    VLines := GeometryLonLatLineToArray(ALine);
    AHasMeta := TGeometryMetaToStreamJson.HasMeta(VLines);
    VLinePoints := TDoublePointsAggregator.Create;
    for VLine in VLines do begin
      if VLinePoints.Count > 0 then begin
        VLinePoints.Add(CEmptyDoublePoint);
      end;
      VLinePoints.AddPoints(VLine.Points, VLine.Meta, VLine.Count);
    end;
    Result := VLinePoints.MakeStaticAndClear;
  end;

var
  I: Integer;
  VStream: TMemoryStream;
  VHasMeta: Boolean;
  VLinePoints: IDoublePoints;
  VParsedMeta: IDoublePointsMeta;
  VLineMetaPtr, VParsedMetaPtr: PDoublePointsMeta;
begin
  VLinePoints := _GetLinePoints(VHasMeta);

  VStream := TMemoryStream.Create;
  try
    FToStream.Save(ALine, VStream);

    if VHasMeta then begin
      CheckTrue(VStream.Size > 0, 'Stream should not be empty');
    end else begin
      CheckTrue(VStream.Size = 0, 'Stream should be empty');
    end;

    VStream.Position := 0;
    VParsedMeta := FFromStream.Parse(VStream);

    if not VHasMeta then begin
      CheckNull(VParsedMeta, 'Parsed meta should be nil');
      Exit;
    end;

    CheckNotNull(VParsedMeta, 'Parsed meta should not be nil');
    CheckEquals(VLinePoints.Count, VParsedMeta.Count, 'Points count mismatch');

    VLineMetaPtr := VLinePoints.Meta;
    VParsedMetaPtr := VParsedMeta.Meta;

    // Evelation
    if VLineMetaPtr.Elevation <> nil then begin
      CheckNotNull(
        Pointer(VParsedMetaPtr.Elevation),
        'Parsed Elevation meta should not be nil'
      );
      for I := 0 to VLinePoints.Count - 1 do begin
        CheckEquals(
          VLineMetaPtr.Elevation[I],
          VParsedMetaPtr.Elevation[I],
          0.0001,
          'Elevation mismatch at index ' + IntToStr(I)
        );
      end;
    end else begin
      CheckNull(
        Pointer(VParsedMetaPtr.Elevation),
        'Parsed Elevation meta should be nil'
      );
    end;

    // TimeStamp
    if VLineMetaPtr.TimeStamp <> nil then begin
      CheckNotNull(
        Pointer(VParsedMetaPtr.TimeStamp),
        'Parsed TimeStamp meta should not be nil'
      );
      for I := 0 to VLinePoints.Count - 1 do begin
        CheckEquals(
          VLineMetaPtr.TimeStamp[I],
          VParsedMetaPtr.TimeStamp[I],
          0.000001,
          'TimeStamp mismatch at index ' + IntToStr(I)
        );
      end;
    end else begin
      CheckNull(
        Pointer(VParsedMetaPtr.TimeStamp),
        'Parsed TimeStamp meta should be nil'
      );
    end;
  finally
    VStream.Free;
  end;
end;

procedure TestGeometryMetaSaveLoadJson.TestEmptyMeta;
type
  TTestCase = record
    Data: RawByteString;
    Count: Integer;
  end;
const
  cEmptyMetaJson: array [0..2] of TTestCase = (
    (Data: ''; Count: 0), // empty string
    (Data: 'JSON{"v":1,"t":1,"g":[{"m":[],"c":10}]}'; Count: 10),
    (Data: 'JSON{"v":1,"t":1,"g":[{"m":[],"c":30},{"m":[],"c":40}]}'; Count: 30+1+40)
  );
var
  I: Integer;
  VStream: TMemoryStream;
  VTest: ^TTestCase;
  VParsedMeta: IDoublePointsMeta;
begin
  VStream := TMemoryStream.Create;
  try
    for I := 0 to High(cEmptyMetaJson) do begin
      VTest := @cEmptyMetaJson[I];
      VStream.Clear;
      VStream.WriteBuffer(Pointer(VTest.Data)^, Length(VTest.Data));
      VStream.Position := 0;
      VParsedMeta := FFromStream.Parse(VStream);
      if VTest.Count = 0 then begin
        CheckNull(VParsedMeta, 'Parsed meta not nil at index ' + IntToStr(I) + ': ' + VTest.Data);
      end else begin
        CheckNotNull(VParsedMeta, 'Parsed meta nil at index ' + IntToStr(I) + ': ' + VTest.Data);
        CheckTrue(VParsedMeta.Count = VTest.Count);
      end;
    end;
  finally
    VStream.Free;
  end;
end;

procedure TestGeometryMetaSaveLoadJson.TestSingleLineWithMeta;
begin
  TestLine( CreateTestSingleLine(1000, True, True) );
end;

procedure TestGeometryMetaSaveLoadJson.TestSingleLineWithoutMeta;
begin
  TestLine( CreateTestSingleLine(10, False, False) );
end;

procedure TestGeometryMetaSaveLoadJson.TestMultiLine;
begin
  TestLine( CreateTestMultilineLine() );
end;

procedure TestGeometryMetaSaveLoadJson.TestPerformance;
const
  cIterations = 100;
  cPointsCount = 9000;
var
  I: Integer;
  VStream: TMemoryStream;
  VStopwatch: TStopwatch;
  VWriteTime, VReadTime: Int64;
  VParsedMeta: IDoublePointsMeta;
  VLine: IGeometryLonLatSingleLine;
begin
  VLine := CreateTestSingleLine(cPointsCount, True, True) as IGeometryLonLatSingleLine;

  VStream := TMemoryStream.Create;
  try
    // base checks
    VStream.Position := 0;
    FToStream.Save(VLine, VStream);
    CheckTrue(VStream.Size > 0, 'Stream should not be empty after Save');

    // test write speed
    VStopwatch := TStopwatch.StartNew;
    for I := 0 to cIterations - 1 do begin
      VStream.Position := 0;
      FToStream.Save(VLine, VStream);
    end;
    VStopwatch.Stop;
    VWriteTime := VStopwatch.ElapsedMilliseconds;

    // base checks
    VStream.Position := 0;
    VParsedMeta := FFromStream.Parse(VStream);
    CheckNotNull(VParsedMeta, 'Parsed meta should not be nil');
    CheckEquals(VLine.Count, VParsedMeta.Count, 'Points count mismatch');

    // test read speed
    VStopwatch := TStopwatch.StartNew;
    for I := 0 to cIterations - 1 do begin
      VStream.Position := 0;
      VParsedMeta := FFromStream.Parse(VStream);
    end;
    VStopwatch.Stop;
    VReadTime := VStopwatch.ElapsedMilliseconds;

    // print results
    Status(Format('%s (Points: %d, Iterations: %d):', [Self.ClassName, cPointsCount, cIterations]));
    Status(Format('  Write: %d ms', [VWriteTime]));
    Status(Format('  Read: %d ms', [VReadTime]));
  finally
    VStream.Free;
  end;
end;

initialization
  RegisterTest(TestGeometryMetaSaveLoadJson.Suite);

end.