unit u_GeometryFromWKB;

interface

uses
  Classes,
  i_DoublePoints,
  i_GeometryLonLat,
  i_GeometryFromStream,
  i_GeometryLonLatFactory,
  u_BaseInterfacedObject;

type
  TGeometryFromWKB = class(TBaseInterfacedObject, IGeometryFromStream)
  private
    FFactory: IGeometryLonLatFactory;
    function LoadPoint(
      const AStream: TStream;
      const AOrder: Boolean
    ): IGeometryLonLatPoint;
    function LoadLine(
      const AStream: TStream;
      const AOrder: Boolean
    ): IGeometryLonLatLine;
    function LoadSingleLine(
      const AStream: TStream;
      const AOrder: Boolean
    ): IDoublePoints;
    function LoadSinglePolygon(
      const AStream: TStream;
      const AOrder: Boolean
    ): IDoublePoints;
    procedure LoadPolygons(
      const ABuilder: IGeometryLonLatPolygonBuilder;
      const AStream: TStream;
      const AOrder: Boolean
    );
    function LoadPolygon(
      const AStream: TStream;
      const AOrder: Boolean
    ): IGeometryLonLatPolygon;
    function LoadMultiLine(
      const AStream: TStream;
      const AOrder: Boolean
    ): IGeometryLonLatLine;
    function LoadMultiPolygon(
      const AStream: TStream;
      const AOrder: Boolean
    ): IGeometryLonLatPolygon;
  private
    function Parse(
      const AStream: TStream
    ): IGeometryLonLat;
  public
    constructor Create(
      const AFactory: IGeometryLonLatFactory
    );
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  u_DoublePoints;

{ TGeometryFromWKB }

constructor TGeometryFromWKB.Create(const AFactory: IGeometryLonLatFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

const
  wkbGeometryTypePoint = 1;
  wkbGeometryTypeLine = 2;
  wkbGeometryTypePolygon = 3;
  wkbGeometryTypeMultiLine = 5;
  wkbGeometryTypeMultiPolygon = 6;

function TGeometryFromWKB.LoadLine(
  const AStream: TStream;
  const AOrder: Boolean
): IGeometryLonLatLine;
var
  VBuilder: IGeometryLonLatLineBuilder;
begin
  Result := nil;
  VBuilder := FFactory.MakeLineBuilder;
  VBuilder.AddLine(LoadSingleLine(AStream, AOrder));
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryFromWKB.LoadSingleLine(
  const AStream: TStream;
  const AOrder: Boolean
): IDoublePoints;
var
  VCount: Cardinal;
  VBuffer: PDoublePointArray;
begin
  Result := nil;
  AStream.ReadBuffer(VCount, SizeOf(VCount));

  if VCount >= MaxInt / 2 / SizeOf(Double) then begin
    Abort;
  end;
  GetMem(VBuffer, VCount * SizeOf(TDoublePoint));
  AStream.ReadBuffer(VBuffer[0], VCount * SizeOf(TDoublePoint));
  Result := TDoublePoints.CreateWithOwn(VBuffer, VCount);
end;

function TGeometryFromWKB.LoadMultiLine(
  const AStream: TStream;
  const AOrder: Boolean
): IGeometryLonLatLine;
var
  VBuilder: IGeometryLonLatLineBuilder;
  VCount: Cardinal;
  i: Integer;
  VWKBType: Cardinal;
  VWKBOrder: Byte;
  VOrder: Boolean;
  VLine: IDoublePoints;
begin
  VBuilder := FFactory.MakeLineBuilder;
  AStream.ReadBuffer(VCount, SizeOf(VCount));

  if VCount >= MaxInt / (1 + 4 + 4 + 2 * SizeOf(Double)) then begin
    Abort;
  end;

  for i := 0 to VCount - 1 do begin
    AStream.ReadBuffer(VWKBOrder, SizeOf(VWKBOrder));
    VOrder := VWKBOrder = 1;
    Assert(VOrder, 'Поддерживается тольк порядок Little Endian');
    if not VOrder then begin
      Abort;
    end;
    AStream.ReadBuffer(VWKBType, SizeOf(VWKBType));
    if VWKBType <> wkbGeometryTypeLine then begin
      Abort;
    end;
    VLine := LoadSingleLine(AStream, VOrder);
    if Assigned(VLine) then begin
      VBuilder.AddLine(VLine);
    end;
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryFromWKB.LoadMultiPolygon(
  const AStream: TStream;
  const AOrder: Boolean
): IGeometryLonLatPolygon;
var
  VBuilder: IGeometryLonLatPolygonBuilder;
  VCount: Cardinal;
  i: Integer;
  VWKBType: Cardinal;
  VWKBOrder: Byte;
  VOrder: Boolean;
begin
  VBuilder := FFactory.MakePolygonBuilder;
  AStream.ReadBuffer(VCount, SizeOf(VCount));

  if VCount >= MaxInt / (1 + 4 + 4 + 2 * SizeOf(Double)) then begin
    Abort;
  end;

  for i := 0 to VCount - 1 do begin
    AStream.ReadBuffer(VWKBOrder, SizeOf(VWKBOrder));
    VOrder := VWKBOrder = 1;
    Assert(VOrder, 'Поддерживается тольк порядок Little Endian');
    if not VOrder then begin
      Abort;
    end;
    AStream.ReadBuffer(VWKBType, SizeOf(VWKBType));
    if VWKBType <> wkbGeometryTypePolygon then begin
      Abort;
    end;
    LoadPolygons(VBuilder, AStream, VOrder);
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryFromWKB.LoadPoint(
  const AStream: TStream;
  const AOrder: Boolean
): IGeometryLonLatPoint;
var
  VPoint: TDoublePoint;
begin
  AStream.ReadBuffer(VPoint, SizeOf(VPoint));
  Result := FFactory.CreateLonLatPoint(VPoint);
end;

function TGeometryFromWKB.LoadPolygon(
  const AStream: TStream;
  const AOrder: Boolean
): IGeometryLonLatPolygon;
var
  VBuilder: IGeometryLonLatPolygonBuilder;
begin
  VBuilder := FFactory.MakePolygonBuilder;
  LoadPolygons(VBuilder, AStream, AOrder);
  Result := VBuilder.MakeStaticAndClear;
end;

procedure TGeometryFromWKB.LoadPolygons(
  const ABuilder: IGeometryLonLatPolygonBuilder;
  const AStream: TStream;
  const AOrder: Boolean
);
var
  VCount: Cardinal;
  i: Integer;
  VLine: IDoublePoints;
begin
  AStream.ReadBuffer(VCount, SizeOf(VCount));

  if VCount >= MaxInt / (1 + 4 + 4 + 2 * SizeOf(Double)) then begin
    Abort;
  end;
  if VCount > 0 then begin
    VLine := LoadSinglePolygon(AStream, AOrder);
    if Assigned(VLine) then begin
      ABuilder.AddOuter(VLine);
    end;
    for i := 1 to VCount - 1 do begin
      VLine := LoadSinglePolygon(AStream, AOrder);
      if Assigned(VLine) then begin
        ABuilder.AddHole(VLine);
      end;
    end;
  end;
end;

function TGeometryFromWKB.LoadSinglePolygon(
  const AStream: TStream;
  const AOrder: Boolean
): IDoublePoints;
var
  VCount: Cardinal;
  VBuffer: PDoublePointArray;
begin
  Result := nil;
  AStream.ReadBuffer(VCount, SizeOf(VCount));

  if VCount >= MaxInt / 2 / SizeOf(Double) then begin
    Abort;
  end;
  GetMem(VBuffer, VCount * SizeOf(TDoublePoint));
  AStream.ReadBuffer(VBuffer[0], VCount * SizeOf(TDoublePoint));
  Result := TDoublePoints.CreateWithOwn(VBuffer, VCount);
end;

function TGeometryFromWKB.Parse(
  const AStream: TStream
): IGeometryLonLat;
var
  VWKBType: Cardinal;
  VWKBOrder: Byte;
  VOrder: Boolean;
begin
  AStream.ReadBuffer(VWKBOrder, SizeOf(VWKBOrder));
  VOrder := VWKBOrder = 1;
  Assert(VOrder, 'Поддерживается тольк порядок Little Endian');
  if not VOrder then begin
    Abort;
  end;
  AStream.ReadBuffer(VWKBType, SizeOf(VWKBType));
  case VWKBType of
    wkbGeometryTypePoint: begin
      Result := LoadPoint(AStream, VOrder);
    end;
    wkbGeometryTypeLine: begin
      Result := LoadLine(AStream, VOrder);
    end;
    wkbGeometryTypePolygon: begin
      Result := LoadPolygon(AStream, VOrder);
    end;
    wkbGeometryTypeMultiLine: begin
      Result := LoadMultiLine(AStream, VOrder);
    end;
    wkbGeometryTypeMultiPolygon: begin
      Result := LoadMultiPolygon(AStream, VOrder);
    end;
  else begin
    Abort;
  end;
  end;
end;

end.
