unit u_GeometryFromStreamSML;

interface

uses
  Classes,
  i_GeometryLonLat,
  i_GeometryFromStream,
  i_GeometryLonLatFactory,
  u_BaseInterfacedObject;

type
  TGeometryFromStreamSML = class(TBaseInterfacedObject, IGeometryFromStream)
  private
    FFactory: IGeometryLonLatFactory;
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
  Math,
  t_GeoTypes,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoFunc;

type
  TExtendedPoint = record
    X, Y: Extended;
  end;

procedure Stream2ExtArr(
  const AStream: TStream;
  const AAggregator: IDoublePointsAggregator
);
const
  CMaxDegres: Extended = 360;
  CMinDegres: Extended = -360;
var
  VSize: Integer;
  VPointsCount: Integer;
  i: Integer;
  VPoint: TExtendedPoint;
  VDoublePoint: TDoublePoint;
begin
    VSize := AStream.Size;
    VPointsCount := VSize div SizeOf(TExtendedPoint);
    for i := 0 to VPointsCount - 1 do begin
      AStream.ReadBuffer(VPoint, SizeOf(TExtendedPoint));
      try
        if IsNan(VPoint.X) or IsNan(VPoint.Y) then begin
          VDoublePoint := CEmptyDoublePoint;
        end else if (VPoint.X >= CMaxDegres) or (VPoint.X <= CMinDegres) or (VPoint.Y >= CMaxDegres) or (VPoint.Y <= CMinDegres) then begin
          VDoublePoint := CEmptyDoublePoint;
        end else begin
          VDoublePoint := DoublePoint(VPoint.X, VPoint.Y);
        end;
      except
        VDoublePoint := CEmptyDoublePoint;
      end;
      AAggregator.Add(VDoublePoint);
    end;
end;

{ TGeometryFromStreamSML }

constructor TGeometryFromStreamSML.Create(
  const AFactory: IGeometryLonLatFactory
);
begin
  Assert(Assigned(AFactory));
  inherited Create;
  FFactory := AFactory;
end;

function TGeometryFromStreamSML.Parse(const AStream: TStream): IGeometryLonLat;
var
  VPoints: IDoublePointsAggregator;
begin
  VPoints := TDoublePointsAggregator.Create;
  Stream2ExtArr(AStream, VPoints);
  Result := nil;
  if VPoints.Count > 0 then begin
    if VPoints.Count = 1 then begin
      if not PointIsEmpty(VPoints.Points[0]) then begin
        Result := FFactory.CreateLonLatPoint(VPoints.Points[0]);
      end;
    end else begin
      if DoublePointsEqual(VPoints.Points[0], VPoints.Points[VPoints.Count - 1]) then begin
        Result := FFactory.CreateLonLatMultiPolygon(VPoints.Points, VPoints.Count - 1);
      end else begin
        Result := FFactory.CreateLonLatMultiLine(VPoints.Points, VPoints.Count);
      end;
    end;
  end;
end;

end.
