unit u_GeometryToWKB;

interface

uses
  Classes,
  i_GeometryLonLat,
  i_GeometryToStream,
  u_BaseInterfacedObject;

type
  TGeometryToWKB = class(TBaseInterfacedObject, IGeometryToStream)
  private
    function CalcFullSize(
      const AGeometry: IGeometryLonLat
    ): Integer;
    procedure SavePoint(
      const AGeometry: IGeometryLonLatPoint;
      const AStream: TStream
    );
    procedure SaveLine(
      const AGeometry: IGeometryLonLatSingleLine;
      const AStream: TStream
    );
    procedure SaveContour(
      const AGeometry: IGeometryLonLatContour;
      const AStream: TStream
    );
    procedure SavePolygon(
      const AGeometry: IGeometryLonLatSinglePolygon;
      const AStream: TStream
    );
    procedure SaveMultiLine(
      const AGeometry: IGeometryLonLatMultiLine;
      const AStream: TStream
    );
    procedure SaveMultiPolygon(
      const AGeometry: IGeometryLonLatMultiPolygon;
      const AStream: TStream
    );
  private
    procedure Save(
      const AGeometry: IGeometryLonLat;
      const AStream: TStream
    );
  end;

implementation

uses
  SysUtils,
  t_GeoTypes;

const
  wkbGeometryTypePoint = 1;
  wkbGeometryTypeLine = 2;
  wkbGeometryTypePolygon = 3;
  wkbGeometryTypeMultiLine = 5;
  wkbGeometryTypeMultiPolygon = 6;

{ TGeometryToWKB }

function TGeometryToWKB.CalcFullSize(const AGeometry: IGeometryLonLat): Integer;
var
  VLine: IGeometryLonLatSingleLine;
  VPolygon: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiLine;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
  i: Integer;
begin
  // for all types ByteOrder and GeometryType
  Result := SizeOf(Byte) + SizeOf(Cardinal);
  if Supports(AGeometry, IGeometryLonLatPoint) then begin
    // simple one point
    Inc(Result, 2 * SizeOf(TDoublePoint));
  end else if Supports(AGeometry, IGeometryLonLatSingleLine, VLine) then begin
    if VLine.Count <= 0 then begin
      Result := 0;
    end else begin
      // number of point
      Inc(Result, SizeOf(Cardinal));
      // points
      Inc(Result, VLine.Count * 2 * SizeOf(TDoublePoint));
    end;
  end else if Supports(AGeometry, IGeometryLonLatSinglePolygon, VPolygon) then begin
    if VPolygon.Count <= 0 then begin
      Result := 0;
    end else begin
      // count of rings
      Inc(Result, SizeOf(Cardinal));
      // number of points in ring
      Inc(Result, SizeOf(Cardinal));
      // points
      Inc(Result, (VPolygon.Count + 1) * 2 * SizeOf(TDoublePoint));
    end;
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    if VMultiLine.Count <= 0 then begin
      Result := 0;
    end else if VMultiLine.Count = 1 then begin
      // number of point
      Inc(Result, SizeOf(Cardinal));
      // points
      Inc(Result, VMultiLine.Item[0].Count * 2 * SizeOf(TDoublePoint));
    end else begin
      // count of lines
      Inc(Result, SizeOf(Cardinal));
      // header for every line
      Inc(Result, VMultiLine.Count * (SizeOf(Byte) + SizeOf(Cardinal) + SizeOf(Cardinal)));
      // points
      for i := 0 to VMultiLine.Count - 1 do begin
        Inc(Result, VMultiLine.Item[i].Count * 2 * SizeOf(TDoublePoint));
      end;
    end;
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    if VMultiPolygon.Count <= 0 then begin
      Result := 0;
    end else if VMultiPolygon.Count = 1 then begin
      // count of rings
      Inc(Result, SizeOf(Cardinal));
      // number of points in ring
      Inc(Result, SizeOf(Cardinal));
      // points
      Inc(Result, (VMultiPolygon.Item[0].Count + 1) * 2 * SizeOf(TDoublePoint));
    end else begin
      // count of polygon
      Inc(Result, SizeOf(Cardinal));
      // header for every polygon
      Inc(Result, VMultiPolygon.Count * (SizeOf(Byte) + SizeOf(Cardinal) + SizeOf(Cardinal) + SizeOf(Cardinal)));
      // points
      for i := 0 to VMultiPolygon.Count - 1 do begin
        Inc(Result, (VMultiPolygon.Item[i].Count + 1) * 2 * SizeOf(TDoublePoint));
      end;
    end;
  end else begin
    Result := 0;
  end;
end;

procedure TGeometryToWKB.Save(
  const AGeometry: IGeometryLonLat;
  const AStream: TStream
);
var
  VNeedSize: Integer;
  VOrder: Byte;
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatSingleLine;
  VPolygon: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiLine;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
begin
  VNeedSize := CalcFullSize(AGeometry);
  if VNeedSize > 0 then begin
    VOrder := 1;
    AStream.WriteBuffer(VOrder, SizeOf(VOrder));
    if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
      SavePoint(VPoint, AStream);
    end else if Supports(AGeometry, IGeometryLonLatSingleLine, VLine) then begin
      SaveLine(VLine, AStream);
    end else if Supports(AGeometry, IGeometryLonLatSinglePolygon, VPolygon) then begin
      SavePolygon(VPolygon, AStream);
    end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
      if VMultiLine.Count <= 0 then begin
        Assert(False);
      end else if VMultiLine.Count = 1 then begin
        SaveLine(VMultiLine.Item[0], AStream);
      end else begin
        SaveMultiLine(VMultiLine, AStream);
      end;
    end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
      if VMultiPolygon.Count <= 0 then begin
        Assert(False);
      end else if VMultiPolygon.Count = 1 then begin
        SavePolygon(VMultiPolygon.Item[0], AStream);
      end else begin
        SaveMultiPolygon(VMultiPolygon, AStream);
      end;
    end else begin
      Assert(False);
    end;
  end;
end;

procedure TGeometryToWKB.SaveLine(
  const AGeometry: IGeometryLonLatSingleLine;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VCount: Cardinal;
begin
  VWKBType := wkbGeometryTypeLine;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VCount := AGeometry.Count;
  AStream.WriteBuffer(VCount, SizeOf(VCount));

  AStream.WriteBuffer(AGeometry.Points^, VCount * SizeOf(TDoublePoint));
end;

procedure TGeometryToWKB.SaveMultiLine(
  const AGeometry: IGeometryLonLatMultiLine;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VCount: Cardinal;
  i: Integer;
  VOrder: Byte;
begin
  VWKBType := wkbGeometryTypeMultiLine;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VCount := AGeometry.Count;
  AStream.WriteBuffer(VCount, SizeOf(VCount));
  for i := 0 to AGeometry.Count - 1 do begin
    VOrder := 1;
    AStream.WriteBuffer(VOrder, SizeOf(VOrder));
    SaveLine(AGeometry.Item[i], AStream);
  end;
end;

procedure TGeometryToWKB.SaveMultiPolygon(
  const AGeometry: IGeometryLonLatMultiPolygon;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VCount: Cardinal;
  i: Integer;
  VOrder: Byte;
begin
  VWKBType := wkbGeometryTypeMultiPolygon;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VCount := AGeometry.Count;
  AStream.WriteBuffer(VCount, SizeOf(VCount));

  for i := 0 to AGeometry.Count - 1 do begin
    VOrder := 1;
    AStream.WriteBuffer(VOrder, SizeOf(VOrder));
    SavePolygon(AGeometry.Item[i], AStream);
  end;
end;

procedure TGeometryToWKB.SavePoint(
  const AGeometry: IGeometryLonLatPoint;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VPoint: TDoublePoint;
begin
  VWKBType := wkbGeometryTypePoint;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VPoint := AGeometry.Point;
  AStream.WriteBuffer(VPoint, SizeOf(TDoublePoint));
end;

procedure TGeometryToWKB.SaveContour(
  const AGeometry: IGeometryLonLatContour;
  const AStream: TStream
);
var
  VCount: Cardinal;
begin
  VCount := AGeometry.Count + 1;
  AStream.WriteBuffer(VCount, SizeOf(VCount));
  AStream.WriteBuffer(AGeometry.Points^, AGeometry.Count * SizeOf(TDoublePoint));
  AStream.WriteBuffer(AGeometry.Points[0], SizeOf(TDoublePoint));
end;

procedure TGeometryToWKB.SavePolygon(
  const AGeometry: IGeometryLonLatSinglePolygon;
  const AStream: TStream
);
var
  VWKBType: Cardinal;
  VCount: Cardinal;
  i: Integer;
begin
  VWKBType := wkbGeometryTypePolygon;
  AStream.WriteBuffer(VWKBType, SizeOf(VWKBType));
  VCount := 1 + AGeometry.HoleCount;
  AStream.WriteBuffer(VCount, SizeOf(VCount));
  SaveContour(AGeometry.OuterBorder, AStream);
  for i := 0 to AGeometry.HoleCount - 1 do begin
    SaveContour(AGeometry.HoleBorder[i], AStream);
  end;
end;

end.
