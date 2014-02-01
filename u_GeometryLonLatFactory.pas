unit u_GeometryLonLatFactory;

interface

uses
  t_GeoTypes,
  i_HashFunction,
  i_ProjectionInfo,
  i_EnumDoublePoint,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  u_BaseInterfacedObject;

type
  TGeometryLonLatFactory = class(TBaseInterfacedObject, IGeometryLonLatFactory)
  private
    FHashFunction: IHashFunction;
    FEmptyLonLatPath: IGeometryLonLatMultiLine;
    FEmptyLonLatPolygon: IGeometryLonLatMultiPolygon;
    function CreateLonLatLineInternal(
      const ARect: TDoubleRect;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatLine;
    function CreateLonLatPolygonInternal(
      const ARect: TDoubleRect;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatPolygon;
  private
    function CreateLonLatPoint(
      const APoint: TDoublePoint
    ): IGeometryLonLatPoint;
    function CreateLonLatLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatLine;
    function CreateLonLatPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatPolygon;

    function MakeGeometryLonLatMultiLineBuilder(): IGeometryLonLatMultiLineBuilder;
    function MakeGeometryLonLatMultiPolygonBuilder(): IGeometryLonLatMultiPolygonBuilder;

    function CreateLonLatMultiLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatMultiLine;
    function CreateLonLatMultiPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatMultiPolygon;
    function CreateLonLatMultiLineByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatMultiLine;
    function CreateLonLatMultiPolygonByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatMultiPolygon;

    function CreateLonLatPolygonLineByRect(
      const ARect: TDoubleRect
    ): IGeometryLonLatPolygon;
    function CreateLonLatMultiPolygonByRect(
      const ARect: TDoubleRect
    ): IGeometryLonLatMultiPolygon;

    function CreateLonLatMultiPolygonCircleByPoint(
      const AProjection: IProjectionInfo;
      const APos: TDoublePoint;
      const ARadius: double
    ): IGeometryLonLatMultiPolygon;

    function CreateLonLatMultiPolygonByLonLatPathAndFilter(
      const ASource: IGeometryLonLatMultiLine;
      const AFilter: ILonLatPointFilter
    ): IGeometryLonLatMultiPolygon;
  public
    constructor Create(const AHashFunction: IHashFunction);
  end;

implementation

uses
  t_Hash,
  i_LonLatRect,
  i_Datum,
  i_InterfaceListSimple,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryLonLat,
  u_LonLatRect,
  u_LonLatRectByPoint,
  u_VectorItemEmpty,
  u_GeometryLonLatMulti;

type
  TGeometryLonLatMultiLineBuilder = class(TBaseInterfacedObject, IGeometryLonLatMultiLineBuilder)
  private
    FHashFunction: IHashFunction;
    FEmpty: IGeometryLonLatMultiLine;
    FHash: THashValue;
    FBounds: TDoubleRect;
    FLine: IGeometryLonLatLine;
    FList: IInterfaceListSimple;
  private
    procedure Add(const AElement: IGeometryLonLatLine);

    function MakeStaticAndClear: IGeometryLonLatMultiLine;
    function MakeStaticCopy: IGeometryLonLatMultiLine;
  public
    constructor Create(
      const AEmpty: IGeometryLonLatMultiLine;
      const AHashFunction: IHashFunction
    );
  end;

{ TGeometryLonLatMultiLineBuilder }

constructor TGeometryLonLatMultiLineBuilder.Create(
  const AEmpty: IGeometryLonLatMultiLine;
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FEmpty := AEmpty;
  FHashFunction := AHashFunction;
end;

procedure TGeometryLonLatMultiLineBuilder.Add(
  const AElement: IGeometryLonLatLine
);
begin
  Assert(Assigned(AElement));
  if not Assigned(FLine) then begin
    FLine := AElement;
    FHash := FLine.Hash;
    FBounds := FLine.Bounds.Rect;
  end else begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
    end;
    FList.Add(FLine);
    FList.Add(AElement);
    FHashFunction.UpdateHashByHash(FHash, AElement.Hash);
    FBounds := AElement.Bounds.UnionWithRect(FBounds);
  end;
end;

function TGeometryLonLatMultiLineBuilder.MakeStaticAndClear: IGeometryLonLatMultiLine;
var
  VRect: ILonLatRect;
begin
  Result := FEmpty;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      VRect := TLonLatRect.Create(FBounds);
      Result := TGeometryLonLatMultiLine.Create(VRect, FHash, FList.MakeStaticAndClear);
    end else begin
      Result := TLonLatPathOneLine.Create(FLine);
    end;
    FLine := nil;
  end;
end;

function TGeometryLonLatMultiLineBuilder.MakeStaticCopy: IGeometryLonLatMultiLine;
var
  VRect: ILonLatRect;
begin
  Result := FEmpty;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      VRect := TLonLatRect.Create(FBounds);
      Result := TGeometryLonLatMultiLine.Create(VRect, FHash, FList.MakeStaticCopy);
    end else begin
      Result := TLonLatPathOneLine.Create(FLine);
    end;
  end;
end;

type
  TGeometryLonLatMultiPolygonBuilder = class(TBaseInterfacedObject, IGeometryLonLatMultiPolygonBuilder)
  private
    FHashFunction: IHashFunction;
    FEmpty: IGeometryLonLatMultiPolygon;
    FHash: THashValue;
    FBounds: TDoubleRect;
    FLine: IGeometryLonLatPolygon;
    FList: IInterfaceListSimple;
  private
    procedure Add(const AElement: IGeometryLonLatPolygon);

    function MakeStaticAndClear: IGeometryLonLatMultiPolygon;
    function MakeStaticCopy: IGeometryLonLatMultiPolygon;
  public
    constructor Create(
      const AEmpty: IGeometryLonLatMultiPolygon;
      const AHashFunction: IHashFunction
    );
  end;

{ TGeometryLonLatMultiPolygonBuilder }

constructor TGeometryLonLatMultiPolygonBuilder.Create(
  const AEmpty: IGeometryLonLatMultiPolygon;
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FEmpty := AEmpty;
  FHashFunction := AHashFunction;
end;

procedure TGeometryLonLatMultiPolygonBuilder.Add(
  const AElement: IGeometryLonLatPolygon
);
begin
  Assert(Assigned(AElement));
  if not Assigned(FLine) then begin
    FLine := AElement;
    FHash := FLine.Hash;
    FBounds := FLine.Bounds.Rect;
  end else begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
    end;
    FList.Add(FLine);
    FList.Add(AElement);
    FHashFunction.UpdateHashByHash(FHash, AElement.Hash);
    FBounds := AElement.Bounds.UnionWithRect(FBounds);
  end;
end;

function TGeometryLonLatMultiPolygonBuilder.MakeStaticAndClear: IGeometryLonLatMultiPolygon;
var
  VRect: ILonLatRect;
begin
  Result := FEmpty;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      VRect := TLonLatRect.Create(FBounds);
      Result := TGeometryLonLatMultiPolygon.Create(VRect, FHash, FList.MakeStaticAndClear);
    end else begin
      Result := TLonLatPolygonOneLine.Create(FLine);
    end;
    FLine := nil;
  end;
end;

function TGeometryLonLatMultiPolygonBuilder.MakeStaticCopy: IGeometryLonLatMultiPolygon;
var
  VRect: ILonLatRect;
begin
  Result := FEmpty;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      VRect := TLonLatRect.Create(FBounds);
      Result := TGeometryLonLatMultiPolygon.Create(VRect, FHash, FList.MakeStaticCopy);
    end else begin
      Result := TLonLatPolygonOneLine.Create(FLine);
    end;
  end;
end;

{ TVectorGeometryLonLatFactory }

constructor TGeometryLonLatFactory.Create(
  const AHashFunction: IHashFunction);
var
  VEmpty: TGeometryLonLatMultiEmpty;
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
  VEmpty := TGeometryLonLatMultiEmpty.Create;
  FEmptyLonLatPath := VEmpty;
  FEmptyLonLatPolygon := VEmpty;
end;

function TGeometryLonLatFactory.CreateLonLatMultiPolygonCircleByPoint(
  const AProjection: IProjectionInfo;
  const APos: TDoublePoint;
  const ARadius: double
): IGeometryLonLatMultiPolygon;
const CPointCount = 64;
var
  VAggreagator: IDoublePointsAggregator;
  j: Integer;
  VDatum : IDatum;
  VAngle: Double;
  VPoint: TDoublePoint;
  VLine: IGeometryLonLatPolygon;
  VBounds: TDoubleRect;
begin
  Assert(not PointIsEmpty(APos));
  VAggreagator := TDoublePointsAggregator.Create(CPointCount);
  VBounds.TopLeft := APos;
  VBounds.BottomRight := APos;
  VDatum :=  AProjection.GeoConverter.Datum;
  for j := 0 to CPointCount - 1 do begin
    VAngle := j * 360 / CPointCount;
    VPoint := VDatum.CalcFinishPosition(APos, VAngle, ARadius);
    VAggreagator.Add(VPoint);
    if VBounds.Left > VPoint.X then begin
      VBounds.Left := VPoint.X;
    end;
    if VBounds.Top < VPoint.Y then begin
      VBounds.Top := VPoint.Y;
    end;
    if VBounds.Right < VPoint.X then begin
      VBounds.Right := VPoint.X;
    end;
    if VBounds.Bottom > VPoint.Y then begin
      VBounds.Bottom := VPoint.Y;
    end;
  end;
  VLine := CreateLonLatPolygonInternal(VBounds, VAggreagator.Points, VAggreagator.Count);
  Result := TLonLatPolygonOneLine.Create(VLine);
end;

function TGeometryLonLatFactory.CreateLonLatLine(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatLine;
var
  i: Integer;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  Result := nil;
  Assert(ACount > 0);
  if ACount > 0 then begin
    VPoint := APoints[0];
    Assert(not PointIsEmpty(VPoint));
    if PointIsEmpty(VPoint) then begin
      Exit;
    end;
    VBounds.TopLeft := VPoint;
    VBounds.BottomRight := VPoint;
    for i := 1 to ACount - 1 do begin
      VPoint := APoints[i];
      Assert(not PointIsEmpty(VPoint));
      if PointIsEmpty(VPoint) then begin
        Exit;
      end;
      if VBounds.Left > VPoint.X then begin
        VBounds.Left := VPoint.X;
      end;
      if VBounds.Top < VPoint.Y then begin
        VBounds.Top := VPoint.Y;
      end;
      if VBounds.Right < VPoint.X then begin
        VBounds.Right := VPoint.X;
      end;
      if VBounds.Bottom > VPoint.Y then begin
        VBounds.Bottom := VPoint.Y;
      end;
    end;
    Result := CreateLonLatLineInternal(VBounds, APoints, ACount);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatPolygon(
  const APoints: PDoublePointArray; ACount: Integer): IGeometryLonLatPolygon;
var
  i: Integer;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  Result := nil;
  Assert(ACount > 0);
  if ACount > 0 then begin
    VPoint := APoints[0];
    Assert(not PointIsEmpty(VPoint));
    if PointIsEmpty(VPoint) then begin
      Exit;
    end;
    VBounds.TopLeft := VPoint;
    VBounds.BottomRight := VPoint;
    for i := 1 to ACount - 1 do begin
      VPoint := APoints[i];
      Assert(not PointIsEmpty(VPoint));
      if PointIsEmpty(VPoint) then begin
        Exit;
      end;
      if VBounds.Left > VPoint.X then begin
        VBounds.Left := VPoint.X;
      end;
      if VBounds.Top < VPoint.Y then begin
        VBounds.Top := VPoint.Y;
      end;
      if VBounds.Right < VPoint.X then begin
        VBounds.Right := VPoint.X;
      end;
      if VBounds.Bottom > VPoint.Y then begin
        VBounds.Bottom := VPoint.Y;
      end;
    end;
    Result := CreateLonLatPolygonInternal(VBounds, APoints, ACount);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatLineInternal(
  const ARect: TDoubleRect;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatLine;
var
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  if ACount > 1 then begin
    VRect := TLonLatRect.Create(ARect);
  end else begin
    VRect := TLonLatRectByPoint.Create(ARect.TopLeft);
  end;
  VHash := FHashFunction.CalcHashByBuffer(APoints, ACount * SizeOf(TDoublePoint));
  Result := TGeometryLonLatLine.Create(VRect, VHash, APoints, ACount);
end;

function TGeometryLonLatFactory.CreateLonLatPolygonInternal(
  const ARect: TDoubleRect;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatPolygon;
var
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  if (ACount > 1) and DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Dec(ACount);
  end;
  if ACount > 1 then begin
    VRect := TLonLatRect.Create(ARect);
  end else begin
    VRect := TLonLatRectByPoint.Create(ARect.TopLeft);
  end;
  VHash := FHashFunction.CalcHashByBuffer(APoints, ACount * SizeOf(TDoublePoint));
  Result := TGeometryLonLatPolygon.Create(VRect, VHash, APoints, ACount);
end;

function TGeometryLonLatFactory.CreateLonLatMultiLine(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatMultiLine;
var
  VLine: IGeometryLonLatLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VPoint: TDoublePoint;
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatMultiLineBuilder;
begin
  VBuilder := MakeGeometryLonLatMultiLineBuilder;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        VLine := CreateLonLatLineInternal(VLineBounds, VStart, VLineLen);
        VBuilder.Add(VLine);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    VLine := CreateLonLatLineInternal(VLineBounds, VStart, VLineLen);
    VBuilder.Add(VLine);
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryLonLatFactory.CreateLonLatMultiLineByEnum(
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLonLatMultiLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryLonLatLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VLinesetHash: THashValue;
begin
  VLinesetHash := 0;
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
            VLinesetHash := VLine.Hash;
          end else begin
            FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := CreateLonLatLineInternal(VLineBounds, VTemp.Points, VTemp.Count);
        if VLineCount > 0 then begin
          VBounds := UnionLonLatRects(VBounds, VLineBounds);
        end else begin
          VBounds := VLineBounds;
        end;
        Inc(VLineCount);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
        VLinesetHash := VLine.Hash;
      end else begin
        FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := CreateLonLatLineInternal(VLineBounds, VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionLonLatRects(VBounds, VLineBounds);
    end else begin
      VBounds := VLineBounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPath;
  end else if VLineCount = 1 then begin
    Result := TLonLatPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiLine.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatPoint(
  const APoint: TDoublePoint
): IGeometryLonLatPoint;
var
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  VHash := FHashFunction.CalcHashByDoublePoint(APoint);
  VRect := TLonLatRectByPoint.Create(APoint);
  Result := TGeometryLonLatPoint.Create(VHash, VRect);
end;

function TGeometryLonLatFactory.CreateLonLatMultiPolygon(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatMultiPolygon;
var
  VLine: IGeometryLonLatPolygon;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VLinesetHash: THashValue;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  VLinesetHash := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
            VLinesetHash := VLine.Hash;
          end else begin
            FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := CreateLonLatPolygonInternal(VLineBounds, VStart, VLineLen);
        if VLineCount > 0 then begin
          VBounds := UnionLonLatRects(VBounds, VLineBounds);
        end else begin
          VBounds := VLineBounds;
        end;
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
        VLinesetHash := VLine.Hash;
      end else begin
        FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := CreateLonLatPolygonInternal(VLineBounds, VStart, VLineLen);
    if VLineCount > 0 then begin
      VBounds := UnionLonLatRects(VBounds, VLineBounds);
    end else begin
      VBounds := VLineBounds;
    end;
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPolygon;
  end else if VLineCount = 1 then begin
    Result := TLonLatPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiPolygon.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatMultiPolygonByEnum(
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLonLatMultiPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryLonLatPolygon;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VLinesetHash: THashValue;
begin
  VLinesetHash := 0;
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
            VLinesetHash := VLine.Hash;
          end else begin
            FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.Points, VTemp.Count);
        if VLineCount > 0 then begin
          VBounds := UnionLonLatRects(VBounds, VLineBounds);
        end else begin
          VBounds := VLineBounds;
        end;
        Inc(VLineCount);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
        VLinesetHash := VLine.Hash;
      end else begin
        FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionLonLatRects(VBounds, VLineBounds);
    end else begin
      VBounds := VLineBounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPolygon;
  end else if VLineCount = 1 then begin
    Result := TLonLatPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiPolygon.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatMultiPolygonByLonLatPathAndFilter(
  const ASource: IGeometryLonLatMultiLine;
  const AFilter: ILonLatPointFilter
): IGeometryLonLatMultiPolygon;
var
  i: Integer;
  VLine: IGeometryLonLatPolygon;
  VEnum: IEnumLonLatPoint;
  VTemp: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VLinesetHash: THashValue;
begin
  VLinesetHash := 0;
  VLineCount := 0;
  VTemp := TDoublePointsAggregator.Create;
  for i := 0 to ASource.Count - 1 do begin
    VEnum := AFilter.CreateFilteredEnum(ASource.Item[i].GetEnum);
    while VEnum.Next(VPoint) do begin
      if PointIsEmpty(VPoint) then begin
        if VTemp.Count > 0 then begin
          if VLineCount > 0 then begin
            if VLineCount = 1 then begin
              VList := TInterfaceListSimple.Create;
              VLinesetHash := VLine.Hash;
            end else begin
              FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
            end;
            VList.Add(VLine);
            VLine := nil;
          end;
          VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.Points, VTemp.Count);
          if VLineCount > 0 then begin
            VBounds := UnionLonLatRects(VBounds, VLineBounds);
          end else begin
            VBounds := VLineBounds;
          end;
          Inc(VLineCount);
          VTemp.Clear;
        end;
      end else begin
        if VTemp.Count = 0 then begin
          VLineBounds.TopLeft := VPoint;
          VLineBounds.BottomRight := VPoint;
        end else begin
          if VLineBounds.Left > VPoint.X then begin
            VLineBounds.Left := VPoint.X;
          end;
          if VLineBounds.Top < VPoint.Y then begin
            VLineBounds.Top := VPoint.Y;
          end;
          if VLineBounds.Right < VPoint.X then begin
            VLineBounds.Right := VPoint.X;
          end;
          if VLineBounds.Bottom > VPoint.Y then begin
            VLineBounds.Bottom := VPoint.Y;
          end;
        end;
        VTemp.Add(VPoint);
      end;
    end;
    if VTemp.Count > 0 then begin
      if VLineCount > 0 then begin
        if VLineCount = 1 then begin
          VList := TInterfaceListSimple.Create;
          VLinesetHash := VLine.Hash;
        end else begin
          FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
        end;
        VList.Add(VLine);
        VLine := nil;
      end;
      VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.Points, VTemp.Count);
      if VLineCount > 0 then begin
        VBounds := UnionLonLatRects(VBounds, VLineBounds);
      end else begin
        VBounds := VLineBounds;
      end;
      Inc(VLineCount);
      VTemp.Clear;
    end;
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPolygon;
  end else if VLineCount = 1 then begin
    Result := TLonLatPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiPolygon.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatMultiPolygonByRect(
  const ARect: TDoubleRect
): IGeometryLonLatMultiPolygon;
begin
  Result := TLonLatPolygonOneLine.Create(CreateLonLatPolygonLineByRect(ARect));
end;

function TGeometryLonLatFactory.CreateLonLatPolygonLineByRect(
  const ARect: TDoubleRect
): IGeometryLonLatPolygon;
var
  VPoints: array [0..4] of TDoublePoint;
begin
  VPoints[0] := ARect.TopLeft;
  VPoints[1].X := ARect.Right;
  VPoints[1].Y := ARect.Top;
  VPoints[2] := ARect.BottomRight;
  VPoints[3].X := ARect.Left;
  VPoints[3].Y := ARect.Bottom;
  Result := CreateLonLatPolygonInternal(ARect, @VPoints[0], 4);
end;

function TGeometryLonLatFactory.MakeGeometryLonLatMultiLineBuilder: IGeometryLonLatMultiLineBuilder;
begin
  Result := TGeometryLonLatMultiLineBuilder.Create(FEmptyLonLatPath, FHashFunction);
end;

function TGeometryLonLatFactory.MakeGeometryLonLatMultiPolygonBuilder: IGeometryLonLatMultiPolygonBuilder;
begin
  Result := TGeometryLonLatMultiPolygonBuilder.Create(FEmptyLonLatPolygon, FHashFunction);
end;

end.
