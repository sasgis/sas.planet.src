unit u_GeometryLocalFactory;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_GeometryLocal,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  i_GeometryLocalFactory,
  u_BaseInterfacedObject;

type
  TGeometryLocalFactory = class(TBaseInterfacedObject, IGeometryLocalFactory)
  private
    function CreateLocalPath(
      const ALocalConverter: ILocalCoordConverter;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLocalMultiLine;
    function CreateLocalPolygon(
      const ALocalConverter: ILocalCoordConverter;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLocalMultiPolygon;
    function CreateLocalPathByEnum(
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLocalMultiLine;
    function CreateLocalPolygonByEnum(
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLocalMultiPolygon;
  end;

implementation

uses
  i_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryLocal,
  u_GeometryLocalMulti,
  u_InterfaceListSimple,
  u_GeoFunc;

{ TVectorGeometryLocalFactory }

function TGeometryLocalFactory.CreateLocalPath(
  const ALocalConverter: ILocalCoordConverter;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLocalMultiLine;
var
  VLine: IGeometryLocalLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TGeometryLocalLine.Create(ALocalConverter, VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TGeometryLocalLine.Create(ALocalConverter, VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := TGeometryLocalMultiLineEmpty.Create(ALocalConverter);
  end else if VLineCount = 1 then begin
    Result := TGeometryLocalMultiLineOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryLocalMultiLine.Create(ALocalConverter, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLocalFactory.CreateLocalPathByEnum(
  const ALocalConverter: ILocalCoordConverter;
  const AEnum: IEnumLocalPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLocalMultiLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryLocalLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
begin
  Assert(ALocalConverter <> nil);
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
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TGeometryLocalLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
        Inc(VLineCount);
        VTemp.Clear;
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TGeometryLocalLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TGeometryLocalMultiLineEmpty.Create(ALocalConverter);
  end else if VLineCount = 1 then begin
    Result := TGeometryLocalMultiLineOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryLocalMultiLine.Create(ALocalConverter, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLocalFactory.CreateLocalPolygon(
  const ALocalConverter: ILocalCoordConverter;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLocalMultiPolygon;
var
  VLine: IGeometryLocalPolygon;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TGeometryLocalPolygon.Create(ALocalConverter, VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TGeometryLocalPolygon.Create(ALocalConverter, VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := TGeometryLocalMultiPolygonEmpty.Create(ALocalConverter);
  end else if VLineCount = 1 then begin
    Result := TGeometryLocalMultiPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryLocalMultiPolygon.Create(ALocalConverter, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLocalFactory.CreateLocalPolygonByEnum(
  const ALocalConverter: ILocalCoordConverter;
  const AEnum: IEnumLocalPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLocalMultiPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryLocalPolygon;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
begin
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
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TGeometryLocalPolygon.Create(ALocalConverter, VTemp.Points, VTemp.Count);
        Inc(VLineCount);
        VTemp.Clear;
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TGeometryLocalPolygon.Create(ALocalConverter, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TGeometryLocalMultiPolygonEmpty.Create(ALocalConverter);
  end else if VLineCount = 1 then begin
    Result := TGeometryLocalMultiPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryLocalMultiPolygon.Create(ALocalConverter, VList.MakeStaticAndClear);
  end;
end;

end.
