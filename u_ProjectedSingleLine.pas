unit u_ProjectedSingleLine;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_ProjectionInfo,
  i_VectorItemProjected;

type
  TProjectedLineBase = class(TInterfacedObject)
  private
    FCount: Integer;
    FPoints: TArrayOfDoublePoint;
    FProjection: IProjectionInfo;
  private
    function GetProjection: IProjectionInfo;
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create(
      AClosed: Boolean;
      AProjection: IProjectionInfo;
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TProjectedPathLine = class(TProjectedLineBase, IProjectedPathLine)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointOnPath(const APoint: TDoublePoint; ADist: Double): Boolean;
  public
    constructor Create(
      AProjection: IProjectionInfo;
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TProjectedPolygonLine = class(TProjectedLineBase, IProjectedPolygonLine)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function CalcArea: Double;
  public
    constructor Create(
      AProjection: IProjectionInfo;
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

implementation

uses
  u_GeoFun,
  u_EnumDoublePointBySingleLine;

{ TProjectedLineBase }

constructor TProjectedLineBase.Create(
  AClosed: Boolean;
  AProjection: IProjectionInfo;
  APoints: PDoublePointArray;
  ACount: Integer
);
begin
  FProjection := AProjection;
  FCount := ACount;
  Assert(FCount > 0, 'Empty line');
  if AClosed and (FCount > 1) and DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Dec(FCount);
  end;

  SetLength(FPoints, FCount);
  Move(APoints^, FPoints[0], FCount * SizeOf(TDoublePoint));
end;

function TProjectedLineBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TProjectedLineBase.GetPoints: PDoublePointArray;
begin
  Result := @FPoints[0];
end;

function TProjectedLineBase.GetProjection: IProjectionInfo;
begin
  Result := FProjection;
end;

{ TProjectedPathLine }

constructor TProjectedPathLine.Create(AProjection: IProjectionInfo;
  APoints: PDoublePointArray; ACount: Integer);
begin
  inherited Create(False, AProjection, APoints, ACount);
end;

function TProjectedPathLine.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumDoublePointBySingleProjectedLine.Create(Self, False, @FPoints[0], FCount);
end;

function TProjectedPathLine.IsPointOnPath(
  const APoint: TDoublePoint;
  ADist: Double
): Boolean;
var
  i: Integer;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VVectorW: TDoublePoint;
  VVectorV: TDoublePoint;
  C1: Double;
  C2: Double;
  B: Double;
  VVectorDist: TDoublePoint;
  VDistSQR: Double;
begin
  Result := False;
  if FCount > 1 then begin
    VDistSQR := ADist * ADist;
    VCurrPoint := FPoints[0];
    for i := 1 to FCount - 1 do begin
      VPrevPoint := VCurrPoint;
      VCurrPoint := FPoints[i];
      VVectorW.X := APoint.X - VPrevPoint.X;
      VVectorW.Y := APoint.Y - VPrevPoint.Y;
      VVectorV.X := VCurrPoint.X - VPrevPoint.X;
      VVectorV.Y := VCurrPoint.Y - VPrevPoint.Y;
      C1 := VVectorW.X * VVectorV.X + VVectorW.Y * VVectorV.Y;
      if C1 > 0 then begin
        C2 := VVectorV.X * VVectorV.X + VVectorV.Y * VVectorV.Y;
        if C2 > C1 then begin
          B := C1 / C2;
          VVectorDist.X := VVectorW.X - B * VVectorV.X;
          VVectorDist.Y := VVectorW.Y - B * VVectorV.Y;
          if (VVectorDist.X * VVectorDist.X + VVectorDist.Y * VVectorDist.Y) < VDistSQR then begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

{ TProjectedPolygonLine }

function TProjectedPolygonLine.CalcArea: Double;
var
  I, J, HP: Integer;
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + (VPrevPoint.X + VCurrPoint.X) * (VPrevPoint.Y - VCurrPoint.Y);
      VPrevPoint := VCurrPoint;
    end;
    Result := Abs(Result) / 2;
  end;
end;

constructor TProjectedPolygonLine.Create(AProjection: IProjectionInfo;
  APoints: PDoublePointArray; ACount: Integer);
begin
  inherited Create(True, AProjection, APoints, ACount);
end;

function TProjectedPolygonLine.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumDoublePointBySingleProjectedLine.Create(Self, True, @FPoints[0], FCount);
end;

function TProjectedPolygonLine.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
var
  VEnum: IEnumDoublePoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  result:=false;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      if
        (((VCurrPoint.y<=APoint.y)and(APoint.y<VPrevPoint.y))or
        ((VPrevPoint.y<=APoint.y)and(APoint.y<VCurrPoint.y)))and
        (APoint.x>(VPrevPoint.x-VCurrPoint.x)*(APoint.y-VCurrPoint.y)/(VPrevPoint.y-VCurrPoint.y)+VCurrPoint.x)
      then begin
        result:=not(result);
      end;
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

end.
