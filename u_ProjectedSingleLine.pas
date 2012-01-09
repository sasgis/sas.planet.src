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

{ TProjectedPolygonLine }

constructor TProjectedPolygonLine.Create(AProjection: IProjectionInfo;
  APoints: PDoublePointArray; ACount: Integer);
begin
  inherited Create(True, AProjection, APoints, ACount);
end;

function TProjectedPolygonLine.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumDoublePointBySingleProjectedLine.Create(Self, True, @FPoints[0], FCount);
end;

end.
