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
    function GetEnum: IEnumDoublePoint;
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
  public
    constructor Create(
      AProjection: IProjectionInfo;
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TProjectedPolygonLine = class(TProjectedLineBase, IProjectedPolygonLine)
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
  Assert(FCount > 0, 'Empty line');
  if AClosed and not DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Inc(FCount);
    SetLength(FPoints, FCount);
    Move(APoints^, FPoints[0], ACount * SizeOf(TDoublePoint));
    FPoints[FCount - 1] := FPoints[0];
  end else begin
    SetLength(FPoints, FCount);
    Move(APoints^, FPoints[0], ACount * SizeOf(TDoublePoint));
  end;
end;

function TProjectedLineBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TProjectedLineBase.GetEnum: IEnumDoublePoint;
begin
  Result := TEnumDoublePointBySingleLine.Create(Self, @FPoints[0], FCount);
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

{ TProjectedPolygonLine }

constructor TProjectedPolygonLine.Create(AProjection: IProjectionInfo;
  APoints: PDoublePointArray; ACount: Integer);
begin
  inherited Create(True, AProjection, APoints, ACount);
end;

end.
