unit u_EnumDoublePointByLineSet;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_VectorItemLonLat,
  i_VectorItemProjected;

type
  TEnumLonLatPointByLineSet = class(TInterfacedObject, IEnumDoublePoint, IEnumLonLatPoint)
  private
    FSourceLineSet: IInterface;
    FClosed: Boolean;
    FCurrentEnum: IEnumLonLatPoint;
    FCount: Integer;
    FIndex: Integer;
    FNeedEmptyPoint: Boolean;
    FFinished: Boolean;
    FPreparedPointExists: Boolean;
    FPreparedPoint: TDoublePoint;
    function GetNextEnum: IEnumLonLatPoint;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(ALineSet: ILonLatPath); overload;
    constructor Create(ALineSet: ILonLatPolygon); overload;
  end;

  TEnumProjectedPointByLineSet = class(TInterfacedObject, IEnumDoublePoint, IEnumProjectedPoint)
  private
    FSourceLineSet: IInterface;
    FClosed: Boolean;
    FCurrentEnum: IEnumProjectedPoint;
    FCount: Integer;
    FIndex: Integer;
    FNeedEmptyPoint: Boolean;
    FFinished: Boolean;
    FPreparedPointExists: Boolean;
    FPreparedPoint: TDoublePoint;
    function GetNextEnum: IEnumProjectedPoint;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(ALineSet: IProjectedPath); overload;
    constructor Create(ALineSet: IProjectedPolygon); overload;
  end;

implementation

uses
  u_GeoFun;

{ TEnumLonLatPointByLineSet }

constructor TEnumLonLatPointByLineSet.Create(ALineSet: ILonLatPolygon);
begin
  FSourceLineSet := ALineSet;
  FClosed := True;
  FCurrentEnum := nil;
  FCount := ALineSet.Count;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

constructor TEnumLonLatPointByLineSet.Create(ALineSet: ILonLatPath);
begin
  FSourceLineSet := ALineSet;
  FClosed := False;
  FCurrentEnum := nil;
  FCount := ALineSet.Count;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

function TEnumLonLatPointByLineSet.GetNextEnum: IEnumLonLatPoint;
begin
  Result := nil;
  if FClosed then begin
    Result := ILonLatPolygon(FSourceLineSet).Item[fIndex].GetEnum;
  end else begin
    Result := ILonLatPath(FSourceLineSet).Item[fIndex].GetEnum;
  end;
end;

function TEnumLonLatPointByLineSet.Next(out APoint: TDoublePoint): Boolean;
begin
  while not FFinished do begin
    if FCurrentEnum <> nil then begin
      if FPreparedPointExists then begin
        APoint := FPreparedPoint;
        FPreparedPointExists := False;
        FNeedEmptyPoint := True;
        Break;
      end;
      if FCurrentEnum.Next(APoint) then begin
        FNeedEmptyPoint := True;
        Break;
      end else begin
        FCurrentEnum := nil;
      end;
    end else begin
      Inc(FIndex);
      if FIndex < FCount then begin
        FCurrentEnum := GetNextEnum;
        if FCurrentEnum <> nil then begin
          if FNeedEmptyPoint then begin
            if FCurrentEnum.Next(FPreparedPoint) then begin
              FPreparedPointExists := True;
              FNeedEmptyPoint := False;
              APoint := CEmptyDoublePoint;
              Break;
            end;
          end;
        end;
      end else begin
        FFinished := True;
        FSourceLineSet := nil;
      end;
    end;
  end;
  Result := not FFinished;
end;

{ TEnumProjectedPointByLineSet }

constructor TEnumProjectedPointByLineSet.Create(ALineSet: IProjectedPolygon);
begin
  FSourceLineSet := ALineSet;
  FClosed := True;
  FCurrentEnum := nil;
  FCount := ALineSet.Count;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

constructor TEnumProjectedPointByLineSet.Create(ALineSet: IProjectedPath);
begin
  FSourceLineSet := ALineSet;
  FClosed := False;
  FCurrentEnum := nil;
  FCount := ALineSet.Count;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

function TEnumProjectedPointByLineSet.GetNextEnum: IEnumProjectedPoint;
begin
  Result := nil;
  if FClosed then begin
    Result := IProjectedPolygon(FSourceLineSet).Item[fIndex].GetEnum;
  end else begin
    Result := IProjectedPath(FSourceLineSet).Item[fIndex].GetEnum;
  end;
end;

function TEnumProjectedPointByLineSet.Next(out APoint: TDoublePoint): Boolean;
begin
  while not FFinished do begin
    if FCurrentEnum <> nil then begin
      if FPreparedPointExists then begin
        APoint := FPreparedPoint;
        FPreparedPointExists := False;
        FNeedEmptyPoint := True;
        Break;
      end;
      if FCurrentEnum.Next(APoint) then begin
        FNeedEmptyPoint := True;
        Break;
      end else begin
        FCurrentEnum := nil;
      end;
    end else begin
      Inc(FIndex);
      if FIndex < FCount then begin
        FCurrentEnum := GetNextEnum;
        if FCurrentEnum <> nil then begin
          if FNeedEmptyPoint then begin
            if FCurrentEnum.Next(FPreparedPoint) then begin
              FPreparedPointExists := True;
              FNeedEmptyPoint := False;
              APoint := CEmptyDoublePoint;
              Break;
            end;
          end;
        end;
      end else begin
        FFinished := True;
        FSourceLineSet := nil;
      end;
    end;
  end;
  Result := not FFinished;
end;

end.
