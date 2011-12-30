unit u_EnumDoublePointByLineSet;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_VectorItemLonLat,
  i_VectorItemProjected;

type
  TEnumDoublePointByLineSet = class(TInterfacedObject, IEnumDoublePoint)
  private
    FSourceLineSet: IInterface;
    FClosed: Boolean;
    FProjected: Boolean;
    FCurrentEnum: IEnumDoublePoint;
    FCount: Integer;
    FIndex: Integer;
    FNeedEmptyPoint: Boolean;
    FFinished: Boolean;
    FPreparedPointExists: Boolean;
    FPreparedPoint: TDoublePoint;
    function GetNextEnum: IEnumDoublePoint;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(ALineSet: ILonLatPath); overload;
    constructor Create(ALineSet: ILonLatPolygon); overload;
    constructor Create(ALineSet: IProjectedPath); overload;
    constructor Create(ALineSet: IProjectedPolygon); overload;
  end;
implementation

uses
  u_GeoFun;

{ TEnumDoublePointBySingleLine }

constructor TEnumDoublePointByLineSet.Create(ALineSet: ILonLatPolygon);
begin
  FSourceLineSet := ALineSet;
  FClosed := True;
  FProjected := False;
  FCurrentEnum := nil;
  FCount := ALineSet.Count;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

constructor TEnumDoublePointByLineSet.Create(ALineSet: ILonLatPath);
begin
  FSourceLineSet := ALineSet;
  FClosed := False;
  FProjected := False;
  FCurrentEnum := nil;
  FCount := ALineSet.Count;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

constructor TEnumDoublePointByLineSet.Create(ALineSet: IProjectedPolygon);
begin
  FSourceLineSet := ALineSet;
  FClosed := True;
  FProjected := True;
  FCurrentEnum := nil;
  FCount := ALineSet.Count;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

constructor TEnumDoublePointByLineSet.Create(ALineSet: IProjectedPath);
begin
  FSourceLineSet := ALineSet;
  FClosed := False;
  FProjected := True;
  FCurrentEnum := nil;
  FCount := ALineSet.Count;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

function TEnumDoublePointByLineSet.GetNextEnum: IEnumDoublePoint;
begin
  Result := nil;
  if FClosed then begin
    if FProjected then begin
      Result := IProjectedPolygon(FSourceLineSet).Item[fIndex].GetEnum;
    end else begin
      Result := ILonLatPolygon(FSourceLineSet).Item[fIndex].GetEnum;
    end;
  end else begin
    if FProjected then begin
      Result := IProjectedPath(FSourceLineSet).Item[fIndex].GetEnum;
    end else begin
      Result := ILonLatPath(FSourceLineSet).Item[fIndex].GetEnum;
    end;
  end;
end;

function TEnumDoublePointByLineSet.Next(out APoint: TDoublePoint): Boolean;
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
