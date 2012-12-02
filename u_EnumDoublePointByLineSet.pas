unit u_EnumDoublePointByLineSet;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_VectorItemLocal,
  u_BaseInterfacedObject;

type
  TEnumDoublePointByLineSetBase = class(TBaseInterfacedObject, IEnumDoublePoint)
  private
    FSourceLineSet: IInterface;
    FClosed: Boolean;
    FCurrentEnum: IEnumDoublePoint;
    FCount: Integer;
    FIndex: Integer;
    FNeedEmptyPoint: Boolean;
    FFinished: Boolean;
    FPreparedPointExists: Boolean;
    FPreparedPoint: TDoublePoint;
    function GetNextEnum: IEnumDoublePoint; virtual; abstract;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  private
    constructor CreateInternal(
      const ALineSet: IInterface;
      ALineCount: Integer;
      AClosed: Boolean
    );
  end;

  TEnumLonLatPointByPath = class(TEnumDoublePointByLineSetBase, IEnumLonLatPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: ILonLatPath);
  end;

  TEnumLonLatPointByPolygon = class(TEnumDoublePointByLineSetBase, IEnumLonLatPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: ILonLatPolygon);
  end;

  TEnumProjectedPointByPath = class(TEnumDoublePointByLineSetBase, IEnumProjectedPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: IProjectedPath);
  end;

  TEnumProjectedPointByPolygon = class(TEnumDoublePointByLineSetBase, IEnumProjectedPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: IProjectedPolygon);
  end;

  TEnumLocalPointByPath = class(TEnumDoublePointByLineSetBase, IEnumLocalPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: ILocalPath);
  end;

  TEnumLocalPointByPolygon = class(TEnumDoublePointByLineSetBase, IEnumLocalPoint)
  private
    function GetNextEnum: IEnumDoublePoint; override;
  public
    constructor Create(const ALineSet: ILocalPolygon);
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointByLineSetBase }

constructor TEnumDoublePointByLineSetBase.CreateInternal(
  const ALineSet: IInterface;
  ALineCount: Integer;
  AClosed: Boolean
);
begin
  inherited Create;
  FSourceLineSet := ALineSet;
  FClosed := AClosed;
  FCurrentEnum := nil;
  FCount := ALineCount;
  FIndex := -1;
  FNeedEmptyPoint := False;
  FFinished := False;
  FPreparedPointExists := False;
end;

function TEnumDoublePointByLineSetBase.Next(out APoint: TDoublePoint): Boolean;
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

{ TEnumLonLatPointByPath }

constructor TEnumLonLatPointByPath.Create(const ALineSet: ILonLatPath);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, False);
end;

function TEnumLonLatPointByPath.GetNextEnum: IEnumDoublePoint;
begin
  Result := ILonLatPath(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumLonLatPointByPolygon }

constructor TEnumLonLatPointByPolygon.Create(const ALineSet: ILonLatPolygon);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, True);
end;

function TEnumLonLatPointByPolygon.GetNextEnum: IEnumDoublePoint;
begin
  Result := ILonLatPolygon(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumProjectedPointByPath }

constructor TEnumProjectedPointByPath.Create(const ALineSet: IProjectedPath);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, False);
end;

function TEnumProjectedPointByPath.GetNextEnum: IEnumDoublePoint;
begin
  Result := IProjectedPath(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumProjectedPointByPolygon }

constructor TEnumProjectedPointByPolygon.Create(const ALineSet: IProjectedPolygon);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, True);
end;

function TEnumProjectedPointByPolygon.GetNextEnum: IEnumDoublePoint;
begin
  Result := IProjectedPolygon(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumLocalPointByPath }

constructor TEnumLocalPointByPath.Create(const ALineSet: ILocalPath);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, False);
end;

function TEnumLocalPointByPath.GetNextEnum: IEnumDoublePoint;
begin
  Result := ILocalPath(FSourceLineSet).Item[FIndex].GetEnum;
end;

{ TEnumLocalPointByPolygon }

constructor TEnumLocalPointByPolygon.Create(const ALineSet: ILocalPolygon);
begin
  inherited CreateInternal(ALineSet, ALineSet.Count, True);
end;

function TEnumLocalPointByPolygon.GetNextEnum: IEnumDoublePoint;
begin
  Result := ILocalPolygon(FSourceLineSet).Item[FIndex].GetEnum;
end;

end.
