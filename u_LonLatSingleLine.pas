unit u_LonLatSingleLine;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_Datum,
  i_VectorItemLonLat;

type
  TLonLatLineBase = class(TInterfacedObject)
  private
    FCount: Integer;
    FBounds: TDoubleRect;
    FPoints: array of TDoublePoint;
  private
    function GetBounds: TDoubleRect;
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create(
      AClosed: Boolean;
      const ABounds: TDoubleRect;
      APoints: PDoublePointArray;
      ACount: Integer
    ); overload;
    constructor Create(
      AClosed: Boolean;
      APoints: PDoublePointArray;
      ACount: Integer
    ); overload;
  end;

  TLonLatPathLine = class(TLonLatLineBase, ILonLatPathLine)
  private
    function GetEnum: IEnumLonLatPoint;
    function CalcLength(ADatum: IDatum): Double;
  public
    constructor Create(
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TLonLatPolygonLine = class(TLonLatLineBase, ILonLatPolygonLine)
  private
    function GetEnum: IEnumLonLatPoint;
    function CalcPerimeter(ADatum: IDatum): Double;
    function CalcArea(ADatum: IDatum): Double;
  public
    constructor Create(
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

implementation

uses
  u_GeoFun,
  u_EnumDoublePointBySingleLine;

{ TLineBase }

constructor TLonLatLineBase.Create(
  AClosed: Boolean;
  APoints: PDoublePointArray;
  ACount: Integer
);
var
  VBounds: TDoubleRect;
  i: Integer;
begin
  VBounds.TopLeft := APoints[0];
  VBounds.BottomRight := APoints[0];
  for i := 1 to ACount - 1 do begin
    if VBounds.Left > APoints[i].X then begin
      VBounds.Left := APoints[i].X;
    end;
    if VBounds.Top < APoints[i].Y then begin
      VBounds.Top := APoints[i].Y;
    end;
    if VBounds.Right < APoints[i].X then begin
      VBounds.Right := APoints[i].X;
    end;
    if VBounds.Bottom > APoints[i].Y then begin
      VBounds.Bottom := APoints[i].Y;
    end;
  end;
  Create(AClosed, VBounds, APoints, ACount);
end;

constructor TLonLatLineBase.Create(
  AClosed: Boolean;
  const ABounds: TDoubleRect;
  APoints: PDoublePointArray;
  ACount: Integer
);
begin
  FBounds := ABounds;
  FCount := ACount;
  Assert(FCount > 0, 'Empty line');

  if AClosed and (FCount > 1) and DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Dec(FCount);
  end;

  SetLength(FPoints, FCount);
  Move(APoints^, FPoints[0], FCount * SizeOf(TDoublePoint));
end;

function TLonLatLineBase.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TLonLatLineBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TLonLatLineBase.GetPoints: PDoublePointArray;
begin
  Result := @FPoints[0];
end;

{ TLonLatPathLine }

function TLonLatPathLine.CalcLength(ADatum: IDatum): Double;
var
  VEnum: IEnumLonLatPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + ADatum.CalcDist(VPrevPoint, VCurrPoint);
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

constructor TLonLatPathLine.Create(APoints: PDoublePointArray; ACount: Integer);
begin
  inherited Create(False, APoints, ACount);
end;

function TLonLatPathLine.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumDoublePointBySingleLonLatLine.Create(Self, False, @FPoints[0], FCount);
end;

{ TLonLatPolygonLine }

function TLonLatPolygonLine.CalcArea(ADatum: IDatum): Double;
begin
  if FCount < 3 then begin
    Result := 0;
  end else begin
    Result := ADatum.CalcPoligonArea(@FPoints[0], FCount);
  end;
end;

function TLonLatPolygonLine.CalcPerimeter(ADatum: IDatum): Double;
var
  VEnum: IEnumLonLatPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + ADatum.CalcDist(VPrevPoint, VCurrPoint);
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

constructor TLonLatPolygonLine.Create(APoints: PDoublePointArray;
  ACount: Integer);
begin
  inherited Create(True, APoints, ACount);
end;

function TLonLatPolygonLine.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumDoublePointBySingleLonLatLine.Create(Self, True, @FPoints[0], FCount);
end;

end.
