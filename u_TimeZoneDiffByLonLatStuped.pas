unit u_TimeZoneDiffByLonLatStuped;

interface

uses
  Classes,
  t_GeoTypes,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_DoublePointsAggregator,
  i_TimeZoneList,
  i_TimeZoneDiffByLonLat;

type
  PSmallIntPoint = ^TSmallIntPoint;

  TSmallIntPoint = packed record
    X: Smallint;
    Y: Smallint;
  end;

  ITimeZonePointCheck = interface(ITimeZone)
    ['{8C51B27B-1257-4A55-AA03-C8041027A090}']
    function IsPointFromThis(const ALonLat: TDoublePoint): Boolean;
  end;

  TTimeZone = class(TInterfacedObject, ITimeZone, ITimeZonePointCheck)
  private
    FDiff: TDateTime;
    FPolygon: ILonLatPolygon;
    function IsPointFromPolygonLine(
      const ALine: ILonLatPolygonLine;
      const ALonLat: TDoublePoint
    ): Boolean;
  protected
    function GetDiff: TDateTime;
    function GetPolygon: ILonLatPolygon;
  protected
    function IsPointFromThis(const ALonLat: TDoublePoint): Boolean;
  public
    constructor Create(
      const ADiffInHour: Double;
      const APolygon: ILonLatPolygon
    );
  end;

  TTimeZoneDiffByLonLatStuped = class(TInterfacedObject, ITimeZoneList, ITimeZoneDiffByLonLat)
  private
    FTimeZoneList: IInterfaceList;
    procedure AddFromSmallIntArray(
      const AAggregator: IDoublePointsAggregator;
      ASmallIntPolygon: PSmallIntPoint;
      ALength: Integer
    );
  protected
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ITimeZone;
  protected
    function GetTimeDiff(const ALonLat: TDoublePoint): TDateTime;
  public
    constructor Create(const AVectorFactory: IVectorItmesFactory);
  end;

implementation

uses
  c_TimeZones,
  i_EnumDoublePoint,
  u_GeoFun,
  u_DoublePointsAggregator;

{ TTimeZone }

constructor TTimeZone.Create(
  const ADiffInHour: Double;
  const APolygon: ILonLatPolygon
);
begin
  inherited Create;
  FDiff := ADiffInHour / 24;
  FPolygon := APolygon;
end;

function TTimeZone.GetDiff: TDateTime;
begin
  Result := FDiff;
end;

function TTimeZone.GetPolygon: ILonLatPolygon;
begin
  Result := FPolygon;
end;

function TTimeZone.IsPointFromPolygonLine(
  const ALine: ILonLatPolygonLine;
  const ALonLat: TDoublePoint
): Boolean;
var
  VEnum: IEnumDoublePoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  result := false;
  if ALine.Bounds.IsPointInRect(ALonLat) then begin
    VEnum := ALine.GetEnum;
    if VEnum.Next(VPrevPoint) then begin
      while VEnum.Next(VCurrPoint) do begin
        if (((VCurrPoint.y <= ALonLat.y) and (ALonLat.y < VPrevPoint.y)) or
          ((VPrevPoint.y <= ALonLat.y) and (ALonLat.y < VCurrPoint.y))) and
          (ALonLat.x > (VPrevPoint.x - VCurrPoint.x) * (ALonLat.y - VCurrPoint.y) / (VPrevPoint.y - VCurrPoint.y) + VCurrPoint.x) then begin
          Result := not (Result);
        end;
        VPrevPoint := VCurrPoint;
      end;
    end;
  end;
end;

function TTimeZone.IsPointFromThis(const ALonLat: TDoublePoint): Boolean;
var
  i: Integer;
  VArea: ILonLatPolygonLine;
begin
  Result := False;
  if FPolygon.Bounds.IsPointInRect(ALonLat) then begin
    for i := 0 to FPolygon.Count - 1 do begin
      VArea := FPolygon.Item[i];
      if IsPointFromPolygonLine(VArea, ALonLat) then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TTimeZoneDiffByLonLatStuped }

constructor TTimeZoneDiffByLonLatStuped.Create(const AVectorFactory: IVectorItmesFactory);
var
  VZone: ITimeZonePointCheck;
  VAggregator: IDoublePointsAggregator;
  VPolygon: ILonLatPolygon;
begin
  inherited Create;
  FTimeZoneList := TInterfaceList.Create;
  VAggregator := TDoublePointsAggregator.Create;

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m12, length(timezone_m12));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m12_1, length(timezone_m12_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-12, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m11, length(timezone_m11));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m11_1, length(timezone_m11_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m11_2, length(timezone_m11_2));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-11, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m10, length(timezone_m10));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m10_1, length(timezone_m10_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m10_2, length(timezone_m10_2));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m10_3, length(timezone_m10_3));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-10, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m9d5, length(timezone_m9d5));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-9.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m9, length(timezone_m9));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m9_1, length(timezone_m9_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-9, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m8d5, length(timezone_m8d5));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-8.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m8, length(timezone_m8));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m8_1, length(timezone_m8_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-8, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m7, length(timezone_m7));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-7, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m6, length(timezone_m6));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-6, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m5, length(timezone_m5));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m3d5, length(timezone_m3d5));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-3.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m3, length(timezone_m3));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m3_1, length(timezone_m3_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-3, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m4, length(timezone_m4));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m4_1, length(timezone_m4_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-4, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m2, length(timezone_m2));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m2_1, length(timezone_m2_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-2, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_m1, length(timezone_m1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m1_1, length(timezone_m1_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_m1_2, length(timezone_m1_2));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(-1, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_0, length(timezone_0));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_0_1, length(timezone_0_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_0_2, length(timezone_0_2));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(0, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_1, length(timezone_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(1, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_2, length(timezone_2));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_2_1, length(timezone_2_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(2, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_3, length(timezone_3));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_3_1, length(timezone_3_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(3, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_3d5, length(timezone_3d5));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(3.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_4, length(timezone_4));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_4_1, length(timezone_4_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_4_2, length(timezone_4_2));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_4_3, length(timezone_4_3));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_4_4, length(timezone_4_4));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(4, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_4d5, length(timezone_4d5));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(4.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_6, length(timezone_6));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_6_1, length(timezone_6_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_6_2, length(timezone_6_2));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_6_3, length(timezone_6_3));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_6_4, length(timezone_6_4));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(6, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_5, length(timezone_5));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_5_1, length(timezone_5_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_5d5, length(timezone_5d5));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_5d5_1, length(timezone_5d5_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_5d5_2, length(timezone_5d5_2));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(5.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_5d75, length(timezone_5d75));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(5.75, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_6d5, length(timezone_6d5));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_6d5_1, length(timezone_6d5_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(6.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_7, length(timezone_7));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_7_1, length(timezone_7_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(7, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_8, length(timezone_8));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_8_1, length(timezone_8_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(8, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_9, length(timezone_9));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_9_1, length(timezone_9_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_9_2, length(timezone_9_2));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(9, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_9d5, length(timezone_9d5));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_9d5_1, length(timezone_9d5_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(9.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_10, length(timezone_10));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_10_1, length(timezone_10_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_10_2, length(timezone_10_2));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_10_3, length(timezone_10_3));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_10_4, length(timezone_10_4));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_10_5, length(timezone_10_5));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(10, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_10d5, length(timezone_10d5));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_10d5_1, length(timezone_10d5_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(10.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_11, length(timezone_11));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_11_1, length(timezone_11_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_11_2, length(timezone_11_2));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_11_3, length(timezone_11_3));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(11, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_11d5, length(timezone_11d5));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_11d5_1, length(timezone_11d5_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(11.5, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_12, length(timezone_12));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_12_1, length(timezone_12_1));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_12_2, length(timezone_12_2));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_12_3, length(timezone_12_3));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_12_4, length(timezone_12_4));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_12_5, length(timezone_12_5));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_12_6, length(timezone_12_6));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(12, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_12d75, length(timezone_12d75));
  VAggregator.Add(CEmptyDoublePoint);
  AddFromSmallIntArray(VAggregator, @timezone_12d75_1, length(timezone_12d75_1));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(12.75, VPolygon);
  FTimeZoneList.Add(VZone);

  VAggregator.Clear;
  AddFromSmallIntArray(VAggregator, @timezone_13, length(timezone_13));
  VPolygon := AVectorFactory.CreateLonLatPolygon(VAggregator.Points, VAggregator.Count);
  VZone := TTimeZone.Create(13, VPolygon);
  FTimeZoneList.Add(VZone);
end;

procedure TTimeZoneDiffByLonLatStuped.AddFromSmallIntArray(
  const AAggregator: IDoublePointsAggregator;
  ASmallIntPolygon: PSmallIntPoint;
  ALength: Integer
);
var
  i: Integer;
  VSmallIntPoint: PSmallIntPoint;
  VPoint: TDoublePoint;
begin
  VSmallIntPoint := ASmallIntPolygon;
  for i := 0 to ALength - 1 do begin
    VPoint.X := VSmallIntPoint.X / 100;
    VPoint.Y := VSmallIntPoint.Y / 100;
    AAggregator.Add(VPoint);
    Inc(VSmallIntPoint);
  end;
end;

function TTimeZoneDiffByLonLatStuped.GetCount: Integer;
begin
  Result := FTimeZoneList.Count;
end;

function TTimeZoneDiffByLonLatStuped.GetItem(AIndex: Integer): ITimeZone;
begin
  Result := ITimeZone(FTimeZoneList.Items[AIndex]);
end;

function TTimeZoneDiffByLonLatStuped.GetTimeDiff(
  const ALonLat: TDoublePoint): TDateTime;
var
  i: Integer;
  VZone: ITimeZonePointCheck;
begin
  Result := 0;
  for i := 0 to FTimeZoneList.Count - 1 do begin
    VZone := ITimeZonePointCheck(FTimeZoneList.Items[i]);
    if VZone.IsPointFromThis(ALonLat) then begin
      Result := VZone.Diff;
      Break;
    end;
  end;
end;

end.
