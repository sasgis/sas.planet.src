unit u_TimeZoneDiffByLonLatStuped;

interface

uses
  Classes,
  t_GeoTypes,
  i_TimeZoneList,
  i_TimeZoneDiffByLonLat;

type
  ITimeZoneAreaPointCheck = interface(ITimeZoneArea)
    ['{07F85664-36E3-4240-840F-64CF9E9A0FB6}']
    function IsPointFromThis(ALonLat: TDoublePoint): Boolean;
  end;

  TTimeZoneArea = class(TInterfacedObject, ITimeZoneArea, ITimeZoneAreaPointCheck)
  private
    FCount: Integer;
    FPolygon: TArrayOfDoublePoint;
  protected
    function GetCount: Integer;
    procedure GetPoints(APoints: PDoublePointArray);
  protected
    function IsPointFromThis(ALonLat: TDoublePoint): Boolean;
  public
    constructor Create(ASmallIntPolygon: Pointer; ALength: Integer);
  end;


  ITimeZonePointCheck = interface(ITimeZone)
    ['{8C51B27B-1257-4A55-AA03-C8041027A090}']
    function IsPointFromThis(ALonLat: TDoublePoint): Boolean;
  end;

  TTimeZone = class(TInterfacedObject, ITimeZone, ITimeZoneAreaList, ITimeZonePointCheck)
  private
    FDiff: TDateTime;
    FCount: Integer;
    FAreaList: IInterfaceList;
  protected
    function GetDiff: TDateTime;
    function GetAreaList: ITimeZoneAreaList;
  protected
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ITimeZoneArea;
  protected
    function IsPointFromThis(ALonLat: TDoublePoint): Boolean;
  public
    constructor Create(ADiffInHour: Double; AAreaList: IInterfaceList);
  end;

  TTimeZoneDiffByLonLatStuped = class(TInterfacedObject, ITimeZoneList, ITimeZoneDiffByLonLat)
  private
    FTimeZoneList: IInterfaceList;
  protected
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ITimeZone;
  protected
    function GetTimeDiff(ALonLat: TDoublePoint): TDateTime;
  public
    constructor Create;
  end;

implementation

uses
  c_TimeZones;

{ TTimeZoneArea }

constructor TTimeZoneArea.Create(ASmallIntPolygon: Pointer; ALength: Integer);
var
  i: Integer;
begin
  FCount := ALength;
  SetLength(FPolygon, FCount);
  for i := 0 to FCount - 1 do begin
    FPolygon[i].X := SmallInt(Pointer(Integer(ASmallIntPolygon)+SizeOf(SmallInt)*2*i+SizeOf(SmallInt)*0)^)/100;
    FPolygon[i].Y := SmallInt(Pointer(Integer(ASmallIntPolygon)+SizeOf(SmallInt)*2*i+SizeOf(SmallInt)*1)^)/100;
  end;
end;

function TTimeZoneArea.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TTimeZoneArea.GetPoints(APoints: PDoublePointArray);
begin
  Move(FPolygon[0], APoints^, FCount * SizeOf(TDoublePoint));
end;

function TTimeZoneArea.IsPointFromThis(ALonLat: TDoublePoint): Boolean;
var
  i: Integer;
  VPi, Vpj: TDoublePoint;
begin
  Result := False;
  for i:=0 to FCount - 1 do begin
    if i=0 then begin
      Vpj := FPolygon[FCount - 1];
    end else begin
      Vpj := VPi;
    end;
    VPi := FPolygon[i];
    if ((((VPi.Y<=ALonLat.y)and(ALonLat.y<VPj.Y))or((VPj.Y<=ALonLat.y)and(ALonLat.y<VPi.Y)))and
      (ALonLat.x>(VPj.X-VPi.X)*(ALonLat.y-VPi.Y)/(VPj.Y-VPi.Y)+VPi.X))
    then begin
      Result := not(Result);
    end;
  end;
end;

{ TTimeZone }

constructor TTimeZone.Create(ADiffInHour: Double; AAreaList: IInterfaceList);
begin
  FDiff := ADiffInHour / 24;
  FAreaList := AAreaList;
  FCount := FAreaList.Count;
end;

function TTimeZone.GetAreaList: ITimeZoneAreaList;
begin
  Result := Self;
end;

function TTimeZone.GetCount: Integer;
begin
  Result := FCount;
end;

function TTimeZone.GetDiff: TDateTime;
begin
  Result := FDiff;
end;

function TTimeZone.GetItem(AIndex: Integer): ITimeZoneArea;
begin
  Result := ITimeZoneArea(FAreaList.Items[AIndex]);
end;

function TTimeZone.IsPointFromThis(ALonLat: TDoublePoint): Boolean;
var
  i: Integer;
  VArea: ITimeZoneAreaPointCheck;
begin
  Result := False;
  for i := 0 to FAreaList.Count - 1 do begin
    VArea := ITimeZoneAreaPointCheck(FAreaList.Items[i]);
    if VArea.IsPointFromThis(ALonLat) then begin
      Result := True;
      Break;
    end;
  end;
end;

{ TTimeZoneDiffByLonLatStuped }

constructor TTimeZoneDiffByLonLatStuped.Create;
var
  VZone: ITimeZonePointCheck;
  VArea: ITimeZoneAreaPointCheck;
  VAreaList: IInterfaceList;
begin
  FTimeZoneList := TInterfaceList.Create;

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m12, length(timezone_m12));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m12_1, length(timezone_m12_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-12, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m11, length(timezone_m11));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m11_1, length(timezone_m11_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m11_2, length(timezone_m11_2));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-11, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m10, length(timezone_m10));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m10_1, length(timezone_m10_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m10_2, length(timezone_m10_2));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m10_3, length(timezone_m10_3));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-10, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m9d5, length(timezone_m9d5));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-9.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m9, length(timezone_m9));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m9_1, length(timezone_m9_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-9, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m8d5, length(timezone_m8d5));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-8.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m8, length(timezone_m8));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m8_1, length(timezone_m8_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-8, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m7, length(timezone_m7));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-7, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m6, length(timezone_m6));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-6, VAreaList);
  FTimeZoneList.Add(VZone);
  VAreaList := TInterfaceList.Create;

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m5, length(timezone_m5));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m3d5, length(timezone_m3d5));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-3.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m3, length(timezone_m3));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m3_1, length(timezone_m3_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-3, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m4, length(timezone_m4));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m4_1, length(timezone_m4_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-4, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m2, length(timezone_m2));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m2_1, length(timezone_m2_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-2, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_m1, length(timezone_m1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m1_1, length(timezone_m1_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_m1_2, length(timezone_m1_2));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(-1, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_0, length(timezone_0));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_0_1, length(timezone_0_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_0_2, length(timezone_0_2));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(0, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_1, length(timezone_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(1, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_2, length(timezone_2));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_2_1, length(timezone_2_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(2, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_3, length(timezone_3));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_3_1, length(timezone_3_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(3, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_3d5, length(timezone_3d5));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(3.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_4, length(timezone_4));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_4_1, length(timezone_4_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_4_2, length(timezone_4_2));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_4_3, length(timezone_4_3));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_4_4, length(timezone_4_4));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(4, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_4d5, length(timezone_4d5));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(4.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_6, length(timezone_6));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_6_1, length(timezone_6_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_6_2, length(timezone_6_2));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_6_3, length(timezone_6_3));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_6_4, length(timezone_6_4));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(6, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_5, length(timezone_5));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_5_1, length(timezone_5_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_5d5, length(timezone_5d5));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_5d5_1, length(timezone_5d5_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_5d5_2, length(timezone_5d5_2));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(5.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_5d75, length(timezone_5d75));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(5.75, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_6d5, length(timezone_6d5));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_6d5_1, length(timezone_6d5_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(6.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_7, length(timezone_7));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_7_1, length(timezone_7_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(7, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_8, length(timezone_8));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_8_1, length(timezone_8_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(8, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_9, length(timezone_9));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_9_1, length(timezone_9_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_9_2, length(timezone_9_2));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(9, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_9d5, length(timezone_9d5));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_9d5_1, length(timezone_9d5_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(9.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_10, length(timezone_10));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_10_1, length(timezone_10_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_10_2, length(timezone_10_2));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_10_3, length(timezone_10_3));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_10_4, length(timezone_10_4));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_10_5, length(timezone_10_5));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(10, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_10d5, length(timezone_10d5));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_10d5_1, length(timezone_10d5_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(10.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_11, length(timezone_11));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_11_1, length(timezone_11_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_11_2, length(timezone_11_2));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_11_3, length(timezone_11_3));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(11, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_11d5, length(timezone_11d5));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_11d5_1, length(timezone_11d5_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(11.5, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_12, length(timezone_12));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_12_1, length(timezone_12_1));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_12_2, length(timezone_12_2));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_12_3, length(timezone_12_3));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_12_4, length(timezone_12_4));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_12_5, length(timezone_12_5));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_12_6, length(timezone_12_6));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(12, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_12d75, length(timezone_12d75));
  VAreaList.Add(VArea);
  VArea := TTimeZoneArea.Create(@timezone_12d75_1, length(timezone_12d75_1));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(12.75, VAreaList);
  FTimeZoneList.Add(VZone);

  VAreaList := TInterfaceList.Create;
  VArea := TTimeZoneArea.Create(@timezone_13, length(timezone_13));
  VAreaList.Add(VArea);
  VZone := TTimeZone.Create(13, VAreaList);
  FTimeZoneList.Add(VZone);
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
  ALonLat: TDoublePoint): TDateTime;
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
