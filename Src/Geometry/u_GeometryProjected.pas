unit u_GeometryProjected;

interface

uses
  t_GeoTypes,
  i_DoublePoints,
  i_EnumDoublePoint,
  i_InterfaceListStatic,
  i_GeometryProjected,
  u_BaseInterfacedObject;

type
  TGeometryProjectedBase = class(TBaseInterfacedObject, IGeometryProjected)
  private
    FCount: Integer;
    FBounds: TDoubleRect;
    FPoints: IDoublePoints;
  private
    function GetBounds: TDoubleRect;
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create(
      AClosed: Boolean;
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    ); overload;
  end;

  TGeometryProjectedMultiPoint = class(TGeometryProjectedBase, IGeometryProjectedMultiPoint)
  private
    function GetEnum: IEnumProjectedPoint;
  public
    constructor Create(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );
  end;

  TGeometryProjectedLine = class(TGeometryProjectedBase, IGeometryProjectedLine, IGeometryProjectedSingleLine)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
  public
    constructor Create(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );
  end;

  TGeometryProjectedContour = class(TGeometryProjectedBase, IGeometryProjectedPolygon, IGeometryProjectedContour)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;
  public
    constructor Create(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );
  end;

  TGeometryProjectedPolygon = class(TGeometryProjectedContour, IGeometryProjectedSinglePolygon)
  private
    function GetOuterBorder: IGeometryProjectedContour;
    function GetHoleCount: Integer;
    function GetHoleBorder(const AIndex: Integer): IGeometryProjectedContour;
  public
    constructor Create(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );
  end;

  TGeometryProjectedPolygonWithHoles = class(TBaseInterfacedObject, IGeometryProjected, IGeometryProjectedPolygon, IGeometryProjectedSinglePolygon)
  private
    FBounds: TDoubleRect;
    FOuterBorder: IGeometryProjectedContour;
    FHoleList: IInterfaceListStatic;
  private
    function GetBounds: TDoubleRect;

    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;

    function GetOuterBorder: IGeometryProjectedContour;
    function GetHoleCount: Integer;
    function GetHoleBorder(const AIndex: Integer): IGeometryProjectedContour;
  public
    constructor Create(
      const ABounds: TDoubleRect;
      const AOuterBorder: IGeometryProjectedContour;
      const AHoleList: IInterfaceListStatic
    );
  end;

implementation

uses
  Math,
  u_GeoFunc,
  u_EnumDoublePointBySingleLine;

{ TGeometryProjectedBase }

constructor TGeometryProjectedBase.Create(
  AClosed: Boolean;
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
begin
  Assert(Assigned(APoints));
  Assert(APoints.Count > 0, 'Empty line');
  inherited Create;
  FPoints := APoints;
  FBounds := ABounds;
  FCount := FPoints.Count;
  if AClosed and (FCount > 1) and DoublePointsEqual(FPoints.Points[0], FPoints.Points[FCount - 1]) then begin
    Dec(FCount);
  end;
end;

function TGeometryProjectedBase.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TGeometryProjectedBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TGeometryProjectedBase.GetPoints: PDoublePointArray;
begin
  Result := FPoints.Points;
end;

{ TGeometryProjectedMultiPoint }

constructor TGeometryProjectedMultiPoint.Create(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
begin
  inherited Create(False, ABounds, APoints);
end;

function TGeometryProjectedMultiPoint.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumDoublePointBySingleProjectedLine.Create(FPoints, False, FPoints.Points, FCount);
end;

{ TGeometryProjectedLine }

constructor TGeometryProjectedLine.Create(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
begin
  inherited Create(False, ABounds, APoints);
end;

function TGeometryProjectedLine.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumDoublePointBySingleProjectedLine.Create(FPoints, False, FPoints.Points, FCount);
end;

function TGeometryProjectedLine.IsPointOnPath(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VVectorW: TDoublePoint;
  VVectorV: TDoublePoint;
  C1: Double;
  C2: Double;
  B: Double;
  VVectorDist: TDoublePoint;
  VDistSQR: Double;
  VEnum: IEnumProjectedPoint;
begin
  Result := False;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    VDistSQR := ADist * ADist;
    while VEnum.Next(VCurrPoint) do begin
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
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TGeometryProjectedLine.IsRectIntersectPath(
  const ARect: TDoubleRect): Boolean;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VIntersect: Double;
  VDelta: TDoublePoint;
begin
  Result := False;
  if IsIntersecProjectedRect(FBounds, ARect) then begin
    VEnum := GetEnum;
    // »щем есть ли пересечени€ пр€моугольника с линией
    if VEnum.Next(VPrevPoint) then begin
      while VEnum.Next(VCurrPoint) do begin
        VDelta.X := VCurrPoint.X - VPrevPoint.X;
        VDelta.Y := VCurrPoint.Y - VPrevPoint.Y;
        if (VDelta.Y < 0) then begin
          if (VCurrPoint.Y <= ARect.Top) and (ARect.Top < VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.Y <= ARect.Bottom) and (ARect.Bottom < VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
        end else if (VDelta.Y > 0) then begin
          if (VCurrPoint.Y > ARect.Top) and (ARect.Top >= VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.Y > ARect.Bottom) and (ARect.Bottom >= VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
        end;

        if (VDelta.X < 0) then begin
          if (VCurrPoint.X <= ARect.Left) and (ARect.Left < VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.X <= ARect.Right) and (ARect.Right < VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
        end else if (VDelta.X > 0) then begin
          if (VCurrPoint.X > ARect.Left) and (ARect.Left >= VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.X > ARect.Right) and (ARect.Right >= VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
        end;
        VPrevPoint := VCurrPoint;
      end;
    end;
  end;
end;

{ TGeometryProjectedContour }

constructor TGeometryProjectedContour.Create(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
begin
  inherited Create(True, ABounds, APoints);
end;

function TGeometryProjectedContour.CalcArea: Double;
var
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

function TGeometryProjectedContour.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumDoublePointBySingleProjectedLine.Create(FPoints, True, FPoints.Points, FCount);
end;

function TGeometryProjectedContour.IsPointInPolygon(
  const APoint: TDoublePoint): Boolean;
var
  VEnum: IEnumDoublePoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  result := false;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      if (((VCurrPoint.y <= APoint.y) and (APoint.y < VPrevPoint.y)) or
        ((VPrevPoint.y <= APoint.y) and (APoint.y < VCurrPoint.y))) and
        (APoint.x > (VPrevPoint.x - VCurrPoint.x) * (APoint.y - VCurrPoint.y) / (VPrevPoint.y - VCurrPoint.y) + VCurrPoint.x) then begin
        result := not (result);
      end;
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TGeometryProjectedContour.IsPointOnBorder(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VVectorW: TDoublePoint;
  VVectorV: TDoublePoint;
  C1: Double;
  C2: Double;
  B: Double;
  VVectorDist: TDoublePoint;
  VDistSQR: Double;
  VEnum: IEnumProjectedPoint;
begin
  Result := False;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    VDistSQR := ADist * ADist;
    while VEnum.Next(VCurrPoint) do begin
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
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TGeometryProjectedContour.IsRectIntersectBorder(
  const ARect: TDoubleRect): Boolean;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VIntersect: Double;
  VDelta: TDoublePoint;
begin
  if not IsIntersecProjectedRect(FBounds, ARect) then begin
    Result := False;
  end else begin
    Result := False;
    VEnum := GetEnum;
    // »щем есть ли пересечени€ пр€моугольника с полигоном
    if VEnum.Next(VPrevPoint) then begin
      while VEnum.Next(VCurrPoint) do begin
        VDelta.X := VCurrPoint.X - VPrevPoint.X;
        VDelta.Y := VCurrPoint.Y - VPrevPoint.Y;
        if (VDelta.Y < 0) then begin
          if (VCurrPoint.Y <= ARect.Top) and (ARect.Top < VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.Y <= ARect.Bottom) and (ARect.Bottom < VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
        end else if (VDelta.Y > 0) then begin
          if (VCurrPoint.Y > ARect.Top) and (ARect.Top >= VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.Y > ARect.Bottom) and (ARect.Bottom >= VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
        end;

        if (VDelta.X < 0) then begin
          if (VCurrPoint.X <= ARect.Left) and (ARect.Left < VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.X <= ARect.Right) and (ARect.Right < VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
        end else if (VDelta.X > 0) then begin
          if (VCurrPoint.X > ARect.Left) and (ARect.Left >= VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.X > ARect.Right) and (ARect.Right >= VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
        end;

        VPrevPoint := VCurrPoint;
      end;
    end;
  end;
end;

function TGeometryProjectedContour.IsRectIntersectPolygon(
  const ARect: TDoubleRect
): Boolean;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VIntersect: Double;
  VDelta: TDoublePoint;
  VRectIn: Boolean;
begin
  if not IsIntersecProjectedRect(FBounds, ARect) then begin
    Result := False;
  end else begin
    if PixelPointInRect(FPoints.Points[0], ARect) then begin
      Result := True;
    end else begin
      VRectIn := False;
      Result := False;
      VEnum := GetEnum;
      // »щем есть ли пересечени€ пр€моугольника с полигоном,
      // и заодно провер€ем попадает ли левый верхний угол в полигон
      if VEnum.Next(VPrevPoint) then begin
        while VEnum.Next(VCurrPoint) do begin
          VDelta.X := VCurrPoint.X - VPrevPoint.X;
          VDelta.Y := VCurrPoint.Y - VPrevPoint.Y;
          if (VDelta.Y < 0) then begin
            if (VCurrPoint.Y <= ARect.Top) and (ARect.Top < VPrevPoint.Y) then begin
              VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
              if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
                Result := True;
                Exit;
              end;
              if (ARect.Left > VIntersect) then begin
                VRectIn := not VRectIn;
              end;
            end;
            if (VCurrPoint.Y <= ARect.Bottom) and (ARect.Bottom < VPrevPoint.Y) then begin
              VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
              if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
                Result := True;
                Exit;
              end;
            end;
          end else if (VDelta.Y > 0) then begin
            if (VCurrPoint.Y > ARect.Top) and (ARect.Top >= VPrevPoint.Y) then begin
              VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
              if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
                Result := True;
                Exit;
              end;
              if (ARect.Left > VIntersect) then begin
                VRectIn := not VRectIn;
              end;
            end;
            if (VCurrPoint.Y > ARect.Bottom) and (ARect.Bottom >= VPrevPoint.Y) then begin
              VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
              if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
                Result := True;
                Exit;
              end;
            end;
          end;

          if (VDelta.X < 0) then begin
            if (VCurrPoint.X <= ARect.Left) and (ARect.Left < VPrevPoint.X) then begin
              VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
              if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
                Result := True;
                Exit;
              end;
            end;
            if (VCurrPoint.X <= ARect.Right) and (ARect.Right < VPrevPoint.X) then begin
              VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
              if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
                Result := True;
                Exit;
              end;
            end;
          end else if (VDelta.X > 0) then begin
            if (VCurrPoint.X > ARect.Left) and (ARect.Left >= VPrevPoint.X) then begin
              VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
              if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
                Result := True;
                Exit;
              end;
            end;
            if (VCurrPoint.X > ARect.Right) and (ARect.Right >= VPrevPoint.X) then begin
              VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
              if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
                Result := True;
                Exit;
              end;
            end;
          end;

          VPrevPoint := VCurrPoint;
        end;
        Result := VRectIn;
      end;
    end;
  end;
end;

{ TGeometryProjectedPolygon }

constructor TGeometryProjectedPolygon.Create(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
begin
  inherited Create(ABounds, APoints);
end;

function TGeometryProjectedPolygon.GetHoleBorder(
  const AIndex: Integer
): IGeometryProjectedContour;
begin
  Result := nil;
  Assert(False);
end;

function TGeometryProjectedPolygon.GetHoleCount: Integer;
begin
  Result := 0;
end;

function TGeometryProjectedPolygon.GetOuterBorder: IGeometryProjectedContour;
begin
  Result := Self;
end;


{ TGeometryProjectedPolygonWithHoles }

constructor TGeometryProjectedPolygonWithHoles.Create(
  const ABounds: TDoubleRect;
  const AOuterBorder: IGeometryProjectedContour;
  const AHoleList: IInterfaceListStatic
);
begin
  Assert(Assigned(AOuterBorder));
  Assert(Assigned(AHoleList));
  Assert(AHoleList.Count > 0);
  FBounds := ABounds;
  FOuterBorder := AOuterBorder;
  FHoleList := AHoleList;
end;

function TGeometryProjectedPolygonWithHoles.CalcArea: Double;
var
  i: Integer;
  VContour: IGeometryProjectedContour;
begin
  Result := FOuterBorder.CalcArea;
  for i := 0 to FHoleList.Count - 1 do begin
    VContour := IGeometryProjectedContour(FHoleList.Items[i]);
    Result := Result - VContour.CalcArea;
  end;
end;

function TGeometryProjectedPolygonWithHoles.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TGeometryProjectedPolygonWithHoles.GetHoleBorder(
  const AIndex: Integer
): IGeometryProjectedContour;
begin
  Result := IGeometryProjectedContour(FHoleList.Items[AIndex]);
end;

function TGeometryProjectedPolygonWithHoles.GetHoleCount: Integer;
begin
  Result := FHoleList.Count;
end;

function TGeometryProjectedPolygonWithHoles.GetOuterBorder: IGeometryProjectedContour;
begin
  Result := FOuterBorder;
end;

function TGeometryProjectedPolygonWithHoles.IsPointInPolygon(
  const APoint: TDoublePoint
): Boolean;
var
  i: Integer;
  VContour: IGeometryProjectedContour;
begin
  Result := False;
  if FOuterBorder.IsPointInPolygon(APoint) then begin
    Result := True;
    for i := 0 to FHoleList.Count - 1 do begin
      VContour := IGeometryProjectedContour(FHoleList.Items[i]);
      if VContour.IsPointInPolygon(APoint) then begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TGeometryProjectedPolygonWithHoles.IsPointOnBorder(
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  i: Integer;
  VContour: IGeometryProjectedContour;
begin
  Result := false;
  if FOuterBorder.IsPointOnBorder(APoint, ADist) then begin
    Result := True;
  end else begin
    for i := 0 to FHoleList.Count - 1 do begin
      VContour := IGeometryProjectedContour(FHoleList.Items[i]);
      if VContour.IsPointOnBorder(APoint, ADist) then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TGeometryProjectedPolygonWithHoles.IsRectIntersectBorder(
  const ARect: TDoubleRect
): Boolean;
var
  i: Integer;
  VContour: IGeometryProjectedContour;
begin
  Result := false;
  if FOuterBorder.IsRectIntersectBorder(ARect) then begin
    Result := False;
    for i := 0 to FHoleList.Count - 1 do begin
      VContour := IGeometryProjectedContour(FHoleList.Items[i]);
      if VContour.IsPointInPolygon(ARect.TopLeft) then begin
        if not VContour.IsRectIntersectBorder(ARect) then begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

function TGeometryProjectedPolygonWithHoles.IsRectIntersectPolygon(
  const ARect: TDoubleRect
): Boolean;
var
  i: Integer;
  VContour: IGeometryProjectedContour;
begin
  Result := false;
  if FOuterBorder.IsRectIntersectPolygon(ARect) then begin
    Result := True;
    for i := 0 to FHoleList.Count - 1 do begin
      VContour := IGeometryProjectedContour(FHoleList.Items[i]);
      if VContour.IsPointInPolygon(ARect.TopLeft) then begin
        if not VContour.IsRectIntersectBorder(ARect) then begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

end.
