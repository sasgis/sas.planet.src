unit u_GeometryProjected;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_GeometryProjected,
  u_BaseInterfacedObject;

type
  TGeometryProjectedBase = class(TBaseInterfacedObject, IGeometryProjected)
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
      const APoints: PDoublePointArray;
      ACount: Integer
    ); overload;
    constructor Create(
      AClosed: Boolean;
      const APoints: PDoublePointArray;
      ACount: Integer
    ); overload;
  end;

  TGeometryProjectedLine = class(TGeometryProjectedBase, IGeometryProjectedLine, IGeometryProjectedSingleLine)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointOnPath(
      const APoint: TDoublePoint;
      ADist: Double
    ): Boolean;
    function IsRectIntersectPath(const ARect: TDoubleRect): Boolean;
  public
    constructor Create(
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TGeometryProjectedPolygon = class(TGeometryProjectedBase, IGeometryProjectedPolygon, IGeometryProjectedSinglePolygon)
  private
    function GetEnum: IEnumProjectedPoint;
    function IsPointInPolygon(const APoint: TDoublePoint): Boolean;
    function IsPointOnBorder(
      const APoint: TDoublePoint;
      ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(const ARect: TDoubleRect): Boolean;
    function IsRectIntersectBorder(const ARect: TDoubleRect): Boolean;
    function CalcArea: Double;
  public
    constructor Create(
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

implementation

uses
  u_GeoFunc,
  u_EnumDoublePointBySingleLine;

{ TGeometryProjectedBase }

constructor TGeometryProjectedBase.Create(
  AClosed: Boolean;
  const APoints: PDoublePointArray;
  ACount: Integer
);
var
  VBounds: TDoubleRect;
  i: Integer;
begin
  inherited Create;
  VBounds.TopLeft := APoints[0];
  VBounds.BottomRight := APoints[0];
  for i := 1 to ACount - 1 do begin
    if VBounds.Left > APoints[i].X then begin
      VBounds.Left := APoints[i].X;
    end;
    if VBounds.Top > APoints[i].Y then begin
      VBounds.Top := APoints[i].Y;
    end;
    if VBounds.Right < APoints[i].X then begin
      VBounds.Right := APoints[i].X;
    end;
    if VBounds.Bottom < APoints[i].Y then begin
      VBounds.Bottom := APoints[i].Y;
    end;
  end;
  Create(AClosed, VBounds, APoints, ACount);
end;

constructor TGeometryProjectedBase.Create(
  AClosed: Boolean;
  const ABounds: TDoubleRect;
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create;
  FBounds := ABounds;
  FCount := ACount;
  Assert(FCount > 0, 'Empty line');
  if AClosed and (FCount > 1) and DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Dec(FCount);
  end;

  SetLength(FPoints, FCount);
  Move(APoints^, FPoints[0], FCount * SizeOf(TDoublePoint));
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
  Result := @FPoints[0];
end;

{ TGeometryProjectedLine }

constructor TGeometryProjectedLine.Create(
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create(False, APoints, ACount);
end;

function TGeometryProjectedLine.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumDoublePointBySingleProjectedLine.Create(Self, False, @FPoints[0], FCount);
end;

function TGeometryProjectedLine.IsPointOnPath(
  const APoint: TDoublePoint;
  ADist: Double
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

{ TGeometryProjectedPolygon }

function TGeometryProjectedPolygon.CalcArea: Double;
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

constructor TGeometryProjectedPolygon.Create(
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create(True, APoints, ACount);
end;

function TGeometryProjectedPolygon.GetEnum: IEnumProjectedPoint;
begin
  Result := TEnumDoublePointBySingleProjectedLine.Create(Self, True, @FPoints[0], FCount);
end;

function TGeometryProjectedPolygon.IsPointInPolygon(
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

function TGeometryProjectedPolygon.IsPointOnBorder(
  const APoint: TDoublePoint;
  ADist: Double
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

function TGeometryProjectedPolygon.IsRectIntersectBorder(
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

function TGeometryProjectedPolygon.IsRectIntersectPolygon(
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
    if PixelPointInRect(FPoints[0], ARect) then begin
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

end.
