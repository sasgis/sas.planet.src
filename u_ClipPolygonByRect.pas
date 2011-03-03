unit u_ClipPolygonByRect;

interface

uses
  Types,
  t_GeoTypes;

type
  IPolygonClip = interface
    ['{DD70326E-B6E0-4550-91B4-8AA974AD2DE5}']
    function Clip(const APoints: TArrayOfDoublePoint; APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
  end;

  TPolygonClipByLineAbstract = class(TInterfacedObject, IPolygonClip)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; virtual; abstract;
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; virtual; abstract;
  public
    function Clip(const APoints: TArrayOfDoublePoint; APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
  end;

  TPolygonClipByVerticalLine = class(TPolygonClipByLineAbstract)
  protected
    FX: Double;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(AX: Double);
  end;

  TPolygonClipByLeftBorder = class(TPolygonClipByVerticalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TPolygonClipByRightBorder = class(TPolygonClipByVerticalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TPolygonClipByHorizontalLine = class(TPolygonClipByLineAbstract)
  protected
    FY: Double;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(AY: Double);
  end;

  TPolygonClipByTopBorder = class(TPolygonClipByHorizontalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TPolygonClipByBottomBorder = class(TPolygonClipByHorizontalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TPolygonClipByRect = class(TInterfacedObject, IPolygonClip)
  private
    FClipLeft: IPolygonClip;
    FClipTop: IPolygonClip;
    FClipRight: IPolygonClip;
    FClipBottom: IPolygonClip;
  public
    constructor Create(ARect: TRect);
    destructor Destroy; override;
    function Clip(const APoints: TArrayOfDoublePoint; APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
  end;

implementation

uses
  SysUtils,
  Ugeofun;

{ TPolygonClipByLineAbstract }

function TPolygonClipByLineAbstract.Clip(const APoints: TArrayOfDoublePoint;
  APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
var
  VPrevPoint: TDoublePoint;
  VPrevPointCode: Byte;
  VCurrPoint: TDoublePoint;
  VCurrPointCode: Byte;
  VLineCode: Byte;
  i: Integer;
  VIntersectPoint: TDoublePoint;
  VOutPointsCapacity: Integer;
begin
  Result := 0;
  if APointsCount > 0 then begin
    if Length(APoints)< APointsCount then begin
      raise EAccessViolation.Create('В переданном массиве меньше точек чем ожидалось');
    end;
    VOutPointsCapacity := Length(AResultPoints);
    VCurrPoint := APoints[0];
    VCurrPointCode := GetPointCode(VCurrPoint);
    if APointsCount > 1 then begin
      for i := 1 to APointsCount - 1 do begin
        VPrevPoint := VCurrPoint;
        VPrevPointCode := VCurrPointCode;
        VCurrPoint := APoints[i];
        VCurrPointCode := GetPointCode(VCurrPoint);
        VLineCode := VPrevPointCode * 3 + VCurrPointCode;
        {
        Код   Стар Нов Выход
        0:     вне-вне нет
        1:     вне-на  конечная
        2:     вне-вну перес,кон
        3:     на -вне нет
        4:     на -на  конечная
        5:     на -вну конечная
        6:     вну-вне пересечен
        7:     вну-на  конечная
        8:     вну-вну конечная
        }
        case VLineCode of
          1, 4, 5, 7, 8: begin
            if Result = 0 then begin
              if Result >= VOutPointsCapacity then begin
                if VOutPointsCapacity  >= 32 then begin
                  VOutPointsCapacity := VOutPointsCapacity * 2;
                end else begin
                  VOutPointsCapacity := 32;
                end;
                SetLength(AResultPoints, VOutPointsCapacity);
              end;
              AResultPoints[Result] := VPrevPoint;
              Inc(Result);
            end;
            if Result >= VOutPointsCapacity then begin
              if VOutPointsCapacity  >= 32 then begin
                VOutPointsCapacity := VOutPointsCapacity * 2;
              end else begin
                VOutPointsCapacity := 32;
              end;
              SetLength(AResultPoints, VOutPointsCapacity);
            end;
            AResultPoints[Result] := VCurrPoint;
            Inc(Result);
          end;
          2: begin
            VIntersectPoint := GetIntersectPoint(VPrevPoint, VCurrPoint);
            if Result >= VOutPointsCapacity then begin
              if VOutPointsCapacity  >= 32 then begin
                VOutPointsCapacity := VOutPointsCapacity * 2;
              end else begin
                VOutPointsCapacity := 32;
              end;
              SetLength(AResultPoints, VOutPointsCapacity);
            end;
            AResultPoints[Result] := VIntersectPoint;
            Inc(Result);
            if Result >= VOutPointsCapacity then begin
              if VOutPointsCapacity  >= 32 then begin
                VOutPointsCapacity := VOutPointsCapacity * 2;
              end else begin
                VOutPointsCapacity := 32;
              end;
              SetLength(AResultPoints, VOutPointsCapacity);
            end;
            AResultPoints[Result] := VCurrPoint;
            Inc(Result);
          end;
          6: begin
            if Result = 0 then begin
              if Result >= VOutPointsCapacity then begin
                if VOutPointsCapacity  >= 32 then begin
                  VOutPointsCapacity := VOutPointsCapacity * 2;
                end else begin
                  VOutPointsCapacity := 32;
                end;
                SetLength(AResultPoints, VOutPointsCapacity);
              end;
              AResultPoints[Result] := VPrevPoint;
              Inc(Result);
            end;
            VIntersectPoint := GetIntersectPoint(VPrevPoint, VCurrPoint);
            if Result >= VOutPointsCapacity then begin
              if VOutPointsCapacity  >= 32 then begin
                VOutPointsCapacity := VOutPointsCapacity * 2;
              end else begin
                VOutPointsCapacity := 32;
              end;
              SetLength(AResultPoints, VOutPointsCapacity);
            end;
            AResultPoints[Result] := VIntersectPoint;
            Inc(Result);
          end;
        end;
      end;
      if Result > 0 then begin
        if compare2EP(APoints[0], APoints[APointsCount - 1]) then begin
          if not compare2EP(AResultPoints[0], AResultPoints[Result - 1]) then begin
            if Result >= VOutPointsCapacity then begin
              if VOutPointsCapacity  >= 32 then begin
                VOutPointsCapacity := VOutPointsCapacity * 2;
              end else begin
                VOutPointsCapacity := 32;
              end;
              SetLength(AResultPoints, VOutPointsCapacity);
            end;
            AResultPoints[Result] := AResultPoints[0];
            Inc(Result);
          end;
        end;
      end;
    end else begin
      if VCurrPointCode = 2 then begin
        if Result >= VOutPointsCapacity then begin
          VOutPointsCapacity := 1;
          SetLength(AResultPoints, VOutPointsCapacity);
        end;
        AResultPoints[Result] := VCurrPoint;
        Inc(Result);
      end;
    end;
  end;
end;

{ TPolygonClipByVerticalLine }

constructor TPolygonClipByVerticalLine.Create(AX: Double);
begin
  FX := AX;
end;

function TPolygonClipByVerticalLine.GetIntersectPoint(APrevPoint,
  ACurrPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := FX;
  Result.Y := (ACurrPoint.Y - APrevPoint.Y) / (ACurrPoint.X - APrevPoint.X) * (FX - APrevPoint.X) + APrevPoint.Y;
end;

{ TPolygonClipByLeftBorder }

function TPolygonClipByLeftBorder.GetPointCode(APoint: TDoublePoint): Byte;
begin
  if APoint.X < FX then begin
    Result := 0;
  end else if APoint.X > FX then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolyClipByRightBorder }

function TPolygonClipByRightBorder.GetPointCode(APoint: TDoublePoint): Byte;
begin
  if APoint.X > FX then begin
    Result := 0;
  end else if APoint.X < FX then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolygonClipByHorizontalLine }

constructor TPolygonClipByHorizontalLine.Create(AY: Double);
begin
  FY := AY;
end;

function TPolygonClipByHorizontalLine.GetIntersectPoint(APrevPoint,
  ACurrPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := (ACurrPoint.X - APrevPoint.X) / (ACurrPoint.Y - APrevPoint.Y) * (FY - APrevPoint.Y) + APrevPoint.X;
  Result.Y := FY;
end;

{ TPolygonClipByTopBorder }

function TPolygonClipByTopBorder.GetPointCode(APoint: TDoublePoint): Byte;
begin
  if APoint.Y < FY then begin
    Result := 0;
  end else if APoint.Y > FY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolygonClipByBottomBorder }

function TPolygonClipByBottomBorder.GetPointCode(APoint: TDoublePoint): Byte;
begin
  if APoint.Y > FY then begin
    Result := 0;
  end else if APoint.Y < FY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolygonClipByRect }

function TPolygonClipByRect.Clip(const APoints: TArrayOfDoublePoint;
  APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
var
  VTempArray: TArrayOfDoublePoint;
begin
  Result := 0;
  if APointsCount > 0 then begin
    SetLength(VTempArray, Length(AResultPoints));
    Result := FClipLeft.Clip(APoints, APointsCount, VTempArray);
    if Result > 0 then begin
      Result := FClipTop.Clip(VTempArray, Result, AResultPoints);
      if Result > 0 then begin
        Result := FClipRight.Clip(AResultPoints, Result, VTempArray);
        if Result > 0 then begin
          Result := FClipBottom.Clip(VTempArray, Result, AResultPoints);
        end;
      end;
    end;
  end;
end;

constructor TPolygonClipByRect.Create(ARect: TRect);
begin
  FClipLeft := TPolygonClipByLeftBorder.Create(ARect.Left);
  FClipTop := TPolygonClipByTopBorder.Create(ARect.Top);
  FClipRight := TPolygonClipByRightBorder.Create(ARect.Right);
  FClipBottom := TPolygonClipByBottomBorder.Create(ARect.Bottom);
end;

destructor TPolygonClipByRect.Destroy;
begin
  FClipLeft := nil;
  FClipTop := nil;
  FClipRight := nil;
  FClipBottom := nil;
  inherited;
end;

end.

