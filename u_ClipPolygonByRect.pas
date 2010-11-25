unit u_ClipPolygonByRect;

interface

uses
  Types,
  t_GeoTypes;

type
  IPolyClip = interface
    ['{DD70326E-B6E0-4550-91B4-8AA974AD2DE5}']
    function Clip(const APoints: TExtendedPointArray; APointsCount: Integer; var AResultPoints: TExtendedPointArray): Integer;
  end;

  TPolyClipByLineAbstract = class(TInterfacedObject, IPolyClip)
  protected
    function GetPointCode(APoint: TExtendedPoint): Byte; virtual; abstract;
    function GetIntersectPoint(APrevPoint,ACurrPoint: TExtendedPoint): TExtendedPoint; virtual; abstract;
  public
    function Clip(const APoints: TExtendedPointArray; APointsCount: Integer; var AResultPoints: TExtendedPointArray): Integer;
  end;

  TPolyClipByVerticalLine = class(TPolyClipByLineAbstract)
  protected
    FX: Extended;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TExtendedPoint): TExtendedPoint; override;
  public
    constructor Create(AX: Extended);
  end;

  TPolyClipByLeftBorder = class(TPolyClipByVerticalLine)
  protected
    function GetPointCode(APoint: TExtendedPoint): Byte; override;
  end;

  TPolyClipByRightBorder = class(TPolyClipByVerticalLine)
  protected
    function GetPointCode(APoint: TExtendedPoint): Byte; override;
  end;

  TPolyClipByHorizontalLine = class(TPolyClipByLineAbstract)
  protected
    FY: Extended;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TExtendedPoint): TExtendedPoint; override;
  public
    constructor Create(AY: Extended);
  end;

  TPolyClipByTopBorder = class(TPolyClipByHorizontalLine)
  protected
    function GetPointCode(APoint: TExtendedPoint): Byte; override;
  end;

  TPolyClipByBottomBorder = class(TPolyClipByHorizontalLine)
  protected
    function GetPointCode(APoint: TExtendedPoint): Byte; override;
  end;

  TPolyClipByRect = class(TInterfacedObject, IPolyClip)
  private
    FClipLeft: IPolyClip;
    FClipTop: IPolyClip;
    FClipRight: IPolyClip;
    FClipBottom: IPolyClip;
  public
    constructor Create(ARect: TRect);
    destructor Destroy; override;
    function Clip(const APoints: TExtendedPointArray; APointsCount: Integer; var AResultPoints: TExtendedPointArray): Integer;
  end;

implementation

uses
  SysUtils,
  Ugeofun;

{ TPolyClipByLineAbstract }

function TPolyClipByLineAbstract.Clip(const APoints: TExtendedPointArray;
  APointsCount: Integer; var AResultPoints: TExtendedPointArray): Integer;
var
  VPrevPoint: TExtendedPoint;
  VPrevPointCode: Byte;
  VCurrPoint: TExtendedPoint;
  VCurrPointCode: Byte;
  VLineCode: Byte;
  i: Integer;
  VIntersectPoint: TExtendedPoint;
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

{ TPolyClipByVerticalLine }

constructor TPolyClipByVerticalLine.Create(AX: Extended);
begin
  FX := AX;
end;

function TPolyClipByVerticalLine.GetIntersectPoint(APrevPoint,
  ACurrPoint: TExtendedPoint): TExtendedPoint;
begin
  Result.X := FX;
  Result.Y := (ACurrPoint.Y - APrevPoint.Y) / (ACurrPoint.X - APrevPoint.X) * (FX - APrevPoint.X) + APrevPoint.Y;
end;

{ TPolyClipByLeftBorder }

function TPolyClipByLeftBorder.GetPointCode(APoint: TExtendedPoint): Byte;
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

function TPolyClipByRightBorder.GetPointCode(APoint: TExtendedPoint): Byte;
begin
  if APoint.X > FX then begin
    Result := 0;
  end else if APoint.X < FX then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolyClipByHorizontalLine }

constructor TPolyClipByHorizontalLine.Create(AY: Extended);
begin
  FY := AY;
end;

function TPolyClipByHorizontalLine.GetIntersectPoint(APrevPoint,
  ACurrPoint: TExtendedPoint): TExtendedPoint;
begin
  Result.X := (ACurrPoint.X - APrevPoint.X) / (ACurrPoint.Y - APrevPoint.Y) * (FY - APrevPoint.Y) + APrevPoint.X;
  Result.Y := FY;
end;

{ TPolyClipByTopBorder }

function TPolyClipByTopBorder.GetPointCode(APoint: TExtendedPoint): Byte;
begin
  if APoint.Y < FY then begin
    Result := 0;
  end else if APoint.Y > FY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolyClipByBottomBorder }

function TPolyClipByBottomBorder.GetPointCode(APoint: TExtendedPoint): Byte;
begin
  if APoint.Y > FY then begin
    Result := 0;
  end else if APoint.Y < FY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolyClipByRect }

function TPolyClipByRect.Clip(const APoints: TExtendedPointArray;
  APointsCount: Integer; var AResultPoints: TExtendedPointArray): Integer;
var
  VTempArray: TExtendedPointArray;
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

constructor TPolyClipByRect.Create(ARect: TRect);
begin
  FClipLeft := TPolyClipByLeftBorder.Create(ARect.Left);
  FClipTop := TPolyClipByTopBorder.Create(ARect.Top);
  FClipRight := TPolyClipByRightBorder.Create(ARect.Right);
  FClipBottom := TPolyClipByBottomBorder.Create(ARect.Bottom);
end;

destructor TPolyClipByRect.Destroy;
begin
  FClipLeft := nil;
  FClipTop := nil;
  FClipRight := nil;
  FClipBottom := nil;
  inherited;
end;

end.
