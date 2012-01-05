unit u_EnumDoublePointWithClip;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint;

type
  TEnumDoublePointClipByLineAbstract = class(TInterfacedObject, IEnumDoublePoint)
  private
    FSourceEnum: IEnumDoublePoint;
    FFinished: Boolean;
    FPrevPoint: TDoublePoint;
    FPrevPointCode: Byte;
    FPreparedPoint: TDoublePoint;
    FPreparedPointExists: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; virtual; abstract;
    function GetIntersectPoint(APrevPoint, ACurrPoint: TDoublePoint): TDoublePoint; virtual; abstract;
  public
    constructor Create(
      ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumDoublePointClipByVerticalLine = class(TEnumDoublePointClipByLineAbstract)
  private
    FLineX: Double;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; override;
    property LineX: Double read FLineX;
  public
    constructor Create(
      AX: Double;
      ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumDoublePointClipByLeftBorder = class(TEnumDoublePointClipByVerticalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TEnumDoublePointClipByRightBorder = class(TEnumDoublePointClipByVerticalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TEnumDoublePointClipByHorizontalLine = class(TEnumDoublePointClipByLineAbstract)
  private
    FLineY: Double;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; override;
    property LineY: Double read FLineY;
  public
    constructor Create(
      AY: Double;
      ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumDoublePointClipByTopBorder = class(TEnumDoublePointClipByHorizontalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TEnumDoublePointClipByBottomBorder = class(TEnumDoublePointClipByHorizontalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TEnumDoublePointClipByRect = class(TInterfacedObject, IEnumDoublePoint)
  private
    FEnum: IEnumDoublePoint;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      AClosed: Boolean;
      ARect: TDoubleRect;
      ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumProjectedPointClipByRect = class(TEnumDoublePointClipByRect, IEnumProjectedPoint)
  public
    constructor Create(
      AClosed: Boolean;
      ARect: TDoubleRect;
      ASourceEnum: IEnumProjectedPoint
    );
  end;


implementation

uses
  u_GeoFun,
  u_EnumDoublePointClosePoly;

{ TEnumDoublePointClipByLineAbstract }

constructor TEnumDoublePointClipByLineAbstract.Create(
  ASourceEnum: IEnumDoublePoint
);
begin
  FSourceEnum := ASourceEnum;
  FFinished := False;
  FPrevPointCode := 3;
  FPreparedPointExists := False;
end;

function TEnumDoublePointClipByLineAbstract.Next(
  out APoint: TDoublePoint): Boolean;
var
  VCurrPoint: TDoublePoint;
  VCurrPointCode: Byte;
  VLineCode: Byte;
  VIntersectPoint: TDoublePoint;
begin
  while not FFinished do begin
    if FPreparedPointExists then begin
      APoint := FPreparedPoint;
      FPreparedPointExists := False;
      Break;
    end else begin
      if FSourceEnum.Next(VCurrPoint) then begin
        VCurrPointCode := GetPointCode(VCurrPoint);
        VLineCode := FPrevPointCode * 16 + VCurrPointCode;
        {
        Код      Стар Нов Выход
        $00:     вне-вне нет
        $01:     вне-на  конечная
        $02:     вне-вну перес,кон
        $03:     вне-раз конечная
        $10:     на -вне нет
        $11:     на -на  конечная
        $12:     на -вну конечная
        $13:     на -раз конечная
        $20:     вну-вне пересечен
        $21:     вну-на  конечная
        $22:     вну-вну конечная
        $23:     вну-раз конечная
        $30:     раз-вне нет
        $31:     раз-на конечная
        $32:     раз-вну конечная
        $33:     раз-раз нет
        }
        case VLineCode of
          $01, $11, $12, $21, $22, $03, $13, $23, $31, $32: begin
            APoint := VCurrPoint;
            FPrevPoint := VCurrPoint;
            FPrevPointCode := VCurrPointCode;
            Break;
          end;
          $02: begin
            VIntersectPoint := GetIntersectPoint(FPrevPoint, VCurrPoint);
            APoint := VIntersectPoint;
            FPreparedPoint := VCurrPoint;
            FPreparedPointExists := True;
            FPrevPoint := VCurrPoint;
            FPrevPointCode := VCurrPointCode;
            Break;
          end;
          $20: begin
            VIntersectPoint := GetIntersectPoint(FPrevPoint, VCurrPoint);
            APoint := VIntersectPoint;
            FPrevPoint := VCurrPoint;
            FPrevPointCode := VCurrPointCode;
            Break;
          end;
          $00, $10, $30: begin
            FPrevPoint := VCurrPoint;
            FPrevPointCode := VCurrPointCode;
          end;
        end;
      end else begin
        APoint := CEmptyDoublePoint;
        FFinished := True;
        Break;
      end;
    end;
  end;
  Result := not FFinished;
end;

{ TEnumDoublePointClipByVerticalLine }

constructor TEnumDoublePointClipByVerticalLine.Create(
  AX: Double;
  ASourceEnum: IEnumDoublePoint
);
begin
  inherited Create(ASourceEnum);
  FLineX := AX;
end;

function TEnumDoublePointClipByVerticalLine.GetIntersectPoint(APrevPoint,
  ACurrPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := LineX;
  Result.Y :=
    (ACurrPoint.Y - APrevPoint.Y) / (ACurrPoint.X - APrevPoint.X) *
    (LineX - APrevPoint.X) + APrevPoint.Y;
end;

{ TEnumDoublePointClipByLeftBorder }

function TEnumDoublePointClipByLeftBorder.GetPointCode(
  APoint: TDoublePoint): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else if APoint.X < LineX then begin
    Result := 0;
  end else if APoint.X > LineX then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TEnumDoublePointClipByRightBorder }

function TEnumDoublePointClipByRightBorder.GetPointCode(
  APoint: TDoublePoint): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else if APoint.X > LineX then begin
    Result := 0;
  end else if APoint.X < LineX then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TEnumDoublePointClipByHorizontalLine }

constructor TEnumDoublePointClipByHorizontalLine.Create(
  AY: Double;
  ASourceEnum: IEnumDoublePoint
);
begin
  inherited Create(ASourceEnum);
  FLineY := AY;
end;

function TEnumDoublePointClipByHorizontalLine.GetIntersectPoint(APrevPoint,
  ACurrPoint: TDoublePoint): TDoublePoint;
begin
  Result.X :=
    (ACurrPoint.X - APrevPoint.X) / (ACurrPoint.Y - APrevPoint.Y) *
    (LineY - APrevPoint.Y) + APrevPoint.X;
  Result.Y := LineY;
end;

{ TEnumDoublePointClipByTopBorder }

function TEnumDoublePointClipByTopBorder.GetPointCode(
  APoint: TDoublePoint): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else   if APoint.Y < LineY then begin
    Result := 0;
  end else if APoint.Y > LineY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TEnumDoublePointClipByBottomBorder }

function TEnumDoublePointClipByBottomBorder.GetPointCode(
  APoint: TDoublePoint): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else   if APoint.Y > LineY then begin
    Result := 0;
  end else if APoint.Y < LineY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TEnumDoublePointClipByRect }

constructor TEnumDoublePointClipByRect.Create(
  AClosed: Boolean;
  ARect: TDoubleRect;
  ASourceEnum: IEnumDoublePoint
);
begin
  if AClosed then begin
    FEnum :=
      TEnumDoublePointClosePoly.Create(
        TEnumDoublePointClipByLeftBorder.Create(
          ARect.Left,
          TEnumDoublePointClosePoly.Create(
            TEnumDoublePointClipByTopBorder.Create(
              ARect.Top,
              TEnumDoublePointClosePoly.Create(
                TEnumDoublePointClipByRightBorder.Create(
                  ARect.Right,
                  TEnumDoublePointClosePoly.Create(
                    TEnumDoublePointClipByBottomBorder.Create(
                      ARect.Bottom,
                      TEnumDoublePointClosePoly.Create(
                        ASourceEnum
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
  end else begin
    FEnum :=
      TEnumDoublePointClipByLeftBorder.Create(
        ARect.Left,
        TEnumDoublePointClipByTopBorder.Create(
          ARect.Top,
          TEnumDoublePointClipByRightBorder.Create(
            ARect.Right,
            TEnumDoublePointClipByBottomBorder.Create(
              ARect.Bottom,
              ASourceEnum
            )
          )
        )
      );
  end;
end;

function TEnumDoublePointClipByRect.Next(out APoint: TDoublePoint): Boolean;
begin
  Result := FEnum.Next(APoint);
end;

{ TEnumProjectedPointClipByRect }

constructor TEnumProjectedPointClipByRect.Create(AClosed: Boolean;
  ARect: TDoubleRect; ASourceEnum: IEnumProjectedPoint);
begin
  inherited Create(AClosed, ARect, ASourceEnum);
end;

end.
