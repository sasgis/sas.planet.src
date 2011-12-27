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
    constructor Create(ASourceEnum: IEnumDoublePoint);
  end;

  TEnumDoublePointClipByVerticalLine = class(TEnumDoublePointClipByLineAbstract)
  private
    FLineX: Double;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; override;
    property LineX: Double read FLineX;
  public
    constructor Create(ASourceEnum: IEnumDoublePoint; AX: Double);
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
    constructor Create(ASourceEnum: IEnumDoublePoint; AY: Double);
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
    constructor Create(ASourceEnum: IEnumDoublePoint; ARect: TDoubleRect);
  end;

implementation

uses
  u_GeoFun,
  u_EnumDoublePointFilterEqual;

{ TEnumDoublePointClipByLineAbstract }

constructor TEnumDoublePointClipByLineAbstract.Create(
  ASourceEnum: IEnumDoublePoint);
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
      end else begin
        VCurrPointCode := 3;
        FFinished := True;
      end;
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
        $01, $10, $12, $21, $22, $03, $13, $23, $31, $32: begin
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
          FPrevPoint := VCurrPoint;
          FPrevPointCode := VCurrPointCode;
          Break;
        end;
      end;
    end;
  end;
  Result := not FFinished;
end;

{ TEnumDoublePointClipByVerticalLine }

constructor TEnumDoublePointClipByVerticalLine.Create(
  ASourceEnum: IEnumDoublePoint; AX: Double);
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
  ASourceEnum: IEnumDoublePoint; AY: Double);
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

constructor TEnumDoublePointClipByRect.Create(ASourceEnum: IEnumDoublePoint; ARect: TDoubleRect);
begin
  FEnum :=
    TEnumDoublePointFilterEqual.Create(
      TEnumDoublePointClipByLeftBorder.Create(
        TEnumDoublePointClipByTopBorder.Create(
          TEnumDoublePointClipByRightBorder.Create(
            TEnumDoublePointClipByBottomBorder.Create(
              ASourceEnum,
              ARect.Bottom
            ),
            ARect.Right
          ),
          ARect.Top
        ),
        ARect.Left
      )
    );
end;

function TEnumDoublePointClipByRect.Next(out APoint: TDoublePoint): Boolean;
begin
  Result := FEnum.Next(APoint);
end;

end.
