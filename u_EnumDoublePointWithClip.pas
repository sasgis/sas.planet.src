unit u_EnumDoublePointWithClip;

interface

uses
  t_GeoTypes,
  i_DoublePointFilter,
  i_EnumDoublePoint;

type
  TEnumDoublePointClipByRect = class(TInterfacedObject, IEnumDoublePoint)
  private
    FEnum: IEnumDoublePoint;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      AClosed: Boolean;
      const ARect: TDoubleRect;
      const ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumProjectedPointClipByRect = class(TEnumDoublePointClipByRect, IEnumProjectedPoint)
  public
    constructor Create(
      AClosed: Boolean;
      const ARect: TDoubleRect;
      const ASourceEnum: IEnumProjectedPoint
    );
  end;

  TEnumLocalPointClipByRect = class(TEnumDoublePointClipByRect, IEnumLocalPoint)
  public
    constructor Create(
      AClosed: Boolean;
      const ARect: TDoubleRect;
      const ASourceEnum: IEnumLocalPoint
    );
  end;

  TDoublePointFilterClipByRect = class(TInterfacedObject, IDoublePointFilter)
  private
    FClosed: Boolean;
    FRect: TDoubleRect;
  private
    function CreateFilteredEnum(const ASource: IEnumDoublePoint): IEnumDoublePoint;
  public
    constructor Create(
      AClosed: Boolean;
      const ARect: TDoubleRect
    );
  end;

  TProjectedPointFilterClipByRect = class(TInterfacedObject, IProjectedPointFilter)
  private
    FClosed: Boolean;
    FRect: TDoubleRect;
  private
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumProjectedPoint;
  public
    constructor Create(
      AClosed: Boolean;
      const ARect: TDoubleRect
    );
  end;


implementation

uses
  u_GeoFun,
  u_EnumDoublePointClosePoly;

type
  TEnumDoublePointClipByLineAbstract = class(TInterfacedObject, IEnumDoublePoint, IEnumProjectedPoint, IEnumLocalPoint)
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
    function GetPointCode(const APoint: TDoublePoint): Byte; virtual; abstract;
    function GetIntersectPoint(const APrevPoint, ACurrPoint: TDoublePoint): TDoublePoint; virtual; abstract;
  public
    constructor Create(
      const ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumDoublePointClipByVerticalLine = class(TEnumDoublePointClipByLineAbstract)
  private
    FLineX: Double;
  protected
    function GetIntersectPoint(const APrevPoint, ACurrPoint: TDoublePoint): TDoublePoint; override;
    property LineX: Double read FLineX;
  public
    constructor Create(
      const AX: Double;
      const ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumDoublePointClipByLeftBorder = class(TEnumDoublePointClipByVerticalLine)
  protected
    function GetPointCode(const APoint: TDoublePoint): Byte; override;
  end;

  TEnumDoublePointClipByRightBorder = class(TEnumDoublePointClipByVerticalLine)
  protected
    function GetPointCode(const APoint: TDoublePoint): Byte; override;
  end;

  TEnumDoublePointClipByHorizontalLine = class(TEnumDoublePointClipByLineAbstract)
  private
    FLineY: Double;
  protected
    function GetIntersectPoint(const APrevPoint, ACurrPoint: TDoublePoint): TDoublePoint; override;
    property LineY: Double read FLineY;
  public
    constructor Create(
      const AY: Double;
      const ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumDoublePointClipByTopBorder = class(TEnumDoublePointClipByHorizontalLine)
  protected
    function GetPointCode(const APoint: TDoublePoint): Byte; override;
  end;

  TEnumDoublePointClipByBottomBorder = class(TEnumDoublePointClipByHorizontalLine)
  protected
    function GetPointCode(const APoint: TDoublePoint): Byte; override;
  end;

{ TEnumDoublePointClipByLineAbstract }

constructor TEnumDoublePointClipByLineAbstract.Create(
  const ASourceEnum: IEnumDoublePoint
);
begin
  inherited Create;
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
  const AX: Double;
  const ASourceEnum: IEnumDoublePoint
);
begin
  inherited Create(ASourceEnum);
  FLineX := AX;
end;

function TEnumDoublePointClipByVerticalLine.GetIntersectPoint(
  const APrevPoint, ACurrPoint: TDoublePoint
): TDoublePoint;
begin
  Result.X := LineX;
  Result.Y :=
    (ACurrPoint.Y - APrevPoint.Y) / (ACurrPoint.X - APrevPoint.X) *
    (LineX - APrevPoint.X) + APrevPoint.Y;
end;

{ TEnumDoublePointClipByLeftBorder }

function TEnumDoublePointClipByLeftBorder.GetPointCode(
  const APoint: TDoublePoint
): Byte;
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
  const APoint: TDoublePoint
): Byte;
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
  const AY: Double;
  const ASourceEnum: IEnumDoublePoint
);
begin
  inherited Create(ASourceEnum);
  FLineY := AY;
end;

function TEnumDoublePointClipByHorizontalLine.GetIntersectPoint(
  const APrevPoint, ACurrPoint: TDoublePoint
): TDoublePoint;
begin
  Result.X :=
    (ACurrPoint.X - APrevPoint.X) / (ACurrPoint.Y - APrevPoint.Y) *
    (LineY - APrevPoint.Y) + APrevPoint.X;
  Result.Y := LineY;
end;

{ TEnumDoublePointClipByTopBorder }

function TEnumDoublePointClipByTopBorder.GetPointCode(
  const APoint: TDoublePoint
): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else if APoint.Y < LineY then begin
    Result := 0;
  end else if APoint.Y > LineY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TEnumDoublePointClipByBottomBorder }

function TEnumDoublePointClipByBottomBorder.GetPointCode(
  const APoint: TDoublePoint
): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else if APoint.Y > LineY then begin
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
  const ARect: TDoubleRect;
  const ASourceEnum: IEnumDoublePoint
);
var
  VEnum: IEnumDoublePoint;
begin
  inherited Create;
  if AClosed then begin
    VEnum := ASourceEnum;
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByBottomBorder.Create(ARect.Bottom, VEnum);
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByRightBorder.Create(ARect.Right, VEnum);
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByTopBorder.Create(ARect.Top, VEnum);
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByLeftBorder.Create(ARect.Left, VEnum);
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    FEnum := VEnum;
  end else begin
    VEnum := ASourceEnum;
    VEnum := TEnumDoublePointClipByBottomBorder.Create(ARect.Bottom, VEnum);
    VEnum := TEnumDoublePointClipByTopBorder.Create(ARect.Top, VEnum);
    VEnum := TEnumDoublePointClipByLeftBorder.Create(ARect.Left, VEnum);
    VEnum := TEnumDoublePointClipByRightBorder.Create(ARect.Right, VEnum);
    FEnum := VEnum;
  end;
end;

function TEnumDoublePointClipByRect.Next(out APoint: TDoublePoint): Boolean;
begin
  Result := FEnum.Next(APoint);
end;

{ TEnumProjectedPointClipByRect }

constructor TEnumProjectedPointClipByRect.Create(
  AClosed: Boolean;
  const ARect: TDoubleRect;
  const ASourceEnum: IEnumProjectedPoint
);
begin
  inherited Create(AClosed, ARect, ASourceEnum);
end;

{ TEnumLocalPointClipByRect }

constructor TEnumLocalPointClipByRect.Create(
  AClosed: Boolean;
  const ARect: TDoubleRect;
  const ASourceEnum: IEnumLocalPoint
);
begin
  inherited Create(AClosed, ARect, ASourceEnum);
end;

{ TDoublePointFilterClipByRect }

constructor TDoublePointFilterClipByRect.Create(
  AClosed: Boolean;
  const ARect: TDoubleRect
);
begin
  inherited Create;
  FClosed := AClosed;
  FRect := ARect;
end;

function TDoublePointFilterClipByRect.CreateFilteredEnum(
  const ASource: IEnumDoublePoint
): IEnumDoublePoint;
var
  VEnum: IEnumDoublePoint;
begin
  if FClosed then begin
    VEnum := ASource;
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByBottomBorder.Create(FRect.Bottom, VEnum);
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByRightBorder.Create(FRect.Right, VEnum);
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByTopBorder.Create(FRect.Top, VEnum);
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByLeftBorder.Create(FRect.Left, VEnum);
    VEnum := TEnumDoublePointClosePoly.Create(VEnum);
    Result := VEnum;
  end else begin
    VEnum := ASource;
    VEnum := TEnumDoublePointClipByBottomBorder.Create(FRect.Bottom, VEnum);
    VEnum := TEnumDoublePointClipByRightBorder.Create(FRect.Right, VEnum);
    VEnum := TEnumDoublePointClipByTopBorder.Create(FRect.Top, VEnum);
    VEnum := TEnumDoublePointClipByLeftBorder.Create(FRect.Left, VEnum);
    Result := VEnum;
  end;
end;

{ TProjectedPointFilterClipByRect }

constructor TProjectedPointFilterClipByRect.Create(
  AClosed: Boolean;
  const ARect: TDoubleRect
);
begin
  inherited Create;
  FClosed := AClosed;
  FRect := ARect;
end;

function TProjectedPointFilterClipByRect.CreateFilteredEnum(
  const ASource: IEnumProjectedPoint
): IEnumProjectedPoint;
var
  VEnum: IEnumProjectedPoint;
begin
  if FClosed then begin
    VEnum := ASource;
    VEnum := TEnumProjectedPointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByBottomBorder.Create(FRect.Bottom, VEnum);
    VEnum := TEnumProjectedPointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByRightBorder.Create(FRect.Right, VEnum);
    VEnum := TEnumProjectedPointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByTopBorder.Create(FRect.Top, VEnum);
    VEnum := TEnumProjectedPointClosePoly.Create(VEnum);
    VEnum := TEnumDoublePointClipByLeftBorder.Create(FRect.Left, VEnum);
    VEnum := TEnumProjectedPointClosePoly.Create(VEnum);
    Result := VEnum;
  end else begin
    VEnum := ASource;
    VEnum := TEnumDoublePointClipByBottomBorder.Create(FRect.Bottom, VEnum);
    VEnum := TEnumDoublePointClipByRightBorder.Create(FRect.Right, VEnum);
    VEnum := TEnumDoublePointClipByTopBorder.Create(FRect.Top, VEnum);
    VEnum := TEnumDoublePointClipByLeftBorder.Create(FRect.Left, VEnum);
    Result := VEnum;
  end;
end;

end.
