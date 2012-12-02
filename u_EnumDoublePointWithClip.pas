unit u_EnumDoublePointWithClip;

interface

uses
  t_GeoTypes,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointClipByRect = class(TBaseInterfacedObject, IEnumDoublePoint)
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

  TDoublePointFilterClipByRect = class(TBaseInterfacedObject, IDoublePointFilter)
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

  TProjectedPointFilterClipByRect = class(TBaseInterfacedObject, IProjectedPointFilter)
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
  u_EnumDoublePointClosePoly,
  u_EnumDoublePointClipInternal;

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
