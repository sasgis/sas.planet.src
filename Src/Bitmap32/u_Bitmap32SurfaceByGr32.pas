unit u_Bitmap32SurfaceByGr32;

interface

uses
  Types,
  GR32,
  t_Bitmap32,
  i_Bitmap32Static,
  i_Bitmap32Surface,
  u_BaseInterfacedObject;

type
  TBitmap32SurfaceByGr32 = class(TBaseInterfacedObject, IBitmap32Surface)
  private
    FBitmap: TCustomBitmap32;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
    function GetIsInited: Boolean;

    procedure Clear;
    procedure FullFill(const AFillColor: TColor32);

    procedure FillRect(
      const ARect: TRect;
      const AValue: TColor32
    );
    procedure FrameRect(
      const ARect: TRect;
      const AValue: TColor32
    );
    procedure Line(
      const APoint1, APoint2: TPoint;
      const AValue: TColor32
    );
    procedure SetPixel(
      const APoint: TPoint;
      const AValue: TColor32
    );

    procedure DrawBitmapStatic(const ASource: IBitmap32Static);
    procedure DrawBitmapStaticAt(
      const APosition: TPoint;
      const ASource: IBitmap32Static
    );
    procedure DrawBitmapData(
      const ASize: TPoint;
      const AData: PColor32Array
    );
    procedure DrawBitmapDataAt(
      const APosition: TPoint;
      const ASize: TPoint;
      const AData: PColor32Array
    );
  public
    constructor Create(ABitmap: TCustomBitmap32);
  end;

implementation

uses
  u_BitmapFunc;

{ TBitmap32SurfaceByGr32 }

constructor TBitmap32SurfaceByGr32.Create(ABitmap: TCustomBitmap32);
begin
  Assert(Assigned(ABitmap));
  inherited Create;
  FBitmap := ABitmap;
end;

procedure TBitmap32SurfaceByGr32.Clear;
begin
  FBitmap.Clear(0);
end;

procedure TBitmap32SurfaceByGr32.DrawBitmapData(
  const ASize: TPoint;
  const AData: PColor32Array
);
begin
  BlockTransferFull(
    FBitmap,
    0, 0,
    ASize,
    AData,
    dmBlend
  );
end;

procedure TBitmap32SurfaceByGr32.DrawBitmapDataAt(
  const APosition, ASize: TPoint;
  const AData: PColor32Array
);
begin
  BlockTransferFull(
    FBitmap,
    APosition.X,
    APosition.Y,
    ASize,
    AData,
    dmBlend
  );
end;

procedure TBitmap32SurfaceByGr32.DrawBitmapStatic(
  const ASource: IBitmap32Static
);
begin
  BlockTransferFull(
    FBitmap,
    0, 0,
    ASource,
    dmBlend
  );
end;

procedure TBitmap32SurfaceByGr32.DrawBitmapStaticAt(
  const APosition: TPoint;
  const ASource: IBitmap32Static
);
begin
  BlockTransferFull(
    FBitmap,
    APosition.X,
    APosition.Y,
    ASource,
    dmBlend
  );
end;

procedure TBitmap32SurfaceByGr32.FillRect(
  const ARect: TRect;
  const AValue: TColor32
);
begin
  FBitmap.FillRectTS(ARect, AValue);
end;

procedure TBitmap32SurfaceByGr32.FrameRect(
  const ARect: TRect;
  const AValue: TColor32
);
begin
  FBitmap.FrameRectTS(ARect, AValue);
end;

procedure TBitmap32SurfaceByGr32.FullFill(const AFillColor: TColor32);
begin
  FBitmap.Clear(AFillColor);
end;

function TBitmap32SurfaceByGr32.GetData: PColor32Array;
begin
  Result := FBitmap.Bits;
end;

function TBitmap32SurfaceByGr32.GetIsInited: Boolean;
begin
  Result := True;
end;

function TBitmap32SurfaceByGr32.GetSize: TPoint;
begin
  Result := Point(FBitmap.Width, FBitmap.Height);
end;

procedure TBitmap32SurfaceByGr32.Line(
  const APoint1, APoint2: TPoint;
  const AValue: TColor32
);
begin
  FBitmap.LineTS(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y, AValue);
end;

procedure TBitmap32SurfaceByGr32.SetPixel(
  const APoint: TPoint;
  const AValue: TColor32
);
begin
  FBitmap.SetPixelTS(APoint.X, APoint.Y, AValue);
end;

end.
