unit u_Bitmap32StaticBuilderByGR32;

interface

uses
  Types,
  GR32,
  t_Bitmap32,
  i_Bitmap32Static,
  i_Bitmap32Surface,
  i_Bitmap32BufferFactory,
  u_BaseInterfacedObject;

type
  TBitmap32StaticBuilderByGR32 = class(TBaseInterfacedObject, IBitmap32Surface, IBitmap32StaticBuilder)
  private
    FBufferFactory: IBitmap32BufferFactory;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FSourceStatic: IBitmap32Static;
    FSize: TPoint;
    FBuffer: IBitmap32Buffer;
    FBitmap: TCustomBitmap32;
    procedure InitBufferIfNeed;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;

    function GetIsInited: Boolean;

    procedure Clear;
    procedure FullFill(const AFillColor: TColor32);

    procedure FillRect(const ARect: TRect; const AValue: TColor32);
    procedure FrameRect(const ARect: TRect; const AValue: TColor32);
    procedure Line(const APoint1, APoint2: TPoint; const AValue: TColor32);
    procedure SetPixel(const APoint: TPoint; const AValue: TColor32);

    procedure DrawBitmapStatic(const ASource: IBitmap32Static);
    procedure DrawBitmapStaticAt(const APosition: TPoint; const ASource: IBitmap32Static);
    procedure DrawBitmapData(const ASize: TPoint; const AData: PColor32Array);
    procedure DrawBitmapDataAt(const APosition: TPoint; const ASize: TPoint; const AData: PColor32Array);

    function MakeStaticAndClear: IBitmap32Static;
    function MakeStaticCopy: IBitmap32Static;
  public
    constructor CreateEmpty(
      const ABufferFactory: IBitmap32BufferFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ASize: TPoint;
      const AFillColor: TColor32 = 0
    );
    constructor CreateByData(
      const ABufferFactory: IBitmap32BufferFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ASize: TPoint;
      const AData: PColor32Array
    );
    constructor CreateBySource(
      const ABufferFactory: IBitmap32BufferFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ASourceStatic: IBitmap32Static
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_LowLevel,
  
  u_BitmapFunc;

type
  TFixedBufferBitmapBackend = class(TCustomBackend)
  private
    FBitmapBuffer: IBitmap32Buffer;
  protected
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  public
    constructor Create(Owner: TCustomBitmap32; const ABitmapBuffer: IBitmap32Buffer);
  end;

  TBitmap32FixedBitmapBuffer = class(TCustomBitmap32)
  private
    FBitmapBuffer: IBitmap32Buffer;
  protected
    procedure InitializeBackend; override;
    procedure SetBackend(const Backend: TCustomBackend); override;
  public
    constructor Create(const ABitmapBuffer: IBitmap32Buffer); reintroduce;
  end;

{ TFixedBufferBitmapBackend }

constructor TFixedBufferBitmapBackend.Create(
  Owner: TCustomBitmap32;
  const ABitmapBuffer: IBitmap32Buffer
);
begin
  Assert(Assigned(ABitmapBuffer));
  inherited Create(Owner);
  FBitmapBuffer := ABitmapBuffer;
end;

procedure TFixedBufferBitmapBackend.FinalizeSurface;
begin
  inherited;
  FBits := nil;
end;

procedure TFixedBufferBitmapBackend.InitializeSurface(
  NewWidth, NewHeight: Integer;
  ClearBuffer: Boolean
);
begin
  inherited;
  Assert(NewWidth = FBitmapBuffer.Size.X);
  Assert(NewHeight = FBitmapBuffer.Size.Y);
  if (NewWidth = FBitmapBuffer.Size.X) and (NewHeight = FBitmapBuffer.Size.Y) then begin
    FBits := FBitmapBuffer.Data;
  end else begin
    FBits := nil;
  end;
end;

{ TBitmap32FixedBitmapBuffer }

constructor TBitmap32FixedBitmapBuffer.Create(
  const ABitmapBuffer: IBitmap32Buffer
);
begin
  Assert(Assigned(ABitmapBuffer));
  inherited Create;
  FBitmapBuffer := ABitmapBuffer;
end;

procedure TBitmap32FixedBitmapBuffer.InitializeBackend;
begin
  inherited;
  TFixedBufferBitmapBackend.Create(Self, FBitmapBuffer);
end;

procedure TBitmap32FixedBitmapBuffer.SetBackend(const Backend: TCustomBackend);
begin
  Assert(Backend is TFixedBufferBitmapBackend);
  inherited;
end;

{ TBitmap32StaticBuilderByGR32 }

constructor TBitmap32StaticBuilderByGR32.CreateByData(
  const ABufferFactory: IBitmap32BufferFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ASize: TPoint;
  const AData: PColor32Array
);
begin
  Assert(Assigned(ABufferFactory));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AData));
  Assert((ASize.X > 0) and (ASize.Y > 0));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBufferFactory := ABufferFactory;
  FSize := ASize;
  FBuffer := FBufferFactory.Build(FSize, AData);
  FBitmap := TBitmap32FixedBitmapBuffer.Create(FBuffer);
end;

constructor TBitmap32StaticBuilderByGR32.CreateBySource(
  const ABufferFactory: IBitmap32BufferFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ASourceStatic: IBitmap32Static
);
begin
  Assert(Assigned(ABufferFactory));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(ASourceStatic));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBufferFactory := ABufferFactory;
  FSourceStatic := ASourceStatic;
  FSize := FSourceStatic.Size;
end;

constructor TBitmap32StaticBuilderByGR32.CreateEmpty(
  const ABufferFactory: IBitmap32BufferFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ASize: TPoint;
  const AFillColor: TColor32
);
begin
  Assert(Assigned(ABufferFactory));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert((ASize.X > 0) and (ASize.Y > 0));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBufferFactory := ABufferFactory;
  FSize := ASize;
  if (AFillColor <> 0) then begin
    FBuffer := FBufferFactory.BuildEmptyClear(FSize, AFillColor);
    FBitmap := TBitmap32FixedBitmapBuffer.Create(FBuffer);
  end;
end;

destructor TBitmap32StaticBuilderByGR32.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TBitmap32StaticBuilderByGR32.Clear;
begin
  if Assigned(FBuffer) or Assigned(FSourceStatic) then begin
    InitBufferIfNeed;
    FillLongword(FBuffer.Data^, FSize.X * FSize.Y, 0);
  end;
end;

procedure TBitmap32StaticBuilderByGR32.DrawBitmapData(
  const ASize: TPoint;
  const AData: PColor32Array
);
begin
  InitBufferIfNeed;
  BlockTransferFull(
    FBitmap,
    0, 0,
    ASize,
    AData,
    dmBlend
  );
end;

procedure TBitmap32StaticBuilderByGR32.DrawBitmapDataAt(
  const APosition, ASize: TPoint;
  const AData: PColor32Array
);
begin
  InitBufferIfNeed;
  BlockTransferFull(
    FBitmap,
    APosition.X,
    APosition.Y,
    ASize,
    AData,
    dmBlend
  );
end;

procedure TBitmap32StaticBuilderByGR32.DrawBitmapStatic(
  const ASource: IBitmap32Static
);
begin
  InitBufferIfNeed;
  BlockTransferFull(
    FBitmap,
    0, 0,
    ASource,
    dmBlend
  );
end;

procedure TBitmap32StaticBuilderByGR32.DrawBitmapStaticAt(
  const APosition: TPoint;
  const ASource: IBitmap32Static
);
begin
  InitBufferIfNeed;
  BlockTransferFull(
    FBitmap,
    APosition.X,
    APosition.Y,
    ASource,
    dmBlend
  );
end;

procedure TBitmap32StaticBuilderByGR32.FillRect(
  const ARect: TRect;
  const AValue: TColor32
);
begin
  InitBufferIfNeed;
  FBitmap.FillRectTS(ARect, AValue);
end;

procedure TBitmap32StaticBuilderByGR32.FrameRect(
  const ARect: TRect;
  const AValue: TColor32
);
begin
  InitBufferIfNeed;
  FBitmap.FrameRectTS(ARect, AValue);
end;

procedure TBitmap32StaticBuilderByGR32.FullFill(const AFillColor: TColor32);
begin
  if Assigned(FBuffer) or (AFillColor <> 0) then begin
    InitBufferIfNeed;
    FBitmap.Clear(AFillColor);
  end;
end;

function TBitmap32StaticBuilderByGR32.GetData: PColor32Array;
begin
  InitBufferIfNeed;
  Result := FBuffer.Data;
end;

function TBitmap32StaticBuilderByGR32.GetIsInited: Boolean;
begin
  Result := Assigned(FBuffer);
end;

function TBitmap32StaticBuilderByGR32.GetSize: TPoint;
begin
  Result := FSize;
end;

procedure TBitmap32StaticBuilderByGR32.InitBufferIfNeed;
begin
  if not Assigned(FBuffer) then begin
    if Assigned(FSourceStatic) then begin
      FBuffer := FBufferFactory.Build(FSize, FSourceStatic.Data);
    end else begin
      FBuffer := FBufferFactory.BuildEmptyClear(FSize, 0);
    end;
    FBitmap := TBitmap32FixedBitmapBuffer.Create(FBuffer);
  end;
end;

procedure TBitmap32StaticBuilderByGR32.Line(
  const APoint1, APoint2: TPoint;
  const AValue: TColor32
);
begin
  InitBufferIfNeed;
  FBitmap.LineTS(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y, AValue);
end;

function TBitmap32StaticBuilderByGR32.MakeStaticAndClear: IBitmap32Static;
begin
  if not Assigned(FBuffer) then begin
    if Assigned(FSourceStatic) then begin
      Result := FSourceStatic;
      FSourceStatic := nil;
    end else begin
      Result := nil;
    end;
    Exit;
  end;
  Result := FBitmap32StaticFactory.BuildWithOwnBuffer(FBuffer);
  if not Assigned(Result) then begin
    Clear;
  end else begin
    FreeAndNil(FBitmap);
    FBuffer := nil;
  end;
end;

function TBitmap32StaticBuilderByGR32.MakeStaticCopy: IBitmap32Static;
begin
  if not Assigned(FBuffer) then begin
    if Assigned(FSourceStatic) then begin
      Result := FSourceStatic;
    end else begin
      Result := nil;
    end;
    Exit;
  end;
  Result := FBitmap32StaticFactory.Build(FBuffer.Size, FBuffer.Data);
end;

procedure TBitmap32StaticBuilderByGR32.SetPixel(
  const APoint: TPoint;
  const AValue: TColor32
);
begin
  InitBufferIfNeed;
  FBitmap.SetPixelTS(APoint.X, APoint.Y, AValue);
end;

end.
