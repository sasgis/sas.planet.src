unit u_Bitmap32StaticFactory;

interface

uses
  GR32,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  u_BaseInterfacedObject;

type
  TBitmap32StaticFactory = class(TBaseInterfacedObject, IBitmap32StaticFactory)
  private
    function Build(
      const ASize: TPoint;
      const AData: PColor32Array
    ): IBitmap32Static;
  end;

implementation

uses
  Types,
  GR32_LowLevel;

type
  TBitmap32StaticSimple = class(TBaseInterfacedObject, IBitmap32Static)
  private
    FSize: TPoint;
    FBits: PColor32Array;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
  public
    constructor Create(
      const ASize: TPoint;
      const AData: PColor32Array
    );
    destructor Destroy; override;
  end;

{ TBitmap32StaticSimple }


constructor TBitmap32StaticSimple.Create(
  const ASize: TPoint;
  const AData: PColor32Array
);
begin
  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);
  Assert(AData <> nil);

  if
    (ASize.X > 0) and (ASize.Y > 0) and
    (ASize.X < 1 shl 16) and (ASize.Y < 1 shl 16) and
    (ASize.X * ASize.Y < 1 shl 28) and (AData <> nil)
  then begin
    GetMem(FBits, ASize.X * ASize.Y * 4);
    FSize := ASize;
    MoveLongWord(AData^, FBits^, ASize.X * ASize.Y);
  end;
end;

destructor TBitmap32StaticSimple.Destroy;
begin
  if Assigned(FBits) then
  begin
    FreeMem(FBits);
    FBits := nil;
  end;
  inherited;
end;

function TBitmap32StaticSimple.GetData: PColor32Array;
begin
  Result := FBits;
end;

function TBitmap32StaticSimple.GetSize: TPoint;
begin
  Result := FSize;
end;

const
  CStandartSize = 256;

type
  TBitmap32StaticStandartSize = class(TBaseInterfacedObject, IBitmap32Static)
  private
    FBits: array [0..(CStandartSize * CStandartSize - 1)] of TColor32;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
  public
    constructor Create(const AData: PColor32Array);
  end;

{ TBitmap32StaticStandartSize }

constructor TBitmap32StaticStandartSize.Create(const AData: PColor32Array);
begin
  Assert(AData <> nil);
  if (AData <> nil) then begin
    MoveLongWord(AData^, FBits[0], CStandartSize * CStandartSize);
  end;
end;

function TBitmap32StaticStandartSize.GetData: PColor32Array;
begin
  Result := PColor32Array(@FBits[0]);
end;

function TBitmap32StaticStandartSize.GetSize: TPoint;
begin
  Result := Point(CStandartSize, CStandartSize);
end;

{ TBitmap32StaticFactory }

function TBitmap32StaticFactory.Build(const ASize: TPoint;
  const AData: PColor32Array): IBitmap32Static;
begin
  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);
  Assert(AData <> nil);


  if
    (ASize.X = CStandartSize) and (ASize.Y = CStandartSize) and
    (AData <> nil)
  then begin
    Result := TBitmap32StaticStandartSize.Create(AData);
  end else if
    (ASize.X > 0) and (ASize.Y > 0) and
    (ASize.X < 1 shl 16) and (ASize.Y < 1 shl 16) and
    (ASize.X * ASize.Y < 1 shl 28) and (AData <> nil)
  then begin
    Result := TBitmap32StaticSimple.Create(ASize, AData);
  end else begin
    Result := nil;
  end;
end;

end.
