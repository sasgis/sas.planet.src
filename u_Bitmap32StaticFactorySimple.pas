unit u_Bitmap32StaticFactorySimple;

interface

uses
  SysUtils,
  GR32,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  u_BaseInterfacedObject;

type
  TBitmap32StaticFactorySimple = class(TBaseInterfacedObject, IBitmap32StaticFactory)
  private
    function Build(
      const ASize: TPoint;
      const AData: PColor32Array
    ): IBitmap32Static;
    function BuildEmpty(const ASize: TPoint): IBitmap32Static;
    function BuildEmptyClear(
      const ASize: TPoint;
      const AColor: TColor32
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
      const ASize: TPoint
    );
    destructor Destroy; override;
  end;

{ TBitmap32StaticSimple }


constructor TBitmap32StaticSimple.Create(
  const ASize: TPoint
);
begin
  inherited Create;
  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);

  if
    (ASize.X > 0) and (ASize.Y > 0) and
    (ASize.X < 1 shl 16) and (ASize.Y < 1 shl 16) and
    (ASize.X * ASize.Y < 1 shl 28)
  then begin
    GetMem(FBits, ASize.X * ASize.Y * 4);
    FSize := ASize;
  end;
end;

destructor TBitmap32StaticSimple.Destroy;
begin
  if Assigned(FBits) then begin
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


{ TBitmap32StaticFactorySimple }

function TBitmap32StaticFactorySimple.Build(
  const ASize: TPoint;
  const AData: PColor32Array
): IBitmap32Static;
begin
  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);
  Assert(AData <> nil);

  Result := BuildEmpty(ASize);
  if (Result <> nil) and (AData <> nil) then begin
    if AData <> nil then begin
      MoveLongWord(AData^, Result.Data^, ASize.X * ASize.Y);
    end;
  end;
end;

function TBitmap32StaticFactorySimple.BuildEmpty(
  const ASize: TPoint
): IBitmap32Static;
begin
  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);

  if
    (ASize.X > 0) and (ASize.Y > 0) and
    (ASize.X < 1 shl 16) and (ASize.Y < 1 shl 16) and
    (ASize.X * ASize.Y < 1 shl 28)
  then begin
    Result := TBitmap32StaticSimple.Create(ASize);
  end else begin
    Result := nil;
  end;
end;

function TBitmap32StaticFactorySimple.BuildEmptyClear(
  const ASize: TPoint;
  const AColor: TColor32
): IBitmap32Static;
begin
  Result := BuildEmpty(ASize);
  if Result <> nil then begin
    FillLongword(Result.Data^, ASize.X * ASize.Y, AColor);
  end;
end;

end.
