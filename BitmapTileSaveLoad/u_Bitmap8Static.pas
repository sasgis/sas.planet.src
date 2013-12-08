unit u_Bitmap8Static;

interface

uses
  Types,
  GR32,
  i_Bitmap8Static,
  u_BaseInterfacedObject;

type
  TBitmap8Static = class(TBaseInterfacedObject, IBitmap8Static)
  private
    FSize: TPoint;
    FData: PByte;
    FPalette: PColor32Array;
    FPaletteSize: Integer;
  private
    { IBitmap8Static }
    function GetSize: TPoint;
    function GetData: PByte;
    function GetPalette: PColor32Array;
    function GetPaletteSize: Integer;
  public
    constructor CreateWithOwn(
      var AData: PByte;
      const ASize: TPoint;
      const APalette: PColor32Array;
      const APaletteSize: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TBitmap8Static }

constructor TBitmap8Static.CreateWithOwn(
  var AData: PByte;
  const ASize: TPoint;
  const APalette: PColor32Array;
  const APaletteSize: Integer
);
begin
  inherited Create;

  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);

  Assert(APaletteSize > 0);
  Assert(APaletteSize <= 256);

  if
    (ASize.X > 0) and (ASize.Y > 0) and
    (ASize.X < 1 shl 16) and (ASize.Y < 1 shl 16) and
    (ASize.X * ASize.Y < 1 shl 28) and
    (APaletteSize > 0) and (APaletteSize <= 256)
  then begin
    FData := AData;
    AData := nil;
    FSize := ASize;
    FPaletteSize := APaletteSize;
    FPalette := GetMemory(FPaletteSize * 4);
    Move(APalette^, FPalette^, FPaletteSize * 4);
  end;
end;

destructor TBitmap8Static.Destroy;
begin
  if Assigned(FData) then begin
    FreeMemory(FData);
    FData := nil;
  end;
  if Assigned(FPalette) then begin
    FreeMemory(FPalette);
    FPalette := nil;
  end;
  inherited;
end;

function TBitmap8Static.GetData: PByte;
begin
  Result := FData;
end;

function TBitmap8Static.GetSize: TPoint;
begin
  Result := FSize;
end;

function TBitmap8Static.GetPalette: PColor32Array;
begin
  Result := FPalette;
end;

function TBitmap8Static.GetPaletteSize: Integer;
begin
  Result := FPaletteSize;
end;

end.
