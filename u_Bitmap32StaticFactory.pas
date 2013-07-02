unit u_Bitmap32StaticFactory;

interface

uses
  SysUtils,
  GR32,
  i_NotifierTime,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  u_BaseInterfacedObject;

type
  TBitmap32StaticFactory = class(TBaseInterfacedObject, IBitmap32StaticFactory)
  private
    FStandartSizePool: IObjectPoolBitmap32Standart;
    FFactorySimple: IBitmap32StaticFactory;
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
  public
    constructor Create(
      const ATTLNotifier: INotifierTime;
      const ASync: IReadWriteSync
    );
  end;

implementation

uses
  Types,
  GR32_LowLevel,
  u_Bitmap32StaticFactorySimple,
  u_ObjectPoolBitmap32Standart;

{ TBitmap32StaticFactory }

constructor TBitmap32StaticFactory.Create(
  const ATTLNotifier: INotifierTime;
  const ASync: IReadWriteSync
);
begin
  inherited Create;
  FStandartSizePool :=
    TObjectPoolBitmap32Standart.Create(
      ATTLNotifier,
      ASync,
      10,
      100
    );
  FFactorySimple := TBitmap32StaticFactorySimple.Create;
end;

function TBitmap32StaticFactory.Build(const ASize: TPoint;
  const AData: PColor32Array): IBitmap32Static;
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

function TBitmap32StaticFactory.BuildEmpty(
  const ASize: TPoint
): IBitmap32Static;
var
  VStandartSize: TPoint;
begin
  VStandartSize := FStandartSizePool.Size;
  if (ASize.X = VStandartSize.X) and (ASize.Y = VStandartSize.Y) then begin
    Result := FStandartSizePool.Build;
  end else begin
    Result := FFactorySimple.BuildEmpty(ASize);
  end;
end;

function TBitmap32StaticFactory.BuildEmptyClear(
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
