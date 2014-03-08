unit u_Bitmap32ByStaticBitmap;

interface

uses
  GR32,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory;

type
  TStaticBitmapBackend = class(TCustomBackend)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FBitmapStatic: IBitmap32Static;
  protected
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  public
    constructor Create(Owner: TCustomBitmap32; const ABitmapFactory: IBitmap32StaticFactory);
  end;

  TBitmap32ByStaticBitmap = class(TCustomBitmap32)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FBackEndByStatic: TStaticBitmapBackend;
  protected
    procedure InitializeBackend; override;
    procedure SetBackend(const Backend: TCustomBackend); override;
  public
    function MakeAndClear: IBitmap32Static;
  public
    constructor Create(const ABitmapFactory: IBitmap32StaticFactory); reintroduce;
  end;

implementation

uses
  Types;

{ TStaticBitmapBackend }

constructor TStaticBitmapBackend.Create(Owner: TCustomBitmap32;
  const ABitmapFactory: IBitmap32StaticFactory
);
begin
  Assert(ABitmapFactory <> nil);
  inherited Create(Owner);
  FBitmapFactory := ABitmapFactory;
end;

procedure TStaticBitmapBackend.FinalizeSurface;
begin
  inherited;
  FBitmapStatic := nil;
  FBits := nil;
end;

procedure TStaticBitmapBackend.InitializeSurface(NewWidth, NewHeight: Integer;
  ClearBuffer: Boolean);
begin
  if ClearBuffer then begin
    FBitmapStatic := FBitmapFactory.BuildEmptyClear(Types.Point(NewWidth, NewHeight), 0);
  end else begin
    FBitmapStatic := FBitmapFactory.BuildEmpty(Types.Point(NewWidth, NewHeight));
  end;
  if FBitmapStatic <> nil then begin
    FBits := FBitmapStatic.Data;
  end else begin
    FBits := nil;
  end;
end;

{ TBitmap32ByStaticBitmap }

constructor TBitmap32ByStaticBitmap.Create(
  const ABitmapFactory: IBitmap32StaticFactory
);
begin
  Assert(ABitmapFactory <> nil);
  FBitmapFactory := ABitmapFactory;
  inherited Create;
end;

procedure TBitmap32ByStaticBitmap.InitializeBackend;
begin
  TStaticBitmapBackend.Create(Self, FBitmapFactory);
end;

function TBitmap32ByStaticBitmap.MakeAndClear: IBitmap32Static;
begin
  Result := FBackEndByStatic.FBitmapStatic;
  Delete;
end;

procedure TBitmap32ByStaticBitmap.SetBackend(const Backend: TCustomBackend);
begin
  Assert(Backend is TStaticBitmapBackend);
  if Backend is TStaticBitmapBackend then begin
    FBackEndByStatic := TStaticBitmapBackend(Backend);
    inherited;
  end;
end;

end.
