unit u_BitmapMarker;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_BitmapMarker;

type
  TBitmapMarker = class(TInterfacedObject, IBitmapMarker)
  private
    FBitmapSize: TPoint;
    FBitmap: TCustomBitmap32;
    FAnchorPoint: TDoublePoint;
    FUseDirection: Boolean;
    FDefaultDirection: Double;
  protected
    function GetBitmapSize: TPoint;
    function GetBitmap: TCustomBitmap32;
    function GetAnchorPoint: TDoublePoint;
    function GetUseDirection: Boolean;
    function GetDefaultDirection: Double;
  public
    constructor Create(
      ABitmap: TCustomBitmap32;
      AAnchorPoint: TDoublePoint;
      AUseDirection: Boolean;
      ADefaultDirection: Double
    );
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils;

{ TBitmapMarker }

constructor TBitmapMarker.Create(ABitmap: TCustomBitmap32; AAnchorPoint: TDoublePoint;
  AUseDirection: Boolean; ADefaultDirection: Double);
begin
  FBitmap := TCustomBitmap32.Create;
  FBitmap.Assign(ABitmap);
  FBitmap.DrawMode := dmBlend;
  FBitmap.CombineMode := cmBlend;
  FBitmapSize := Point(FBitmap.Width, FBitmap.Height);
  FAnchorPoint := AAnchorPoint;
  FUseDirection := AUseDirection;
  FDefaultDirection := ADefaultDirection;
end;

destructor TBitmapMarker.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TBitmapMarker.GetAnchorPoint: TDoublePoint;
begin
  Result := FAnchorPoint;
end;

function TBitmapMarker.GetBitmap: TCustomBitmap32;
begin
  Result := FBitmap;
end;

function TBitmapMarker.GetBitmapSize: TPoint;
begin
  Result := FBitmapSize;
end;

function TBitmapMarker.GetDefaultDirection: Double;
begin
  Result := FDefaultDirection;
end;

function TBitmapMarker.GetUseDirection: Boolean;
begin
  Result := FUseDirection;
end;

end.
