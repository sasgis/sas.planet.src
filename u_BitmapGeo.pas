unit u_BitmapGeo;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_BitmapGeo;

type
  TBitmapGeo = class(TInterfacedObject, IBitmapGeo)
  private
    FBitmap: TCustomBitmap32;
    FConverter: ILocalCoordConverter;
  private
    function GetBitmap: TCustomBitmap32;
    function GetConverter: ILocalCoordConverter;
  public
    constructor Create(
      ABitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    );
    constructor CreateWithOwn(
      ABitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    );
    destructor Destroy; override;
  end;

  TBitmapGeoTile = class(TBitmapGeo, IBitmapGeoTile)
  private
    FTile: TPoint;
  private
    function GetTile: TPoint;
  public
    constructor Create(
      ATile: TPoint;
      ABitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    );
    constructor CreateWithOwn(
      ATile: TPoint;
      ABitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    );
  end;

implementation

uses
  SysUtils;

{ TBitmapGeo }

constructor TBitmapGeo.Create(
  ABitmap: TCustomBitmap32;
  AConverter: ILocalCoordConverter
);
var
  VBitmap: TCustomBitmap32;
begin
  VBitmap := TCustomBitmap32.Create;
  VBitmap.Assign(ABitmap);
  CreateWithOwn(VBitmap, AConverter);
end;

constructor TBitmapGeo.CreateWithOwn(ABitmap: TCustomBitmap32;
  AConverter: ILocalCoordConverter);
begin
  FBitmap := ABitmap;
  FConverter := AConverter;
end;

destructor TBitmapGeo.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TBitmapGeo.GetBitmap: TCustomBitmap32;
begin
  Result := FBitmap;
end;

function TBitmapGeo.GetConverter: ILocalCoordConverter;
begin
  Result := FConverter;
end;

{ TBitmapGeoTile }

constructor TBitmapGeoTile.Create(ATile: TPoint; ABitmap: TCustomBitmap32;
  AConverter: ILocalCoordConverter);
begin
  inherited Create(ABitmap, AConverter);
  FTile := ATile;
end;

constructor TBitmapGeoTile.CreateWithOwn(ATile: TPoint;
  ABitmap: TCustomBitmap32; AConverter: ILocalCoordConverter);
begin
  inherited CreateWithOwn(ABitmap, AConverter);
  FTile := ATile;
end;

function TBitmapGeoTile.GetTile: TPoint;
begin
  Result := FTile;
end;

end.
