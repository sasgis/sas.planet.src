unit u_BitmapGeo;

interface

uses
  Types,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_BitmapGeo;

type
  TBitmapGeo = class(TInterfacedObject, IBitmapGeo)
  private
    FBitmap: IBitmap32Static;
    FConverter: ILocalCoordConverter;
  private
    function GetBitmap: IBitmap32Static;
    function GetConverter: ILocalCoordConverter;
  public
    constructor Create(
      ABitmap: IBitmap32Static;
      AConverter: ILocalCoordConverter
    );
  end;

  TBitmapGeoTile = class(TBitmapGeo, IBitmapGeoTile)
  private
    FTile: TPoint;
  private
    function GetTile: TPoint;
  public
    constructor Create(
      ATile: TPoint;
      ABitmap: IBitmap32Static;
      AConverter: ILocalCoordConverter
    );
  end;

implementation

uses
  SysUtils;

{ TBitmapGeo }

constructor TBitmapGeo.Create(
  ABitmap: IBitmap32Static;
  AConverter: ILocalCoordConverter
);
begin
  FBitmap := ABitmap;
  FConverter := AConverter;
end;

function TBitmapGeo.GetBitmap: IBitmap32Static;
begin
  Result := FBitmap;
end;

function TBitmapGeo.GetConverter: ILocalCoordConverter;
begin
  Result := FConverter;
end;

{ TBitmapGeoTile }

constructor TBitmapGeoTile.Create(ATile: TPoint; ABitmap: IBitmap32Static;
  AConverter: ILocalCoordConverter);
begin
  inherited Create(ABitmap, AConverter);
  FTile := ATile;
end;

function TBitmapGeoTile.GetTile: TPoint;
begin
  Result := FTile;
end;

end.
