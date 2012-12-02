unit u_BitmapGeo;

interface

uses
  Types,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_BitmapGeo,
  u_BaseInterfacedObject;

type
  TBitmapGeo = class(TBaseInterfacedObject, IBitmapGeo)
  private
    FBitmap: IBitmap32Static;
    FConverter: ILocalCoordConverter;
  private
    function GetBitmap: IBitmap32Static;
    function GetConverter: ILocalCoordConverter;
  public
    constructor Create(
      const ABitmap: IBitmap32Static;
      const AConverter: ILocalCoordConverter
    );
  end;

  TBitmapGeoTile = class(TBitmapGeo, IBitmapGeoTile)
  private
    FTile: TPoint;
  private
    function GetTile: TPoint;
  public
    constructor Create(
      const ATile: TPoint;
      const ABitmap: IBitmap32Static;
      const AConverter: ILocalCoordConverter
    );
  end;

implementation



{ TBitmapGeo }

constructor TBitmapGeo.Create(
  const ABitmap: IBitmap32Static;
  const AConverter: ILocalCoordConverter
);
begin
  inherited Create;
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

constructor TBitmapGeoTile.Create(
  const ATile: TPoint;
  const ABitmap: IBitmap32Static;
  const AConverter: ILocalCoordConverter
);
begin
  inherited Create(ABitmap, AConverter);
  FTile := ATile;
end;

function TBitmapGeoTile.GetTile: TPoint;
begin
  Result := FTile;
end;

end.
