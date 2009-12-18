unit u_BitmapTilePngLoader;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TPngBitmapTileLoader = class(TInterfacedObject, IBitmapTileLoader)
  public
    procedure LoadFromFile(AFileName: string; ABtm: TBitmap32);
    procedure LoadFromStream(AStream: TStream; ABtm: TBitmap32);
  end;

implementation

uses
  pngimage,
  Uimgfun;

{ TPngBitmapTileLoader }

procedure TPngBitmapTileLoader.LoadFromFile(AFileName: string;
  ABtm: TBitmap32);
var
  VPng: TPNGObject;
begin
  Vpng := TPNGObject.Create;
  try
    Vpng.LoadFromFile(AFileName);
    ABtm.DrawMode := dmOpaque;
    PNGintoBitmap32(Abtm, Vpng);
  finally
    Vpng.Free;
  end;
end;

procedure TPngBitmapTileLoader.LoadFromStream(AStream: TStream;
  ABtm: TBitmap32);
var
  VPng: TPNGObject;
begin
  Vpng := TPNGObject.Create;
  try
    Vpng.LoadFromStream(AStream);
    ABtm.DrawMode := dmOpaque;
    PNGintoBitmap32(Abtm, Vpng);
  finally
    Vpng.Free;
  end;
end;

end.
