unit u_BitmapTileGifLoader;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TGifBitmapTileLoader = class(TInterfacedObject, IBitmapTileLoader)
  public
    procedure LoadFromFile(AFileName: string; ABtm: TBitmap32);
    procedure LoadFromStream(AStream: TStream; ABtm: TBitmap32);
  end;

implementation

uses
  GIFImage;

{ TGifBitmapTileLoader }

procedure GifToBitmap32(AGif: TGIFImage; ABtm: TBitmap32);
var
  p: PColor32;
  c: TColor32;
  h,w: integer;
begin
  ABtm.DrawMode:=dmOpaque;
  If (AGif.isTransparent) then begin
    c:=Color32(AGif.Images[0].GraphicControlExtension.TransparentColor);
    AGif.Images[0].GraphicControlExtension.Transparent:=false;
    ABtm.Assign(AGif);
    p := @ABtm.Bits[0];
    for H:=0 to ABtm.Height-1 do begin
      for W:=0 to ABtm.Width-1 do begin
        if p^=c then begin
          p^:=$00000000;
        end;
        inc(p);
      end;
    end;
  end else begin
    ABtm.Assign(AGif);
  end;
end;


procedure TGifBitmapTileLoader.LoadFromFile(AFileName: string;
  ABtm: TBitmap32);
var
  VGif: TGIFImage;
begin
  VGif:=TGIFImage.Create;
  try
    VGif.LoadFromFile(AFileName);
    GifToBitmap32(VGif, ABtm);
  finally
    VGif.Free;
  end;
end;

procedure TGifBitmapTileLoader.LoadFromStream(AStream: TStream;
  ABtm: TBitmap32);
var
  VGif: TGIFImage;
begin
  VGif:=TGIFImage.Create;
  try
    VGif.LoadFromStream(AStream);
    GifToBitmap32(VGif, ABtm);
  finally
    VGif.Free;
  end;
end;

end.
 