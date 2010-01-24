unit u_BitmapTileGifSaver;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TGifBitmapTileSaver = class(TInterfacedObject, IBitmapTileSaver)
  public
    procedure SaveToFile(ABtm: TBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TBitmap32; AStream: TStream);
  end;

implementation

uses
  Graphics,
  GIFImage;


{ TGifBitmapTileSaver }

procedure TGifBitmapTileSaver.SaveToFile(ABtm: TBitmap32;
  AFileName: string);
var
  VGif_ex: TGIFImage;
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
    VBtm_ex.Assign(Abtm as TBitmap32);
    VGif_ex := TGIFImage.Create;
    try
      VGif_ex.Assign(VBtm_ex);
      VGif_ex.SaveToFile(AFileName);
    finally
      VGif_ex.Free;
    end;
  finally
    VBtm_ex.Free;
  end;
end;

procedure TGifBitmapTileSaver.SaveToStream(ABtm: TBitmap32;
  AStream: TStream);
var
  VGif_ex: TGIFImage;
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
    VBtm_ex.Assign(Abtm as TBitmap32);
    VGif_ex := TGIFImage.Create;
    try
      VGif_ex.Assign(VBtm_ex);
      VGif_ex.SaveToStream(AStream);
    finally
      VGif_ex.Free;
    end;
  finally
    VBtm_ex.Free;
  end;
end;

end.
 