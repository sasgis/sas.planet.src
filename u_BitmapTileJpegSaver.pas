unit u_BitmapTileJpegSaver;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TJpegBitmapTileSaver = class(TInterfacedObject, IBitmapTileSaver)
  public
    FCompressionQuality:byte;
    constructor create(ACompressionQuality:byte);overload;
    procedure SaveToFile(ABtm: TBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TBitmap32; AStream: TStream);
  end;

implementation

uses
  Graphics,
  Jpeg;

{ TJpegBitmapTileSaver }
constructor TJpegBitmapTileSaver.create(ACompressionQuality:byte);
begin
 FCompressionQuality:=ACompressionQuality;
end;

procedure TJpegBitmapTileSaver.SaveToFile(ABtm: TBitmap32;
  AFileName: string);
var
  VJpg_ex: TJpegImage;
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
   VBtm_ex.Assign(Abtm as TBitmap32);
   VJpg_ex := TJpegImage.Create;
   try
     VJpg_ex.CompressionQuality := FCompressionQuality;
     VJpg_ex.Assign(VBtm_ex);
     VJpg_ex.SaveToFile(AFileName);
   finally
     VJpg_ex.Free;
   end;
  finally
    VBtm_ex.Free;
  end;
end;

procedure TJpegBitmapTileSaver.SaveToStream(ABtm: TBitmap32;
  AStream: TStream);
var
  VJpg_ex: TJpegImage;
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
   VBtm_ex.Assign(Abtm as TBitmap32);
   VJpg_ex := TJpegImage.Create;
   try
     VJpg_ex.CompressionQuality := FCompressionQuality;
     VJpg_ex.Assign(VBtm_ex);
     VJpg_ex.SaveToStream(AStream);
   finally
     VJpg_ex.Free;
   end;
  finally
    VBtm_ex.Free;
  end;
end;

end.
 