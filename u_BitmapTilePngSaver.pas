unit u_BitmapTilePngSaver;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TPngBitmapTileSaver = class(TInterfacedObject, IBitmapTileSaver)
  public
    procedure SaveToFile(ABtm: TBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TBitmap32; AStream: TStream);
  end;

implementation

uses
  Graphics,
  pngimage;

{ TPngBitmapTileSaver }

procedure TPngBitmapTileSaver.SaveToFile(ABtm: TBitmap32;
  AFileName: string);
var
  VPng_ex: TPNGObject;
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
   VBtm_ex.Assign(Abtm as TBitmap32);
   VPng_ex := TPNGObject.Create;
   try
     VPng_ex.Assign(VBtm_ex);
     VPng_ex.SaveToFile(AFileName);
   finally
     VPng_ex.Free;
   end;
  finally
    VBtm_ex.Free;
  end;
end;

procedure TPngBitmapTileSaver.SaveToStream(ABtm: TBitmap32;
  AStream: TStream);
var
  VPng_ex:TPNGObject;
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
   VBtm_ex.Assign(Abtm as TBitmap32);
   VPng_ex := TPNGObject.Create;
   try
     VPng_ex.Assign(VBtm_ex);
     VPng_ex.SaveToStream(AStream);
   finally
     VPng_ex.Free;
   end;
  finally
    VBtm_ex.Free;
  end;
end;

end.
 