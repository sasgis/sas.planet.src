unit u_BitmapTileBmpSaver;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TBmpBitmapTileSaver = class(TInterfacedObject, IBitmapTileSaver)
  public
    procedure SaveToFile(ABtm: TCustomBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
  end;

implementation

uses
  Graphics;

{ TBmpBitmapTileSaver }

procedure TBmpBitmapTileSaver.SaveToFile(ABtm: TCustomBitmap32;
  AFileName: string);
var
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
    VBtm_ex.Assign(Abtm as TCustomBitmap32);
    VBtm_ex.SaveToFile(AFileName);
  finally
    VBtm_ex.Free;
  end;
end;

procedure TBmpBitmapTileSaver.SaveToStream(ABtm: TCustomBitmap32;
  AStream: TStream);
var
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
    VBtm_ex.Assign(Abtm as TCustomBitmap32);
    VBtm_ex.SaveToStream(AStream);
  finally
    VBtm_ex.Free;
  end;
end;

end.
 