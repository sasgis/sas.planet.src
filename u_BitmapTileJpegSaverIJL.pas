unit u_BitmapTileJpegSaverIJL;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TJpegBitmapTileSaverIJL = class(TInterfacedObject, IBitmapTileSaver)
  public
    procedure SaveToFile(ABtm: TBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TBitmap32; AStream: TStream);
  end;

implementation

uses
  SysUtils,
  Graphics,
  IJL,
  Jpeg;

{ TJpegBitmapTileSaver }

procedure TJpegBitmapTileSaverIJL.SaveToFile(ABtm: TBitmap32;
  AFileName: string);
var
  jcprops: TJPEG_CORE_PROPERTIES;
  VStatus: Integer;
  VSource: PColor32Entry;
  i: integer;
  VTarget: PByte;
begin
  VStatus := ijlInit(@jcprops);
  if VStatus < 0 then begin
    raise Exception.Create('ijlInit Error');
  end;
  try
    jcprops.DIBWidth := ABtm.Width;
    jcprops.DIBHeight := ABtm.Height;
    jcprops.DIBChannels := 3;
    jcprops.DIBColor := IJL_RGB;
    jcprops.DIBPadBytes := 0;
    GetMem(jcprops.DIBBytes,jcprops.DIBWidth*jcprops.DIBHeight*3);
    try
      VSource := PColor32Entry(ABtm.ScanLine[0]);
      VTarget := jcprops.DIBBytes;
      for i := 0 to jcprops.DIBWidth*jcprops.DIBHeight - 1 do begin
        VTarget^ := (VSource^).R;
        Inc(VTarget);
        VTarget^ := (VSource^).G;
        Inc(VTarget);
        VTarget^ := (VSource^).B;
        Inc(VTarget);
        Inc(VSource);
      end;
      jcprops.JPGFile := PChar(AFileName);
      jcprops.JPGWidth := jcprops.DIBWidth;
      jcprops.JPGHeight := jcprops.DIBHeight;
      jcprops.JPGChannels := 3;
      jcprops.JPGColor := IJL_YCBCR;
      jcprops.jquality := 85;
      VStatus := ijlWrite(@jcprops,IJL_JFILE_WRITEWHOLEIMAGE);
    if VStatus < 0 then begin
      raise Exception.Create('Save Jpeg Error' + inttostr(vstatus));
    end;
    finally
      FreeMem(jcprops.DIBBytes);
    end;
  finally
    ijlFree(@jcprops);
  end;
end;

procedure TJpegBitmapTileSaverIJL.SaveToStream(ABtm: TBitmap32;
  AStream: TStream);
var
  VJpg_ex: TJpegImage;
  VBtm_ex: TBitmap;
begin
  VBtm_ex := TBitmap.Create;
  try
   VBtm_ex.Assign(Abtm);
   VJpg_ex := TJpegImage.Create;
   try
     VJpg_ex.CompressionQuality := 85;
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

