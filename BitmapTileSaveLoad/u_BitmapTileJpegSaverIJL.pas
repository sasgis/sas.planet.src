unit u_BitmapTileJpegSaverIJL;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TJpegBitmapTileSaverIJL = class(TInterfacedObject, IBitmapTileSaver)
  private
    FCompressionQuality: byte;
  public
    constructor create(ACompressionQuality: byte);
    procedure SaveToFile(ABtm: TCustomBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
  end;

implementation

uses
  SysUtils,
  Graphics,
  IJL;

{ TJpegBitmapTileSaver }

constructor TJpegBitmapTileSaverIJL.create(ACompressionQuality: byte);
begin
  inherited Create;
  FCompressionQuality := ACompressionQuality;
end;

procedure TJpegBitmapTileSaverIJL.SaveToFile(ABtm: TCustomBitmap32;
  AFileName: string);
var
  jcprops: TJPEG_CORE_PROPERTIES;
  VStatus: Integer;
  VSource: PColor32Entry;
  i: integer;
  VTarget: PByte;
  VWidth: Integer;
  VHeight: Integer;
  VPixelsCount: Integer;
begin
  VStatus := ijlInit(@jcprops);
  if VStatus < 0 then begin
    raise Exception.Create('ijlInit Error');
  end;
  try
    VWidth := ABtm.Width;
    VHeight := ABtm.Height;
    VPixelsCount := VWidth * VHeight;
    jcprops.DIBWidth := VWidth;
    jcprops.DIBHeight := VHeight;

    jcprops.DIBChannels := 3;
    jcprops.DIBColor := IJL_RGB;
    jcprops.DIBPadBytes := 0;
    GetMem(jcprops.DIBBytes, VPixelsCount * 3);
    try
      VSource := PColor32Entry(ABtm.ScanLine[0]);
      VTarget := jcprops.DIBBytes;
      for i := 0 to VPixelsCount - 1 do begin
        VTarget^ := (VSource^).R;
        Inc(VTarget);
        VTarget^ := (VSource^).G;
        Inc(VTarget);
        VTarget^ := (VSource^).B;
        Inc(VTarget);
        Inc(VSource);
      end;
      jcprops.JPGFile := PChar(AFileName);
      jcprops.JPGWidth := VWidth;
      jcprops.JPGHeight := VHeight;
      jcprops.JPGChannels := 3;
      jcprops.JPGColor := IJL_YCBCR;
      jcprops.jquality := FCompressionQuality;
      VStatus := ijlWrite(@jcprops, IJL_JFILE_WRITEWHOLEIMAGE);
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

procedure TJpegBitmapTileSaverIJL.SaveToStream(ABtm: TCustomBitmap32;
  AStream: TStream);
var
  jcprops: TJPEG_CORE_PROPERTIES;
  VStatus: Integer;
  VSource: PColor32Entry;
  i: integer;
  VTarget: PByte;
  VWidth: Integer;
  VHeight: Integer;
  VPixelsCount: Integer;
begin
  VStatus := ijlInit(@jcprops);
  if VStatus < 0 then begin
    raise Exception.Create('ijlInit Error');
  end;
  try
    VWidth := ABtm.Width;
    VHeight := ABtm.Height;
    VPixelsCount := VWidth * VHeight;
    jcprops.DIBWidth := VWidth;
    jcprops.DIBHeight := VHeight;
    jcprops.DIBChannels := 3;
    jcprops.DIBColor := IJL_RGB;
    jcprops.DIBPadBytes := 0;
    jcprops.JPGSizeBytes := VPixelsCount * 3;
    GetMem(jcprops.DIBBytes, VPixelsCount * 3);
    GetMem(jcprops.JPGBytes, jcprops.JPGSizeBytes);
    try
      VSource := PColor32Entry(ABtm.ScanLine[0]);
      VTarget := jcprops.DIBBytes;
      for i := 0 to VPixelsCount - 1 do begin
        VTarget^ := (VSource^).R;
        Inc(VTarget);
        VTarget^ := (VSource^).G;
        Inc(VTarget);
        VTarget^ := (VSource^).B;
        Inc(VTarget);
        Inc(VSource);
      end;
      jcprops.JPGWidth := jcprops.DIBWidth;
      jcprops.JPGHeight := jcprops.DIBHeight;
      jcprops.JPGChannels := 3;
      jcprops.JPGColor := IJL_YCBCR;
      jcprops.jquality := FCompressionQuality;

      VStatus := ijlWrite(@jcprops, IJL_JBUFF_WRITEWHOLEIMAGE);
      if VStatus < 0 then begin
        raise Exception.Create('Save Jpeg Error' + inttostr(vstatus));
      end;
      AStream.WriteBuffer(jcprops.JPGBytes^, jcprops.JPGSizeBytes);
    finally
      FreeMem(jcprops.DIBBytes);
      FreeMem(jcprops.JPGBytes);
    end;
  finally
    ijlFree(@jcprops);
  end;
end;

end.
