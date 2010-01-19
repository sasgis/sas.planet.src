unit u_BitmapTileJpegLoader;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TJpegBitmapTileLoader = class(TInterfacedObject, IBitmapTileLoader)
  public
    procedure LoadFromFile(AFileName: string; ABtm: TBitmap32);
    procedure LoadFromStream(AStream: TStream; ABtm: TBitmap32);
  end;

implementation

uses
  SysUtils,
  IJL;
{ TJpegBitmapTileLoader }

procedure RGBA2BGRA2(var jcprops : TJPEG_CORE_PROPERTIES);
var W, H : Integer;
    p : PIntegerArray;
    pData : Pointer;
    Width, Height : Integer;
begin
  Width := jcprops.JPGWidth;
  Height := jcprops.JPGHeight;
  pData := jcprops.DIBBytes;
  p := PIntegerArray(pData);
  for H := 0 to Height-1 do begin
    for W := 0 to Width-1 do begin
      p^[W]:=(p^[W] and $FF000000)or((p^[W] and $00FF0000) shr 16)or(p^[W] and $0000FF00)or((p^[W] and $000000FF) shl 16);
    end;
    inc(p,width)
  end;
end;

procedure RGBA2BGRA3(var jcprops : TJPEG_CORE_PROPERTIES);
var W, H : Integer;
    p : PInteger;
    pData : Pointer;
    Width, Height : Integer;
begin
  Width := jcprops.JPGWidth;
  Height := jcprops.JPGHeight;
  pData := jcprops.DIBBytes;
  p := PInteger(pData);
  for H := 0 to Height-1 do begin
    for W := 0 to Width-1 do begin
      p^:=(p^ and $FF000000)or((p^ and $00FF0000) shr 16)or(p^ and $0000FF00)or((p^ and $000000FF) shl 16);
      inc(p);
    end;
  end;
end;

function LoadJpeg(var jcprops : TJPEG_CORE_PROPERTIES; Btm: TBitmap32): Boolean;
var
  iWidth, iHeight, iNChannels : Integer;
begin
  Result := true;
  iWidth := jcprops.JPGWidth;
  iHeight := jcprops.JPGHeight;
  iNChannels := 4;
  Btm.SetSize(iWidth,iHeight);
  jcprops.DIBWidth := iWidth;
  jcprops.DIBHeight := iHeight;
  jcprops.DIBChannels := iNChannels;
  jcprops.DIBColor := IJL_RGBA_FPX;
  jcprops.DIBPadBytes := ((((iWidth*iNChannels)+3) div 4)*4)-(iWidth*iNChannels);
  jcprops.DIBBytes := PByte(Btm.Bits);

  if (jcprops.JPGChannels = 3) then begin
    jcprops.JPGColor := IJL_YCBCR;
  end else if (jcprops.JPGChannels = 4) then begin
    jcprops.JPGColor := IJL_YCBCRA_FPX;
  end else if (jcprops.JPGChannels = 1) then begin
    jcprops.JPGColor := IJL_G;
  end else begin
    jcprops.DIBColor := TIJL_COLOR (IJL_OTHER);
    jcprops.JPGColor := TIJL_COLOR (IJL_OTHER);
  end;
end;

procedure TJpegBitmapTileLoader.LoadFromFile(AFileName: string;
  ABtm: TBitmap32);
var
  iStatus : Integer;
  jcprops : TJPEG_CORE_PROPERTIES;
begin
  iStatus := ijlInit(@jcprops);
  if iStatus < 0 then begin
    raise Exception.Create('ijlInit Error');
  end;
  try
    jcprops.JPGFile := PChar(AFileName);
    iStatus := ijlRead(@jcprops,IJL_JFILE_READPARAMS);
    if iStatus < 0 then begin
      raise Exception.Create('ijlRead from file Error');
    end;
    if not LoadJpeg(jcprops, ABtm) then begin
      raise Exception.Create('Prepare load Jpeg Error');
    end;
    iStatus := ijlRead(@jcprops,IJL_JFILE_READWHOLEIMAGE);
    if iStatus < 0 then begin
      raise Exception.Create('Load Jpeg Error');
    end;
    RGBA2BGRA3(jcprops);
  finally
    ijlFree(@jcprops);
  end;
end;

procedure TJpegBitmapTileLoader.LoadFromStream(AStream: TStream;
  ABtm: TBitmap32);
var
  VInternalStream: TMemoryStream;
  VMemStream: TCustomMemoryStream;
  iStatus : Integer;
  jcprops : TJPEG_CORE_PROPERTIES;
begin
  if AStream is TCustomMemoryStream then begin
    VInternalStream := nil;
    VMemStream := AStream as TCustomMemoryStream;
  end else begin
    VInternalStream := TMemoryStream.Create;
    VInternalStream.LoadFromStream(AStream);
    VMemStream := VInternalStream;
  end;
  try
    iStatus := ijlInit(@jcprops);
    if iStatus < 0 then begin
      raise Exception.Create('ijlInit Error');
    end;
    try
      jcprops.JPGBytes := VMemStream.Memory;
      jcprops.JPGSizeBytes := VMemStream.Size;
      iStatus := ijlRead(@jcprops,IJL_JBUFF_READPARAMS);
      if iStatus < 0 then begin
        raise Exception.Create('ijlRead from stream Error');
      end;
      if not LoadJpeg(jcprops, ABtm) then begin
        raise Exception.Create('Prepare load Jpeg Error');
      end;
      iStatus := ijlRead(@jcprops,IJL_JBUFF_READWHOLEIMAGE);
      if iStatus < 0 then begin
        raise Exception.Create('Load Jpeg Error');
      end;
      RGBA2BGRA3(jcprops);
    finally
      ijlFree(@jcprops);
    end;
  finally
    FreeAndNil(VInternalStream);
  end;
end;

end.
