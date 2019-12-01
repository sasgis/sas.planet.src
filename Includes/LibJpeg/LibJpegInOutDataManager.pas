unit LibJpegInOutDataManager;

interface

uses
  Classes,
  LibJpeg62;

const
  CJpegInOutBufferSize = 4096;

type
  TJpegDataManager = record
    FStream: TStream;
    FBuffer: array [0..CJpegInOutBufferSize-1] of Byte;
  end;
  PJpegDataManager = ^TJpegDataManager;

// compress callbacks
procedure libjpeg_init_destination(cinfo: j_compress_ptr); cdecl;
function  libjpeg_empty_output_buffer(cinfo: j_compress_ptr): boolean; cdecl;
procedure libjpeg_term_destination(cinfo: j_compress_ptr); cdecl;

// decompress callbacks
procedure libjpeg_init_source(cinfo: j_decompress_ptr); cdecl;
function  libjpeg_fill_input_buffer(cinfo: j_decompress_ptr): boolean; cdecl;
procedure libjpeg_skip_input_data(cinfo: j_decompress_ptr; num_bytes: Longint); cdecl;
procedure libjpeg_term_source(cinfo: j_decompress_ptr); cdecl;

implementation

procedure libjpeg_init_destination(cinfo: j_compress_ptr); cdecl;
begin
  with PJpegDataManager(cinfo.client_data)^ do begin
    cinfo.dest.next_output_byte := @FBuffer[0];
    cinfo.dest.free_in_buffer := Length(FBuffer);
  end;
end;

function libjpeg_empty_output_buffer(cinfo: j_compress_ptr): boolean; cdecl;
var
  VData: PJpegDataManager;
  VLen: Integer;
begin
  VData := PJpegDataManager(cinfo.client_data);
  VLen := Length(VData.FBuffer);
  with cinfo.dest^ do begin
    VData.FStream.WriteBuffer(VData.FBuffer[0], VLen);
    next_output_byte := @VData.FBuffer[0];
    free_in_buffer := VLen;
    {$IFDEF DEBUG}
    FillChar(VData.FBuffer[0], VLen, 0);
    {$ENDIF}
  end;
  Result := True;
end;

procedure libjpeg_term_destination(cinfo: j_compress_ptr); cdecl;
var
  I: Integer;
begin
  with PJpegDataManager(cinfo.client_data)^ do begin
    I := Cardinal(Length(FBuffer)) - cinfo.dest.free_in_buffer;
    if I > 0 then begin
      FStream.WriteBuffer(FBuffer[0], I);
      Assert(FBuffer[I-1] = JPEG_EOI);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure libjpeg_init_source(cinfo: j_decompress_ptr); cdecl;
begin
  with cinfo.src^ do begin
    bytes_in_buffer := 0;
    next_input_byte := nil;
  end;
end;

function libjpeg_fill_input_buffer(cinfo: j_decompress_ptr): boolean; cdecl;
var
  VLen: Integer;
begin
  with PJpegDataManager(cinfo.client_data)^ do begin
    VLen := FStream.Read(FBuffer[0], Length(FBuffer));
    if VLen <= 0 then begin
      // Insert a fake EOI marker:
      FBuffer[0] := $FF;
      FBuffer[1] := JPEG_EOI;
      VLen := 2;
    end;
    with cinfo.src^ do begin
      bytes_in_buffer := VLen;
      next_input_byte := @FBuffer[0];
    end;
  end;
  Result := True;
end;

procedure libjpeg_skip_input_data(cinfo: j_decompress_ptr; num_bytes: Longint); cdecl;
begin
  if num_bytes <= 0 then begin
    Exit;
  end;
  with cinfo.src^ do begin
    while num_bytes > bytes_in_buffer do begin
      num_bytes := num_bytes - bytes_in_buffer;
      fill_input_buffer(cinfo);
    end;
    Inc(next_input_byte, num_bytes);
    Dec(bytes_in_buffer, num_bytes);
  end;
end;

procedure libjpeg_term_source(cinfo: j_decompress_ptr); cdecl;
begin
  // no work necessary here
end;

end.
