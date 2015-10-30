unit LibJpegInOutDataManager;

interface

{.$DEFINE LIB_JPEG_VERBOSE}

uses
  {$IFDEF LIB_JPEG_VERBOSE}
  Windows,
  {$ENDIF}
  Classes,
  SysUtils,
  LibJpeg62;

const
  CJpegInOutBufferSize = 4096;

type
  // compress
  PJpeg62OutPutDataManager = ^TJpeg62OutPutDataManager;
  TJpeg62OutPutDataManager = record
    jpeg_dest_mgr: LibJpeg62.jpeg_destination_mgr;
    OutPutStream: TStream;
    OutPutBuffer: array [1..CJpegInOutBufferSize] of Byte;
  end;

  // decompress
  PJpeg62InPutDataManager = ^TJpeg62InPutDataManager;
  TJpeg62InPutDataManager = record
    jpeg_src_mgr: LibJpeg62.jpeg_source_mgr;
    InPutStream: TStream;
    InPutBuffer: array [1..CJpegInOutBufferSize] of Byte;
  end;

procedure libjpeg_init_destination(cinfo: LibJpeg62.j_compress_ptr); cdecl;
function  libjpeg_empty_output_buffer(cinfo: LibJpeg62.j_compress_ptr): boolean; cdecl;
procedure libjpeg_term_destination(cinfo: LibJpeg62.j_compress_ptr); cdecl;

procedure libjpeg_init_source(cinfo: LibJpeg62.j_decompress_ptr); cdecl;
function  libjpeg_fill_input_buffer(cinfo: LibJpeg62.j_decompress_ptr): boolean; cdecl;
procedure libjpeg_skip_input_data(cinfo: LibJpeg62.j_decompress_ptr; num_bytes: Longint); cdecl;
procedure libjpeg_term_source(cinfo: LibJpeg62.j_decompress_ptr); cdecl;
//function  libjpeg_resync_to_restart(cinfo: LibJpeg62.j_decompress_ptr; desired: integer): boolean; cdecl;

implementation

procedure libjpeg_init_destination(cinfo: LibJpeg62.j_compress_ptr); cdecl;
begin
  {$IFDEF LIB_JPEG_VERBOSE}
  OutputDebugString('libjpeg_init_destination');
  {$ENDIF}
  with PJpeg62OutPutDataManager(cinfo^.dest)^ do begin
    OutPutStream := TStream(cinfo^.client_data^);
    jpeg_dest_mgr.next_output_byte := @OutPutBuffer[1];
    jpeg_dest_mgr.free_in_buffer := Length(OutPutBuffer);
  end;
end;

function libjpeg_empty_output_buffer(cinfo: LibJpeg62.j_compress_ptr): boolean; cdecl;
begin
  with PJpeg62OutPutDataManager(cinfo^.dest)^ do begin
    {$IFDEF LIB_JPEG_VERBOSE}
    OutputDebugString(PChar(
      'libjpeg_empty_output_buffer [free_in_buffer = ' + IntToStr(jpeg_dest_mgr.free_in_buffer) + ']:'
    ));
    {$ENDIF}
    if jpeg_dest_mgr.free_in_buffer < Cardinal(Length(OutPutBuffer)) then begin
      OutPutStream.WriteBuffer(OutPutBuffer[1], Length(OutPutBuffer));
      jpeg_dest_mgr.next_output_byte := @OutPutBuffer[1];
      jpeg_dest_mgr.free_in_buffer := Length(OutPutBuffer);
      {$IFDEF DEBUG}
      FillChar(OutPutBuffer[1], Length(OutPutBuffer), 0);
      {$ENDIF}
    end;
  end;
  Result := True;
end;

procedure libjpeg_term_destination(cinfo: LibJpeg62.j_compress_ptr); cdecl;
var
  I: Integer;
begin
  with PJpeg62OutPutDataManager(cinfo^.dest)^ do begin
    {$IFDEF LIB_JPEG_VERBOSE}
    OutputDebugString(PChar(
      'libjpeg_term_destination [free_in_buffer = ' + IntToStr(jpeg_dest_mgr.free_in_buffer) + ']:'
    ));
    {$ENDIF}
    I := Cardinal(Length(OutPutBuffer)) - jpeg_dest_mgr.free_in_buffer;
    if I > 0 then begin
      OutPutStream.WriteBuffer(OutPutBuffer[1], I);
      Assert(OutPutBuffer[I] = JPEG_EOI);
    end;
  end;
end;

procedure libjpeg_init_source(cinfo: LibJpeg62.j_decompress_ptr); cdecl;
begin
  with PJpeg62InPutDataManager(cinfo^.src)^ do begin
    InPutStream := TStream(cinfo^.client_data^);
    jpeg_src_mgr.bytes_in_buffer := 0;
    jpeg_src_mgr.next_input_byte := nil;
  end;
end;

function libjpeg_fill_input_buffer(cinfo: LibJpeg62.j_decompress_ptr): boolean; cdecl;
var
  VBytesInBuf: Integer;
begin
  with PJpeg62InPutDataManager(cinfo^.src)^ do begin
    VBytesInBuf := InPutStream.Read(InPutBuffer[1], Length(InPutBuffer));
    if (VBytesInBuf <= 0) then begin // Insert a fake EOI marker:
      InPutBuffer[1] := $FF;
      InPutBuffer[2] := JPEG_EOI;
      VBytesInBuf := 2;
    end;
    jpeg_src_mgr.bytes_in_buffer := VBytesInBuf;
    jpeg_src_mgr.next_input_byte := @InPutBuffer[1];
  end;
  Result := True;
end;

procedure libjpeg_skip_input_data(cinfo: LibJpeg62.j_decompress_ptr; num_bytes: Longint); cdecl;
begin
  with PJpeg62InPutDataManager(cinfo^.src)^ do begin
    if (num_bytes > 0) then begin
      while num_bytes > jpeg_src_mgr.bytes_in_buffer do begin
        num_bytes := num_bytes - jpeg_src_mgr.bytes_in_buffer;
        jpeg_src_mgr.fill_input_buffer(cinfo);
      end;
      Inc(jpeg_src_mgr.next_input_byte, num_bytes);
      Dec(jpeg_src_mgr.bytes_in_buffer, num_bytes);
    end;
  end;
end;

procedure libjpeg_term_source(cinfo: LibJpeg62.j_decompress_ptr); cdecl;
begin
  //
end;

//function libjpeg_resync_to_restart(cinfo: LibJpeg62.j_decompress_ptr; desired: integer): boolean; cdecl;
//begin
//   ???
//end;

end.
