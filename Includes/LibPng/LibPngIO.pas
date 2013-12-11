unit LibPngIO;

interface

uses
  Windows,
  Classes,
  LibPng;

type
  png_rw_io_stream = record
    DestStream: TStream;
    DestBuffer: Pointer;
    DestBufferSize: Cardinal;
    BufferedDataSize: Cardinal;
  end;
  png_rw_io_stream_ptr = ^png_rw_io_stream;

const
  cIOBufferSize = 64 * 1024; // 64k

procedure FlashBuffer(const rw_io_ptr: png_rw_io_stream_ptr);
procedure lib_png_write_data_callback(png_ptr: png_structp; data: png_bytep; data_length: png_size_t); cdecl;

implementation

procedure lib_png_write_data_callback(png_ptr: png_structp; data: png_bytep; data_length: png_size_t); cdecl;
var
  rw_io_ptr: png_rw_io_stream_ptr;
begin
  rw_io_ptr := png_rw_io_stream_ptr(png_ptr.io_ptr);
  if data_length >= rw_io_ptr.DestBufferSize then begin // buffer is too small
    FlashBuffer(rw_io_ptr);
    rw_io_ptr.DestStream.WriteBuffer(data^, data_length);
  end else if (rw_io_ptr.BufferedDataSize + data_length) >= rw_io_ptr.DestBufferSize then begin // buffer is full
    FlashBuffer(rw_io_ptr);
    CopyMemory(Pointer(Cardinal(rw_io_ptr.DestBuffer) + rw_io_ptr.BufferedDataSize), data, data_length);
    Inc(rw_io_ptr.BufferedDataSize, data_length);
  end else begin // (rw_io_ptr.BufferedDataSize + data_length) < rw_io_ptr.DestBufferSize  // buffer is OK
    CopyMemory(Pointer(Cardinal(rw_io_ptr.DestBuffer) + rw_io_ptr.BufferedDataSize), data, data_length);
    Inc(rw_io_ptr.BufferedDataSize, data_length);
  end;
end;

procedure FlashBuffer(const rw_io_ptr: png_rw_io_stream_ptr);
begin
  if rw_io_ptr.BufferedDataSize > 0 then begin
    rw_io_ptr.DestStream.WriteBuffer(rw_io_ptr.DestBuffer^, rw_io_ptr.BufferedDataSize);
    rw_io_ptr.BufferedDataSize := 0;
  end;
end; 

end.
