unit LibJpegErrorHandler;

interface

uses
  SysUtils,
  LibJpeg62;

type
  ELibJpegException = class(Exception);

procedure libjpeg_error_exit(cinfo: LibJpeg62.j_common_ptr); cdecl;
procedure libjpeg_output_message(cinfo: LibJpeg62.j_common_ptr); cdecl;

implementation

procedure libjpeg_error_exit(cinfo: LibJpeg62.j_common_ptr); cdecl;
var
  Msg: AnsiString;
begin
  SetLength(Msg, 256);
  cinfo^.err^.format_message(cinfo, PAnsiChar(Msg));
  cinfo^.global_state := 0;
  LibJpeg62.jpeg_abort(cinfo);
  raise ELibJpegException.Create(
    'ERROR [' + IntToStr(cinfo^.err^.msg_code) + '] ' + string(PAnsiChar(Msg))
  );
end;

procedure libjpeg_output_message(cinfo: LibJpeg62.j_common_ptr); cdecl;
var
  Msg: AnsiString;
begin
  SetLength(Msg, 256);
  cinfo^.err^.format_message(cinfo, PAnsiChar(Msg));
  cinfo^.global_state := 0;
  raise ELibJpegException.Create(
    'OUTPUT [' + IntToStr(cinfo^.err^.msg_code) + '] ' + string(PAnsiChar(Msg))
  );
end;

end.
