unit LibJpegErrorHandler;

interface

{$INCLUDE LibJpeg.inc}

{$IFNDEF LIB_JPEG_62_SUPPORT}
    {$IFNDEF LIB_JPEG_8_SUPPORT}
      {$DEFINE LIB_JPEG_62_SUPPORT}
    {$ENDIF}
{$ENDIF}

uses
  SysUtils
  {$IFDEF LIB_JPEG_62_SUPPORT}
  ,LibJpeg62
  {$ENDIF}
  {$IFDEF LIB_JPEG_8_SUPPORT}
  ,LibJpeg8
  {$ENDIF}
  ;

type
  ELibJpegException = class(Exception);

{$IFDEF LIB_JPEG_62_SUPPORT}
procedure libjpeg_error_exit(cinfo: LibJpeg62.j_common_ptr); cdecl; {$IFDEF LIB_JPEG_8_SUPPORT} overload; {$ENDIF}
procedure libjpeg_output_message(cinfo: LibJpeg62.j_common_ptr); cdecl; {$IFDEF LIB_JPEG_8_SUPPORT} overload; {$ENDIF}
{$ENDIF}

{$IFDEF LIB_JPEG_8_SUPPORT}
procedure libjpeg_error_exit(cinfo: LibJpeg8.j_common_ptr); cdecl; {$IFDEF LIB_JPEG_62_SUPPORT} overload; {$ENDIF}
procedure libjpeg_output_message(cinfo: LibJpeg8.j_common_ptr); cdecl; {$IFDEF LIB_JPEG_62_SUPPORT} overload; {$ENDIF}
{$ENDIF}

implementation

{$IFDEF LIB_JPEG_62_SUPPORT}
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
{$ENDIF}

{$IFDEF LIB_JPEG_8_SUPPORT}
procedure libjpeg_error_exit(cinfo: LibJpeg8.j_common_ptr); cdecl;
var
  Msg: AnsiString;
begin
  SetLength(Msg, 256);
  cinfo^.err^.format_message(cinfo, PAnsiChar(Msg));
  cinfo^.global_state := 0;
  LibJpeg8.jpeg_abort(cinfo);
  raise ELibJpegException.Create(
    'ERROR [' + IntToStr(cinfo^.err^.msg_code) + '] ' + string(PAnsiChar(Msg))
  );
end;

procedure libjpeg_output_message(cinfo: LibJpeg8.j_common_ptr); cdecl;
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
{$ENDIF}

end.
