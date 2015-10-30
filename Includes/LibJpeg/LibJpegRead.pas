unit LibJpegRead;

interface

{$INCLUDE LibJpeg.inc}

uses
  Classes,
  LibJpeg62,
  LibJpegErrorHandler,
  LibJpegInOutDataManager;

type
  TReadScanLineCallBack = function(
    Sender: TObject;
    ALine: PByte;
    ALineSize: Cardinal;
    ALineNumber: Integer;
    ABGRAColorSpace: Boolean
  ): Boolean of object;

  TJpegMarker = record
    ID: Byte;           // $FE: COM; $E0..$EF: APP0..APP15
    Data: Pointer;      //
    Size: Integer;      // Max Size = 64k
  end;

  TJpegMarkersList = array of TJpegMarker;

  TJpegReader = class(TObject)
  protected
    FStream: TStream;
    FLibInitilized: Boolean;
    FUseBGRAColorSpace: Boolean;
    FJpegHeaderParsed: Boolean;
    FSaveMarkers: Boolean;
    FMarkersList: TJpegMarkersList;
    FComMarker: AnsiString;
    FExifMarker: TMemoryStream;
    FAppData: Pointer;
    jpeg62: LibJpeg62.jpeg_decompress_struct;
    jpeg62_err: LibJpeg62.jpeg_error_mgr;
    function InitDecomp62: Boolean;
    function DoDecomp62(
      AReadCallBack: TReadScanLineCallBack = nil;
      AOutStream: TStream = nil
    ): Boolean;
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    constructor Create(AJpegSource: TStream; AUseBGRAColorSpace: Boolean);
    destructor Destroy; override;
    function ReadHeader: Boolean;
    function Decompress(AReadCallBack: TReadScanLineCallBack): Boolean; overload;
    function Decompress(AOutStream: TStream): Boolean; overload;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property SaveMarkers: Boolean read FSaveMarkers write FSaveMarkers;
    property MarkersList: TJpegMarkersList read FMarkersList;
    property CommentMarker: AnsiString read FComMarker;
    property ExifMarker: TMemoryStream read FExifMarker;
    property AppData: Pointer read FAppData write FAppData;
  end;

  TJpegReaderExtended = class(TJpegReader)
  public
    function GetDecompressStruct(out jpeg: LibJpeg62.j_decompress_ptr): Boolean;
  end;

implementation

{ TJpegReader }

constructor TJpegReader.Create(AJpegSource: TStream; AUseBGRAColorSpace: Boolean);
begin
  inherited Create;
  FStream := AJpegSource;
  FUseBGRAColorSpace := AUseBGRAColorSpace;
  FJpegHeaderParsed := False;
  FSaveMarkers := False;
  SetLength(FMarkersList, 0);
  FComMarker := '';
  FExifMarker := nil;
  FAppData := nil;
  FLibInitilized := InitDecomp62;
end;

destructor TJpegReader.Destroy;
var
  I: Integer;
begin
  if FLibInitilized then begin
    LibJpeg62.jpeg_destroy_decompress(@jpeg62);
  end;
  for I := Low(FMarkersList) to High(FMarkersList) do begin
    if FMarkersList[I].Data <> nil then begin
      FreeMem(FMarkersList[I].Data);
    end;
  end;
  SetLength(FMarkersList, 0);
  if Assigned(FExifMarker) then begin
    FExifMarker.Free;
  end;
  FComMarker := '';
  inherited Destroy;
end;

function TJpegReader.Decompress(AReadCallBack: TReadScanLineCallBack): Boolean;
begin
  Result := False;
  if FLibInitilized then begin
    Result := DoDecomp62(AReadCallBack, nil);
  end;
end;

function TJpegReader.Decompress(AOutStream: TStream): Boolean;
begin
  Result := False;
  if FLibInitilized then begin
    Result := DoDecomp62(nil, AOutStream);
  end;
end;

function TJpegReader.ReadHeader:Boolean;
var
  I: Integer;
  jpeg62_marker: LibJpeg62.jpeg_saved_marker_ptr;
begin
  if FLibInitilized and not FJpegHeaderParsed then begin
    if FSaveMarkers then begin
      LibJpeg62.jpeg_save_markers(@jpeg62, LibJpeg62.JPEG_COM, $FFFF);
      for I := 0 to 15 do begin
        LibJpeg62.jpeg_save_markers(@jpeg62, LibJpeg62.JPEG_APP0 + I, $FFFF);
      end;
    end;
    FJpegHeaderParsed := LibJpeg62.jpeg_read_header(@jpeg62, True) > 0;
    if FSaveMarkers and FJpegHeaderParsed then begin
      jpeg62_marker := jpeg62.marker_list;
      while jpeg62_marker <> nil do begin
        I := Length(FMarkersList);
        SetLength(FMarkersList, I + 1);
        FMarkersList[I].Size := jpeg62_marker.data_length;
        GetMem(FMarkersList[I].Data, FMarkersList[I].Size);
        Move(jpeg62_marker.data^, FMarkersList[I].Data^, FMarkersList[I].Size);
        FMarkersList[I].ID := jpeg62_marker.marker;

        if FMarkersList[I].ID = LibJpeg62.JPEG_COM then begin
          SetLength(FComMarker, FMarkersList[I].Size);
          Move(FMarkersList[I].Data^, FComMarker[1], FMarkersList[I].Size);
        end else if (FMarkersList[I].ID = LibJpeg62.JPEG_APP0 + 1) then begin
          if Assigned(FExifMarker) then begin
            FExifMarker.Clear;
          end else begin
            FExifMarker := TMemoryStream.Create;
          end;
          FExifMarker.WriteBuffer(FMarkersList[I].Data^, FMarkersList[I].Size);
          FExifMarker.Position := 0;
        end;

        jpeg62_marker := jpeg62_marker.next;
      end;
    end;
  end;
  Result := FJpegHeaderParsed;
end;

function TJpegReader.GetWidth: Integer;
begin
  Result := -1;
  if FLibInitilized then begin
    if not FJpegHeaderParsed then begin
      ReadHeader;
    end;
    if FJpegHeaderParsed then begin
      Result := jpeg62.image_width;
    end;
  end;
end;

function TJpegReader.GetHeight: Integer;
begin
  Result := -1;
  if FLibInitilized then begin
    if not FJpegHeaderParsed then begin
      ReadHeader;
    end;
    if FJpegHeaderParsed then begin
      Result := jpeg62.image_height;
    end;
  end;
end;

function TJpegReader.InitDecomp62: Boolean;
begin
  if {$IFNDEF LIB_JPEG_62_STATIC_LINK} InitLibJpeg62 {$ELSE} True {$ENDIF} then begin
    FillChar(jpeg62, SizeOf(LibJpeg62.jpeg_decompress_struct), $00);
    FillChar(jpeg62_err, SizeOf(LibJpeg62.jpeg_error_mgr), $00);

    jpeg62.err := LibJpeg62.jpeg_std_error(@jpeg62_err);
    jpeg62_err.error_exit := libjpeg_error_exit;
    jpeg62_err.output_message := libjpeg_output_message;

    LibJpeg62.jpeg_create_decompress(@jpeg62);

    jpeg62.src := jpeg62.mem^.alloc_small(
      @jpeg62, JPOOL_PERMANENT, SizeOf(TJpeg62InPutDataManager)
    );

    with PJpeg62InPutDataManager(jpeg62.src)^ do begin
      jpeg_src_mgr.init_source := libjpeg_init_source;
      jpeg_src_mgr.fill_input_buffer := libjpeg_fill_input_buffer;
      jpeg_src_mgr.skip_input_data := libjpeg_skip_input_data;
      jpeg_src_mgr.resync_to_restart := nil; // use default method
      jpeg_src_mgr.term_source := libjpeg_term_source;
    end;

    jpeg62.client_data := @FStream;

    jpeg62.global_state := LibJpeg62.DSTATE_START;

    Result := True;
  end else begin
    raise ELibJpegException.Create('LibJpeg62 init failed!');
  end;
end;

function TJpegReader.DoDecomp62(
  AReadCallBack: TReadScanLineCallBack = nil;
  AOutStream: TStream = nil
): Boolean;
var
  I: Integer;
  VLine: Pointer;
  VSize: Cardinal;
  VAborted: Boolean;
begin
  Result := False;
  VAborted := False;
  if ReadHeader then begin
    if FUseBGRAColorSpace then begin
      {$IFDEF LIB_JPEG_62_TURBO_JCS_ALPHA_EXTENSIONS}
      jpeg62.out_color_space := LibJpeg62.JCS_EXT_BGRA;
      {$ELSE}
      raise ELibJpegException.Create('Extended color spaces disabled by config!');
      {$ENDIF}
    end else begin
      jpeg62.out_color_space := LibJpeg62.JCS_RGB;
    end;
    LibJpeg62.jpeg_start_decompress(@jpeg62);
    try
      VSize := jpeg62.output_width * Cardinal(jpeg62.out_color_components);
      GetMem(VLine, VSize);
      try
        for I := 0 to jpeg62.output_height - 1 do begin
          if jpeg62.global_state = 0 then begin
            VAborted := True;
            Break; // abort by exception
          end;
          LibJpeg62.jpeg_read_scanlines(@jpeg62, @VLine, 1);
          if Addr(AReadCallBack) <> nil then begin
            if not AReadCallBack(Self, VLine, VSize, I, FUseBGRAColorSpace) then begin
              VAborted := True;
              Break; // abort by user
            end;
          end;
          if Assigned(AOutStream) then begin
            AOutStream.WriteBuffer(VLine^, VSize);
          end;
        end;
      finally
        FreeMem(VLine);
      end;
    finally
      if jpeg62.global_state <> 0 then begin
        LibJpeg62.jpeg_finish_decompress(@jpeg62);
      end;
    end;
    Result := not VAborted;
  end;
end;

{ TJpegReaderExtended }

function TJpegReaderExtended.GetDecompressStruct(
  out jpeg: LibJpeg62.j_decompress_ptr
): Boolean;
begin
  if FLibInitilized then begin
    jpeg := @jpeg62;
    Result := True;
  end else begin
    jpeg := nil;
    Result := False;
  end;
end;

{******************************  Example: **************************************

uses
  ...
  LibJpegRead;

  ...

var
  VStream: TFileStream;
  VMem: TMemoryStream;
  VJpegReader: TJpegReader;
begin
try
  VStream := TFileStream.Create('Test.jpg', fmOpenRead);
  try
    VJpegReader := TJpegReader.Create(VStream);
    try
      VMem := TMemoryStream.Create;
      try
        if VJpegReader.Decompress(VMem) then begin
          VMem.SaveToFile('Test.raw');
        end;
      finally
        VMem.Free;
      end;
    finally
      VJpegReader.Free;
    end;
  finally
    VStream.Free;
  end;
end;

********************************************************************************}

end.
