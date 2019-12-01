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
  private
    jerr: jpeg_error_mgr;
    jsrc: jpeg_source_mgr;
    cinfo: jpeg_decompress_struct;
  private
    FLibInitilized: Boolean;
    FUseBGRAColorSpace: Boolean;
    FJpegHeaderParsed: Boolean;
    FSaveMarkers: Boolean;
    FMarkersList: TJpegMarkersList;
    FComMarker: AnsiString;
    FExifMarker: TMemoryStream;
    FAppData: Pointer;
    FDataManager: TJpegDataManager;
    function InitDecompress: Boolean;
    function DoDecompress(
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
    function GetDecompressStruct(out jpeg: j_decompress_ptr): Boolean;
  end;

implementation

{ TJpegReader }

constructor TJpegReader.Create(AJpegSource: TStream; AUseBGRAColorSpace: Boolean);
begin
  inherited Create;
  FDataManager.FStream := AJpegSource;
  FUseBGRAColorSpace := AUseBGRAColorSpace;
  FJpegHeaderParsed := False;
  FSaveMarkers := False;
  SetLength(FMarkersList, 0);
  FComMarker := '';
  FExifMarker := nil;
  FAppData := nil;
  FLibInitilized := InitDecompress;
end;

destructor TJpegReader.Destroy;
var
  I: Integer;
begin
  if FLibInitilized then begin
    jpeg_destroy_decompress(@cinfo);
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
    Result := DoDecompress(AReadCallBack, nil);
  end;
end;

function TJpegReader.Decompress(AOutStream: TStream): Boolean;
begin
  Result := False;
  if FLibInitilized then begin
    Result := DoDecompress(nil, AOutStream);
  end;
end;

function TJpegReader.ReadHeader:Boolean;
var
  I: Integer;
  jpeg62_marker: jpeg_saved_marker_ptr;
begin
  if FLibInitilized and not FJpegHeaderParsed then begin
    if FSaveMarkers then begin
      jpeg_save_markers(@cinfo, JPEG_COM, $FFFF);
      for I := 0 to 15 do begin
        jpeg_save_markers(@cinfo, JPEG_APP0 + I, $FFFF);
      end;
    end;
    FJpegHeaderParsed := jpeg_read_header(@cinfo, True) > 0;
    if FSaveMarkers and FJpegHeaderParsed then begin
      jpeg62_marker := cinfo.marker_list;
      while jpeg62_marker <> nil do begin
        I := Length(FMarkersList);
        SetLength(FMarkersList, I + 1);
        FMarkersList[I].Size := jpeg62_marker.data_length;
        GetMem(FMarkersList[I].Data, FMarkersList[I].Size);
        Move(jpeg62_marker.data^, FMarkersList[I].Data^, FMarkersList[I].Size);
        FMarkersList[I].ID := jpeg62_marker.marker;

        if FMarkersList[I].ID = JPEG_COM then begin
          SetLength(FComMarker, FMarkersList[I].Size);
          Move(FMarkersList[I].Data^, FComMarker[1], FMarkersList[I].Size);
        end else if (FMarkersList[I].ID = JPEG_APP0 + 1) then begin
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
      Result := cinfo.image_width;
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
      Result := cinfo.image_height;
    end;
  end;
end;

function TJpegReader.InitDecompress: Boolean;
begin
  {$IFNDEF LIB_JPEG_62_STATIC_LINK}
  if not InitLibJpeg62 then begin
    raise ELibJpegException.Create('LibJpeg62 initialization failed!');
  end;
  {$ENDIF}

  FillChar(jerr, SizeOf(jpeg_error_mgr), 0);
  FillChar(jsrc, SizeOf(jpeg_source_mgr), 0);
  FillChar(cinfo, SizeOf(jpeg_decompress_struct), 0);

  cinfo.err := jpeg_std_error(@jerr);
  with cinfo.err^ do begin
    error_exit := libjpeg_error_exit;
    output_message := libjpeg_output_message;
  end;

  jpeg_create_decompress(@cinfo);

  cinfo.src := @jsrc;
  with cinfo.src^ do begin
    init_source := libjpeg_init_source;
    fill_input_buffer := libjpeg_fill_input_buffer;
    skip_input_data := libjpeg_skip_input_data;
    term_source := libjpeg_term_source;
  end;

  cinfo.client_data := @FDataManager;
  cinfo.global_state := DSTATE_START;

  Result := True;
end;

function TJpegReader.DoDecompress(
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
      cinfo.out_color_space := JCS_EXT_BGRA;
      {$ELSE}
      raise ELibJpegException.Create('Extended color spaces disabled by config!');
      {$ENDIF}
    end else begin
      cinfo.out_color_space := JCS_RGB;
    end;
    jpeg_start_decompress(@cinfo);
    try
      VSize := cinfo.output_width * Cardinal(cinfo.out_color_components);
      GetMem(VLine, VSize);
      try
        for I := 0 to cinfo.output_height - 1 do begin
          if cinfo.global_state = 0 then begin
            VAborted := True;
            Break; // abort by exception
          end;
          jpeg_read_scanlines(@cinfo, @VLine, 1);
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
      if cinfo.global_state <> 0 then begin
        jpeg_finish_decompress(@cinfo);
      end;
    end;
    Result := not VAborted;
  end;
end;

{ TJpegReaderExtended }

function TJpegReaderExtended.GetDecompressStruct(
  out jpeg: j_decompress_ptr
): Boolean;
begin
  if FLibInitilized then begin
    jpeg := @cinfo;
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

procedure ReaderTest;
var
  VStream: TFileStream;
  VMem: TMemoryStream;
  VJpegReader: TJpegReader;
begin
  VStream := TFileStream.Create('Test.jpg', fmOpenRead);
  try
    VJpegReader := TJpegReader.Create(VStream, False);
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
