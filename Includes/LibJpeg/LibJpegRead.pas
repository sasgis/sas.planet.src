unit LibJpegRead;

interface

{$INCLUDE LibJpeg.inc}

{$IFNDEF LIB_JPEG_62_SUPPORT}
    {$IFNDEF LIB_JPEG_8_SUPPORT}
      {$DEFINE LIB_JPEG_62_SUPPORT}
    {$ENDIF}
{$ENDIF}

uses
  Classes,
  {$IFDEF LIB_JPEG_62_SUPPORT}
  LibJpeg62,
  {$ENDIF}
  {$IFDEF LIB_JPEG_8_SUPPORT}
  LibJpeg8,
  {$ENDIF}
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
    FUseLibJpeg8: Boolean;
    FUseBGRAColorSpace: Boolean;
    FJpegHeaderParsed: Boolean;
    FSaveMarkers: Boolean;
    FMarkersList: TJpegMarkersList;
    FComMarker: AnsiString;
    FExifMarker: TMemoryStream;
    FAppData: Pointer;
    {$IFDEF LIB_JPEG_62_SUPPORT}
    jpeg62: LibJpeg62.jpeg_decompress_struct;
    jpeg62_err: LibJpeg62.jpeg_error_mgr;
    {$ENDIF}
    {$IFDEF LIB_JPEG_8_SUPPORT}
    jpeg8: LibJpeg8.jpeg_decompress_struct;
    jpeg8_err: LibJpeg8.jpeg_error_mgr;
    {$ENDIF}
    {$IFDEF LIB_JPEG_62_SUPPORT}
    function InitDecomp62: Boolean;
    function DoDecomp62(
      AReadCallBack: TReadScanLineCallBack = nil;
      AOutStream: TStream = nil
    ): Boolean;
    {$ENDIF}
    {$IFDEF LIB_JPEG_8_SUPPORT}
    function InitDecomp8:Boolean;
    function DoDecomp8(
      AReadCallBack: TReadScanLineCallBack = nil;
      AOutStream: TStream = nil
    ): Boolean;
    {$ENDIF}
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    constructor Create(AJpegSource: TStream; AUseBGRAColorSpace: Boolean; AUseLibJpeg8: Boolean = False);
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
    {$IFDEF LIB_JPEG_62_SUPPORT}
    function GetDecompressStruct(out jpeg: LibJpeg62.j_decompress_ptr): Boolean; {$IFDEF LIB_JPEG_8_SUPPORT} overload; {$ENDIF}
    {$ENDIF}
    {$IFDEF LIB_JPEG_8_SUPPORT}
    function GetDecompressStruct(out jpeg: LibJpeg8.j_decompress_ptr): Boolean; {$IFDEF LIB_JPEG_62_SUPPORT} overload; {$ENDIF}
    {$ENDIF}
  end;

implementation

{ TJpegReader }

constructor TJpegReader.Create(AJpegSource: TStream; AUseBGRAColorSpace: Boolean; AUseLibJpeg8: Boolean);
begin
  inherited Create;
  FStream := AJpegSource;
  FUseLibJpeg8 := AUseLibJpeg8;
  FUseBGRAColorSpace := AUseBGRAColorSpace;
  FJpegHeaderParsed := False;
  FSaveMarkers := False;
  SetLength(FMarkersList, 0);
  FComMarker := '';
  FExifMarker := nil;
  FAppData := nil;
  if not FUseLibJpeg8 then begin
    {$IFDEF LIB_JPEG_62_SUPPORT}
    FLibInitilized := InitDecomp62;
    {$ELSE}
    raise ELibJpegException.Create('LibJpeg62 disabled by config!');
    {$ENDIF}
  end else begin
    {$IFDEF LIB_JPEG_8_SUPPORT}
    FLibInitilized := InitDecomp8;
    {$ELSE}
    raise ELibJpegException.Create('LibJpeg8 disabled by config!');
    {$ENDIF}
  end;
end;

destructor TJpegReader.Destroy;
var
  I: Integer;
begin
  if FLibInitilized then begin
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
      LibJpeg62.jpeg_destroy_decompress(@jpeg62);
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      LibJpeg8.jpeg_destroy_decompress(@jpeg8);
      {$ENDIF}
    end;
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
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
      Result := DoDecomp62(AReadCallBack, nil);
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      Result := DoDecomp8(AReadCallBack, nil);
      {$ENDIF}
    end;
  end;
end;

function TJpegReader.Decompress(AOutStream: TStream): Boolean;
begin
  Result := False;
  if FLibInitilized then begin
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
      Result := DoDecomp62(nil, AOutStream);
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      Result := DoDecomp8(nil, AOutStream);
      {$ENDIF}
    end;
  end;
end;

function TJpegReader.ReadHeader:Boolean;
var
  I: Integer;
  {$IFDEF LIB_JPEG_62_SUPPORT}
  jpeg62_marker: LibJpeg62.jpeg_saved_marker_ptr;
  {$ENDIF}
  {$IFDEF LIB_JPEG_8_SUPPORT}
  jpeg8_marker: LibJpeg8.jpeg_saved_marker_ptr;
  {$ENDIF}
begin
  if FLibInitilized and not FJpegHeaderParsed then begin
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
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
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      if FSaveMarkers then begin
        LibJpeg8.jpeg_save_markers(@jpeg8, LibJpeg8.JPEG_COM, $FFFF);
        for I := 0 to 15 do begin
          LibJpeg8.jpeg_save_markers(@jpeg8, LibJpeg8.JPEG_APP0 + I, $FFFF);
        end;
      end;
      FJpegHeaderParsed := LibJpeg8.jpeg_read_header(@jpeg8, True) > 0;
      if FSaveMarkers and FJpegHeaderParsed then begin
        jpeg8_marker := jpeg8.marker_list;
        while jpeg8_marker <> nil do begin
          I := Length(FMarkersList);
          SetLength(FMarkersList, I + 1);
          FMarkersList[I].Size := jpeg8_marker.data_length;
          GetMem(FMarkersList[I].Data, FMarkersList[I].Size);
          Move(jpeg8_marker.data^, FMarkersList[I].Data^, FMarkersList[I].Size);
          FMarkersList[I].ID := jpeg8_marker.marker;

          if FMarkersList[I].ID = LibJpeg8.JPEG_COM then begin
            SetLength(FComMarker, FMarkersList[I].Size);
            Move(FMarkersList[I].Data^, FComMarker[1], FMarkersList[I].Size);
          end else if (FMarkersList[I].ID = LibJpeg8.JPEG_APP0 + 1) then begin
            if Assigned(FExifMarker) then begin
              FExifMarker.Clear;
            end else begin
              FExifMarker := TMemoryStream.Create;
            end;
            FExifMarker.WriteBuffer(FMarkersList[I].Data^, FMarkersList[I].Size);
            FExifMarker.Position := 0;
          end;

          jpeg8_marker := jpeg8_marker.next;
        end;
      end;
      {$ENDIF}
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
      if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
        Result := jpeg62.image_width;
      {$ENDIF}
      end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
        Result := jpeg8.image_width;
      {$ENDIF}
      end;
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
      if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
        Result := jpeg62.image_height;
      {$ENDIF}
      end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
        Result := jpeg8.image_height;
      {$ENDIF}
      end;
    end;
  end;
end;

{$IFDEF LIB_JPEG_62_SUPPORT}
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
    jpeg62.out_color_space := LibJpeg62.JCS_RGB;
    {$IFNDEF LIB_JPEG_62_TURBO_JCS_ALPHA_EXTENSIONS}
    if FUseBGRAColorSpace then begin
      raise ELibJpegException.Create('Extended color spaces disabled by config!');
    end;
    {$ELSE}
    if FUseBGRAColorSpace then begin
      jpeg62.out_color_space := LibJpeg62.JCS_EXT_BGRA;
    end;
    {$ENDIF}
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
{$ENDIF}

{$IFDEF LIB_JPEG_8_SUPPORT}
function TJpegReader.InitDecomp8:Boolean;
begin
  if {$IFNDEF LIB_JPEG_8_STATIC_LINK} InitLibJpeg8 {$ELSE} True {$ENDIF} then begin
    FillChar(jpeg8, SizeOf(LibJpeg8.jpeg_decompress_struct), $00);
    FillChar(jpeg8_err, SizeOf(LibJpeg8.jpeg_error_mgr), $00);

    jpeg8.err := LibJpeg8.jpeg_std_error(@jpeg8_err);
    jpeg8_err.error_exit := libjpeg_error_exit;
    jpeg8_err.output_message := libjpeg_output_message;

    LibJpeg8.jpeg_create_decompress(@jpeg8);

    jpeg8.src := jpeg8.mem^.alloc_small(
      @jpeg8, JPOOL_PERMANENT, SizeOf(TJpeg8InPutDataManager)
    );

    with PJpeg8InPutDataManager(jpeg8.src)^ do begin
      jpeg_src_mgr.init_source := libjpeg_init_source;
      jpeg_src_mgr.fill_input_buffer := libjpeg_fill_input_buffer;
      jpeg_src_mgr.skip_input_data := libjpeg_skip_input_data;
      jpeg_src_mgr.resync_to_restart := nil; // use default method
      jpeg_src_mgr.term_source := libjpeg_term_source;
    end;

    jpeg8.client_data := @FStream;

    jpeg8.global_state := LibJpeg8.DSTATE_START;

    Result := True;
  end else begin
    raise ELibJpegException.Create('LibJpeg8 init failed!');
  end;
end;

function TJpegReader.DoDecomp8(
  AReadCallBack: TReadScanLineCallBack = nil;
  AOutStream: TStream = nil
): Boolean;
var
  I: Integer;
  VLine: Pointer;
  VSize: Cardinal;
  VAborted: Boolean;
  VBGRAColorSpace: Boolean;
begin
  Result := False;
  VAborted := False;
  if ReadHeader then begin
    jpeg8.out_color_space := LibJpeg8.JCS_RGB;
    {$IFNDEF LIB_JPEG_8_TURBO_JCS_ALPHA_EXTENSIONS}
    if FUseBGRAColorSpace then begin
      raise ELibJpegException.Create('Extended color spaces disabled by config!');
    end;
    {$ELSE}
    if FUseBGRAColorSpace then begin
      jpeg8.out_color_space := LibJpeg8.JCS_EXT_BGRA;
    end;
    {$ENDIF}
    LibJpeg8.jpeg_start_decompress(@jpeg8);
    try 
      VSize := jpeg8.output_width * Cardinal(jpeg8.out_color_components);
      GetMem(VLine, VSize);
      try
        for I := 0 to jpeg8.output_height - 1 do begin
          if jpeg8.global_state = 0 then begin
            VAborted := True;
            Break; // abort by exception
          end;
          LibJpeg8.jpeg_read_scanlines(@jpeg8, @VLine, 1);
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
      if jpeg8.global_state <> 0 then begin
        LibJpeg8.jpeg_finish_decompress(@jpeg8);
      end;
    end;
    Result := not VAborted;
  end;
end;
{$ENDIF}

{ TJpegReaderExtended }

{$IFDEF LIB_JPEG_62_SUPPORT}
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
{$ENDIF}

{$IFDEF LIB_JPEG_8_SUPPORT}
function TJpegReaderExtended.GetDecompressStruct(
  out jpeg: LibJpeg8.j_decompress_ptr
): Boolean;
begin
  if FLibInitilized then begin
    jpeg := @jpeg8;
    Result := True;
  end else begin
    jpeg := nil;
    Result := False;
  end;
end;
{$ENDIF}

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
