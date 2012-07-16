unit LibJpegWrite;

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
  TWriteScanLineCallBack = function(
    Sender: TObject;
    ALineNumber: Integer;
    ALineSize: Cardinal;
    out Abort: Boolean
  ): PByte of object;

  TJpegWriter = class(TObject)
  protected
    FStream: TStream;
    FLibInitilized: Boolean;
    FUseLibJpeg8: Boolean;
    FUseBGRAColorSpace: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FQuality: Integer;
    FComMarker: AnsiString;
    FAppMarker: array [$E0..$EF] of TMemoryStream;
    FAppData: Pointer;
    {$IFDEF LIB_JPEG_62_SUPPORT}
    jpeg62: LibJpeg62.jpeg_compress_struct;
    jpeg62_err: LibJpeg62.jpeg_error_mgr;
    {$ENDIF}
    {$IFDEF LIB_JPEG_8_SUPPORT}
    jpeg8: LibJpeg8.jpeg_compress_struct;
    jpeg8_err: LibJpeg8.jpeg_error_mgr;
    {$ENDIF}
    {$IFDEF LIB_JPEG_62_SUPPORT}
    function InitComp62: Boolean;
    function DoComp62(
      AWriteCallBack: TWriteScanLineCallBack = nil;
      AInPutStream: TStream = nil
    ): Boolean;
    {$ENDIF}
    {$IFDEF LIB_JPEG_8_SUPPORT}
    function InitComp8: Boolean;
    function DoComp8(
      AWriteCallBack: TWriteScanLineCallBack = nil;
      AInPutStream: TStream = nil
    ): Boolean;
    {$ENDIF}
    function SetCompOptions: Boolean;
    function WriteMarkers: Boolean;
  public
    constructor Create(AJpegDest: TStream; AUseBGRAColorSpace: Boolean; AUseLibJpeg8: Boolean = False);
    destructor Destroy; override;
    function Compress(AWriteCallBack: TWriteScanLineCallBack): Boolean; overload;
    function Compress(AInPutStream: TStream): Boolean; overload;
    function AddMarker(AMarkerID: Byte; AData: Pointer; ASize: Integer): Boolean;
    function AddCommentMarker(AComment: AnsiString): Boolean;
    function AddExifMarker(AData: Pointer; ASize: Integer): Boolean; overload;
    function AddExifMarker(AData: TStream): Boolean; overload;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Quality: Integer read FQuality write FQuality;
    property AppData: Pointer read FAppData write FAppData;
  end;

  TJpegWriterExtended = class(TJpegWriter)
  public
    {$IFDEF LIB_JPEG_62_SUPPORT}
    function GetCompressStruct(out jpeg: LibJpeg62.j_compress_ptr): Boolean; {$IFDEF LIB_JPEG_8_SUPPORT} overload; {$ENDIF}
    {$ENDIF}
    {$IFDEF LIB_JPEG_8_SUPPORT}
    function GetCompressStruct(out jpeg: LibJpeg8.j_compress_ptr): Boolean; {$IFDEF LIB_JPEG_62_SUPPORT} overload; {$ENDIF}
    {$ENDIF}
  end;

implementation

{ TJpegWriter }

constructor TJpegWriter.Create(AJpegDest: TStream; AUseBGRAColorSpace: Boolean; AUseLibJpeg8: Boolean);
var
  I: Integer;
begin
  FStream := AJpegDest;
  FUseLibJpeg8 := AUseLibJpeg8;
  FUseBGRAColorSpace := AUseBGRAColorSpace;
  FWidth := -1;
  FHeight := -1;
  FQuality := 95;
  FComMarker := '';
  for I := Low(FAppMarker) to High(FAppMarker) do begin
    FAppMarker[I] := nil;
  end;
  FAppData := nil;
  if not FUseLibJpeg8 then begin
    {$IFDEF LIB_JPEG_62_SUPPORT}
    FLibInitilized := InitComp62;
    {$ELSE}
    raise ELibJpegException.Create('LibJpeg62 disabled by config!');
    {$ENDIF}
  end else begin
    {$IFDEF LIB_JPEG_8_SUPPORT}
    FLibInitilized := InitComp8;
    {$ELSE}
    raise ELibJpegException.Create('LibJpeg8 disabled by config!');
    {$ENDIF}
  end;
end;

destructor TJpegWriter.Destroy;
var
  I: Integer;
begin
  if FLibInitilized then begin
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
      LibJpeg62.jpeg_destroy_compress(@jpeg62);
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      LibJpeg8.jpeg_destroy_compress(@jpeg8);
      {$ENDIF}
    end;
  end;
  FComMarker := '';
  for I := Low(FAppMarker) to High(FAppMarker) do begin
    if Assigned(FAppMarker[I]) then begin
      FAppMarker[I].Free;
    end;
  end;
end;

function TJpegWriter.Compress(AWriteCallBack: TWriteScanLineCallBack): Boolean;
begin
  Result := False;
  if FLibInitilized then begin
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
      Result := DoComp62(AWriteCallBack, nil);
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      Result := DoComp8(AWriteCallBack, nil);
      {$ENDIF}
    end;
  end;
end;

function TJpegWriter.Compress(AInPutStream: TStream): Boolean;
begin
  Result := False;
  if FLibInitilized then begin
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
      Result := DoComp62(nil, AInPutStream);
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      Result := DoComp8(nil, AInPutStream);
      {$ENDIF}
    end;
  end;
end;

function TJpegWriter.AddMarker(AMarkerID: Byte; AData: Pointer; ASize: Integer): Boolean;
begin
  if AMarkerID = $FE then begin
    SetLength(FComMarker, ASize);
    Move(AData^, FComMarker[1], ASize);
    Result := True;
  end else if AMarkerID in [$E0..$EF] then begin
    if Assigned(FAppMarker[AMarkerID]) then begin
      FAppMarker[AMarkerID].Clear;
    end else begin
      FAppMarker[AMarkerID] := TMemoryStream.Create;
    end;
    FAppMarker[AMarkerID].WriteBuffer(AData^, ASize);
    FAppMarker[AMarkerID].Position := 0;
    Result := True;
  end else begin
    raise ELibJpegException.CreateFmt('Unknown marker ID: %d', [AMarkerID]);
  end;
end;

function TJpegWriter.AddCommentMarker(AComment: AnsiString): Boolean;
begin
  Result := AddMarker($FE, @AComment[1], Length(AComment));
end;

function TJpegWriter.AddExifMarker(AData: Pointer; ASize: Integer): Boolean;
begin
  Result := AddMarker($E1, AData, ASize);
end;

function TJpegWriter.AddExifMarker(AData: TStream): Boolean;
var
  VData: TMemoryStream;
  P: Pointer;
begin
  if AData is TMemoryStream then begin
    VData := AData as TMemoryStream;
    Result := AddMarker($E1, VData.Memory, VData.Size);
  end else begin
    GetMem(P, AData.Size);
    try
      AData.ReadBuffer(P^, AData.Size);
      Result := AddMarker($E1, P, AData.Size);
    finally
      FreeMem(P);
    end;
  end;
end;

function TJpegWriter.SetCompOptions: Boolean;
const
  JPG_MAX_HEIGHT = 65536;
  JPG_MAX_WIDTH  = 65536;
  COutPutResolutionIsTooBig =
    'OutPut resolution is too big for JPEG format!' + #13#10 +
    'OutPutWidht = %d (max = %d)' + #13#10 + 'OutPutHeight = %d (max = %d)';
begin
  Result := False;
  if FLibInitilized then begin
    if (FWidth >= JPG_MAX_WIDTH) or (FHeight >= JPG_MAX_HEIGHT) then begin
      raise ELibJpegException.CreateFmt(
        COutPutResolutionIsTooBig, [FWidth, JPG_MAX_WIDTH, FHeight, JPG_MAX_HEIGHT]
      );
    end;
    if (FWidth <= 0) or (FHeight <= 0) then begin
      raise ELibJpegException.Create('Set output resolution first!');
    end;
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
      jpeg62.image_width := FWidth;
      jpeg62.image_height := FHeight;
      jpeg62.input_components :=3;
      jpeg62.in_color_space := LibJpeg62.JCS_RGB;
      {$IFNDEF LIB_JPEG_62_TURBO_JCS_ALPHA_EXTENSIONS}
      if FUseBGRAColorSpace then begin
        raise ELibJpegException.Create('Extended color spaces disabled by config!');
      end;
      {$ELSE}
      if FUseBGRAColorSpace then begin
        jpeg62.input_components := 4;
        jpeg62.in_color_space := LibJpeg62.JCS_EXT_BGRA;
      end;
      {$ENDIF}
      LibJpeg62.jpeg_set_defaults(@jpeg62);
      LibJpeg62.jpeg_set_quality(@jpeg62, FQuality, True);
      Result := jpeg62.global_state <> 0;
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      jpeg8.image_width := FWidth;
      jpeg8.image_height := FHeight;
      jpeg8.input_components := 3;
      jpeg8.in_color_space := LibJpeg8.JCS_RGB;
      {$IFNDEF LIB_JPEG_8_TURBO_JCS_ALPHA_EXTENSIONS}
      if FUseBGRAColorSpace then begin
        raise ELibJpegException.Create('Extended color spaces disabled by config!');
      end;
      {$ELSE}
      if FUseBGRAColorSpace then begin
        jpeg8.input_components := 4;
        jpeg8.in_color_space := LibJpeg8.JCS_EXT_BGRA;
      end;
      {$ENDIF}
      LibJpeg8.jpeg_set_defaults(@jpeg8);
      LibJpeg8.jpeg_set_quality(@jpeg8, FQuality, True);
      Result := jpeg8.global_state <> 0;
      {$ENDIF}
    end;
  end;
end;

function TJpegWriter.WriteMarkers: Boolean;
var
  I: Integer;
begin
  if FComMarker <> '' then begin
    if not FUseLibJpeg8 then begin
      {$IFDEF LIB_JPEG_62_SUPPORT}
      LibJpeg62.jpeg_write_marker(@jpeg62, LibJpeg62.JPEG_COM, @FComMarker[1], Length(FComMarker));
      {$ENDIF}
    end else begin
      {$IFDEF LIB_JPEG_8_SUPPORT}
      LibJpeg8.jpeg_write_marker(@jpeg8, LibJpeg8.JPEG_COM, @FComMarker[1], Length(FComMarker));
      {$ENDIF}
    end;
  end;
  for I := Low(FAppMarker) to High(FAppMarker) do begin
    if Assigned(FAppMarker[I]) then begin
      FAppMarker[I].Position := 0;
      if not FUseLibJpeg8 then begin
        {$IFDEF LIB_JPEG_62_SUPPORT}
        LibJpeg62.jpeg_write_marker(@jpeg62, I, FAppMarker[I].Memory, FAppMarker[I].Size);
        {$ENDIF}
      end else begin
        {$IFDEF LIB_JPEG_8_SUPPORT}
        LibJpeg8.jpeg_write_marker(@jpeg8, I, FAppMarker[I].Memory, FAppMarker[I].Size);
        {$ENDIF}
      end;
    end;
  end;
  Result := True;
end;

{$IFDEF LIB_JPEG_62_SUPPORT}
function TJpegWriter.InitComp62: Boolean;
begin
  if {$IFNDEF LIB_JPEG_62_STATIC_LINK} InitLibJpeg62() {$ELSE} True {$ENDIF} then begin
    FillChar(jpeg62, SizeOf(LibJpeg62.jpeg_compress_struct), $00);
    FillChar(jpeg62_err, SizeOf(LibJpeg62.jpeg_error_mgr), $00);

    jpeg62.err := LibJpeg62.jpeg_std_error(@jpeg62_err);
    jpeg62_err.error_exit := libjpeg_error_exit;
    jpeg62_err.output_message := libjpeg_output_message;

    LibJpeg62.jpeg_create_compress(@jpeg62);

    jpeg62.dest := jpeg62.mem^.alloc_small(
      @jpeg62, JPOOL_PERMANENT, SizeOf(TJpeg62OutPutDataManager)
    );

    with PJpeg62OutPutDataManager(jpeg62.dest)^ do begin
      jpeg_dest_mgr.init_destination := libjpeg_init_destination;
      jpeg_dest_mgr.empty_output_buffer := libjpeg_empty_output_buffer;
      jpeg_dest_mgr.term_destination := libjpeg_term_destination;
    end;

    jpeg62.client_data := @FStream;

    jpeg62.global_state := LibJpeg62.CSTATE_START;

    Result := True;
  end else begin
    raise ELibJpegException.Create('LibJpeg62 init failed!');
  end;
end;

function TJpegWriter.DoComp62(
  AWriteCallBack: TWriteScanLineCallBack = nil;
  AInPutStream: TStream = nil
): Boolean;
var
  I: Integer;
  VLine: PByte;
  VSize: Integer;
  VAborted: Boolean;
  VStreamPos: Int64;
  VMemoryStream: TMemoryStream;
begin
  Result := False;
  VAborted := False;
  VStreamPos := 0;
  VMemoryStream := nil;
  VLine := nil;

  if SetCompOptions then begin
    LibJpeg62.jpeg_start_compress(@jpeg62, True);
    try
      if WriteMarkers then begin
        VSize := jpeg62.image_width * Cardinal(jpeg62.input_components);

        if (Addr(AWriteCallBack) = nil) and Assigned(AInPutStream) then begin
          VStreamPos := AInPutStream.Position;
          if AInPutStream is TMemoryStream then begin
            VMemoryStream := AInPutStream as TMemoryStream;
          end else begin
            GetMem(VLine, VSize);
          end;
        end;

        try
          for I := 0 to jpeg62.image_height - 1 do begin
            if jpeg62.global_state = 0 then begin
              VAborted := True;
              Break; // abort by exception
            end;

            if Addr(AWriteCallBack) <> nil then begin
              VLine := AWriteCallBack(Self, I, VSize, VAborted);
              if VAborted then begin
                Break; // abort by user
              end;
            end else if Assigned(VMemoryStream) then begin
              VLine := VMemoryStream.Memory;
              Inc(VLine, (VStreamPos + VSize * I));
            end else if Assigned(AInPutStream) then begin
              AInPutStream.Position := VStreamPos + VSize * I;
              AInPutStream.ReadBuffer(VLine^, VSize);
            end else begin
              raise ELibJpegException.Create('Input data not assigned!');
            end;

            LibJpeg62.jpeg_write_scanlines(@jpeg62, @VLine, 1);

          end;
        finally
          if (Addr(AWriteCallBack) = nil) and
              Assigned(AInPutStream) and
              not Assigned(VMemoryStream) then
          begin
            FreeMem(VLine);
          end;
        end;
      end;
    finally
      if jpeg62.global_state <> 0 then begin
        LibJpeg62.jpeg_finish_compress(@jpeg62);
      end;
    end;
    Result := not VAborted;
  end;
end;
{$ENDIF}

{$IFDEF LIB_JPEG_8_SUPPORT}
function TJpegWriter.InitComp8: Boolean;
begin
  if {$IFNDEF LIB_JPEG_8_STATIC_LINK} InitLibJpeg8() {$ELSE} True {$ENDIF} then begin
    FillChar(jpeg8, SizeOf(LibJpeg8.jpeg_compress_struct), $00);
    FillChar(jpeg8_err, SizeOf(LibJpeg8.jpeg_error_mgr), $00);

    jpeg8.err := LibJpeg8.jpeg_std_error(@jpeg8_err);
    jpeg8_err.error_exit := libjpeg_error_exit;
    jpeg8_err.output_message := libjpeg_output_message;

    LibJpeg8.jpeg_create_compress(@jpeg8);

    jpeg8.dest := jpeg8.mem^.alloc_small(
      @jpeg8, JPOOL_PERMANENT, SizeOf(TJpeg8OutPutDataManager)
    );

    with PJpeg8OutPutDataManager(jpeg8.dest)^ do begin
      jpeg_dest_mgr.init_destination := libjpeg_init_destination;
      jpeg_dest_mgr.empty_output_buffer := libjpeg_empty_output_buffer;
      jpeg_dest_mgr.term_destination := libjpeg_term_destination;
    end;

    jpeg8.client_data := @FStream;

    jpeg8.global_state := LibJpeg8.CSTATE_START;

    Result := True;
  end else begin
    raise ELibJpegException.Create('LibJpeg8 init failed!');
  end;
end;

function TJpegWriter.DoComp8(
  AWriteCallBack: TWriteScanLineCallBack = nil;
  AInPutStream: TStream = nil
): Boolean;
var
  I: Integer;
  VLine: PByte;
  VSize: Integer;
  VAborted: Boolean;
  VStreamPos: Int64;
  VMemoryStream: TMemoryStream;
begin
  Result := False;
  VAborted := False;
  VStreamPos := 0;
  VMemoryStream := nil;
  VLine := nil;

  if SetCompOptions() then begin
    LibJpeg8.jpeg_start_compress(@jpeg8, True);
    try
      if WriteMarkers then begin
        VSize := jpeg8.image_width * Cardinal(jpeg8.input_components);

        if (Addr(AWriteCallBack) = nil) and Assigned(AInPutStream) then begin
          VStreamPos := AInPutStream.Position;
          if AInPutStream is TMemoryStream then begin
            VMemoryStream := AInPutStream as TMemoryStream;
          end else begin
            GetMem(VLine, VSize);
          end;
        end;

        try
          for I := 0 to jpeg8.image_height - 1 do begin
            if jpeg8.global_state = 0 then begin
              VAborted := True;
              Break; // abort by exception
            end;

            if Addr(AWriteCallBack) <> nil then begin
              VLine := AWriteCallBack(Self, I, VSize, VAborted);
              if VAborted then begin
                Break; // abort by user
              end;
            end else if Assigned(VMemoryStream) then begin
              VLine := VMemoryStream.Memory;
              Inc(VLine, (VStreamPos + VSize * I));
            end else if Assigned(AInPutStream) then begin
              AInPutStream.Position := VStreamPos + VSize * I;
              AInPutStream.ReadBuffer(VLine^, VSize);
            end else begin
              raise ELibJpegException.Create('Input data not assigned!');
            end;

            LibJpeg8.jpeg_write_scanlines(@jpeg8, @VLine, 1);

          end;
        finally
          if (Addr(AWriteCallBack) = nil) and
              Assigned(AInPutStream) and
              not Assigned(VMemoryStream) then
          begin
            FreeMem(VLine);
          end;
        end;
      end;
    finally
      if jpeg8.global_state <> 0 then begin
        LibJpeg8.jpeg_finish_compress(@jpeg8);
      end;
    end;
    Result := not VAborted;
  end;
end;
{$ENDIF}

{ TJpegWriterExtended }

{$IFDEF LIB_JPEG_62_SUPPORT}
function TJpegWriterExtended.GetCompressStruct(
  out jpeg: LibJpeg62.j_compress_ptr
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
function TJpegWriterExtended.GetCompressStruct(
  out jpeg: LibJpeg8.j_compress_ptr
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
  LibJpegWrite;

procedure WriterTest;
const
  CUseJpeg8 = False;
var
  VInPut: TFileStream;
  VOutPut: TMemoryStream;
  VJpegWriter: TJpegWriter;
begin
  VOutPut := TMemoryStream.Create;
  try
    VJpegWriter := TJpegWriter.Create(VOutPut, CUseJpeg8);
    try
      VInPut := TFileStream.Create('Test.raw', fmOpenRead);
      try
        VJpegWriter.Width := 256;
        VJpegWriter.Height := 256;
        VInPut.Position := 0;
        if VJpegWriter.Compress(VInPut) then begin
          VOutPut.SaveToFile('TestFromRaw.jpg');
        end;
      finally
        VInPut.Free;
      end;
    finally
      VJpegWriter.Free;
    end;
  finally
    VOutPut.Free;
  end;
end;

********************************************************************************}

end.
