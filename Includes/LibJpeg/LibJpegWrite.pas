unit LibJpegWrite;

interface

{$INCLUDE LibJpeg.inc}

uses
  Classes,
  LibJpeg62,
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
    FUseBGRAColorSpace: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FQuality: Integer;
    FComMarker: AnsiString;
    FAppMarker: array [$E0..$EF] of TMemoryStream;
    FAppData: Pointer;
    jpeg62: LibJpeg62.jpeg_compress_struct;
    jpeg62_err: LibJpeg62.jpeg_error_mgr;
    function InitComp62: Boolean;
    function DoComp62(
      AWriteCallBack: TWriteScanLineCallBack = nil;
      AInPutStream: TStream = nil
    ): Boolean;
    function SetCompOptions: Boolean;
    function WriteMarkers: Boolean;
  public
    constructor Create(AJpegDest: TStream; AUseBGRAColorSpace: Boolean);
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
    function GetCompressStruct(out jpeg: LibJpeg62.j_compress_ptr): Boolean;
  end;

implementation

{ TJpegWriter }

constructor TJpegWriter.Create(AJpegDest: TStream; AUseBGRAColorSpace: Boolean);
var
  I: Integer;
begin
  FStream := AJpegDest;
  FUseBGRAColorSpace := AUseBGRAColorSpace;
  FWidth := -1;
  FHeight := -1;
  FQuality := 95;
  FComMarker := '';
  for I := Low(FAppMarker) to High(FAppMarker) do begin
    FAppMarker[I] := nil;
  end;
  FAppData := nil;
  FLibInitilized := InitComp62;
end;

destructor TJpegWriter.Destroy;
var
  I: Integer;
begin
  if FLibInitilized then begin
    LibJpeg62.jpeg_destroy_compress(@jpeg62);
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
    Result := DoComp62(AWriteCallBack, nil);
  end;
end;

function TJpegWriter.Compress(AInPutStream: TStream): Boolean;
begin
  Result := False;
  if FLibInitilized then begin
    Result := DoComp62(nil, AInPutStream);
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
    jpeg62.image_width := FWidth;
    jpeg62.image_height := FHeight;
    if FUseBGRAColorSpace then begin
      {$IFDEF LIB_JPEG_62_TURBO_JCS_ALPHA_EXTENSIONS}
      jpeg62.input_components := 4;
      jpeg62.in_color_space := LibJpeg62.JCS_EXT_BGRA;
      {$ELSE}
      raise ELibJpegException.Create('Extended color spaces disabled by config!');
      {$ENDIF}
    end else begin
      jpeg62.input_components := 3;
      jpeg62.in_color_space := LibJpeg62.JCS_RGB;
    end;
    LibJpeg62.jpeg_set_defaults(@jpeg62);
    LibJpeg62.jpeg_set_quality(@jpeg62, FQuality, True);
    Result := jpeg62.global_state <> 0;
  end;
end;

function TJpegWriter.WriteMarkers: Boolean;
var
  I: Integer;
begin
  if FComMarker <> '' then begin
    LibJpeg62.jpeg_write_marker(@jpeg62, LibJpeg62.JPEG_COM, @FComMarker[1], Length(FComMarker));
  end;
  for I := Low(FAppMarker) to High(FAppMarker) do begin
    if Assigned(FAppMarker[I]) then begin
      FAppMarker[I].Position := 0;
      LibJpeg62.jpeg_write_marker(@jpeg62, I, FAppMarker[I].Memory, FAppMarker[I].Size);
    end;
  end;
  Result := True;
end;

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

{ TJpegWriterExtended }

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

{******************************  Example: **************************************

uses
  ...
  LibJpegWrite;

procedure WriterTest;
var
  VInPut: TFileStream;
  VOutPut: TMemoryStream;
  VJpegWriter: TJpegWriter;
begin
  VOutPut := TMemoryStream.Create;
  try
    VJpegWriter := TJpegWriter.Create(VOutPut);
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
