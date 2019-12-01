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
  private
    jerr: jpeg_error_mgr;
    jdest: jpeg_destination_mgr;
    cinfo: jpeg_compress_struct;
  private
    FLibInitilized: Boolean;
    FUseBGRAColorSpace: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FQuality: Integer;
    FComMarker: AnsiString;
    FAppMarker: array [$E0..$EF] of TMemoryStream;
    FAppData: Pointer;
    FDataManager: TJpegDataManager;
    function InitCompress: Boolean;
    function DoCompress(
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
    function GetCompressStruct(out jpeg: j_compress_ptr): Boolean;
  end;

implementation

{ TJpegWriter }

constructor TJpegWriter.Create(AJpegDest: TStream; AUseBGRAColorSpace: Boolean);
var
  I: Integer;
begin
  FDataManager.FStream := AJpegDest;
  FUseBGRAColorSpace := AUseBGRAColorSpace;
  FWidth := -1;
  FHeight := -1;
  FQuality := 95;
  FComMarker := '';
  for I := Low(FAppMarker) to High(FAppMarker) do begin
    FAppMarker[I] := nil;
  end;
  FAppData := nil;
  FLibInitilized := InitCompress;
end;

destructor TJpegWriter.Destroy;
var
  I: Integer;
begin
  if FLibInitilized then begin
    jpeg_destroy_compress(@cinfo);
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
    Result := DoCompress(AWriteCallBack, nil);
  end;
end;

function TJpegWriter.Compress(AInPutStream: TStream): Boolean;
begin
  Result := False;
  if FLibInitilized then begin
    Result := DoCompress(nil, AInPutStream);
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
    cinfo.image_width := FWidth;
    cinfo.image_height := FHeight;
    if FUseBGRAColorSpace then begin
      {$IFDEF LIB_JPEG_62_TURBO_JCS_ALPHA_EXTENSIONS}
      cinfo.input_components := 4;
      cinfo.in_color_space := JCS_EXT_BGRA;
      {$ELSE}
      raise ELibJpegException.Create('Extended color spaces disabled by config!');
      {$ENDIF}
    end else begin
      cinfo.input_components := 3;
      cinfo.in_color_space := JCS_RGB;
    end;
    jpeg_set_defaults(@cinfo);
    jpeg_set_quality(@cinfo, FQuality, True);
    Result := cinfo.global_state <> 0;
  end;
end;

function TJpegWriter.WriteMarkers: Boolean;
var
  I: Integer;
begin
  if FComMarker <> '' then begin
    jpeg_write_marker(@cinfo, JPEG_COM, @FComMarker[1], Length(FComMarker));
  end;
  for I := Low(FAppMarker) to High(FAppMarker) do begin
    if Assigned(FAppMarker[I]) then begin
      FAppMarker[I].Position := 0;
      jpeg_write_marker(@cinfo, I, FAppMarker[I].Memory, FAppMarker[I].Size);
    end;
  end;
  Result := True;
end;

function TJpegWriter.InitCompress: Boolean;
begin
  {$IFNDEF LIB_JPEG_62_STATIC_LINK}
  if not InitLibJpeg62() then begin
    raise ELibJpegException.Create('LibJpeg62 initialization failed!');
  end;
  {$ENDIF}

  FillChar(jerr, SizeOf(jpeg_error_mgr), 0);
  FillChar(jdest, SizeOf(jpeg_destination_mgr), 0);
  FillChar(cinfo, SizeOf(jpeg_compress_struct), 0);

  cinfo.err := jpeg_std_error(@jerr);
  with cinfo.err^ do begin
    error_exit := libjpeg_error_exit;
    output_message := libjpeg_output_message;
  end;

  jpeg_create_compress(@cinfo);

  cinfo.dest := @jdest;
  with cinfo.dest^ do begin
    init_destination := libjpeg_init_destination;
    empty_output_buffer := libjpeg_empty_output_buffer;
    term_destination := libjpeg_term_destination;
  end;

  cinfo.client_data := @FDataManager;
  cinfo.global_state := CSTATE_START;

  Result := True;
end;

function TJpegWriter.DoCompress(
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
    jpeg_start_compress(@cinfo, True);
    try
      if WriteMarkers then begin
        VSize := cinfo.image_width * Cardinal(cinfo.input_components);

        if (Addr(AWriteCallBack) = nil) and Assigned(AInPutStream) then begin
          VStreamPos := AInPutStream.Position;
          if AInPutStream is TMemoryStream then begin
            VMemoryStream := AInPutStream as TMemoryStream;
          end else begin
            GetMem(VLine, VSize);
          end;
        end;

        try
          for I := 0 to cinfo.image_height - 1 do begin
            if cinfo.global_state = 0 then begin
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

            jpeg_write_scanlines(@cinfo, @VLine, 1);

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
      if cinfo.global_state <> 0 then begin
        jpeg_finish_compress(@cinfo);
      end;
    end;
    Result := not VAborted;
  end;
end;

{ TJpegWriterExtended }

function TJpegWriterExtended.GetCompressStruct(
  out jpeg: j_compress_ptr
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
  LibJpegWrite;

procedure WriterTest;
var
  VInPut: TFileStream;
  VOutPut: TMemoryStream;
  VJpegWriter: TJpegWriter;
begin
  VOutPut := TMemoryStream.Create;
  try
    VJpegWriter := TJpegWriter.Create(VOutPut, False);
    try
      VInPut := TFileStream.Create('Test.raw', fmOpenRead);
      try
        VInPut.Position := 0;

        // you must know the raw image resolution before start compression
        VJpegWriter.Width := 256;
        VJpegWriter.Height := 256;

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
