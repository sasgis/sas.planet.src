unit u_ThreadMapCombineJPG;

interface

uses
  Types,
  SysUtils,
  Classes,
  GR32,
  i_OperationNotifier,
  i_GlobalViewMainConfig,
  i_BitmapLayerProvider,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  u_MapType,
  i_BitmapPostProcessingConfig,
  u_ResStrings,
  u_ThreadMapCombineBase,
  libJPEG;

type
  TThreadMapCombineJPG = class(TThreadMapCombineBase)
  private
    FQuality: Integer;
  protected
    procedure SaveRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AFileName: string;
      AImageProvider: IBitmapLayerProvider;
      ALocalConverter: ILocalCoordConverter;
      AConverterFactory: ILocalCoordConverterFactorySimpe
    ); override;
  public
    constructor Create(
      APolygon: ILonLatPolygon;
      ATargetConverter: ILocalCoordConverter;
      AImageProvider: IBitmapLayerProvider;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      ASplitCount: TPoint;
      AQuality: Integer
    );
  end;

implementation

uses
  gnugettext,
  i_CoordConverter,
  i_ImageLineProvider,
  u_ImageLineProvider;

type
  my_dest_mgr_ptr = ^my_dest_mgr;
  my_dest_mgr = record
    pub: jpeg_destination_mgr;
    DestStream: TStream;
    DestBuffer: array [1..4096] of byte;
  end;

procedure error_exit(cinfo: j_common_ptr); cdecl; forward;
procedure output_message(cinfo: j_common_ptr); cdecl; forward;
procedure init_destination(cinfo: j_compress_ptr); cdecl; forward;
function empty_output_buffer(cinfo: j_compress_ptr): boolean; cdecl; forward;
procedure term_destination(cinfo: j_compress_ptr); cdecl; forward;

{ TThreadMapCombineJPG }

constructor TThreadMapCombineJPG.Create(
  APolygon: ILonLatPolygon;
  ATargetConverter: ILocalCoordConverter;
  AImageProvider: IBitmapLayerProvider;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  ASplitCount: TPoint;
  AQuality: Integer
);
begin
  inherited Create(
    APolygon,
    ATargetConverter,
    AImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    ASplitCount
  );
  FQuality := AQuality;
end;

procedure TThreadMapCombineJPG.SaveRect(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  AFileName: string;
  AImageProvider: IBitmapLayerProvider;
  ALocalConverter: ILocalCoordConverter;
  AConverterFactory: ILocalCoordConverterFactorySimpe
);
const
  JPG_MAX_HEIGHT = 65536;
  JPG_MAX_WIDTH = 65536;
var
  i: integer;
  jpeg: jpeg_compress_struct;
  jpeg_err: jpeg_error_mgr;
  VComment: string;
  VStream: TFileStream;
  VCurrentPieceRect: TRect;
  VGeoConverter: ICoordConverter;
  VMapPieceSize: TPoint;
  VLineProvider: IImageLineProvider;
  VLineRGB: Pointer;
begin
  VGeoConverter := ALocalConverter.GeoConverter;
  VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
  VMapPieceSize := ALocalConverter.GetLocalRectSize;
  VLineProvider :=
    TImageLineProviderRGB.Create(
      AImageProvider,
      ALocalConverter,
      AConverterFactory
    );
  if (VMapPieceSize.X >= JPG_MAX_WIDTH) or (VMapPieceSize.Y >= JPG_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(SAS_ERR_ImageIsTooBig, ['JPG', VMapPieceSize.X, JPG_MAX_WIDTH, VMapPieceSize.Y, JPG_MAX_HEIGHT, 'JPG']);
  end;

  if not init_libJPEG then begin
    raise Exception.Create( _('Initialization of LibJPEG failed.') );
  end;

  VStream := TFileStream.Create(AFileName, fmCreate);

  try
    FillChar(jpeg, SizeOf(jpeg_compress_struct), $00);
    FillChar(jpeg_err, SizeOf(jpeg_error_mgr), $00);

    // error managment
    jpeg.err := jpeg_std_error(@jpeg_err);
    jpeg_err.error_exit := error_exit;
    jpeg_err.output_message := output_message;

    // compression struct
    jpeg_create_compress(@jpeg);
    try
      if jpeg.dest = nil then begin

        // allocation space for streaming methods
        jpeg.dest := jpeg.mem^.alloc_small(@jpeg, JPOOL_PERMANENT, SizeOf(my_dest_mgr));

        // seeting up custom functions
        with my_dest_mgr_ptr(jpeg.dest)^ do begin
          pub.init_destination    := init_destination;
          pub.empty_output_buffer := empty_output_buffer;
          pub.term_destination    := term_destination;

          pub.next_output_byte    := @DestBuffer[1];
          pub.free_in_buffer      := Length(DestBuffer);

          DestStream := VStream;
        end;
      end;

      // very important state
      jpeg.global_state := CSTATE_START;

      jpeg.image_width := VMapPieceSize.X;
      jpeg.image_height := VMapPieceSize.Y;
      jpeg.input_components := 3;
      jpeg.in_color_space := JCS_RGB;

      // setting defaults
      jpeg_set_defaults(@jpeg);

      // compression quality
      jpeg_set_quality(@jpeg, FQuality, True);

      // start compression
      jpeg_start_compress(@jpeg, true);

      // write marker (comment)
      VComment := 'Created with SAS.Planet and libjpeg-turbo' + #0;
      jpeg_write_marker(@jpeg, JPEG_COM, @VComment[1], length(VComment));

      for i := 0 to jpeg.image_height - 1 do begin
        if jpeg.global_state = 0 then begin
          Break;
        end;
        VLineRGB := VLineProvider.GetLine(AOperationID, ACancelNotifier, i);
        if VLineRGB <> nil then begin
          // write row
          jpeg_write_scanlines(@jpeg, @VLineRGB, 1);
        end;
        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          Break;
        end;
      end;
    finally
      if jpeg.global_state <> 0 then begin
        // finish compression
        jpeg_finish_compress(@jpeg);
      end;

      // destroy compression
      jpeg_destroy_compress(@jpeg);
    end;
  finally
    VStream.Free;
  end;
end;

procedure error_exit(cinfo: j_common_ptr);
var
  Msg: String;
  ErrCode: Integer;
begin
  SetLength(Msg, 256);
  cinfo^.err^.format_message(cinfo, PChar(Msg));
  ErrCode := cinfo^.err^.msg_code;
  cinfo^.global_state := 0;
  jpeg_abort(cinfo);
  raise Exception.Create('LibJPEG: ERROR [' + IntToStr(ErrCode) + '] ' + PChar(Msg) );
end;

procedure output_message(cinfo: j_common_ptr);
var
  Msg: String;
  ErrCode: Integer;
begin
  SetLength(Msg, 256);
  cinfo^.err^.format_message(cinfo, PChar(Msg));
  ErrCode := cinfo^.err^.msg_code;
  cinfo^.global_state := 0;
  raise Exception.Create('LibJPEG: OUTPUT [' + IntToStr(ErrCode) + '] ' + PChar(Msg) );
end;

procedure init_destination(cinfo: j_compress_ptr);
begin
//
end;

function empty_output_buffer(cinfo: j_compress_ptr): boolean;
var
  dest: my_dest_mgr_ptr;
begin
  dest := my_dest_mgr_ptr(cinfo^.dest);
  if dest^.pub.free_in_buffer < Cardinal(Length(dest^.DestBuffer)) then begin
    // write complete buffer
    dest^.DestStream.Write(dest^.DestBuffer[1], SizeOf(dest^.DestBuffer));
    // reset buffer
    dest^.pub.next_output_byte := @dest^.DestBuffer[1];
    dest^.pub.free_in_buffer := Length(dest^.DestBuffer);
  end;
  Result := True;
end;

procedure term_destination(cinfo: j_compress_ptr);
var
  Idx: Integer;
  dest: my_dest_mgr_ptr;
begin
  dest := my_dest_mgr_ptr(cinfo^.dest);
  for Idx := low(dest^.DestBuffer) to High(dest^.DestBuffer) do begin
    // check for endblock
    if (dest^.DestBuffer[Idx] = $FF) and (dest^.DestBuffer[Idx +1] = JPEG_EOI) then begin
      // write endblock
      dest^.DestStream.Write(dest^.DestBuffer[Idx], 2);
      // leave
      Break;
    end else begin
      dest^.DestStream.Write(dest^.DestBuffer[Idx], 1);
    end;
  end;
end;

end.
