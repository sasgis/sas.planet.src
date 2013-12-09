unit u_ThreadMapCombinePNG;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GR32,
  LibPNG,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_LocalCoordConverter,
  i_VectorItemLonLat,
  i_LocalCoordConverterFactorySimpe,
  u_ThreadMapCombineBase;

type
  TThreadMapCombinePNG = class(TThreadMapCombineBase)
  private
    FBgColor: TColor32;
    FWithAlpha: Boolean;
  protected
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverter: ILocalCoordConverter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe
    ); override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: IGeometryLonLatMultiPolygon;
      const ATargetConverter: ILocalCoordConverter;
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMapCalibrationList: IMapCalibrationList;
      const AFileName: string;
      const ASplitCount: TPoint;
      ABgColor: TColor32;
      AWithAlpha: Boolean
    );
  end;

implementation

uses
  gnugettext,
  i_CoordConverter,
  i_ImageLineProvider,
  u_ImageLineProvider,
  u_ResStrings;

type
  sas_png_rw_io_ptr = ^sas_png_rw_io;

  sas_png_rw_io = record
    DestStream: TFileStream;
    DestBuffer: Pointer;
    DestBufferSize: Cardinal;
    BufferedDataSize: Cardinal;
  end;

procedure flash_dest_buffer(rw_io_ptr: sas_png_rw_io_ptr);
begin
  if rw_io_ptr^.BufferedDataSize > 0 then begin
    rw_io_ptr^.DestStream.WriteBuffer(rw_io_ptr^.DestBuffer^, rw_io_ptr^.BufferedDataSize);
    rw_io_ptr^.BufferedDataSize := 0;
  end;
end;

procedure sas_png_write_data(
  png_ptr: png_structp;
  data: png_bytep;
  data_length: png_size_t
); cdecl;
var
  rw_io_ptr: sas_png_rw_io_ptr;
begin
  rw_io_ptr := sas_png_rw_io_ptr(png_ptr.io_ptr);
  if data_length >= rw_io_ptr^.DestBufferSize then begin // buffer is too small
    flash_dest_buffer(rw_io_ptr);
    rw_io_ptr^.DestStream.WriteBuffer(data^, data_length);
  end else if (rw_io_ptr^.BufferedDataSize + data_length) >= rw_io_ptr^.DestBufferSize then begin // buffer is full
    flash_dest_buffer(rw_io_ptr);
    CopyMemory(Pointer(Cardinal(rw_io_ptr^.DestBuffer) + rw_io_ptr^.BufferedDataSize), data, data_length);
    Inc(rw_io_ptr^.BufferedDataSize, data_length);
  end else begin // (rw_io_ptr^.BufferedDataSize + data_length) < rw_io_ptr^.DestBufferSize  // buffer is OK
    CopyMemory(Pointer(Cardinal(rw_io_ptr^.DestBuffer) + rw_io_ptr^.BufferedDataSize), data, data_length);
    Inc(rw_io_ptr^.BufferedDataSize, data_length);
  end;
end;

{ TThreadMapCombinePNG }

constructor TThreadMapCombinePNG.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatMultiPolygon;
  const ATargetConverter: ILocalCoordConverter;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMapCalibrationList: IMapCalibrationList;
  const AFileName: string;
  const ASplitCount: TPoint;
  ABgColor: TColor32;
  AWithAlpha: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    ATargetConverter,
    AImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    ASplitCount,
    Self.ClassName
  );
  FBgColor := ABgColor;
  FWithAlpha := AWithAlpha;
end;

procedure TThreadMapCombinePNG.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe
);
const
  PNG_MAX_HEIGHT = 65536;
  PNG_MAX_WIDTH = 65536;
var
  i: integer;
  png_ptr: png_structp;
  info_ptr: png_infop;
  rw_io: sas_png_rw_io;
  VPngColorType: Integer;
  VCurrentPieceRect: TRect;
  VGeoConverter: ICoordConverter;
  VMapPieceSize: TPoint;
  VLineProvider: IImageLineProvider;
  VLineRGB: Pointer;
begin
  VGeoConverter := ALocalConverter.GeoConverter;
  VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
  VMapPieceSize := ALocalConverter.GetLocalRectSize;
  if FWithAlpha then begin
    VPngColorType := PNG_COLOR_TYPE_RGB_ALPHA;
    VLineProvider :=
      TImageLineProviderRGBA.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
  end else begin
    VPngColorType := PNG_COLOR_TYPE_RGB;
    VLineProvider :=
      TImageLineProviderRGB.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
  end;

  if (VMapPieceSize.X >= PNG_MAX_WIDTH) or (VMapPieceSize.Y >= PNG_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(SAS_ERR_ImageIsTooBig, ['PNG', VMapPieceSize.X, PNG_MAX_WIDTH, VMapPieceSize.Y, PNG_MAX_HEIGHT, 'PNG']);
  end;

  if not Init_LibPNG then begin
    raise Exception.Create(_('Initialization of LibPNG failed.'));
  end;

  rw_io.DestStream := TFileStream.Create(AFileName, fmCreate);
  rw_io.DestBufferSize := 64 * 1024; // 64k
  GetMem(rw_io.DestBuffer, rw_io.DestBufferSize);
  rw_io.BufferedDataSize := 0;
  try
    png_ptr := png_create_write_struct(PNG_LIBPNG_VER_STRING, nil, nil, nil);
    if Assigned(png_ptr) then begin
      info_ptr := png_create_info_struct(png_ptr);
    end else begin
      raise Exception.Create(_('LibPNG: Failed to Create PngStruct!'));
    end;
    if Assigned(info_ptr) then begin
      try
        png_set_write_fn(png_ptr, @rw_io, @sas_png_write_data, nil);

        // Write header (8 bit colour depth)
        png_set_IHDR(png_ptr, info_ptr, VMapPieceSize.X, VMapPieceSize.Y, 8, VPngColorType,
          PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

        png_write_info(png_ptr, info_ptr);
        try
          for i := 0 to info_ptr.height - 1 do begin
            VLineRGB := VLineProvider.GetLine(AOperationID, ACancelNotifier, i);
            if VLineRGB <> nil then begin
              // write row
              png_write_row(png_ptr, VLineRGB);
            end;

            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              Break;
            end;
            if i mod 256 = 0 then begin
              ProgressFormUpdateOnProgress(i / info_ptr.height);
            end;
          end;
        finally
          // End write
          png_write_end(png_ptr, info_ptr);
        end;
      finally
        png_free_data(png_ptr, info_ptr, PNG_FREE_ALL);

        png_destroy_write_struct(@png_ptr, @info_ptr);
      end;
    end;
  finally
    flash_dest_buffer(@rw_io);
    FreeMem(rw_io.DestBuffer);
    rw_io.DestStream.Free;
  end;
end;

end.
