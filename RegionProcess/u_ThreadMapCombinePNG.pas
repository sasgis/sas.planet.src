unit u_ThreadMapCombinePNG;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  LibPNG,
  i_GlobalViewMainConfig,
  i_BitmapLayerProvider,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_LocalCoordConverterFactorySimpe,
  u_MapType,
  i_BitmapPostProcessingConfig,
  u_ResStrings,
  u_ThreadMapCombineBase;

type
  TThreadMapCombinePNG = class(TThreadMapCombineBase)
  private
    FWithAlpha: Boolean;
    function AllocateArray255(AWidth: Cardinal): Pointer;
    procedure FreeArray255(APArray: Pointer);
  protected
    procedure SaveRect; override;
  public
    constructor Create(
      AViewConfig: IGlobalViewMainConfig;
      AMarksImageProvider: IBitmapLayerProvider;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: ILonLatPolygon;
      AProjectedPolygon: IProjectedPolygon;
      ASplitCount: TPoint;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AWithAlpha: Boolean
    );
  end;

implementation

uses
  gnugettext;

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

procedure sas_png_write_data(png_ptr: png_structp; data: png_bytep;
  data_length: png_size_t); cdecl;
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
  AViewConfig: IGlobalViewMainConfig;
  AMarksImageProvider: IBitmapLayerProvider;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: ILonLatPolygon;
  AProjectedPolygon: IProjectedPolygon;
  ASplitCount: TPoint;
  Atypemap: TMapType;
  AHtypemap: TMapType;
  AusedReColor: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AWithAlpha: Boolean
);
begin
  inherited Create(
    AViewConfig,
    AMarksImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    APolygon,
    AProjectedPolygon,
    ASplitCount,
    Atypemap,
    AHtypemap,
    AusedReColor,
    ARecolorConfig
  );
  FWithAlpha := AWithAlpha;
end;

function TThreadMapCombinePNG.AllocateArray255(AWidth: Cardinal): Pointer;
var
  i: Integer;
begin
  if FWithAlpha then begin
    GetMem(FArray256ABGR, 256 * sizeof(P256ArrayABGR));
    for i := 0 to 255 do begin
      GetMem(FArray256ABGR[i], (AWidth + 1) * 4);
    end;
    Result := FArray256ABGR;
  end else begin
    GetMem(FArray256BGR, 256 * sizeof(P256ArrayBGR));
    for i := 0 to 255 do begin
      GetMem(FArray256BGR[i], (AWidth + 1) * 3);
    end;
    Result := FArray256BGR;
  end;
end;

procedure TThreadMapCombinePNG.FreeArray255(APArray: Pointer);
var
  i: Integer;
begin
  if FWithAlpha then begin
    for i := 0 to 255 do begin
      FreeMem(FArray256ABGR[i]);
    end;
    FreeMem(FArray256ABGR);
  end else begin
    for i := 0 to 255 do begin
      FreeMem(FArray256BGR[i]);
    end;
    FreeMem(FArray256BGR);
  end;
end;

procedure TThreadMapCombinePNG.SaveRect;
const
  PNG_MAX_HEIGHT = 65536;
  PNG_MAX_WIDTH = 65536;
var
  iWidth, iHeight: integer;
  i,j: integer;
  png_ptr: png_structp;
  info_ptr: png_infop;
  prow: png_bytep;
  rw_io: sas_png_rw_io;
  SwapBuf: Byte;
  VImgBuf: Pointer;
  VBytesRead: Byte;
  VPngColorType: Integer;
begin
  if FWithAlpha then begin
    VBytesRead := 4;
    VPngColorType := PNG_COLOR_TYPE_RGB_ALPHA;
  end else begin
    VBytesRead := 3;
    VPngColorType := PNG_COLOR_TYPE_RGB;
  end;

  sx := (FCurrentPieceRect.Left mod 256);
  sy := (FCurrentPieceRect.Top mod 256);
  ex := (FCurrentPieceRect.Right mod 256);
  ey := (FCurrentPieceRect.Bottom mod 256);

  iWidth := FMapPieceSize.X;
  iHeight := FMapPieceSize.y;

  if (iWidth >= PNG_MAX_WIDTH) or (iHeight >= PNG_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(SAS_ERR_ImageIsTooBig, ['PNG', iWidth, PNG_MAX_WIDTH, iHeight, PNG_MAX_HEIGHT, 'PNG']);
  end;

  if not Init_LibPNG then begin
    raise Exception.Create( _('Initialization of LibPNG failed.') );
  end;

  rw_io.DestStream := TFileStream.Create(FCurrentFileName, fmCreate);
  rw_io.DestBufferSize := 64*1024; // 64k
  GetMem(rw_io.DestBuffer, rw_io.DestBufferSize);
  rw_io.BufferedDataSize := 0;
  try
    png_ptr := png_create_write_struct(PNG_LIBPNG_VER_STRING, nil, nil, nil);
    if Assigned(png_ptr) then begin
      info_ptr := png_create_info_struct(png_ptr);
    end else begin
      raise Exception.Create( _('LibPNG: Failed to Create PngStruct!') );
    end;
    if Assigned(info_ptr) then begin
      try
        png_set_write_fn(png_ptr, @rw_io, @sas_png_write_data, nil);

        // Write header (8 bit colour depth)
        png_set_IHDR(png_ptr, info_ptr, iWidth, iHeight, 8, VPngColorType,
          PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

        png_write_info(png_ptr, info_ptr);

        // allocate row
        GetMem(prow, info_ptr.width * VBytesRead);

        VImgBuf := AllocateArray255(info_ptr.width);
        try
          btmm := TCustomBitmap32.Create;
          try
            btmm.Width := 256;
            btmm.Height := 256;

            for i := 0 to info_ptr.height - 1 do begin
              ReadLine(i, prow, VImgBuf, FWithAlpha);

              // BGR to RGB swap
              for j := 0 to info_ptr.width - 1 do begin
                SwapBuf := PByte(Integer(prow) + j*VBytesRead)^;
                PByte(Integer(prow) + j*VBytesRead)^ := PByte(Integer(prow) + j*VBytesRead + 2)^;
                PByte(Integer(prow) + j*VBytesRead + 2)^ := SwapBuf;
              end;

              // write row
              png_write_row(png_ptr, prow);

              if CancelNotifier.IsOperationCanceled(OperationID) then begin
                Break;
              end;
            end;
          finally
            btmm.Free;
          end;
        finally
          FreeArray255(VImgBuf);

          // freeing row
          FreeMem(prow);

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
