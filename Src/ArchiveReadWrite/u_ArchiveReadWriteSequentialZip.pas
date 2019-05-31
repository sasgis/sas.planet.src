{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_ArchiveReadWriteSequentialZip;

interface

uses
  i_ArchiveReadWrite,
  i_ArchiveReadWriteConfig,
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TArchiveReaderSequentialFactoryZip = class(TBaseInterfacedObject, IArchiveReaderSequentialFactory)
  private
    { IArchiveReaderSequentialFactory }
    function Build(
      const AFileName: string;
      const AConfig: IArchiveReadConfig = nil
    ): IArchiveReaderSequential;
  end;

  TArchiveWriterSequentialFactoryZip = class(TBaseInterfacedObject, IArchiveWriterSequentialFactory)
  private
    { IArchiveWriterSequentialFactory }
    function Build(
      const AFileName: string;
      const AConfig: IArchiveWriteConfig = nil
    ): IArchiveWriterSequential;
  end;

implementation

uses
  Classes,
  SysUtils,
  DateUtils,
  libminizip,
  i_BinaryData,
  u_BinaryData,
  u_StreamReadOnlyByBinaryData,
  u_StrFunc;

type
  TArchiveReaderSequentialZip = class(TBaseInterfacedObject, IArchiveReaderSequential)
  private
    FReader: Pointer;
    FRetCode: int32_t;
  private
    procedure Reset;
    function Next(
      out AFileData: IBinaryData;
      out AFileNameInArchive: string;
      out AFileDate: TDateTime
    ): Boolean;
  public
    constructor Create(
      const AFileName: string;
      const AConfig: IArchiveReadConfig = nil
    );
    destructor Destroy; override;
  end;

  TArchiveWriterSequentialZip = class(TBaseInterfacedObject, IArchiveWriterSequential)
  private
    FWriter: Pointer;
    FCompressMethod: uint16_t;
  private
    procedure Add(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    );
  public
    constructor Create(
      const AFileName: string;
      const AConfig: IArchiveWriteConfig = nil
    );
    destructor Destroy; override;
  end;

  EArchiveWriterSequentialZip = class(Exception);

{ TArchiveReaderSequentialFactoryZip }

function TArchiveReaderSequentialFactoryZip.Build(
  const AFileName: string;
  const AConfig: IArchiveReadConfig
): IArchiveReaderSequential;
begin
  Result := TArchiveReaderSequentialZip.Create(AFileName, AConfig);
end;

{ TArchiveWriterSequentialFactoryZip }

function TArchiveWriterSequentialFactoryZip.Build(
  const AFileName: string;
  const AConfig: IArchiveWriteConfig
): IArchiveWriterSequential;
begin
  Result := TArchiveWriterSequentialZip.Create(AFileName, AConfig);
end;

{ TArchiveWriteSequentialZip }

constructor TArchiveWriterSequentialZip.Create(
  const AFileName: string;
  const AConfig: IArchiveWriteConfig
);
var
  VFileName: mz_string_t;
  VVolumSize: int64_t;
  VCompressLevel: int16_t;
  VConfig: IArchiveWriteZipConfig;
begin
  Assert(AFileName <> '');
  inherited Create;

  LoadLibMiniZip;

  VFileName := mz_string_encode(AFileName);

  VConfig := nil;
  if AConfig <> nil then begin
    if not Supports(AConfig, IArchiveWriteZipConfig, VConfig) then begin
      raise EArchiveWriterSequentialZip.Create('Unexpected interface type!');
    end;
  end;

  if VConfig <> nil then begin
    case VConfig.CompressionLevel of
      zclDefault : VCompressLevel := MZ_COMPRESS_LEVEL_DEFAULT;
      zclFast    : VCompressLevel := MZ_COMPRESS_LEVEL_FAST;
      zclNormal  : VCompressLevel := MZ_COMPRESS_LEVEL_NORMAL;
      zclBest    : VCompressLevel := MZ_COMPRESS_LEVEL_BEST;
    else
      raise EArchiveWriterSequentialZip.CreateFmt(
        'Unexpected CompressionLevel value: %d', [Integer(VConfig.CompressionLevel)]
      );
    end;
    case VConfig.CompressionMethod of
      zcmStore   : FCompressMethod := MZ_COMPRESS_METHOD_STORE;
      zcmDeflate : FCompressMethod := MZ_COMPRESS_METHOD_DEFLATE;
      zcmBZip2   : FCompressMethod := MZ_COMPRESS_METHOD_BZIP2;
      zcmLZMA    : FCompressMethod := MZ_COMPRESS_METHOD_LZMA;
    else
      raise EArchiveWriterSequentialZip.CreateFmt(
        'Unexpected CompressionMethod value: %d', [Integer(VConfig.CompressionMethod)]
      );
    end;
    VVolumSize := VConfig.VolumeSize;
  end else begin
    FCompressMethod := MZ_COMPRESS_METHOD_STORE;
    VCompressLevel := MZ_COMPRESS_LEVEL_DEFAULT;
    VVolumSize := 0;
  end;

  mz_check( mz_zip_writer_create(FWriter) );

  mz_zip_writer_set_compress_method(FWriter, FCompressMethod);
  mz_zip_writer_set_compress_level(FWriter, VCompressLevel);

  mz_check( mz_zip_writer_open_file(FWriter, @VFileName[1], VVolumSize, 0) );
end;

destructor TArchiveWriterSequentialZip.Destroy;
begin
  if FWriter <> nil then begin
    mz_zip_writer_close(FWriter);
    mz_zip_writer_delete(FWriter);
  end;
  inherited;
end;

procedure TArchiveWriterSequentialZip.Add(
  const AFileData: IBinaryData;
  const AFileNameInArchive: string;
  const AFileDate: TDateTime
);
var
  VFileInfo: mz_zip_file;
  VFileNameInZip: mz_string_t;
  VData: Pointer;
  VDataSize: int64_t;
begin
  Assert(AFileNameInArchive <> '');

  FillChar(VFileInfo, SizeOf(VFileInfo), 0);

  VFileNameInZip := mz_string_encode(AFileNameInArchive);
  VFileInfo.filename := @VFileNameInZip[1];

  VFileInfo.compression_method := FCompressMethod;

  VFileInfo.creation_date := DateTimeToUnix(AFileDate);
  VFileInfo.accessed_date := VFileInfo.creation_date;
  VFileInfo.modified_date := VFileInfo.creation_date;

  if AFileData <> nil then begin
    VData := AFileData.Buffer;
    VDataSize := AFileData.Size;
  end else begin
    VData := nil;
    VDataSize := 0;
  end;

  mz_check( mz_zip_writer_add_buffer(FWriter, VData, VDataSize, @VFileInfo) );
end;

{ TArchiveReaderSequentialZip }

constructor TArchiveReaderSequentialZip.Create(
  const AFileName: string;
  const AConfig: IArchiveReadConfig
);
var
  VFileName: mz_string_t;
begin
  Assert(AFileName <> '');
  inherited Create;

  LoadLibMiniZip;

  VFileName := mz_string_encode(AFileName);

  mz_check( mz_zip_reader_create(FReader) );
  mz_check( mz_zip_reader_open_file(FReader, @VFileName[1]) );

  Reset;
end;

destructor TArchiveReaderSequentialZip.Destroy;
begin
  if FReader <> nil then begin
    mz_zip_reader_close(FReader);
    mz_zip_reader_delete(FReader);
  end;
  inherited;
end;

procedure TArchiveReaderSequentialZip.Reset;
begin
  FRetCode := mz_zip_reader_goto_first_entry(FReader);
end;

function TArchiveReaderSequentialZip.Next(
  out AFileData: IBinaryData;
  out AFileNameInArchive: string;
  out AFileDate: TDateTime
): Boolean;
var
  VData: Pointer;
  VDataSize: Int64;
  VFileInfo: p_mz_zip_file;
begin
  Result := False;
  while FRetCode <> MZ_END_OF_LIST do begin
    mz_check(FRetCode);

    mz_check( mz_zip_reader_entry_get_info(FReader, VFileInfo) );

    // ToDo: check if entry is file

    AFileNameInArchive := mz_string_decode(VFileInfo.filename);
    AFileDate := UnixToDateTime(VFileInfo.creation_date);

    if VFileInfo.uncompressed_size > 0 then begin
      VDataSize := VFileInfo.uncompressed_size;
      VData := GetMemory(VDataSize);
      try
        mz_check( mz_zip_reader_entry_save_buffer(FReader, VData, VDataSize) );
        AFileData := TBinaryData.CreateWithOwn(VDataSize, VData);
        VData := nil;
      finally
        FreeMem(VData);
      end;
    end else begin
      AFileData := nil;
    end;

    FRetCode := mz_zip_reader_goto_next_entry(FReader);

    Result := True;
    Break;
  end;
end;

end.
