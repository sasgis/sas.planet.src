{
   MiniZip project

   Copyright (C) 2010-2019 Nathan Moinvaziri
     https://github.com/nmoinvaz/minizip
   Copyright (C) 2009-2010 Mathias Svensson
     Modifications for Zip64 support
     http://result42.com
   Copyright (C) 1998-2010 Gilles Vollant
     https://www.winimage.com/zLibDll/minizip.html

   This program is distributed under the terms of the same license as zlib.
   See the accompanying LICENSE file for the full text of the license.
}

unit libminizip;

{.$DEFINE MZ_ZIP_H}

// dll compile-time defines:
{$DEFINE MZ_ZLIB}
{$DEFINE MZ_BZIP2}
{$DEFINE MZ_LZMA}
{.$DEFINE MZ_WZAES}

interface

uses
  SysUtils;

type
  EMiniZipError = class(Exception);

const
  libminizip_dll = 'libminizip.dll';

const
{$IFDEF MZ_LZMA}
  MZ_VERSION_MADEBY_ZIP_VERSION = 63;
{$ELSE}
  {$IFDEF MZ_WZAES}
  MZ_VERSION_MADEBY_ZIP_VERSION = 51;
  {$ELSE}
    {$IFDEF MZ_BZIP2}
    MZ_VERSION_MADEBY_ZIP_VERSION = 46;
    {$ELSE}
    MZ_VERSION_MADEBY_ZIP_VERSION = 45;
    {$ENDIF MZ_BZIP2}
  {$ENDIF MZ_WZAES}
{$ENDIF MZ_LZMA}

const
  MZ_VERSION = '2.8.8';

  MZ_OK = 0;

  MZ_STREAM_ERROR = -(1);
  MZ_DATA_ERROR = -(3);
  MZ_MEM_ERROR = -(4);
  MZ_BUF_ERROR = -(5);
  MZ_VERSION_ERROR = -(6);
  MZ_END_OF_LIST = -(100);
  MZ_END_OF_STREAM = -(101);
  MZ_PARAM_ERROR = -(102);
  MZ_FORMAT_ERROR = -(103);
  MZ_INTERNAL_ERROR = -(104);
  MZ_CRC_ERROR = -(105);
  MZ_CRYPT_ERROR = -(106);
  MZ_EXIST_ERROR = -(107);
  MZ_PASSWORD_ERROR = -(108);
  MZ_SUPPORT_ERROR = -(109);
  MZ_HASH_ERROR = -(110);
  MZ_OPEN_ERROR = -(111);
  MZ_CLOSE_ERROR = -(112);
  MZ_SEEK_ERROR = -(113);
  MZ_TELL_ERROR = -(114);
  MZ_READ_ERROR = -(115);
  MZ_WRITE_ERROR = -(116);
  MZ_SIGN_ERROR = -(117);
  MZ_SYMLINK_ERROR = -(118);

  MZ_OPEN_MODE_READ = $01;
  MZ_OPEN_MODE_WRITE = $02;
  MZ_OPEN_MODE_READWRITE = MZ_OPEN_MODE_READ or MZ_OPEN_MODE_WRITE;
  MZ_OPEN_MODE_APPEND = $04;
  MZ_OPEN_MODE_CREATE = $08;
  MZ_OPEN_MODE_EXISTING = $10;

  MZ_SEEK_SET = 0;
  MZ_SEEK_CUR = 1;
  MZ_SEEK_END = 2;

  MZ_COMPRESS_METHOD_STORE = 0;
  {$IFDEF MZ_ZLIB}
  MZ_COMPRESS_METHOD_DEFLATE = 8;
  {$ENDIF}
  {$IFDEF MZ_BZIP2}
  MZ_COMPRESS_METHOD_BZIP2 = 12;
  {$ENDIF}
  {$IFDEF MZ_LZMA}
  MZ_COMPRESS_METHOD_LZMA = 14;
  {$ENDIF}
  {$IFDEF MZ_WZAES}
  MZ_COMPRESS_METHOD_AES = 99;
  {$ENDIF}

  MZ_COMPRESS_LEVEL_DEFAULT = -(1);
  MZ_COMPRESS_LEVEL_FAST = 2;
  MZ_COMPRESS_LEVEL_NORMAL = 6;
  MZ_COMPRESS_LEVEL_BEST = 9;

  MZ_ZIP_FLAG_ENCRYPTED = 1 shl 0;
  MZ_ZIP_FLAG_LZMA_EOS_MARKER = 1 shl 1;
  MZ_ZIP_FLAG_DEFLATE_MAX = 1 shl 1;
  MZ_ZIP_FLAG_DEFLATE_NORMAL = 0;
  MZ_ZIP_FLAG_DEFLATE_FAST = 1 shl 2;
  MZ_ZIP_FLAG_DEFLATE_SUPER_FAST = MZ_ZIP_FLAG_DEFLATE_FAST or MZ_ZIP_FLAG_DEFLATE_MAX;
  MZ_ZIP_FLAG_DATA_DESCRIPTOR = 1 shl 3;
  MZ_ZIP_FLAG_UTF8 = 1 shl 11;
  MZ_ZIP_FLAG_MASK_LOCAL_INFO = 1 shl 13;

  MZ_ZIP_EXTENSION_ZIP64 = $0001;
  MZ_ZIP_EXTENSION_NTFS = $000a;
  MZ_ZIP_EXTENSION_AES = $9901;
  MZ_ZIP_EXTENSION_UNIX1 = $000d;
  MZ_ZIP_EXTENSION_SIGN = $10c5;
  MZ_ZIP_EXTENSION_HASH = $1a51;
  MZ_ZIP_EXTENSION_CDCD = $cdcd;

  MZ_ZIP64_AUTO = 0;
  MZ_ZIP64_FORCE = 1;
  MZ_ZIP64_DISABLE = 2;

  MZ_HOST_SYSTEM_MSDOS = 0;
  MZ_HOST_SYSTEM_UNIX = 3;
  MZ_HOST_SYSTEM_WINDOWS_NTFS = 10;
  MZ_HOST_SYSTEM_OSX_DARWIN = 19;
  MZ_VERSION_MADEBY_HOST_SYSTEM = MZ_HOST_SYSTEM_WINDOWS_NTFS;

  MZ_PKCRYPT_HEADER_SIZE = 12;

  MZ_AES_VERSION = 1;
  MZ_AES_ENCRYPTION_MODE_128 = $01;
  MZ_AES_ENCRYPTION_MODE_192 = $02;
  MZ_AES_ENCRYPTION_MODE_256 = $03;
  MZ_AES_KEY_LENGTH_MAX = 32;
  MZ_AES_BLOCK_SIZE = 16;
  MZ_AES_FOOTER_SIZE = 10;

  MZ_HASH_MD5 = 10;
  MZ_HASH_MD5_SIZE = 16;
  MZ_HASH_SHA1 = 20;
  MZ_HASH_SHA1_SIZE = 20;
  MZ_HASH_SHA256 = 23;
  MZ_HASH_SHA256_SIZE = 32;
  MZ_HASH_MAX_SIZE = 256;

  MZ_ENCODING_CODEPAGE_437 = 437;
  MZ_ENCODING_CODEPAGE_932 = 932;
  MZ_ENCODING_CODEPAGE_936 = 936;
  MZ_ENCODING_CODEPAGE_950 = 950;
  MZ_ENCODING_UTF8 = 65001;

  MZ_VERSION_MADEBY = (MZ_VERSION_MADEBY_HOST_SYSTEM shl 8) or MZ_VERSION_MADEBY_ZIP_VERSION;

type
  int8_t = AnsiChar;
  int16_t = smallint;
  int32_t = longint;
  int64_t = int64;

  uint8_t = byte;
  uint16_t = word;
  uint32_t = cardinal;
  uint64_t = uint64;

  {$if CompilerVersion <= 18.5}
  NativeUInt = Cardinal;
  {$ifend}

  time_t = NativeUInt;

  p_char = PAnsiChar;
  p_int64_t  = ^int64_t;

  p_uint8_t = ^uint8_t;
  p_uint16_t = ^uint16_t;
  p_uint32_t = ^uint32_t;
  p_uint64_t = ^uint64_t;

  {$A8}
  mz_zip_file = record
    version_madeby      : uint16_t;   // version made by
    version_needed      : uint16_t;   // version needed to extract
    flag                : uint16_t;   // general purpose bit flag
    compression_method  : uint16_t;   // compression method
    modified_date       : time_t;     // last modified date in unix time
    accessed_date       : time_t;     // last accessed date in unix time
    creation_date       : time_t;     // creation date in unix time
    crc                 : uint32_t;   // crc-32
    compressed_size     : int64_t;    // compressed size
    uncompressed_size   : int64_t;    // uncompressed size
    filename_size       : uint16_t;   // filename length
    extrafield_size     : uint16_t;   // extra field length
    comment_size        : uint16_t;   // file comment length
    disk_number         : uint32_t;   // disk number start
    disk_offset         : int64_t;    // relative offset of local header
    internal_fa         : uint16_t;   // internal file attributes
    external_fa         : uint32_t;   // external file attributes
    filename            : p_char;     // filename utf8 null-terminated string
    extrafield          : p_uint8_t;  // extrafield data
    comment             : p_char;     // comment utf8 null-terminated string
    linkname            : p_char;     // sym-link filename utf8 null-terminated string
    zip64               : uint16_t;   // zip64 extension mode
    aes_version         : uint16_t;   // winzip aes extension if not 0
    aes_encryption_mode : uint8_t;    // winzip aes encryption mode
  end;
  p_mz_zip_file = ^mz_zip_file;

{$IFDEF MZ_ZIP_H}
type
  mz_zip_locate_entry_cb = function(handle: pointer; userdata: pointer; var file_info: mz_zip_file): int32_t; cdecl;

var
  mz_zip_create: function(var handle: pointer): pointer; cdecl;
  mz_zip_delete: procedure(var handle: pointer); cdecl;
  mz_zip_open: function(handle: pointer; stream: pointer; mode: int32_t):  int32_t; cdecl;
  mz_zip_close: function(handle: pointer): int32_t; cdecl;
  mz_zip_get_comment: function(handle: pointer; out comment: p_char): int32_t; cdecl;
  mz_zip_set_comment: function(handle: pointer; comment: p_char): int32_t; cdecl;
  mz_zip_get_version_madeby: function(handle: pointer; version_madeby: p_uint16_t): int32_t; cdecl;
  mz_zip_set_version_madeby: function(handle: pointer; version_madeby: uint16_t): int32_t; cdecl;
  mz_zip_set_recover: function(handle: pointer; recover: uint8_t): int32_t; cdecl;
  mz_zip_get_stream: function(handle: pointer; out stream: pointer): int32_t; cdecl;
  mz_zip_set_cd_stream: function(handle: pointer; cd_start_pos: int64_t; cd_stream: pointer): int32_t; cdecl;
  mz_zip_get_cd_mem_stream: function(handle: pointer; out cd_mem_stream: pointer): int32_t; cdecl;
  mz_zip_entry_is_open: function(handle: pointer): int32_t; cdecl;
  mz_zip_entry_read_open: function(handle: pointer; raw: uint8_t; password: p_char): int32_t; cdecl;
  mz_zip_entry_read: function(handle: pointer; buf: pointer; len: int32_t): int32_t; cdecl;
  mz_zip_entry_read_close: function(handle: pointer; crc32: p_uint32_t; compressed_size: p_int64_t; uncompressed_size: p_int64_t): int32_t; cdecl;
  mz_zip_entry_write_open: function(handle: pointer; var file_info: mz_zip_file; compress_level: int16_t; raw: uint8_t; password: p_char): int32_t; cdecl;
  mz_zip_entry_write: function(handle: pointer; buf: pointer; len: int32_t): int32_t; cdecl;
  mz_zip_entry_write_close: function(handle: pointer; crc32: uint32_t; compressed_size: int64_t; uncompressed_size: int64_t): int32_t; cdecl;
  mz_zip_entry_is_dir: function(handle: pointer ): int32_t; cdecl;
  mz_zip_entry_is_symlink: function(handle: pointer ): int32_t; cdecl;
  mz_zip_entry_get_info: function(handle: pointer ; var file_info: p_mz_zip_file ): int32_t; cdecl;
  mz_zip_entry_get_local_info: function(handle: pointer; var local_file_info: p_mz_zip_file): int32_t; cdecl;
  mz_zip_entry_set_extrafield: function(handle: pointer; extrafield: p_uint8_t; extrafield_size: uint16_t): int32_t; cdecl;
  mz_zip_entry_close_raw: function(handle: pointer; uncompressed_size: int64_t; crc32: uint32_t): int32_t; cdecl;
  mz_zip_entry_close: function(handle: pointer): int32_t; cdecl;
  mz_zip_set_number_entry: function(handle: pointer; number_entry: uint64_t): int32_t; cdecl;
  mz_zip_get_number_entry: function(handle: pointer; number_entry: p_uint64_t): int32_t; cdecl;
  mz_zip_set_disk_number_with_cd: function(handle: pointer; disk_number_with_cd: uint32_t): int32_t; cdecl;
  mz_zip_get_disk_number_with_cd: function(handle: pointer; disk_number_with_cd: p_uint32_t): int32_t; cdecl;
  mz_zip_get_entry: function(handle: pointer): int64_t; cdecl;
  mz_zip_goto_entry: function(handle: pointer; cd_pos: int64_t): int32_t; cdecl;
  mz_zip_goto_first_entry: function(handle: pointer): int32_t; cdecl;
  mz_zip_goto_next_entry: function(handle: pointer): int32_t; cdecl;
  mz_zip_locate_entry: function(handle: pointer; filename: p_char; ignore_case: uint8_t): int32_t; cdecl;
  mz_zip_locate_first_entry: function(handle: pointer; userdata: pointer; cb: mz_zip_locate_entry_cb): int32_t; cdecl;
  mz_zip_locate_next_entry: function(handle: pointer; userdata: pointer; cb: mz_zip_locate_entry_cb): int32_t; cdecl;
  mz_zip_attrib_is_dir: function(attrib: uint32_t; version_madeby: int32_t): int32_t; cdecl;
  mz_zip_attrib_is_symlink: function(attrib: uint32_t; version_madeby: int32_t): int32_t; cdecl;
  mz_zip_attrib_convert: function(src_sys: uint8_t; src_attrib: uint32_t; target_sys: uint8_t; target_attrib: p_uint32_t): int32_t; cdecl;
  mz_zip_attrib_posix_to_win32: function(posix_attrib: uint32_t; win32_attrib: p_uint32_t): int32_t; cdecl;
  mz_zip_attrib_win32_to_posix: function(win32_attrib: uint32_t; posix_attrib: p_uint32_t): int32_t; cdecl;
  mz_zip_extrafield_find: function(stream: pointer; _type: uint16_t; length: p_uint16_t): int32_t; cdecl;
  mz_zip_extrafield_contains: function(extrafield: p_uint8_t; extrafield_size: int32_t; _type: uint16_t; length: p_uint16_t): int32_t; cdecl;
  mz_zip_extrafield_read: function(stream: pointer; _type: p_uint16_t; length: p_uint16_t): int32_t; cdecl;
  mz_zip_extrafield_write: function(stream: pointer; _type: uint16_t; length: uint16_t): int32_t; cdecl;
  //mz_zip_dosdate_to_tm: function(dos_date: uint64_t; ptm: Ptm): int32_t; cdecl;
  mz_zip_dosdate_to_time_t: function(dos_date: uint64_t): time_t; cdecl;
  //mz_zip_time_t_to_tm: function(unix_time: time_t; ptm: Ptm): int32_t; cdecl;
  mz_zip_time_t_to_dos_date: function(unix_time: time_t): uint32_t; cdecl;
  //mz_zip_tm_to_dosdate: function(ptm: Ptm): uint32_t; cdecl;
  mz_zip_ntfs_to_unix_time: function(ntfs_time: uint64_t; out unix_time: time_t): int32_t; cdecl;
  mz_zip_unix_to_ntfs_time: function(unix_time: time_t; out ntfs_time: uint64_t): int32_t; cdecl;
  mz_zip_path_compare: function(path1: p_char; path2: p_char; ignore_case: uint8_t): int32_t; cdecl;
{$ENDIF MZ_ZIP_H}

type
  mz_zip_reader_overwrite_cb = function(handle: pointer; userdata: pointer; file_info: p_mz_zip_file; path: p_char): int32_t; cdecl;
  mz_zip_reader_password_cb = function(handle: pointer; userdata: pointer; file_info: p_mz_zip_file; password: p_char; max_password: int32_t): int32_t; cdecl;
  mz_zip_reader_progress_cb = function(handle: pointer; userdata: pointer; file_info: p_mz_zip_file; position: int64_t): int32_t; cdecl;
  mz_zip_reader_entry_cb = function(handle: pointer; userdata: pointer; file_info: p_mz_zip_file; path: p_char): int32_t; cdecl;

var
  mz_zip_reader_is_open: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_open: function(handle: pointer; stream: pointer): int32_t; cdecl;
  mz_zip_reader_open_file: function(handle: pointer; path: p_char): int32_t; cdecl;
  mz_zip_reader_open_file_in_memory: function(handle: pointer; path: p_char): int32_t; cdecl;
  mz_zip_reader_open_buffer: function(handle: pointer; buf: p_uint8_t; len: int32_t; copy: uint8_t): int32_t; cdecl;
  mz_zip_reader_close: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_unzip_cd: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_goto_first_entry: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_goto_next_entry: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_locate_entry: function(handle: pointer; filename: p_char; ignore_case: uint8_t): int32_t; cdecl;
  mz_zip_reader_entry_open: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_entry_close: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_entry_read: function(handle: pointer; buf: pointer; len: int32_t): int32_t; cdecl;
  mz_zip_reader_entry_has_sign: function(handle: pointer): int32_t; cdecl;
  //mz_zip_reader_entry_sign_verify: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_entry_get_hash: function(handle: pointer; algorithm: uint16_t; digest: p_uint8_t; digest_size: int32_t): int32_t; cdecl;
  mz_zip_reader_entry_get_first_hash: function(handle: pointer; algorithm: p_uint16_t; digest_size: p_uint16_t): int32_t; cdecl;
  mz_zip_reader_entry_get_info: function(handle: pointer; var file_info: p_mz_zip_file): int32_t; cdecl;
  mz_zip_reader_entry_is_dir: function(handle: pointer): int32_t; cdecl;
  //mz_zip_reader_entry_save: function(handle: pointer; stream: pointer; write_cb: mz_stream_write_cb): int32_t; cdecl;
  //mz_zip_reader_entry_save_process: function(handle: pointer; stream: pointer; write_cb: mz_stream_write_cb): int32_t; cdecl;
  mz_zip_reader_entry_save_file: function(handle: pointer; path: p_char): int32_t; cdecl;
  mz_zip_reader_entry_save_buffer: function(handle: pointer; buf: pointer; len: int32_t): int32_t; cdecl;
  mz_zip_reader_entry_save_buffer_length: function(handle: pointer): int32_t; cdecl;
  mz_zip_reader_save_all: function(handle: pointer; destination_dir: p_char): int32_t; cdecl;
  mz_zip_reader_set_pattern: procedure(handle: pointer; pattern: p_char; ignore_case: uint8_t); cdecl;
  mz_zip_reader_set_password: procedure(handle: pointer; password: p_char); cdecl;
  mz_zip_reader_set_raw: procedure(handle: pointer; raw: uint8_t); cdecl;
  mz_zip_reader_get_raw: function(handle: pointer; raw: p_uint8_t): int32_t; cdecl;
  mz_zip_reader_get_zip_cd: function(handle: pointer; zip_cd: p_uint8_t): int32_t; cdecl;
  mz_zip_reader_get_comment: function(handle: pointer; out comment: p_char): int32_t; cdecl;
  mz_zip_reader_set_encoding: procedure(handle: pointer; encoding: int32_t); cdecl;
  mz_zip_reader_set_sign_required: procedure(handle: pointer; sign_required: uint8_t); cdecl;
  mz_zip_reader_set_overwrite_cb: procedure(handle: pointer; userdata: pointer; cb: mz_zip_reader_overwrite_cb); cdecl;
  mz_zip_reader_set_password_cb: procedure(handle: pointer; userdata: pointer; cb: mz_zip_reader_password_cb); cdecl;
  mz_zip_reader_set_progress_cb: procedure(handle: pointer; userdata: pointer; cb: mz_zip_reader_progress_cb); cdecl;
  mz_zip_reader_set_progress_interval: procedure(handle: pointer; milliseconds: uint32_t); cdecl;
  mz_zip_reader_set_entry_cb: procedure(handle: pointer; userdata: pointer; cb: mz_zip_reader_entry_cb); cdecl;
  mz_zip_reader_get_zip_handle: function(handle: pointer; out zip_handle: pointer): int32_t; cdecl;
  mz_zip_reader_create: function(var handle: pointer): pointer; cdecl;
  mz_zip_reader_delete: procedure(var handle: pointer); cdecl;

type
  mz_zip_writer_overwrite_cb = function (handle: pointer; userdata: pointer; path: p_char): int32_t; cdecl;
  mz_zip_writer_password_cb = function (handle: pointer; userdata: pointer; file_info: p_mz_zip_file; password: p_char; max_password: int32_t): int32_t; cdecl;
  mz_zip_writer_progress_cb = function (handle: pointer; userdata: pointer; file_info: p_mz_zip_file; position: int64_t): int32_t; cdecl;
  mz_zip_writer_entry_cb = function (handle: pointer; userdata: pointer; file_info: p_mz_zip_file): int32_t; cdecl;

var
  mz_zip_writer_is_open: function(handle: pointer): int32_t; cdecl;
  mz_zip_writer_open: function(handle: pointer; stream: pointer): int32_t; cdecl;
  mz_zip_writer_open_file: function(handle: pointer; path: p_char; disk_size: int64_t; append: uint8_t): int32_t; cdecl;
  mz_zip_writer_open_file_in_memory: function(handle: pointer; path: p_char): int32_t; cdecl;
  mz_zip_writer_close: function(handle: pointer): int32_t; cdecl;
  mz_zip_writer_zip_cd: function(handle: pointer): int32_t; cdecl;
  mz_zip_writer_entry_open: function(handle: pointer; file_info: p_mz_zip_file): int32_t; cdecl;
  mz_zip_writer_entry_close: function(handle: pointer): int32_t; cdecl;
  mz_zip_writer_entry_write: function(handle: pointer; buf: pointer; len: int32_t): int32_t; cdecl;
  //mz_zip_writer_entry_sign: function(handle: pointer; msg: p_uint8_t; message_size: int32_t; cert_data: p_uint8_t; cert_data_size: int32_t;
  //    cert_pwd: p_char): int32_t; cdecl;
  //mz_zip_writer_add: function(handle: pointer; stream: pointer; read_cb: mz_stream_read_cb): int32_t; cdecl;
  //mz_zip_writer_add_process: function(handle: pointer; stream: pointer; read_cb: mz_stream_read_cb): int32_t; cdecl;
  //mz_zip_writer_add_info: function(handle: pointer; stream: pointer; read_cb: mz_stream_read_cb; file_info: p_mz_zip_file): int32_t; cdecl;
  mz_zip_writer_add_buffer: function(handle: pointer; buf: pointer; len: int32_t; file_info: p_mz_zip_file): int32_t; cdecl;
  mz_zip_writer_add_file: function(handle: pointer; path: p_char; filename_in_zip: p_char): int32_t; cdecl;
  mz_zip_writer_add_path: function(handle: pointer; path: p_char; root_path: p_char; include_path: uint8_t; recursive: uint8_t): int32_t; cdecl;
  mz_zip_writer_copy_from_reader: function(handle: pointer; reader: pointer): int32_t; cdecl;
  mz_zip_writer_set_password: procedure(handle: pointer; password: p_char); cdecl;
  mz_zip_writer_set_comment: procedure(handle: pointer; comment: p_char); cdecl;
  mz_zip_writer_set_raw: procedure(handle: pointer; raw: uint8_t); cdecl;
  mz_zip_writer_get_raw: function(handle: pointer; raw: p_uint8_t): int32_t; cdecl;
  mz_zip_writer_set_aes: procedure(handle: pointer; aes: uint8_t); cdecl;
  mz_zip_writer_set_compress_method: procedure(handle: pointer; compress_method: uint16_t); cdecl;
  mz_zip_writer_set_compress_level: procedure(handle: pointer; compress_level: int16_t); cdecl;
  mz_zip_writer_set_follow_links: procedure(handle: pointer; follow_links: uint8_t); cdecl;
  mz_zip_writer_set_store_links: procedure(handle: pointer; store_links: uint8_t); cdecl;
  mz_zip_writer_set_zip_cd: procedure(handle: pointer; zip_cd: uint8_t); cdecl;
  mz_zip_writer_set_certificate: function(handle: pointer; cert_path: p_char; cert_pwd: p_char): int32_t; cdecl;
  mz_zip_writer_set_overwrite_cb: procedure(handle: pointer; userdata: pointer; cb: mz_zip_writer_overwrite_cb); cdecl;
  mz_zip_writer_set_password_cb: procedure(handle: pointer; userdata: pointer; cb: mz_zip_writer_password_cb); cdecl;
  mz_zip_writer_set_progress_cb: procedure(handle: pointer; userdata: pointer; cb: mz_zip_writer_progress_cb); cdecl;
  mz_zip_writer_set_progress_interval: procedure(handle: pointer; milliseconds: uint32_t); cdecl;
  mz_zip_writer_set_entry_cb: procedure(handle: pointer; userdata: pointer; cb: mz_zip_writer_entry_cb); cdecl;
  mz_zip_writer_get_zip_handle: function(handle: pointer; out zip_handle: pointer): int32_t; cdecl;
  mz_zip_writer_create: function(var handle: pointer): pointer; cdecl;
  mz_zip_writer_delete: procedure(var handle: pointer); cdecl;

//******************************************************************************

type
  mz_string_t = {$IFDEF UNICODE} UTF8String {$ELSE} AnsiString {$ENDIF};

procedure mz_check(const p: pointer); inline; overload;
procedure mz_check(const err: int32_t); inline; overload;

function mz_string_encode(const s: string): mz_string_t; inline;
function mz_string_decode(const s: p_char): string; overload; inline;
function mz_string_decode(const s: mz_string_t): string; overload; inline;

function LoadLibMiniZip(const ALibName: string = libminizip_dll; const ASilent: Boolean = False): Boolean;
procedure UnloadLibMiniZip;

implementation

uses
  Windows,
  SyncObjs;

var
  GLock: TCriticalSection = nil;
  GLibHandle: THandle = 0;

procedure mz_check(const p: pointer);
begin
  if p = nil then begin
    raise EMiniZipError.Create('MZ_ERROR: memory allocation failed');
  end;
end;

procedure mz_check(const err: int32_t);
begin
  if err <> MZ_OK then begin
    raise EMiniZipError.CreateFmt('MZ_ERROR: %d', [err]);
  end;
end;

function mz_string_encode(const s: string): mz_string_t;
begin
  Result := UTF8Encode(s);
end;

function mz_string_decode(const s: p_char): string;
begin
  Result :=
    {$IF CompilerVersion >= 33}
    UTF8ToString(s);
    {$ELSE}
    UTF8Decode(s);
    {$IFEND}
end;

function mz_string_decode(const s: mz_string_t): string;
begin
  if s <> '' then begin
    Result := mz_string_decode(@s[1]);
  end else begin
    Result := '';
  end;
end;

function LoadProc(const AProcName: PAnsiChar): Pointer; inline;
begin
  Result := GetProcAddress(GLibHandle, AProcName);
  if Result = nil then begin
    raise EMiniZipError.Create('Could not load proc: ' + string(AProcName));
  end;
end;

function LoadLibMiniZip(const ALibName: string; const ASilent: Boolean): Boolean;
begin
  Result := False;

  if GLibHandle <> 0 then begin
    Result := True;
    Exit;
  end;

  GLock.Acquire;
  try
    if GLibHandle <> 0 then begin
      Result := True;
      Exit;
    end;

    GLibHandle := LoadLibrary(PChar(ALibName));

    if GLibHandle = 0 then begin
      if ASilent then begin
        Exit;
      end else begin
        raise EMiniZipError.CreateFmt(
          'Could not load library: %s' + #13#10 + '%s',
          [ALibName, SysErrorMessage(GetLastError)]
        );
      end;
    end;

    {$IFDEF MZ_ZIP_H}
    mz_zip_create := LoadProc('mz_zip_create');
    mz_zip_delete := LoadProc('mz_zip_delete');
    mz_zip_open := LoadProc('mz_zip_open');
    mz_zip_close := LoadProc('mz_zip_close');
    mz_zip_get_comment := LoadProc('mz_zip_get_comment');
    mz_zip_set_comment := LoadProc('mz_zip_set_comment');
    mz_zip_get_version_madeby := LoadProc('mz_zip_get_version_madeby');
    mz_zip_set_version_madeby := LoadProc('mz_zip_set_version_madeby');
    mz_zip_set_recover := LoadProc('mz_zip_set_recover');
    mz_zip_get_stream := LoadProc('mz_zip_get_stream');
    mz_zip_set_cd_stream := LoadProc('mz_zip_set_cd_stream');
    mz_zip_get_cd_mem_stream := LoadProc('mz_zip_get_cd_mem_stream');
    mz_zip_entry_is_open := LoadProc('mz_zip_entry_is_open');
    mz_zip_entry_read_open := LoadProc('mz_zip_entry_read_open');
    mz_zip_entry_read := LoadProc('mz_zip_entry_read');
    mz_zip_entry_read_close := LoadProc('mz_zip_entry_read_close');
    mz_zip_entry_write_open := LoadProc('mz_zip_entry_write_open');
    mz_zip_entry_write := LoadProc('mz_zip_entry_write');
    mz_zip_entry_write_close := LoadProc('mz_zip_entry_write_close');
    mz_zip_entry_is_dir := LoadProc('mz_zip_entry_is_dir');
    mz_zip_entry_is_symlink := LoadProc('mz_zip_entry_is_symlink');
    mz_zip_entry_get_info := LoadProc('mz_zip_entry_get_info');
    mz_zip_entry_get_local_info := LoadProc('mz_zip_entry_get_local_info');
    mz_zip_entry_set_extrafield := LoadProc('mz_zip_entry_set_extrafield');
    mz_zip_entry_close_raw := LoadProc('mz_zip_entry_close_raw');
    mz_zip_entry_close := LoadProc('mz_zip_entry_close');
    mz_zip_set_number_entry := LoadProc('mz_zip_set_number_entry');
    mz_zip_get_number_entry := LoadProc('mz_zip_get_number_entry');
    mz_zip_set_disk_number_with_cd := LoadProc('mz_zip_set_disk_number_with_cd');
    mz_zip_get_disk_number_with_cd := LoadProc('mz_zip_get_disk_number_with_cd');
    mz_zip_get_entry := LoadProc('mz_zip_get_entry');
    mz_zip_goto_entry := LoadProc('mz_zip_goto_entry');
    mz_zip_goto_first_entry := LoadProc('mz_zip_goto_first_entry');
    mz_zip_goto_next_entry := LoadProc('mz_zip_goto_next_entry');
    mz_zip_locate_entry := LoadProc('mz_zip_locate_entry');
    mz_zip_locate_first_entry := LoadProc('mz_zip_locate_first_entry');
    mz_zip_locate_next_entry := LoadProc('mz_zip_locate_next_entry');
    mz_zip_attrib_is_dir := LoadProc('mz_zip_attrib_is_dir');
    mz_zip_attrib_is_symlink := LoadProc('mz_zip_attrib_is_symlink');
    mz_zip_attrib_convert := LoadProc('mz_zip_attrib_convert');
    mz_zip_attrib_posix_to_win32 := LoadProc('mz_zip_attrib_posix_to_win32');
    mz_zip_attrib_win32_to_posix := LoadProc('mz_zip_attrib_win32_to_posix');
    mz_zip_extrafield_find := LoadProc('mz_zip_extrafield_find');
    mz_zip_extrafield_contains := LoadProc('mz_zip_extrafield_contains');
    mz_zip_extrafield_read := LoadProc('mz_zip_extrafield_read');
    mz_zip_extrafield_write := LoadProc('mz_zip_extrafield_write');
    //mz_zip_dosdate_to_tm := LoadProc('mz_zip_dosdate_to_tm');
    mz_zip_dosdate_to_time_t := LoadProc('mz_zip_dosdate_to_time_t');
    //mz_zip_time_t_to_tm := LoadProc('mz_zip_time_t_to_tm');
    mz_zip_time_t_to_dos_date := LoadProc('mz_zip_time_t_to_dos_date');
    //mz_zip_tm_to_dosdate := LoadProc('mz_zip_tm_to_dosdate');
    mz_zip_ntfs_to_unix_time := LoadProc('mz_zip_ntfs_to_unix_time');
    mz_zip_unix_to_ntfs_time := LoadProc('mz_zip_unix_to_ntfs_time');
    mz_zip_path_compare := LoadProc('mz_zip_path_compare');
    {$ENDIF MZ_ZIP_H}

    mz_zip_reader_is_open := LoadProc('mz_zip_reader_is_open');
    mz_zip_reader_open := LoadProc('mz_zip_reader_open');
    mz_zip_reader_open_file := LoadProc('mz_zip_reader_open_file');
    mz_zip_reader_open_file_in_memory := LoadProc('mz_zip_reader_open_file_in_memory');
    mz_zip_reader_open_buffer := LoadProc('mz_zip_reader_open_buffer');
    mz_zip_reader_close := LoadProc('mz_zip_reader_close');
    mz_zip_reader_unzip_cd := LoadProc('mz_zip_reader_unzip_cd');
    mz_zip_reader_goto_first_entry := LoadProc('mz_zip_reader_goto_first_entry');
    mz_zip_reader_goto_next_entry := LoadProc('mz_zip_reader_goto_next_entry');
    mz_zip_reader_locate_entry := LoadProc('mz_zip_reader_locate_entry');
    mz_zip_reader_entry_open := LoadProc('mz_zip_reader_entry_open');
    mz_zip_reader_entry_close := LoadProc('mz_zip_reader_entry_close');
    mz_zip_reader_entry_read := LoadProc('mz_zip_reader_entry_read');
    mz_zip_reader_entry_has_sign := LoadProc('mz_zip_reader_entry_has_sign');
    //mz_zip_reader_entry_sign_verify := LoadProc('mz_zip_reader_entry_sign_verify');
    mz_zip_reader_entry_get_hash := LoadProc('mz_zip_reader_entry_get_hash');
    mz_zip_reader_entry_get_first_hash := LoadProc('mz_zip_reader_entry_get_first_hash');
    mz_zip_reader_entry_get_info := LoadProc('mz_zip_reader_entry_get_info');
    mz_zip_reader_entry_is_dir := LoadProc('mz_zip_reader_entry_is_dir');
    //mz_zip_reader_entry_save := LoadProc('mz_zip_reader_entry_save');
    //mz_zip_reader_entry_save_process := LoadProc('mz_zip_reader_entry_save_process');
    mz_zip_reader_entry_save_file := LoadProc('mz_zip_reader_entry_save_file');
    mz_zip_reader_entry_save_buffer := LoadProc('mz_zip_reader_entry_save_buffer');
    mz_zip_reader_entry_save_buffer_length := LoadProc('mz_zip_reader_entry_save_buffer_length');
    mz_zip_reader_save_all := LoadProc('mz_zip_reader_save_all');
    mz_zip_reader_set_pattern := LoadProc('mz_zip_reader_set_pattern');
    mz_zip_reader_set_password := LoadProc('mz_zip_reader_set_password');
    mz_zip_reader_set_raw := LoadProc('mz_zip_reader_set_raw');
    mz_zip_reader_get_raw := LoadProc('mz_zip_reader_get_raw');
    mz_zip_reader_get_zip_cd := LoadProc('mz_zip_reader_get_zip_cd');
    mz_zip_reader_get_comment := LoadProc('mz_zip_reader_get_comment');
    mz_zip_reader_set_encoding := LoadProc('mz_zip_reader_set_encoding');
    mz_zip_reader_set_sign_required := LoadProc('mz_zip_reader_set_sign_required');
    mz_zip_reader_set_overwrite_cb := LoadProc('mz_zip_reader_set_overwrite_cb');
    mz_zip_reader_set_password_cb := LoadProc('mz_zip_reader_set_password_cb');
    mz_zip_reader_set_progress_cb := LoadProc('mz_zip_reader_set_progress_cb');
    mz_zip_reader_set_progress_interval := LoadProc('mz_zip_reader_set_progress_interval');
    mz_zip_reader_set_entry_cb := LoadProc('mz_zip_reader_set_entry_cb');
    mz_zip_reader_get_zip_handle := LoadProc('mz_zip_reader_get_zip_handle');
    mz_zip_reader_create := LoadProc('mz_zip_reader_create');
    mz_zip_reader_delete := LoadProc('mz_zip_reader_delete');

    mz_zip_writer_is_open := LoadProc('mz_zip_writer_is_open');
    mz_zip_writer_open := LoadProc('mz_zip_writer_open');
    mz_zip_writer_open_file := LoadProc('mz_zip_writer_open_file');
    mz_zip_writer_open_file_in_memory := LoadProc('mz_zip_writer_open_file_in_memory');
    mz_zip_writer_close := LoadProc('mz_zip_writer_close');
    mz_zip_writer_zip_cd := LoadProc('mz_zip_writer_zip_cd');
    mz_zip_writer_entry_open := LoadProc('mz_zip_writer_entry_open');
    mz_zip_writer_entry_close := LoadProc('mz_zip_writer_entry_close');
    mz_zip_writer_entry_write := LoadProc('mz_zip_writer_entry_write');
    //mz_zip_writer_entry_sign := LoadProc('mz_zip_writer_entry_sign');
    //mz_zip_writer_add := LoadProc('mz_zip_writer_add');
    //mz_zip_writer_add_process := LoadProc('mz_zip_writer_add_process');
    //mz_zip_writer_add_info := LoadProc('mz_zip_writer_add_info');
    mz_zip_writer_add_buffer := LoadProc('mz_zip_writer_add_buffer');
    mz_zip_writer_add_file := LoadProc('mz_zip_writer_add_file');
    mz_zip_writer_add_path := LoadProc('mz_zip_writer_add_path');
    mz_zip_writer_copy_from_reader := LoadProc('mz_zip_writer_copy_from_reader');
    mz_zip_writer_set_password := LoadProc('mz_zip_writer_set_password');
    mz_zip_writer_set_comment := LoadProc('mz_zip_writer_set_comment');
    mz_zip_writer_set_raw := LoadProc('mz_zip_writer_set_raw');
    mz_zip_writer_get_raw := LoadProc('mz_zip_writer_get_raw');
    mz_zip_writer_set_aes := LoadProc('mz_zip_writer_set_aes');
    mz_zip_writer_set_compress_method := LoadProc('mz_zip_writer_set_compress_method');
    mz_zip_writer_set_compress_level := LoadProc('mz_zip_writer_set_compress_level');
    mz_zip_writer_set_follow_links := LoadProc('mz_zip_writer_set_follow_links');
    mz_zip_writer_set_store_links := LoadProc('mz_zip_writer_set_store_links');
    mz_zip_writer_set_zip_cd := LoadProc('mz_zip_writer_set_zip_cd');
    mz_zip_writer_set_certificate := LoadProc('mz_zip_writer_set_certificate');
    mz_zip_writer_set_overwrite_cb := LoadProc('mz_zip_writer_set_overwrite_cb');
    mz_zip_writer_set_password_cb := LoadProc('mz_zip_writer_set_password_cb');
    mz_zip_writer_set_progress_cb := LoadProc('mz_zip_writer_set_progress_cb');
    mz_zip_writer_set_progress_interval := LoadProc('mz_zip_writer_set_progress_interval');
    mz_zip_writer_set_entry_cb := LoadProc('mz_zip_writer_set_entry_cb');
    mz_zip_writer_get_zip_handle := LoadProc('mz_zip_writer_get_zip_handle');
    mz_zip_writer_create := LoadProc('mz_zip_writer_create');
    mz_zip_writer_delete := LoadProc('mz_zip_writer_delete');

    Result := True;
  finally
    GLock.Release;
  end;
end;

procedure UnloadLibMiniZip;
begin
  if GLibHandle = 0 then begin
    Exit;
  end;

  GLock.Acquire;
  try
    if GLibHandle <> 0 then begin
      FreeLibrary(GLibHandle);
      GLibHandle := 0;
    end;

    {$IFDEF MZ_ZIP_H}
    mz_zip_create := nil;
    mz_zip_delete := nil;
    mz_zip_open := nil;
    mz_zip_close := nil;
    mz_zip_get_comment := nil;
    mz_zip_set_comment := nil;
    mz_zip_get_version_madeby := nil;
    mz_zip_set_version_madeby := nil;
    mz_zip_set_recover := nil;
    mz_zip_get_stream := nil;
    mz_zip_set_cd_stream := nil;
    mz_zip_get_cd_mem_stream := nil;
    mz_zip_entry_is_open := nil;
    mz_zip_entry_read_open := nil;
    mz_zip_entry_read := nil;
    mz_zip_entry_read_close := nil;
    mz_zip_entry_write_open := nil;
    mz_zip_entry_write := nil;
    mz_zip_entry_write_close := nil;
    mz_zip_entry_is_dir := nil;
    mz_zip_entry_is_symlink := nil;
    mz_zip_entry_get_info := nil;
    mz_zip_entry_get_local_info := nil;
    mz_zip_entry_set_extrafield := nil;
    mz_zip_entry_close_raw := nil;
    mz_zip_entry_close := nil;
    mz_zip_set_number_entry := nil;
    mz_zip_get_number_entry := nil;
    mz_zip_set_disk_number_with_cd := nil;
    mz_zip_get_disk_number_with_cd := nil;
    mz_zip_get_entry := nil;
    mz_zip_goto_entry := nil;
    mz_zip_goto_first_entry := nil;
    mz_zip_goto_next_entry := nil;
    mz_zip_locate_entry := nil;
    mz_zip_locate_first_entry := nil;
    mz_zip_locate_next_entry := nil;
    mz_zip_attrib_is_dir := nil;
    mz_zip_attrib_is_symlink := nil;
    mz_zip_attrib_convert := nil;
    mz_zip_attrib_posix_to_win32 := nil;
    mz_zip_attrib_win32_to_posix := nil;
    mz_zip_extrafield_find := nil;
    mz_zip_extrafield_contains := nil;
    mz_zip_extrafield_read := nil;
    mz_zip_extrafield_write := nil;
    //mz_zip_dosdate_to_tm := nil;
    mz_zip_dosdate_to_time_t := nil;
    //mz_zip_time_t_to_tm := nil;
    mz_zip_time_t_to_dos_date := nil;
    //mz_zip_tm_to_dosdate := nil;
    mz_zip_ntfs_to_unix_time := nil;
    mz_zip_unix_to_ntfs_time := nil;
    mz_zip_path_compare := nil;
    {$ENDIF MZ_ZIP_H}

    mz_zip_reader_is_open := nil;
    mz_zip_reader_open := nil;
    mz_zip_reader_open_file := nil;
    mz_zip_reader_open_file_in_memory := nil;
    mz_zip_reader_open_buffer := nil;
    mz_zip_reader_close := nil;
    mz_zip_reader_unzip_cd := nil;
    mz_zip_reader_goto_first_entry := nil;
    mz_zip_reader_goto_next_entry := nil;
    mz_zip_reader_locate_entry := nil;
    mz_zip_reader_entry_open := nil;
    mz_zip_reader_entry_close := nil;
    mz_zip_reader_entry_read := nil;
    mz_zip_reader_entry_has_sign := nil;
    //mz_zip_reader_entry_sign_verify := nil;
    mz_zip_reader_entry_get_hash := nil;
    mz_zip_reader_entry_get_first_hash := nil;
    mz_zip_reader_entry_get_info := nil;
    mz_zip_reader_entry_is_dir := nil;
    //mz_zip_reader_entry_save := nil;
    //mz_zip_reader_entry_save_process := nil;
    mz_zip_reader_entry_save_file := nil;
    mz_zip_reader_entry_save_buffer := nil;
    mz_zip_reader_entry_save_buffer_length := nil;
    mz_zip_reader_save_all := nil;
    mz_zip_reader_set_pattern := nil;
    mz_zip_reader_set_password := nil;
    mz_zip_reader_set_raw := nil;
    mz_zip_reader_get_raw := nil;
    mz_zip_reader_get_zip_cd := nil;
    mz_zip_reader_get_comment := nil;
    mz_zip_reader_set_encoding := nil;
    mz_zip_reader_set_sign_required := nil;
    mz_zip_reader_set_overwrite_cb := nil;
    mz_zip_reader_set_password_cb := nil;
    mz_zip_reader_set_progress_cb := nil;
    mz_zip_reader_set_progress_interval := nil;
    mz_zip_reader_set_entry_cb := nil;
    mz_zip_reader_get_zip_handle := nil;
    mz_zip_reader_create := nil;
    mz_zip_reader_delete := nil;

    mz_zip_writer_is_open := nil;
    mz_zip_writer_open := nil;
    mz_zip_writer_open_file := nil;
    mz_zip_writer_open_file_in_memory := nil;
    mz_zip_writer_close := nil;
    mz_zip_writer_zip_cd := nil;
    mz_zip_writer_entry_open := nil;
    mz_zip_writer_entry_close := nil;
    mz_zip_writer_entry_write := nil;
    //mz_zip_writer_entry_sign := nil;
    //mz_zip_writer_add := nil;
    //mz_zip_writer_add_process := nil;
    //mz_zip_writer_add_info := nil;
    mz_zip_writer_add_buffer := nil;
    mz_zip_writer_add_file := nil;
    mz_zip_writer_add_path := nil;
    mz_zip_writer_copy_from_reader := nil;
    mz_zip_writer_set_password := nil;
    mz_zip_writer_set_comment := nil;
    mz_zip_writer_set_raw := nil;
    mz_zip_writer_get_raw := nil;
    mz_zip_writer_set_aes := nil;
    mz_zip_writer_set_compress_method := nil;
    mz_zip_writer_set_compress_level := nil;
    mz_zip_writer_set_follow_links := nil;
    mz_zip_writer_set_store_links := nil;
    mz_zip_writer_set_zip_cd := nil;
    mz_zip_writer_set_certificate := nil;
    mz_zip_writer_set_overwrite_cb := nil;
    mz_zip_writer_set_password_cb := nil;
    mz_zip_writer_set_progress_cb := nil;
    mz_zip_writer_set_progress_interval := nil;
    mz_zip_writer_set_entry_cb := nil;
    mz_zip_writer_get_zip_handle := nil;
    mz_zip_writer_create := nil;
    mz_zip_writer_delete := nil;
  finally
    GLock.Release;
  end;
end;

initialization
  GLock := TCriticalSection.Create;

finalization
  UnloadLibMiniZip;
  FreeAndNil(GLock);

end.