unit NTFiles;

interface

uses
  Windows;

const
  INVALID_FILE_ATTRIBUTES = -1;

// from windows 2000
function GetFileSizeEx(
  hFile: THandle;
  lpFileSize: PLargeInteger
): BOOL; stdcall; external kernel32;

function OpenExistingFileReadOnly(
  const AFilename: String;
  const ARaiseIfFailed: Boolean
): THandle;

function OpenExistingFileReadOnlyA(
  const AFilename: AnsiString;
  const ARaiseIfFailed: Boolean
): THandle;

function OpenExistingFileReadOnlyW(
  const AFilename: WideString;
  const ARaiseIfFailed: Boolean
): THandle;

function CreateOrOpenFileReadWrite(
  const AFilename: String;
  const ARaiseIfFailed: Boolean
): THandle;

function CreateOrOpenFileWriteOnly(
  const AFilename: String;
  const ARaiseIfFailed: Boolean
): THandle;

function CreateDirIfNotExists(
  const AFilename: string;
  out AWasNotExist: Boolean
): Boolean;

function ReadFileAttributes(
  const AFilename: AnsiString;
  const AInfo: PWin32FileAttributeData
): Boolean; overload;

function ReadFileAttributes(
  const AFilename: WideString;
  const AInfo: PWin32FileAttributeData
): Boolean; overload;

function PrepareFileName(const ASource: string): string;

function NTFileExists(const AFilename: string): Boolean;

function OpenExistingReadOnlyInFolderW(
  const ARootHandle: THandle;
  const AFilename: WideString
): THandle;

function ReadExistingFileInFolderW(
  const ARootHandle: THandle;
  const AFilename: WideString;
  const AMaxSize: Integer;
  out AContent: AnsiString
): Boolean;

implementation

uses
  NativeNTAPI,
  SysUtils;

procedure _CheckLastError(
  const ARaiseIfFailed: Boolean;
  const AHandle: THandle
);
begin
  if ARaiseIfFailed and (INVALID_HANDLE_VALUE = AHandle) then begin
    RaiseLastOSError;
  end;
end;

function OpenExistingFileReadOnly(
  const AFilename: String;
  const ARaiseIfFailed: Boolean
): THandle;
begin
  Result := CreateFile(
    PChar(AFilename),
    GENERIC_READ,
    FILE_SHARE_READ,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  _CheckLastError(ARaiseIfFailed, Result);
end;

function OpenExistingFileReadOnlyA(
  const AFilename: AnsiString;
  const ARaiseIfFailed: Boolean
): THandle;
begin
  Result := CreateFileA(
    PAnsiChar(AFilename),
    GENERIC_READ,
    FILE_SHARE_READ,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  _CheckLastError(ARaiseIfFailed, Result);
end;

function OpenExistingFileReadOnlyW(
  const AFilename: WideString;
  const ARaiseIfFailed: Boolean
): THandle;
begin
  Result := CreateFileW(
    PWideChar(AFilename),
    GENERIC_READ,
    FILE_SHARE_READ,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  _CheckLastError(ARaiseIfFailed, Result);
end;

function CreateOrOpenFileReadWrite(
  const AFilename: String;
  const ARaiseIfFailed: Boolean
): THandle;
begin
  // open or create new
  Result := CreateFile(
    PChar(AFilename),
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ,
    nil,
    CREATE_ALWAYS,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  _CheckLastError(ARaiseIfFailed, Result);
end;

function CreateOrOpenFileWriteOnly(
  const AFilename: String;
  const ARaiseIfFailed: Boolean
): THandle;
begin
  Result := CreateFile(
    PChar(AFilename),
    GENERIC_WRITE,
    (FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE),
    nil,
    OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
end;

function CreateDirIfNotExists(
  const AFilename: string;
  out AWasNotExist: Boolean
): Boolean;
var
  i: integer;
  VPath: string;
begin
  i := LastDelimiter(PathDelim, AFilename);
  VPath := copy(AFilename, 1, i);
  Result := DirectoryExists(VPath);
  if (not Result) then begin
    Result := ForceDirectories(VPath);
    AWasNotExist := True;
  end;
end;

function ReadFileAttributes(
  const AFilename: AnsiString;
  const AInfo: PWin32FileAttributeData
): Boolean;
begin
  Result := (GetFileAttributesExA(PAnsiChar(AFilename), GetFileExInfoStandard, AInfo) <> False);
end;

function ReadFileAttributes(
  const AFilename: WideString;
  const AInfo: PWin32FileAttributeData
): Boolean;
begin
  Result := (GetFileAttributesExW(PWideChar(AFilename), GetFileExInfoStandard, AInfo) <> False);
end;

function PrepareFileName(const ASource: string): string;
var
  i: Integer;
  VOkCharsFound: Boolean; // cannot create file with name = '..'
begin
  Result := Trim(ASource);
  if (0 < Length(Result)) then begin
    // replace special chars
    VOkCharsFound := False;
    for i := 1 to Length(Result) do
    case Result[i] of
      #0..#31,'\','/',':','*','?','"','>','<','|': begin
        Result[i] := '-';
        VOkCharsFound := True;
      end;
      #160: begin
        Result[i] := ' ';
        VOkCharsFound := True;
      end;
      '.': begin
        // nothing
      end;
      else begin
        VOkCharsFound := True;
      end;
    end;
    // check for special names
    if (not VOkCharsFound) then begin
      Result := '-' + Result;
    end;
  end;
end;

function NTFileExists(const AFilename: string): Boolean;
var
  VCode: Integer;
  //VLastError: Cardinal;
begin
{$IFDEF MSWINDOWS}
  VCode := GetFileAttributes(PChar(AFilename));
  Result := (VCode <> INVALID_FILE_ATTRIBUTES);
  if Result then begin
    Result := ((FILE_ATTRIBUTE_DIRECTORY and VCode) = 0);
  end else begin
    //VLastError := GetLastError;
    //Result := (0 = VLastError);
    //Result := False;
  end;
{$ELSE}
  Result := SysUtils.FileExists(AFilename);
{$ENDIF}
end;

function OpenExistingReadOnlyInFolderW(
  const ARootHandle: THandle;
  const AFilename: WideString
): THandle;
var
  VObj: OBJECT_ATTRIBUTES;
  VFileNameU: UNICODE_STRING;
  VResult: NTSTATUS;
  VBlock: IO_STATUS_BLOCK;
begin
  FillChar(VFileNameU, SizeOf(VFileNameU), 0);
  FillChar(VObj, SizeOf(VObj), 0);
  VObj.Length := SizeOf(VObj);
  VObj.RootDirectory := ARootHandle;

  try
    if (0 = ARootHandle) then begin
      // абсолютный путь - надо преобразовать в родной формат
      if (RtlDosPathNameToNtPathName_U(PCWSTR(AFilename), @VFileNameU, nil, nil) = False) then begin
        // ошибка
        Exit;
      end;
    end else begin
      // относительный путь - не преобразовываем
      with VFileNameU do begin
        Length_ := Length(AFilename) * SizeOf(WideChar);
        MaximumLength := Length_;
        Buffer := PWideChar(AFilename);
      end;
    end;

    VObj.ObjectName := @VFileNameU;

    VResult := NtCreateFile(
      @Result,
      (FILE_READ_ACCESS or FILE_READ_ATTRIBUTES),
      @VObj,
      @VBlock,
      nil,
      0,
      (FILE_SHARE_READ or FILE_SHARE_WRITE),
      FILE_OPEN,
      0,
      nil,
      0
    );
  finally
    // free buffer
    if (0 = ARootHandle) then begin
      if (VFileNameU.Buffer <> nil) then begin
        Assert(RtlFreeHeap(GetProcessHeap(), 0, VFileNameU.Buffer) <> False);
      end;
    end;
  end;

  if (VResult < STATUS_SUCCESS) then begin
    // failed
    Result := 0;
  end;
end;

function ReadExistingFileInFolderW(
  const ARootHandle: THandle;
  const AFilename: WideString;
  const AMaxSize: Integer;
  out AContent: AnsiString
): Boolean;
var
  VFileHandle: THandle;
  VStatus: NTSTATUS;
  VRead_IOSB: IO_STATUS_BLOCK;
  VOffset: LARGE_INTEGER;
begin
  Result := False;
  VOffset := 0;
  
  // open file
  VFileHandle := OpenExistingReadOnlyInFolderW(
    ARootHandle,
    AFilename
  );

  if (0 <> VFileHandle) then
  try
    // prepare buffer
    SetLength(AContent, AMaxSize);
    
    // read opened file
    VStatus := NtReadFile(
      VFileHandle,
      0,
      nil,
      nil,
      @VRead_IOSB,
      @(AContent[1]),
      AMaxSize,
      @VOffset,
      nil
    );

    if (VStatus >= STATUS_SUCCESS) then begin
      // read some data
      SetLength(AContent, VRead_IOSB.Information);
      Inc(Result);
    end;
  finally
    NtClose(VFileHandle);
  end;
end;

end.
