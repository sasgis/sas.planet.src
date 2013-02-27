unit u_MappedFile;

interface

uses
  Windows,
  SysUtils;

type
  TMappedFile = class
  private
    FMapping: THandle;
    FContent: Pointer;
    FSize: Integer;
    procedure MapFile(const AFileName: WideString);
  public
    constructor Create(const AFileName: WideString);
    destructor Destroy; override;
    property Content: Pointer read FContent;
    property Size: Integer read FSize;
  end;

implementation

function FileExistsLongFileNames(const FileName: WideString): Boolean;
begin
  if Length(FileName) < 2 then begin
    Result := False;
    Exit;
  end;
  if CompareMem(@FileName[1], @WideString('\\')[1], 2) then begin
    Result := (GetFileAttributesW(PWideChar(FileName)) and FILE_ATTRIBUTE_DIRECTORY = 0);
  end else begin
    Result := (GetFileAttributesW(PWideChar(WideString('\\?\' + FileName))) and FILE_ATTRIBUTE_DIRECTORY = 0)
  end;
end;

{ TMappedFile }

constructor TMappedFile.Create(const AFileName: WideString);
begin
  inherited Create;
  if FileExistsLongFileNames(AFileName) then begin
    MapFile(AFileName)
  end else begin
    raise Exception.Create('File "' + AFileName + '" does not exists.');
  end;
end;

destructor TMappedFile.Destroy;
begin
  if Assigned(FContent) then begin
    UnmapViewOfFile(FContent);
    CloseHandle(FMapping);
  end;
  inherited Destroy;
end;

procedure TMappedFile.MapFile(const AFileName: WideString);
var
  VFileHandle: THandle;
begin
  if CompareMem(@(AFileName[1]), @('\\'[1]), 2) then begin
    { Allready an UNC path }
    VFileHandle := CreateFileW(
      PWideChar(AFileName),
      GENERIC_ALL,
      FILE_SHARE_READ,
      nil,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
    );
  end else begin
    VFileHandle := CreateFileW(
      PWideChar(WideString('\\?\' + AFileName)),
      GENERIC_ALL,
      FILE_SHARE_READ,
      nil,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
    );
  end;
  if VFileHandle <> INVALID_HANDLE_VALUE then
  try
    FSize := GetFileSize(VFileHandle, nil);
    if FSize > 0 then begin
      FMapping := CreateFileMappingW(
        VFileHandle,
        nil,
        PAGE_READWRITE,
        0,
        0,
        nil
      );
      {$WARN SYMBOL_PLATFORM OFF}
      Win32Check(FMapping <> 0);
      {$WARN SYMBOL_PLATFORM ON}      
      FContent := MapViewOfFile(FMapping, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
      {$WARN SYMBOL_PLATFORM OFF}
      Win32Check(FContent <> nil);
      {$WARN SYMBOL_PLATFORM ON}
    end else begin
      raise Exception.Create('File "' + AFileName + '" have size of 0 bytes!');
    end;
  finally
    CloseHandle(VFileHandle);
  end;
end;

end.
