unit u_FileNameIteratorInFolderByMask;

interface

uses
  Windows,
  i_IFileNameIterator;

type
  TFileNameIteratorInFolderByMask = class(TInterfacedObject, IFileNameIterator)
  private
    FRootFolderName: WideString;
    FFolderNameFromRoot: WideString;
    FFileMask: WideString;
    FValidFindData: Boolean;
    FFindHandle: THandle;
    FFindFileData: TWIN32FindDataW;
    function GetRootFolderName: WideString;
    function Next(var AFileName: WideString): Boolean;
    procedure Reset;
  public
    constructor Create(
      ARootFolderName: WideString;
      AFolderNameFromRoot: WideString;
      AFileMask: WideString
    );
    destructor Destroy; override;
  end;

implementation

{ TFileNameIteratorInFolderByMask }

constructor TFileNameIteratorInFolderByMask.Create(ARootFolderName,
  AFolderNameFromRoot, AFileMask: WideString);
begin
  FRootFolderName := ARootFolderName;
  FFolderNameFromRoot := AFolderNameFromRoot;
  FFileMask := AFileMask;
  FFindHandle := INVALID_HANDLE_VALUE;
  Reset;
end;

destructor TFileNameIteratorInFolderByMask.Destroy;
begin
  if not (FFindHandle = INVALID_HANDLE_VALUE) then begin
    Windows.FindClose(FFindHandle);
  end;
  inherited;
end;

function TFileNameIteratorInFolderByMask.GetRootFolderName: WideString;
begin
  Result := FRootFolderName;
end;

function TFileNameIteratorInFolderByMask.Next(
  var AFileName: WideString): Boolean;
begin
  Result := False;
  AFileName := '';
  if FValidFindData then begin
    repeat
      if (FFindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
        AFileName := FFolderNameFromRoot + FFindFileData.cFileName;
        Result := True;
        FValidFindData := Windows.FindNextFileW(FFindHandle, FFindFileData);
        Break;
      end;
    until not FValidFindData;
  end;
end;

procedure TFileNameIteratorInFolderByMask.Reset;
var
  VCurrFullFilesMask: WideString;
begin
  if not (FFindHandle = INVALID_HANDLE_VALUE) then begin
    Windows.FindClose(FFindHandle);
  end;
  VCurrFullFilesMask := FRootFolderName + FFolderNameFromRoot + FFileMask;
  FFindHandle := Windows.FindFirstFileW(PWideChar(VCurrFullFilesMask), FFindFileData);
  FValidFindData := not (FFindHandle = INVALID_HANDLE_VALUE);
end;

end.
