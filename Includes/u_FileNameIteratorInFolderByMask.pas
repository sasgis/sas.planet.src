unit u_FileNameIteratorInFolderByMask;

interface

uses
  Windows,
  i_FileNameIterator;

type
  TFileNameIteratorInFolderByMask = class(TInterfacedObject, IFileNameIterator)
  private
    FRootFolderName: WideString;
    FFolderNameFromRoot: WideString;
    FFileMask: WideString;
    FFilesOnly: Boolean;
    FValidFindData: Boolean;
    FFindHandle: THandle;
    FFindFileData: TWIN32FindDataW;
  protected
    function IsNeedProcess(AFindFileData: TWIN32FindDataW): Boolean; virtual;
  protected
    function GetRootFolderName: WideString;
    function Next(var AFileName: WideString): Boolean;
    procedure Reset;
  public
    constructor Create(
      const ARootFolderName: WideString;
      const AFolderNameFromRoot: WideString;
      const AFileMask: WideString;
      const AFilesOnly: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TFileNameIteratorInFolderByMask }

constructor TFileNameIteratorInFolderByMask.Create(
  const ARootFolderName: WideString;
  const AFolderNameFromRoot: WideString;
  const AFileMask: WideString;
  const AFilesOnly: Boolean
);
begin
  FRootFolderName := ARootFolderName;
  FFolderNameFromRoot := AFolderNameFromRoot;
  if FFolderNameFromRoot <> '' then begin
    FFolderNameFromRoot := FFolderNameFromRoot + PathDelim;
  end;
  FFileMask := AFileMask;
  FFindHandle := INVALID_HANDLE_VALUE;
  FFilesOnly := AFilesOnly;
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

function TFileNameIteratorInFolderByMask.IsNeedProcess(
  AFindFileData: TWIN32FindDataW): Boolean;
begin
  if FFilesOnly then begin
    Result := (AFindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0;
  end else begin
    Result := (WideCompareStr(AFindFileData.cFileName, '.') <> 0) and
      (WideCompareStr(AFindFileData.cFileName, '..') <> 0);
  end;
end;

function TFileNameIteratorInFolderByMask.Next(
  var AFileName: WideString): Boolean;
begin
  Result := False;
  AFileName := '';
  if FValidFindData then begin
    repeat
      if IsNeedProcess(FFindFileData) then begin
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
